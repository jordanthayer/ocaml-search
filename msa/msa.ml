(* $Id: msa.ml,v 1.1 2008/01/19 21:25:27 ruml Exp $
   search space for MSA
*)


type cell = {
  indices : int array;
  (* root points to self *)
  parent : cell;
}


let make_initial_indices i =
  Array.make (Msa_instance.num_seqs i) 0


let make_root i =
  let rec r = { indices = make_initial_indices i;
		parent = r; } in
    r


exception Not_done

let make_goal_p instance =
  let nmo = (Msa_instance.num_seqs instance) - 1
  and lens = Array.map String.length instance in
    (fun c ->
       try
	 let indices = c.indices in
	   for i = 0 to nmo do
	     if indices.(i) <> lens.(i) then
	       raise Not_done
	   done;
	   true
       with Not_done -> false)


let incr_cost prev i k consumed indices instance =
  (** the incremental cost of assigning sequence [i] as in [consumed].
      assumes all higher indexed sequences have already been assigned. *)
  let cost = ref prev in
    if consumed.(i) then
      let char = instance.(i).[(indices.(i))-1] in
	for j = i+1 to k-1 do
	  if consumed.(j) then
	    (if char != instance.(j).[(indices.(j))-1] then
	       cost := !cost + Msa_instance.subst_cost)
	  else
	    cost := !cost + Msa_instance.gap_cost
	done
    else
      for j = i+1 to k-1 do
	if consumed.(j) then
	  cost := !cost + Msa_instance.gap_cost
      done;
    !cost


let child indices parent =
  { indices = Array.copy indices;
    parent = parent; }


let make_expand instance =
  let k = Msa_instance.num_seqs instance in
    (* which indices were incremented *)
  let consumed = Array.make k false in
    (fun parent g ->
       (* each child represents adding a column to the solution matrix -
	  the question is which sequences to consume a character from, rather
	  than inserting a gap (and not incrementing their index).  These
	  possibilies are enumerated here using a depth-first tree traversal,
	  skipping the one leaf that represents all gaps.  We assume the
	  sum-of-pairs objective, which means that as we determine what to do
	  for each sequence, we can score it against each previously
	  determined sequence.  *)
       let children = ref []
       and a = parent.indices
	 (* skip first candidate child, which has no changes *)
       and skip = ref true in
       let rec dfs i cost =
	 (** consider whether to consume from sequence [i].  [cost] reflects
	     previous decisions so far. *)
	 if i < 0 then
	   if !skip then
	     skip := false
	   else
	     Wrutils.push ((child a parent), (float cost)) children
	 else
	   (dfs (i - 1) (incr_cost cost i k consumed a instance);
	    let old = a.(i) in
	      if old < (String.length instance.(i)) then
		(a.(i) <- old + 1;
		 consumed.(i) <- true;
		 dfs (i - 1) (incr_cost cost i k consumed a instance);
		 a.(i) <- old;
		 consumed.(i) <- false))
       in
	 dfs (k-1) (truncate g);
	 !children)


let key c =
  c.indices


let key_printer c =
  Array.iteri (fun ele i -> if i <> 0 then Verb.pe Verb.often " ";
		  Verb.pe Verb.often "%i" i) c.indices;
  Verb.pe Verb.often "\n"

(********** checking **********)


let build_solution i path =
  (** given list of index arrays, returns alignment matrix *)
  let sol_len = (List.length path) - 1
  and k = Msa_instance.num_seqs i in
  let sol = Array.init k (fun _ -> String.make sol_len '#')
  in
  let rec do_col col prev rest =
    match rest with
      [] -> sol
    | indices::rest ->
	for j = 0 to k-1 do
	  sol.(j).[col] <- (if indices.(j) = prev.(j) then
			      Msa_instance.gap
			    else
			      i.(j).[prev.(j)])
	done;
	do_col (col+1) indices rest
  in
    match path with
      [] -> failwith "no path?"
    | first::rest ->
	if not (Wrarray.all_equal 0 first) then
	  failwith "solution path doesn't start with 0 indices!";
	do_col 0 first rest


let print_trace sol =
  Array.iter print_endline sol


let check_goal i path cost =
  let sol = build_solution i path in
    Msa_instance.check i sol;
    let score = Msa_instance.score sol in
      if cost <> score then
	failwith (Wrutils.str "claimed score of %d is not %d" cost score);
      sol


(***************** heuristic ****************)


let all_suffixes s =
  (** array of all non-empty suffixes (starting with full string) *)
  let n = String.length s in
    Array.init (n+1) (fun start -> String.sub s start (n - start))


let all_suffix_costs a a_suffs b b_suffs =
  (** returns a 2d array of costs of the optimal alignments of all possible
    suffixes of [a] and [b] *)
  let an = String.length a
  and bn = String.length b in
    Array.init (an+1)
      (fun a_start ->
	 let a = a_suffs.(a_start) in
	   Array.init (bn+1)
	     (fun b_start ->
		let b = b_suffs.(b_start) in
		  Msa_dp.pair_optimal_cost a b))


let make_all_suffix_tables instance =
  (** returns a triangular matrix with the first index >= the second *)
  let suffs = Array.map all_suffixes instance in
    Array.init (Msa_instance.num_seqs instance)
      (fun i ->
	 Array.init i
	 (fun j ->
	    all_suffix_costs instance.(i) suffs.(i) instance.(j) suffs.(j)))


let table_cache = Cache.create 2

let all_suffix_tables instance =
  try
    Cache.find table_cache instance
  with Not_found -> make_all_suffix_tables instance

let set_table instance tables =
  Cache.insert table_cache instance tables


let make_pair_combining_h instance =
  (** combine pair-wise optimal alignments *)
  let tables = all_suffix_tables instance
  and k = Msa_instance.num_seqs instance in
    (fun indices ->
       let cost = ref 0 in
	 for i = 1 to k-1 do
	   for j = 0 to i-1 do
	     let table = tables.(i).(j) in
	       cost := !cost + table.(indices.(i)).(indices.(j))
	   done
	 done;
	 (float !cost))


let make_min_gap_cost instance =
  (** simple lower bound *)
  let g0 = String.length instance.(0)
  and g1 = String.length instance.(1) in
    (fun indices ->
       let off_diag = abs ((g0 - indices.(0)) - (g1 - indices.(1))) in
	 float (off_diag * Msa_instance.gap_cost))


let make_h instance =
  let h = (if Msa_instance.num_seqs instance > 2 then
	     make_pair_combining_h instance
	   else
	     make_min_gap_cost instance) in
    (fun c -> h c.indices)


(***************** distance and cost ****************)


let assert_not_two i =
  if Msa_instance.num_seqs i <= 2 then
    failwith "wanted more than two sequences"


let min_goal_depth i =
  (** minimum depth of goal = maximum number of characters in any
      sequence *)
  Array.fold_left (fun prev s ->
		     Math.imax prev (String.length s))
    0 i


let make_max_remaining i =
  (** maximum number of characters left in any sequence, which is a lower
    bound on the distance to any goal *)
  let nmo = (Msa_instance.num_seqs i) - 1
  and lens = Array.map String.length i in
    (fun indices ->
       let max = ref (lens.(0) - indices.(0)) in
	 for i = 1 to nmo do
	   let r = lens.(i) - indices.(i) in
	     if r > !max then max := r
	 done;
	 !max)


let make_cheap_hd i =
  (** cost and distance along cheapest path to goal.  actually it is a
      lower bound on the cheapest goal and a lower bound on the distance to any
      goal. *)
  (* h/2 is also a bound since cost must come from some moves.  but
     not max of pair-wise path lengths, because shorter can be cheaper
     in multi-way.  NOTE: code from bugsy paper had h not h/2!?! *)
  assert_not_two i;
  let h = make_pair_combining_h i
  and r = make_max_remaining i in
    (fun c ->
       let h = h c.indices
       and r = float (r c.indices) in
	 h, Math.fmax (h /. 2.) r)

(*
  let make_close_hd i =
(** cost and distance along shortest path to goal *)
  (* there are possibly many (O(n^(k-1))) shortest paths to goal.  in two
     d, there are (dist.

     difference between each diff and the max-diff(=distance to goal).
     these rests can be taken at any time.  multiply them all together.

     for >2d, max of (min of pair distances) and min_distance_to_goal

     cost on min path: for 2d: # gaps
                       for higher: assume matches everywhere there's a gap
  *)
  (* just choose one and calculate its cost? *)
  let r = make_max_remaining i in
    (fun c ->
       foo, (r c)
*)


(******** Depth ********)

let prob_depth i =
  let depth = ref 0 in
    Array.iter (fun x ->
		  depth := !depth + (String.length x))
      i;
    !depth


(******** instance I/O (caches heuristic tables) **********)


let fprint ch i =
  Wrutils.pf ch "%d\n" (Msa_instance.num_seqs i);
  Array.iter (fun s -> output_string ch s; output_char ch '\n') i;
  Marshal.to_channel ch (make_all_suffix_tables i) [Marshal.No_sharing]


let save path i =
  Wrio.with_outfile path (fun ch -> fprint ch i)


let read ch =
  let n = Wrio.input_int ch in
  let i = Array.init n (fun _ -> input_line ch) in
  let t = (Marshal.from_channel ch : int array array array array) in
    set_table i t;
    i


let load path i =
  Wrio.with_infile path read


(***************** algorithms ****************)


let rec unwind_path tail n =
  if n.parent == n then
    n.indices::tail
  else
    unwind_path (n.indices::tail) n.parent


let unwrap_sol s =
  match s with
      None -> None
    | Some (node,cost) ->
	if not (Math.integral_p cost) then
	  failwith "non-integral sol cost?";
	Some ((unwind_path [] node), (truncate cost))


let default_interface instance limit =
  let hd = make_cheap_hd instance in
  Search_interface.make
    ~h:(make_h instance)
    ~d:(fun n -> snd (hd n))
    ~hd:hd
    ~key:key
    ~goal_p:(make_goal_p instance)
    ~halt_on:limit
    ~equals:(=)
    ~domain_expand:(make_expand instance)
    Search_interface.Sequence
    (make_root instance)
    (fun _ _ -> false)
    (fun _ -> ())
(* EOF *)
