(** Yummy Yummy pancakes.

    @author eaburns
    @since 2009-12-02
*)


type positions = int array


let int_array_to_string ary =
  (** [int_array_to_string ary] gets the string representation. *)
  Array.fold_left (fun s v -> Wrutils.str "%s %d" s v) "" ary


let int_array_to_record_string ary =
  (** [int_array_to_string ary] gets the string representation. *)
  let accum = ref (Wrutils.str "%d" ary.(0)) in
    for i = 1 to (Array.length ary) - 1 do
      accum := Wrutils.str "%s;%d" !accum ary.(i)
    done;
    !accum


let cost_function_by_name ?abstd ncakes name =
  (** [cost_function_by_name ?non_abstd ncakes name] get the cost
      function given the name.  [not_abstd] is an (optional) array of
      pancakes that have *not* been abstracted away.  If [non_abstd]
      is given the cost function uses the values of the 'non_abstd'
      pancakes when computing the cost of an abstracted cake. *)
  let abstracted_cakes = match abstd with
    | Some cakes ->
	Array.sort compare cakes;
	Array.map float cakes
    | None -> [||]
  in
  let fs = [
    (* IMPORTANT: These need to work with abstracted states too. A
       pancake value of ~-1 is abstracted away so ignore its cost (or
       treat it as the minimum possible value).

       ALSO NOTE: If the values get smaller when fewer pancakes are
       abstracted then this will not work with the PDB. *)
    "unit", (fun _ _ -> 1.);
    (* Unit cost moves *)

    "sum", (fun s n ->
	      let sum = ref 0. in
	      let j = ref 0 in
		for i = 0 to n - 1 do
		  if s.(i) >= 0
		  then sum := !sum +. ((float s.(i)) +. 1.)
		  else if abstracted_cakes <> [||] then begin
		    sum := !sum +. (abstracted_cakes.(!j) +. 1.);
		    incr j;
		  end
		done;
		!sum);
    (* The cost a flip is the sum of the numbers of the cakes being
       flipped + 1. *)

    "inv_sum", (fun s n ->
		  let sum = ref 0. in
		  let j = ref 0 in
		    for i = 0 to n - 1 do
		      if s.(i) >= 0
		      then sum := !sum +. 1. /. ((float s.(i)) +. 1.)
		      else if abstracted_cakes <> [||] then begin
			sum := (!sum
				+. (1. /. (abstracted_cakes.(!j)) +. 1.));
			incr j;
		      end
		    done;
		    !sum);
    (* The cost a flip is the inverse of the sum of the numbers of the
       cakes being flipped + 1. *)

    "wtd_sum", (fun s n ->
		  let sum = ref 0. in
		  let j = ref 0 in
		  let ncakes = float (Array.length s) in
		    for i = 0 to n - 1 do
		      if s.(i) >= 0
		      then sum := !sum +. (((float s.(i)) +. 1.) /. ncakes)
		      else if abstracted_cakes <> [||] then begin
			sum :=
			  (!sum
			   +. ((abstracted_cakes.(!j) +. 1.) /. ncakes));
			incr j;
		      end
		    done;
		    !sum);
    (* The cost a flip is the inverse of the sum of the numbers of the
       cakes being flipped. *)

    "sqrt_sum", (fun s n ->
		   let sum = ref 0. in
		   let j = ref 0 in
		     for i = 0 to n - 1 do
		       if s.(i) >= 0
		       then sum := !sum +. (sqrt ((float s.(i)) +. 1.))
		       else if abstracted_cakes <> [||] then begin
			 sum :=
			   !sum +. (sqrt ((abstracted_cakes.(!j) +. 1.)));
			 incr j;
		       end
		     done;
		     !sum);
    (* The cost a flip is the sum of sqrts of the numbers of the cakes
       being flipped. *)
  ] in
    try List.assoc name fs
    with Not_found ->
      invalid_arg
	(Wrutils.str "%s is an invalid cost function try one of:%s"
	   name
	   (List.fold_left (fun s (x,_) -> Wrutils.str "%s %s" s x) "" fs))


let goal ncakes = Array.init ncakes Fn.identity
  (** [goal ncakes] gets the goal state for [ncakes]. *)


let random_state n =
  (** [random_state n] gets a random state with [n] pancakes. *)
  let inst = goal n in
    Wrarray.permute inst;
    Verb.pr Verb.toplvl "Random instance:%s\n%!" (int_array_to_string inst);
    inst

(*
let instance_type s =
  (** [instance_type s] gets the type of the a set of pancake
      positions. *)
  let rec do_get_type s i =
    if i < (Array.length s)
    then if s.(i) = 0 then i else do_get_type s (i + 1)
    else assert false
  in do_get_type s 0
*)

let instance_type s =
  (** [instance_type s] gets the type of the a set of pancake
      positions by counting the number of pancakes that are out of
      position. *)
  let rec do_get_type s sum i =
    if i = (Array.length s)
    then sum
    else
      if s.(i) = i
      then do_get_type s sum (i + 1)
      else do_get_type s (sum + 1) (i + 1)
  in do_get_type s 0 0


let make_instances ncakes num =
  (** [make_instances ncakes num] makes [num] random instances with
      [ncakes] pancakes each. *)
  let root = "./group/data/pancake" in
    for i = 0 to num - 1 do
      let state = random_state ncakes in
      let path = Rdb.path_for root [ "type", "instance";
				     "ncakes", string_of_int ncakes;
				     "num", string_of_int i;
				   ]
      in

	Verb.pr Verb.often "inst=%s\n\t%s\n%!"
	  path (int_array_to_string state);
	if not (Sys.file_exists path)
	then
	  (Wrio.with_outfile path (fun o ->
				     Printf.fprintf o "%d\n" ncakes;
				     Wrarray.write_ints_line o state))
    done


let do_flip s n =
  (** [do_flip s n] flips the top [n] pancakes given the set [s] of
      burnt pancakes.  The result is the new stack of pancakes. *)
  let s' = Array.copy s in

  let nswaps = (n / 2) + if Math.divisible n 2 then 0 else 1 in
    (* I don't think that this if clause is needed, I think [n/2] is
       sufficient. *)

    for i = 0 to nswaps - 1 do
      let j = (n - 1) - i in
      let a = s.(i) and b = s.(j) in
	s'.(i) <- b;
	s'.(j) <- a;
    done;
    s'


let do_expand ncakes cost =
  (** [do_expand ncakes] makes an expand function that operates on the
      pancake set (not the state). *)
  (fun s g ->
     let lst = ref [] in
       for n = 2 to ncakes do
	 let s' = do_flip s n in lst := (s', g +. (cost s n), n) :: !lst
       done;
       !lst)


module PDB = struct

  let abstracted_cakes size ncakes =
    (** [abstracted_cakes size ncakes] get an array of the abstracted
	pancake numbers. *)
    Array.init size Fn.identity

  let make_phi abstd = Pdb_ints.positions_of abstd
    (** [make_phi abstd] makes an abstraction function for the given
	size. *)

  let make ncakes cost_name size =
    (** [make ncakes cost_name size] makes a pancake PDB. *)
    let abstd = abstracted_cakes size ncakes in
    let phi = make_phi abstd in
    let cost_fun = cost_function_by_name ~abstd ncakes cost_name in
    let expand node cost =
      let s = Array.make ncakes ~-1 in
	for i = 0 to size - 1 do s.(node.(i)) <- i; done;
	List.map
	  (fun (child, child_cost, _) -> (phi child), child_cost)
	  (do_expand ncakes cost_fun s cost)
    in
    let goal = phi (goal ncakes) in
      Pdb_ints.make "first" expand ncakes goal Fn.identity


  let load ?(should_make=false) ncakes cost_name size =
    let make_fun =
      if should_make
      then Some (fun () -> make ncakes cost_name size)
      else None
    in
    let attrs = [ "ncakes", string_of_int ncakes;
		  "additive", "false";
		  "cost", cost_name  ] in
      Pdb.fetch ?make:make_fun "pancake" attrs "first" size


  let lookup = Pdb_ints.lookup
    (** [lookup pdb phi s] looks up a state in the pattern
	database. *)

end


let h_gap positions =
  (** [h_gap positions] the gap heuristic. *)
  let ncakes = Array.length positions in
  let gaps = ref 0 in
    for i = 0 to ncakes - 1 do
      if i = ncakes - 1
      then begin
	if positions.(i) <> i then gaps := !gaps + 1;
      end else begin
	let abs_diff = abs (positions.(i) - positions.(i + 1)) in
	if abs_diff > 1 then gaps := !gaps + 1;
      end
    done;
    float !gaps


type state = {
  cakes : positions;
  (* The pancakes. *)

  depth : int;
  (* The depth (for the [sol_length] function). *)

  nflipped : int;
  (* The number of pancakes that was flipped by the parent to get
     here. *)

  this_h : float;
  (* the heuristic *)
}


let read inch =
  (** [read inch] loads the initial state of the instance from the
      given channel. *)
  let ncakes = int_of_string (input_line inch) in
  let cakes = Wrarray.read_ints inch ncakes in
    {
      cakes = cakes;
      depth = 0;
      nflipped = ~-1;
      this_h = (h_gap cakes);
    }


let load file = Wrio.with_infile file read


let npancakes s = Array.length s.cakes
  (** [npancakes s] get the number of pancakes given a state. *)


let key s = s.cakes
  (** [key s] is the hash table key value. *)


let make_hash n_cakes = 
  Hashtbl.hash_param n_cakes n_cakes


let eq a b = 
 let rec do_eq (a:int array) b i =
    if i >= (Array.length a) then true
    else a.(i) = b.(i) && do_eq a b (i + 1)
  in do_eq a b 0


let print_key k = Verb.pr Verb.always "%s" (int_array_to_string k)
  (** [print_key k] prints the key. *)


let is_goal s =
  (** [is_goal s] tests if [s] is a goal state (all pancakes are in
      their correct position. *)
  let n = npancakes s in
  let rec do_is_goal i =
    if i < n
    then s.cakes.(i) = i && do_is_goal (i + 1)
    else true
  in do_is_goal 0

let make_arb_goal s =
  let ncakes = (npancakes s) - 1 in
    (fun n ->
       let v = ref true in
	 for i = 0 to ncakes do
	   v := !v && (n.cakes.(i) = s.cakes.(i))
	 done;
	 !v)


let adj n1 n2 = 
  let diff = (n1 - n2) in
    if (diff = 1)
    then true
    else if (diff = (-1))
    then true
    else false


let inversion_delta arr ix = 
  if(ix = (Array.length arr))
  then 
    (
      if(arr.(0) = (ix - 1))
      then 1.0
      else if (arr.(ix - 1) = (ix - 1))
      then (-1.0)
      else 0.0
    )
  else 
    (
      let prior_match = adj arr.(0) arr.(ix) in
      let current_match = adj arr.(ix - 1) arr.(ix) in
	if(current_match && prior_match) then
	  0.0
	else if (current_match && (not prior_match)) then
	  (-1.0)
	else if (prior_match && (not current_match)) then
	  1.0
	else 
	  0.0
    )


let make_expand ncakes cost h =
  (** [make_expand ncakes cost h] makes a function that expands a
      state. *)
  (fun s g ->
     let rec get_children ?(accum=[]) = function
       | [] -> accum
       | (_, _, nflipped) :: tl when nflipped = s.nflipped ->
	   (* prune the parent *)
	   get_children ~accum:accum tl
       | (s', g, n) :: tl ->
	   let h_prep = (inversion_delta s' n) +. s.this_h in
(*
	   let h_gap_check = h_gap s' in
	     assert (h_prep = h_gap_check);
*)
	     let state = 
	       { 
		 cakes = s'; 
		 depth = s.depth + 1; 
		 nflipped = n;
		 this_h = h_prep
	       }, g
	     in get_children ~accum:(state::accum) tl
     in
     let cs = get_children (do_expand ncakes cost s.cakes g) in
       cs)


let sol_length s = s.depth
  (** [sol_length s] gets the solution length of state [s]. *)


(*
let node_type s = instance_type s.cakes
  (** [node_type s] gets the type of the node. *)

and type_max ncakes = ncakes
  (** [type_max] gets the maximum type. *)
*)


let node_type s = 0
  (** [node_type s] gets the type of the node. *)


