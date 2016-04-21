(*
  H^m family of heuristic. Implementation of H^2 in this file.
*)

open Tpl_ground

type reg_set = {
  (* atoms in this regresion set *)
  props : ground_atom list;
  (* actions associated with this reg_set *)
  acts : ground_action list;
  (* pair gives the max (best) value; can represent a single atom
     also; use to help to make the update easier (only update when the
     change happen on max_pair)*)
  mutable max_pair : int * int;
  (* best value of this set *)
  mutable value : float;
  (* if the value of all pairs less than infinity *)
  mutable supported : int;
}


let dummy_reg_set = {
  props = [];
  acts = [];
  max_pair = (-1, -1);
  value = 0.;
  supported = 0;
}


type pair = {
  (* current best achievable time *)
  mutable time : float;
  (* pairs * index of the in-link for that pair containing this pair *)
  mutable out_links : (int * int * int) list;
  (* updating rules using "in" edges * regression distance to that reg_set *)
  in_links : (reg_set * float) array;
}

(********** debug printing ***********)

let print_update (f1,f2) t1 t2 =
  let lit1 = lookup_atom f1 and lit2 = lookup_atom f2 in
    Verb.pe 4 "(%s,%s): %f -> %f\n" (Tpl_domain.lit_str lit1)
      (Tpl_domain.lit_str lit2) t1 t2;
    flush_all ()


let print_regset (rs,d) =
  List.iter (fun atom -> Verb.pe 4 " %s" (atom_str atom)) rs.props;
  Verb.pe 4 " | ";
  List.iter (fun act -> Verb.pe 4 "%s" act.printout) rs.acts;
  Verb.pe 4 " | ";
  Verb.pe 4 "(%f,%d) - %f\n" rs.value rs.supported d


let print_pair pairs (i,j) =
  Verb.pe 4 "\n[%d,%d] Atoms: " i j;
  Verb.pe 4 " %s %s \n" (atom_str i) (atom_str j);
  Verb.pe 4 "In links:\n";
  Array.iter (fun rs -> print_regset rs) pairs.(i).(j).in_links;
  Verb.pe 4 "Out links:\n";
  List.iter (fun (i,j,k) -> Verb.pe 4 " (%d,%d,%d) " i j k)
    pairs.(i).(j).out_links;
  Verb.pe 4 "\n";
  flush_all ()


let print_pairs pairs nF =
  flush_all ();
  for i = 0 to (nF-1) do
    for j = 0 to (nF-1) do
      print_pair pairs (i,j)
    done;
  done


let start_h2, set_time, get_time =
  let start_time = ref 0. and setup_time = ref 0. and  h2_time = ref 0. in
    (fun () ->
       start_time := Unix.gettimeofday ()),
    (fun option ->
       if option = 1 then
	 setup_time := Unix.gettimeofday ()
       else if option = 2 then
	 h2_time := Unix.gettimeofday ()
       else failwith "wrong option!"),
    (fun option ->
       if option = 1 then (!setup_time -. !start_time)
       else if option = 2 then (!h2_time -. !start_time)
       else if option = 3 then (!h2_time -. !setup_time)
       else failwith "wrong option(1)!")


(*** initialization **)

let num_pair_regress s1 s2 =
  (** calculate the number of states regressed from pair of actions.
    not (sup_only_i * sup_only_j) because there can be pairs of actions
    with same duration *)
  List.fold_left (fun n1 a1 ->
		    List.fold_left (fun n2 a2 ->
				      if not (compatible a1 a2) then n2
				      else if a1.dur = a2.dur then (n2+1)
				      else (n2+2))
		    n1 s2)
    0 s1


let init_reg_set atoms acts =
  let atoms = Wrlist.remove_dups Fn.identity atoms in
  let n = List.length atoms in
    { props = List.sort compare atoms;
      acts = acts;
      max_pair = (-1,-1);
      value = infinity;
      supported = n*(n+1)/2;}


let regress_ij (i,j) a =
  (** regress pair through action [a] supporting both (i,j) *)
  (init_reg_set a.pre [a], a.dur)


let regress_i (i,j) a =
  (** regress pair through action [a] supporting only i *)
  (init_reg_set (j::a.pre) [a], a.dur)


let regress_j (i,j) a =
  (** regress pair through action [a] supporting only j *)
  (init_reg_set (i::a.pre) [a], a.dur)


let regress_i_j (i,j) ai aj =
  (** regress pair through [ai] supporting [i] and [aj] supporting [j] *)
  if not (compatible ai aj) then []
  else let s = Wrlist.remove_dups Fn.identity
		   (List.merge compare ai.pre aj.pre) in
    if ai.dur = aj.dur then [(init_reg_set s [ai;aj],ai.dur)]
    else if ai.dur < aj.dur then
      [(init_reg_set s [ai;aj],ai.dur);(init_reg_set aj.pre [aj],aj.dur)]
    else [(init_reg_set s [ai;aj],aj.dur);(init_reg_set ai.pre [ai],ai.dur)]


let make_inlinks (i,j) sup_ij sup_i sup_j =
  (** actions: [sup_ij] have both (i,j) in its add list; sup_i, sup_j
    only support i or j *)
  let nij = List.length sup_ij
  and ni = List.length sup_i and nj = List.length sup_j in
  let n_ij = (num_pair_regress sup_i sup_j) in
  let num_in = nij + ni + nj + n_ij in
  let in_links = Array.make num_in (dummy_reg_set,0.) in
    ignore (List.fold_left (fun index a ->
			      in_links.(index) <- regress_ij (i,j) a;
			      index + 1)
	      0 sup_ij);
    ignore (List.fold_left (fun index a ->
			      in_links.(index) <- regress_i (i,j) a;
			      index + 1)
	      nij sup_i);
    ignore (List.fold_left (fun index a ->
			      in_links.(index) <- regress_j (i,j) a;
			      index + 1)
	      (nij + ni) sup_j);
    ignore (List.fold_left
	      (fun index ai ->
		 List.fold_left (fun i1 aj ->
				   let l = regress_i_j (i,j) ai aj in
				     if (List.length l) = 0 then i1
				     else
				       List.fold_left
					 (fun i2 s -> in_links.(i2) <- s;
					    i2+1)
					 i1 l)
		 index sup_j)
	      (nij + ni + nj) sup_i);
    in_links


let make_pair (i,j) achievers =
  (** create a pair structure for a pair (i,j) of facts *)
  let filter_da actions f =
    (* filter out action do not add or delete [f] *)
    List.filter (fun a -> not ((List.mem f a.delete) || (List.mem f a.add)))
      actions in
  let sup_i = achievers i and sup_j = achievers j in
  let sup_ij = List.filter (fun a -> List.mem a sup_j) sup_i
  and sup_only_i = filter_da sup_i j
  and sup_only_j = filter_da sup_j i in
  let in_links =  make_inlinks (i,j) sup_ij sup_only_i sup_only_j in
    { time = infinity;
      out_links = [];
      in_links = in_links;}


let make_single i achievers =
  (** basically create the pair structure for (i,i) rep single fact *)
  let sup_i = achievers i in
  let inlinks = Array.make (List.length sup_i) (dummy_reg_set,0.) in
    ignore (List.fold_left
	      (fun index a ->
		 inlinks.(index) <- (init_reg_set a.pre [a],a.dur);
		 index+1)
	      0 sup_i);
    { time = infinity;
      out_links = [];
      in_links = inlinks; }


let build_outlinks nF pairs =
  (** based on the in_links, build the out_links *)
  let add_ol (xo,yo) (xi,yi,index) =
    pairs.(xo).(yo).out_links <- (xi,yi,index)::pairs.(xo).(yo).out_links in
  let analyze (i,j) (reg_set,_) index =
    (* [reg_set] is an in_link of the pair [(i,j)] *)
    let rec process l =
      match l with
	  [] -> ()
	| f::rest ->
	    List.iter (fun f1 -> add_ol (f,f1) (i,j,index)) l;
	    process rest
    in process reg_set.props
  in
    for i = 0 to (nF - 1) do
      for j = i to (nF -1) do
	for k = 0 to (Array.length pairs.(i).(j).in_links) - 1 do
	  analyze (i,j) pairs.(i).(j).in_links.(k) k
	done;
      done;
    done


let init_pairs pairs atoms achievers =
  (** build the pairs structure; preparing for Dynamic Programming procedure *)
  let assign_pair (i,j) pair =
    pairs.(i).(j) <- pair;
    pairs.(j).(i) <- pair
  in let nF = List.length atoms in
    for i = 0 to (nF - 2) do
      for j = (i+1) to (nF - 1) do
	assign_pair (i,j) (make_pair (i,j) achievers)
      done
    done;
    for i = 0 to (nF - 1) do
      pairs.(i).(i) <- make_single i achievers
    done;
    build_outlinks nF pairs


(***** dynamic programing updates to propagate h2 ****)


let init_update, add_update, pop_update, has_update =
  (** an update = (f1,f2),(old_value,new_value) *)
  let q = Queue.create () in
    (fun atoms ->
       let rec initialize l =
	 match l with
	     [] -> ()
	   | f::rest ->
	       List.iter (fun f1 ->
			    if f < f1 then
			      Queue.push ((f,f1),(infinity,0.)) q
			    else
			      Queue.push ((f1,f),(infinity,0.)) q)
	       l;
	       initialize rest
       in initialize atoms),
    (fun item -> Queue.push item q),
    (fun () -> Queue.pop q),
    (fun () -> not (Queue.is_empty q))


let update_value pairs s =
  (** [s] is a regression set supporting a pair, now all singles and
    pairs of atoms in s are achieved and the max-value changed. We need
    to reevaluate the time-to-achieve this [s] set of atoms *)
  let rec eval (v,pair) atoms =
    match atoms with
	[] -> v,pair
      | f::rest ->
	  eval (List.fold_left (fun (v1,p) f2 ->
				  if v1 > pairs.(f).(f2).time then (v1,p)
				  else (pairs.(f).(f2).time, (f,f2)))
		  (v,pair) atoms) rest
  in let max_val,max_pair = eval (-1.,(-1,-1)) s.props in
    if (max_val = infinity) || (max_val < 0.) || (max_pair = (-1,-1)) then
      failwith "hm.update_value: something wrong here"
    else
      (s.value <- max_val;
       s.max_pair <- max_pair;
       max_val)



let do_update pairs ((f1,f2),(old_time,new_time)) =
  (** time to achieve (f1,f2) reduces from [old_time] to [new_time] *)
  let equal_pair (x1,y1) (x2,y2) =
  ((x1 = x2) && (y1 = y2)) or ((x1 = y2) && (y1 = x2)) in
    pairs.(f1).(f2).time <- new_time;
    List.iter (fun (i,j,k) ->
		 let (s,d) = pairs.(i).(j).in_links.(k) in
		   if old_time = infinity then
		     (s.supported <- s.supported - 1;
		      if s.supported < 0 then failwith "supported < 0!!");
		   if ((old_time = infinity) && (s.supported = 0)) ||
		     ((old_time < infinity) &&
		      (equal_pair s.max_pair (f1,f2)))
		   then
		     let v = update_value pairs s in
		       (* if new reg_set provide a new min-val for [i,j]*)
		       if (v +. d < pairs.(i).(j).time) then
			 ( add_update ((i,j),(pairs.(i).(j).time,v +.d));
			   pairs.(i).(j).time <- v +. d))
      pairs.(f1).(f2).out_links


let propagate pairs init =
  (** using DP rules through in/out links to update all the values in
    [pairs] until fix_point *)
  init_update init;
  Verb.pe 4 "Done initialization. Start propagation:\n";
  while (has_update ()) do
    do_update pairs (pop_update ())
  done;
  Verb.pe 4 "Done propagating!!\n";
  flush_all ()



(**** main function: return heuristic value ****)

let dummy_pair = {
  time = 0.;
  out_links = [];
  in_links = Array.make 1  (dummy_reg_set,0.);
}


let h2_reg p =
  (** create a causal links between pairs of facts (i.e. when one pair
  is updated the value, then the other pair should be potentially
  affected). Then do the single-source all-destination shortest path
  dynamic programming until fix-point to get the H^2 values. Return
  the function that take each a regressed state (goals + in-progress
  actions) and return a H^2 value for that one *)
  start_h2 ();
  let nF = List.length p.atoms  (* maybe number fluents? *)
  and init = List.filter p.in_initial p.atoms in (* initial fluents *)
  let pairs = Array.make_matrix nF nF dummy_pair in
    init_pairs pairs p.atoms p.establishers;
    set_time 1;
    Verb.pe 3 "DP rules initialization time: %f secs\n" (get_time 1);
    propagate pairs init;
    set_time 2;
    Verb.pe 3 "H2 fix-point calculation time: %f secs\n\n" (get_time 3);
    flush_all ();
    let rec eval v atoms =
      match atoms with
	[] -> v
      | f::rest ->
	  eval (List.fold_left
		  (fun v1 f2 -> Math.fmax v1 pairs.(f).(f2).time)
		  v atoms) rest
    in
      (fun atoms -> eval 0. atoms)


let h2_pro p =
  (** quite close to h2_reg but need to reset the whole structure
    before doing the propagation *)
  failwith "Unimplemented"

(* EOF *)
