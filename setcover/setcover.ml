(* $Id: tree.ml,v 1.3 2004/08/17 20:02:02 ruml Exp $

   simple tree-structured search space for weighted set cover
*)


open Setcover_problem


type node = {
  (* subsets are referred to by index. resorted when covered changes *)
  remaining : (float * int) list;
  cost : float;
  covered : bool array;
  added : int list;
  prob : t;
}


(************ get_child ***********)


let update_covered prob covered index =
  (** destructively modifies [covered] using subset [index] *)
  List.iter (fun i ->
	       covered.(i) <- true)
    (snd prob.subsets.(index))


(*** updating sorted list of remaining ***)


let avg_uncovered_cost prob covered i =
  (** returns heuristic cost for subset [i].  infinity = useless. *)
  let cost, elts = prob.subsets.(i)
  and n = ref 0 in
    List.iter (fun e ->
		 if not covered.(e) then incr n)
      elts;
    cost /. (float !n)


let rec add_to_sorted e l =
  (** adds [e] (which is (float * 'a)) to a (float * 'a) list which is
    already in increasing order *)
  match l with
    [] -> e::l
  | h::t ->
      if ((fst e) : float) <= (fst h)
      then e::l
      else h::(add_to_sorted e t)


let reorder_subsets h prob covered prev =
  (** returns updated list of subsets with new heuristic values, and
    list of subsets which must be added now. *)
  (* uses insertion sort, starting at the end of the old list *)
  let rec get_ordered = function
      [] -> []
    | (_, i)::t ->
	let tail = get_ordered t in
	let h = h prob covered i in
	  if h = infinity then
	    tail
	  else
	    add_to_sorted (h, i) tail
  in
    get_ordered prev


(*** checking for forced additions ***)


let make_coverable prob covered remaining =
  let coverable = Array.make prob.num_objs [] in
    List.iter (fun (_, i) ->
		 List.iter (fun e ->
			      if not covered.(e) then
				match coverable.(e) with
				  [] -> coverable.(e) <- [i]
				| ([_] as l) -> coverable.(e) <- i::l
				| _ -> ())
		 (snd prob.subsets.(i)))
      remaining;
    coverable


let forced prob covered remaining =
  (** returns those subsets that cover elements that no other subset
    covers *)
  let coverable = make_coverable prob covered remaining
  and forced = ref [] in
    Array.iteri (fun i s ->
		   if not covered.(i) then
		     match s with
		       [] -> failwith "suddenly infeasible?"
		     | [j] -> Wrutils.push_new j forced
		     | _ -> ())
      coverable;
    !forced


let check_forced h prob covered remaining =
  (** possibly destructive on covered.  returns new remaining because
    heuristic values may change after forcing (as after any addition).
    a subset may only be forced when options are decreased for an
    element, eg, after a subset is excluded. *)
  match forced prob covered remaining with
    [] -> remaining, 0., []
  | forced ->
      List.iter (update_covered prob covered) forced;
      let remaining = List.fold_left (fun r x ->
					Wrlist.remove_if_first (fun (_, y) ->
								  x = y) r)
			remaining forced
      and incr = Wrlist.sum_floats (fun i -> fst prob.subsets.(i)) forced in
      let remaining = reorder_subsets h prob covered remaining in
	(* no need to re-check forced because coverable hasn't changed *)
	remaining, incr, forced


(*** main ***)


let check p r a =
  match uncovered p.num_objs (List.map (fun i -> p.subsets.(i))
				(List.append (List.map snd r) a)) with
    [] -> ()
  | _ -> failwith "some have become uncoverable!"


let get_child h node index =
  match node.remaining with
      [] -> failwith "get_child called on leaf"
    | (_, s)::r ->
	let cost = node.cost
	  (* can be modified either by adding or forcing *)
	and covered = Array.copy node.covered
	and a = node.added
	and p = node.prob in
	  match index with
	      0 ->
		update_covered p covered s;
		(* Wrutils.pr "Added %d.\n%!" s; *)
		let r = reorder_subsets h p covered r in
		  (* check p r (s::a); *)
		  { remaining = r;
		    cost = cost +. (fst p.subsets.(s));
		    covered = covered;
		    added = s::a;
		    prob = p; }
	    | 1 ->
		let r, incr, forced = check_forced h p covered r in
		  (* List.iter (fun i -> Wrutils.pr "Forced %d.\n%!" i)
		     forced; check p r (forced @ a); *)
		  { remaining = r;
		    cost = cost +. incr;
		    covered = covered;
		    added = List.append forced a;
		    prob = p; }
	    | _ -> failwith "invalid child number"


let get_child_auc node index =
  get_child avg_uncovered_cost node index


(*** make_root ***)


let increasing_p (a, _) (b, _) =
  if (a : float) < b
  then -1
  else if a > b
  then 1
  else 0


let print_r r =
  Wrutils.pr "Remaining: ";
  Wrlist.fprint stdout (fun (_,i) -> string_of_int i) " " r;
  Wrutils.newline stdout


let initial_remaining h p =
  let covered = Array.make p.num_objs false in
  let r = Wrlist.map_opt (fun i ->
			    let h = h p covered i in
			      if h = infinity then None
			      else Some (h, i))
	    (undominated p) in
    let r = List.sort increasing_p r in
      let r, incr, forced = check_forced h p covered r in
	(* List.iter (fun i -> Wrutils.pr "Forced %d.\n%!" i) forced;
	   print_r r; *)
	r, incr, forced, covered


let make_root heuristic p =
  ensure_feasible p;
  let r, i, f, c = initial_remaining heuristic p in
    (* check p r f; *)
    { remaining = r;
      cost = i;
      covered = c;
      added = f;
      prob = p; }


let make_root_auc p =
  make_root avg_uncovered_cost p


(*** other interface functions ***)


let check_sol n =
  (** sanity check *)
  if Wrarray.mem false n.covered then failwith "covered has false!";
  let truth = Setcover_problem.check_sol n.prob n.added in
    if not (Math.within_rel n.cost truth (sqrt epsilon_float)) then
      failwith (Wrutils.str "%d subsets = %f <> claimed %f"
		  (List.length n.added) truth n.cost)


let leaf_p n =
  n.remaining = []


let num_children n =
  (** [num_children n] gets the number of children (always 2 for
      interior nodes). *)
  if leaf_p n then 0 else 2



let get_cost saved =
  (** for logger *)
  saved.cost


let better_p a b =
  a.cost < b.cost


let prune_p best node =
  node.cost >= best.cost


let get_solution n =
  if not (leaf_p n) then
    failwith "called get_solution on non-leaf!";
  n.added


(************************************************************)


let make_interface prob =
  (** [make_interface prob] makes a bounded depth search interface. *)
  let module I = Bounded_depth_interface in
    ((make_root_auc prob),
     { (I.default num_children get_child_auc leaf_p better_p Fn.identity)
       with
	 I.max_depth = max_depth prob;
	 I.should_prune = prune_p;
	 I.leaf_cost = (fun n -> n.cost);
     })


(* EOF *)
