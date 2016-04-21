(**
   Fringe search from Bjoernsson, Enzenberger, Holte, and Schaeffer

    @author jtd7
    @since 2011-02-16
*)


type 'a node = {
  data : 'a;
  g: float;
  f: float;
}


let do_iteration expand feasible goal_p info current_nodes threshold =
  let min_oob = ref infinity in
  let frontier = List.fold_left
    (fun accum n ->
       if n.f <= threshold && not (Limit.halt_p info)
       then (let kids = expand threshold n in
	       Limit.incr_exp info;
	       (List.iter (fun c ->
			     Limit.incr_gen info;
			     if goal_p c
			     then Limit.new_incumbent info
			       (Limit.Incumbent (0. ,c));
			     min_oob := Math.fmin !min_oob c.f) kids;
		kids @ accum))
       else (min_oob := Math.fmin !min_oob n.f;
	     n::accum)) [] current_nodes in
    frontier, !min_oob


let make_continue_p info =
  (** Returns true if we need to do another search itteration *)
  (fun threshold ->
     match info.Limit.incumbent with
       | Limit.Nothing -> true
       | Limit.Incumbent (q,n) -> n.f >= threshold)


let make_feasible hashtbl key info =
  (fun n ->
     if not (Limit.promising_p info n)
     then (Limit.incr_prune info;
	   false)
     else (let state = key n in
	     try
	       let prev = Htable.find hashtbl state in
		 if prev.f <= n.f
		 then false
		 else (Htable.replace hashtbl state n;
		       true)
	     with Not_found ->
	       Htable.add hashtbl state n;
	       true))


let make_expand feasible info goal_p expand h =
  let rec make_kids threshold n =
    if not (Limit.halt_p info)
    then List.fold_left (fun accum (d,g) ->
			   let c = { data = d;
				     f = g +. h d;
				     g = g; } in
			     if goal_p c
			     then (Limit.new_incumbent info
				     (Limit.Incumbent (0.,c)));
			     if feasible c
			     then
			       (if c.f > threshold
				then c::accum
				else (make_kids threshold c) @ accum)
			     else accum) [] (expand n.data n.g)
    else [] in
    (fun threshold n -> make_kids threshold n)


let do_search sface initial continue info key goal_p =
  let threshold = ref initial.f
  and open_list = ref [initial]
  and closed = (Htable.create sface.Search_interface.hash
		  sface.Search_interface.equals 100) in
  let feasible = make_feasible closed key info in
  let expand = (make_expand feasible info goal_p
		  sface.Search_interface.domain_expand
		  sface.Search_interface.h) in
  let next_it = do_iteration expand feasible goal_p info in
    while (continue !threshold) && not (Limit.halt_p info) do
      (let next_frontier, next_threshold = next_it !open_list !threshold in
	 open_list := next_frontier;
	 threshold := next_threshold;
	 (*Verb.pe Verb.always "Next f: %f\t Open Size: %i\n%!" !threshold
	   (List.length !open_list)*))
    done

let wrap fn = (fun n -> fn n.data)

let unwrap_sol s =
  (** Unwraps a solution which is in the form of a search node and presents
      it in the format the domain expects it, which is domain data followed
      by cost *)
  match s with
    | Limit.Nothing -> None
    | Limit.Incumbent (q,n) -> Some (n.data, n.g)


let dups sface args =
  Search_args.is_empty "Fringe.dups" args;
  let initial = sface.Search_interface.initial in
  let initial = { data = initial;
		  g = 0.;
		  f = sface.Search_interface.h initial; } in
  let key = wrap sface.Search_interface.key
  and goal_p = wrap sface.Search_interface.goal_p
  and info = (Limit.make Limit.Nothing sface.Search_interface.halt_on
		(fun a b -> a.f < b.f)
		(Limit.make_default_logger (fun n -> n.f)
		   (wrap sface.Search_interface.get_sol_length))) in
  let continue = make_continue_p info in
    do_search sface initial continue info key goal_p;
    Limit.unwrap_sol6 unwrap_sol (Limit.results6 info)
(* EOF *)
