(**

    @author jtd7
    @since 2012-02-20
*)

open Tqs_rewrite
let prev_it = -14

let search i key hash equals goal expand initial closed bound =
  let max_guess = truncate initial.fp.d in
  let openlist = (Geq.create_with open_sort focal_sort (make_close_enough bound)
		    set_open get_open initial)
  and clean = Dpq.create clean_sort set_clean max_guess initial in
  let get_node = make_get_node bound clean openlist i in

  let insert node state =
    Dpq.insert clean node;
    let ge = Geq.insert openlist node in
    set_geqe node ge;
    Htable.replace closed state node in

  let add_node n =
    Limit.incr_gen i;
    if not (Limit.promising_p i n) then Limit.incr_prune i
    else (let state = key n in
	  try (let prev = Htable.find closed state in
	       Limit.incr_dups i;
	       if prev.fp.f > n.fp.f ||
		 (prev.fp.f = n.fp.f && prev.ints.clean_pos = prev_it)
	       then (if prev.ints.clean_pos >= 0
		     then (Dpq.remove clean prev.ints.clean_pos;
			   Geq.remove openlist prev.geqe;
		           insert n state)
		     else insert n state))
	  with Not_found -> (* new state *)
	    insert n state) in

  let do_expand n =
    Limit.incr_exp i;
    Geq.remove openlist (get_geqe n);
    Dpq.remove clean (n.ints.clean_pos);
    set_open n Dpq.no_position;
    set_clean n Dpq.no_position;
    if not (Limit.promising_p i n) then (Limit.incr_prune i; [])
    else expand n in

  let rec do_loop () =
    if not (Limit.halt_p i) && ((Geq.count openlist) > 0)
    then (let n = get_node () in
	    if goal n
	    then Limit.new_incumbent i (Limit.Incumbent (bound, n))
	    else (let children = do_expand n in
		List.iter add_node children;
		Limit.curr_q i (Geq.count openlist);
		do_loop())) in

  (* this is the part that actually does the search *)
  Dpq.insert clean initial;
  set_geqe initial (Geq.insert openlist initial);
  Htable.add closed (key initial) initial;
  do_loop ();
  i


(**** The interface code that calls the search algorithm *****)
let dups sface args =
  let module SI = Search_interface in
  let wts = Restarts_wastar.richterl1 in
  let key = wrap sface.SI.key
  and hash = sface.SI.hash
  and equals = sface.SI.equals
  and goal = wrap sface.SI.goal_p
  and hd = sface.Search_interface.hd in
  let initial = make_initial sface.SI.initial hd
  and expand = make_expand sface.SI.domain_expand hd in
  let i = (Limit.make Limit.Nothing sface.SI.halt_on better_p
	     (Limit.make_default_logger (fun n -> n.fp.g)
		(fun n -> n.ints.depth))) in
  let closed = Htable.create hash equals (truncate initial.fp.d) in
  reset();
    ignore
      (List.fold_left
	 (fun i wt ->
	    Htable.iter (fun key element ->
			   element.ints.open_pos <- prev_it;
			   element.ints.clean_pos <- prev_it;) closed;
	    search i key hash equals goal expand initial closed wt) i wts);
       Limit.unwrap_sol6 unwrap_sol (Limit.results6 i)

let dups_pm sface args =
  let module SI = Search_interface in
  let wts = Restarts_wastar.richterl1 in
  let key = wrap sface.SI.key
  and hash = sface.SI.hash
  and equals = sface.SI.equals
  and goal = wrap sface.SI.goal_p
  and hd = sface.Search_interface.hd in
  let initial = make_initial sface.SI.initial hd
  and expand = make_expand_pathmax sface.SI.domain_expand hd in
  let i = (Limit.make Limit.Nothing sface.SI.halt_on better_p
	     (Limit.make_default_logger (fun n -> n.fp.g)
		(fun n -> n.ints.depth))) in
  let closed = Htable.create hash equals (truncate initial.fp.d) in
    reset();
    ignore
      (List.fold_left
	 (fun i wt ->
	    Htable.iter (fun key element ->
			   element.ints.open_pos <- prev_it;
			   element.ints.clean_pos <- prev_it;) closed;
	    search i key hash equals goal expand initial closed wt) i wts);
    Limit.unwrap_sol6 unwrap_sol (Limit.results6 i)


