(**

    @author jtd7
    @since 2012-02-20
*)

open Tqs_rewrite

let search i key hash equals goal expand initial bound =
  let max_guess = truncate initial.fp.d in
  let openlist = (Geq.create_with open_sort focal_sort (make_close_enough bound)
		    set_open get_open initial)
  and clean = Dpq.create clean_sort set_clean max_guess initial
  and closed = Htable.create hash equals max_guess in
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
	       if prev.fp.f > n.fp.f
	       then (if prev.ints.clean_pos <> Dpq.no_position
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
	  if (Limit.promising_p i n)
	  then (if goal n
		then (Limit.new_incumbent i (Limit.Incumbent (bound, n));
		      do_loop())
		else (let children = do_expand n in
			List.iter add_node children;
			Limit.curr_q i (Geq.count openlist)));
	    do_loop()) in

  (* this is the part that actually does the search *)
  Dpq.insert clean initial;
  set_geqe initial (Geq.insert openlist initial);
  Htable.add closed (key initial) initial;
  do_loop ();
  i


(**** The interface code that calls the search algorithm *****)
let dups sface args =
  let module SI = Search_interface in
  let key = wrap sface.SI.key
  and hash = sface.SI.hash
  and equals = sface.SI.equals
  and goal = wrap sface.SI.goal_p
  and hd = sface.Search_interface.hd in
  let initial = make_initial sface.SI.initial hd
  and expand = make_expand sface.SI.domain_expand hd in
  let bound = Search_args.get_float "Tqs_rewrite.dups" args 0 in
  let i = (Limit.make Limit.Nothing sface.SI.halt_on better_p
	     (Limit.make_default_logger (fun n -> n.fp.g) (fun n -> n.ints.depth))) in
  reset();
    let i = search i key hash equals goal expand initial bound in
      Limit.unwrap_sol6 unwrap_sol (Limit.results6 i)

let dups_pm sface args =
  let module SI = Search_interface in
  let key = wrap sface.SI.key
  and hash = sface.SI.hash
  and equals = sface.SI.equals
  and goal = wrap sface.SI.goal_p
  and hd = sface.Search_interface.hd in
  let initial = make_initial sface.SI.initial hd
  and expand = make_expand_pathmax sface.SI.domain_expand hd in
  let bound = Search_args.get_float "Tqs_rewrite.dups" args 0 in
  let i = (Limit.make Limit.Nothing sface.SI.halt_on better_p
	     (Limit.make_default_logger (fun n -> n.fp.g) (fun n -> n.ints.depth))) in
  reset();
  let i = search i key hash equals goal expand initial bound in
  Limit.unwrap_sol6 unwrap_sol (Limit.results6 i)

