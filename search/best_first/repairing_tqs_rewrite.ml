(**

    @author jtd7
    @since 2012-02-20
*)

open Tqs_rewrite

let delayed = -3

let is_delayed n =
  n.ints.open_pos = delayed && n.ints.clean_pos <> Dpq.no_position

let search i key hash equals goal expand initial weights =
  let max_guess = truncate initial.fp.d in
  let openlist = (Geq.create_with open_sort focal_sort
		    (make_close_enough weights.(0)) set_open get_open initial)
  and clean = Dpq.create clean_sort set_clean max_guess initial
  and delay = Dpq.create clean_sort set_clean max_guess initial
  and closed = Htable.create hash equals max_guess in
  let it = ref 0
  and num_weights = Array.length weights in

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
	       then (if (is_delayed prev)
		     then (Dpq.swap delay prev.ints.clean_pos n;
			   n.ints.open_pos <- delayed;
			   Htable.replace closed state n)
		     else if prev.ints.open_pos <> Dpq.no_position
		     then (Dpq.remove clean prev.ints.clean_pos;
			   Geq.remove openlist prev.geqe;
		           insert n state)
		     else (n.ints.open_pos <- delayed;
			   Htable.replace closed state n;
			   Dpq.insert delay n)))
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

  let on_goal () =
    Dpq.iter (fun n -> n.ints.clean_pos <- Dpq.no_position;
		n.ints.open_pos <- Dpq.no_position;
		add_node n) delay;
    Dpq.clear delay;
    it := !it + 1;
    if !it < num_weights
    then (Geq.update_close_enough openlist (make_close_enough weights.(!it));
	  Some (make_get_node weights.(!it) clean openlist i))
    else None in

  let rec do_loop get_node =
    if not (Limit.halt_p i) && ((Geq.count openlist) > 0)
    then (let n = get_node () in
	  if goal n
	  then (Limit.new_incumbent i (Limit.Incumbent (0., n));
		match on_goal() with
		  | None -> ()
		  | Some gn -> do_loop gn)
	  else (let children = do_expand n in
		List.iter add_node children;
		Limit.curr_q i (Geq.count openlist);
		do_loop get_node)) in

  (* this is the part that actually does the search *)
  Dpq.insert clean initial;
  set_geqe initial (Geq.insert openlist initial);
  Htable.add closed (key initial) initial;
  do_loop (make_get_node weights.(!it) clean openlist i);
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
  let i = (Limit.make Limit.Nothing sface.SI.halt_on better_p
	     (Limit.make_default_logger (fun n -> n.fp.g) (fun n -> n.ints.depth))) in
  reset();
    let i = search i key hash equals goal expand initial
      (Array.of_list Restarts_wastar.richterl1) in
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
  let i = (Limit.make Limit.Nothing sface.SI.halt_on better_p
	     (Limit.make_default_logger (fun n -> n.fp.g) (fun n -> n.ints.depth))) in
  reset();
  let i = search i key hash equals goal expand initial
    (Array.of_list Restarts_wastar.richterl1) in
  Limit.unwrap_sol6 unwrap_sol (Limit.results6 i)


