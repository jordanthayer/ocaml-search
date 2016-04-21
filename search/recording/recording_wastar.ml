(** Recording for uniform cost searches *)

open Recording_astar


let make_expand bound recorder expand f_hd r_hd =
  (** Takes the domain expand function and a heuristic calculator
      and creates an expand function which returns search nodes. *)
  (fun n ->
     let children =
       List.map (fun (dat, g) ->
		   let h,d = f_hd dat
		   and rh, rd = r_hd dat in
		     { data = dat;
		       parent = n;
		       f = g +. bound *. h;
		       g = g;
		       h = h;
		       d = d;
		       rev_h = rh;
		       rev_d = rd;
		       depth = n.depth + 1;
		       pos = Dpq.no_position; }) (expand n.data n.g)  in
       recorder n n.parent children;
       children)


let expand_recorder_nodups sface args =
  let bound = Search_args.get_float "Recording_wastar.expand_recorder_nodups"
    args 0 in
    no_dups ~create_expand:(make_expand bound) sface
      (exp_rec sface.Search_interface.key_printer
	 sface.Search_interface.key) Recorders.none

and expand_recorder_dups sface args =
  let bound = Search_args.get_float "Recording_wastar.expand_recorder_dups"
    args 0 in
    dups ~create_expand:(make_expand bound) sface
      (exp_rec sface.Search_interface.key_printer
	 sface.Search_interface.key) Recorders.none

and expand_recorder_dd sface args =
  let bound = Search_args.get_float "Recording_wastar.expand_recorder_dd"
    args 0 in
    drop ~create_expand:(make_expand bound) sface
      (exp_rec sface.Search_interface.key_printer
	 sface.Search_interface.key) Recorders.none

and fprime_recorder_dd sface args =
  let bound = Search_args.get_float "Recording_wastar.expand_recorder_dd"
    args 0 in
    drop_nogoal ~create_expand:(make_expand bound) sface
      (exp_rec sface.Search_interface.key_printer
	 sface.Search_interface.key) Recorders.none


and queue_recorder_nodups sface args =
  let bound = Search_args.get_float "Recording_wastar.queue_recorder_nodups"
    args 0 in
    no_dups ~create_expand:(make_expand bound) sface Recorders.no_node_record
      (queue_rec sface.Search_interface.key_printer sface.Search_interface.key)

and queue_recorder_dups sface args =
  let bound = Search_args.get_float "Recording_wastar.queue_recorder_dups"
    args 0 in
    dups ~create_expand:(make_expand bound) sface Recorders.no_node_record
      (queue_rec sface.Search_interface.key_printer sface.Search_interface.key)

and queue_recorder_dd sface args =
  let bound = Search_args.get_float "Recording_wastar.queue_recorder_dd"
    args 0 in
    drop ~create_expand:(make_expand bound) sface Recorders.no_node_record
      (queue_rec sface.Search_interface.key_printer sface.Search_interface.key)


(* EOF *)
