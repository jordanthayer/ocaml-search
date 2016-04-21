(** Recording for uniform cost searches *)

open Recording_astar


let make_expand recorder expand f_hd r_hd =
  (** Takes the domain expand function and a heuristic calculator
      and creates an expand function which returns search nodes. *)
  (fun n ->
     let children =
       List.map (fun (dat, g) ->
		   let h,d = f_hd dat
		   and rh, rd = r_hd dat in
		     { data = dat;
		       parent = n;
		       f = h;
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
  Search_args.is_empty "Recording_greedy.expand_recorder_nodups" args;
  no_dups ~create_expand:make_expand sface
    (exp_rec sface.Search_interface.key_printer
       sface.Search_interface.key) Recorders.none

and expand_recorder_dups sface args =
  Search_args.is_empty "Recording_greedy.expand_recorder_dups" args;
  dups ~create_expand:make_expand sface
    (exp_rec sface.Search_interface.key_printer
       sface.Search_interface.key) Recorders.none

and expand_recorder_dd sface args =
  Search_args.is_empty "Recording_greedy.expand_recorder_dd" args;
  drop ~create_expand:make_expand sface
    (exp_rec sface.Search_interface.key_printer
       sface.Search_interface.key) Recorders.none

and queue_recorder_nodups sface args =
  Search_args.is_empty "Recording_greedy.queue_recorder_nodups" args;
  no_dups ~create_expand:make_expand sface Recorders.no_node_record
    (queue_rec sface.Search_interface.key_printer sface.Search_interface.key)

and queue_recorder_dups sface args =
  Search_args.is_empty "Recording_greedy.queue_recorder_dups" args;
  dups ~create_expand:make_expand sface Recorders.no_node_record
    (queue_rec sface.Search_interface.key_printer
       sface.Search_interface.key)

and queue_recorder_dd sface args =
  Search_args.is_empty "Recording_greedy.queue_recorder_dd" args;
  drop ~create_expand:make_expand sface Recorders.no_node_record
    (queue_rec sface.Search_interface.key_printer
       sface.Search_interface.key)


(* EOF *)
