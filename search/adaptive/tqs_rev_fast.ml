(** Simplified calls to three queue searches using reverse h and d heuristics
    to estimate the true search cost / distance to go *)

let h_globalavg_reckless_no_dups sface args =
  (** perform a search based on this estimated f function on a domain
      with few duplicates.  Never update f estimates or resort queues *)
  let bound = Search_args.get_float "Tqs_rev_fast.h_globalavg_reckless_no_dups"
    args 0 in
  let h_calc,f_calc = Rev_h_globalavg.make_unbounded_correction
    (fun n -> n.Three_queue_search_rev_fast.g)
    (fun n -> n.Three_queue_search_rev_fast.rev_h)
    (fun n -> n.Three_queue_search_rev_fast.h) in
    Three_queue_search_rev_fast.no_dups sface bound
      Timers.reckless h_calc f_calc


let h_globalavg_reckless_dups sface args =
  (** perform a search based on this estimated f function on a domain
      with few duplicates.  Never update f estimates or resort queues *)
  let bound = Search_args.get_float "Tqs_rev_fast.h_globalavg_reckless_dups"
    args 0 in
  let h_calc,f_calc = Rev_h_globalavg.make_unbounded_correction
    (fun n -> n.Three_queue_search_rev_fast.g)
    (fun n -> n.Three_queue_search_rev_fast.rev_h)
    (fun n -> n.Three_queue_search_rev_fast.h) in
    Three_queue_search_rev_fast.dups sface bound Timers.reckless h_calc f_calc


let vector sface args =
  let bound = Search_args.get_float "Tqs_rev_fast.vector" args 0
  and vector = Search_args.get_float_array "Tqs_rev_fast.vector"
    (Array.sub args 1 ((Array.length args) - 1)) in
  let h_calc,f_calc = Vector_h_ss.make_unbounded_correction
    (fun node ->
       [|node.Three_queue_search_rev_fast.h;node.Three_queue_search_rev_fast.d;
	 node.Three_queue_search_rev_fast.g;
	 float_of_int node.Three_queue_search_rev_fast.depth;
	 node.Three_queue_search_rev_fast.rev_h;
	 node.Three_queue_search_rev_fast.rev_d|])
    vector in
    Three_queue_search_rev_fast.no_dups
      ~node_get:Three_queue_search_rev_fast.no_fh_getnode sface bound
      Timers.reckless h_calc f_calc


let vector_dups sface args =
  let bound = Search_args.get_float "Tqs_rev_fast.vector" args 0
  and vector = Search_args.get_float_array "Tqs_rev_fast.vector"
    (Array.sub args 1 ((Array.length args) - 1)) in
  let h_calc,f_calc = Vector_h_ss.make_unbounded_correction
    (fun node ->
       [|node.Three_queue_search_rev_fast.h;node.Three_queue_search_rev_fast.d;
	 node.Three_queue_search_rev_fast.g;
	 float_of_int node.Three_queue_search_rev_fast.depth;
	 node.Three_queue_search_rev_fast.rev_h;
	 node.Three_queue_search_rev_fast.rev_d|])
    vector in
    Three_queue_search_rev_fast.dups
      ~node_get:Three_queue_search_rev_fast.no_fh_getnode sface bound
      Timers.reckless h_calc f_calc


(* V2 Calls! *)
let hd_globalavg_reckless_no_dups sface args =
  (** perform a search based on this estimated f function on a domain
      with few duplicates.  Never update f estimates or resort queues *)
  let bound = Search_args.get_float
    "Tqs_rev_fast.hd_globalavg_reckless_no_dups" args 0 in
  let h_calc,f_calc = Rev_fd.make_unbounded_correction
    (fun n -> n.Three_queue_search_rev_v2.g)
    (fun n -> sface.Search_interface.rev_hd n.Three_queue_search_rev_v2.data)
    (fun n -> n.Three_queue_search_rev_v2.h)
    (fun n -> n.Three_queue_search_rev_v2.d)
    (fun n -> n.Three_queue_search_rev_v2.depth)
  in
    Three_queue_search_rev_v2.no_dups sface bound
      Timers.reckless h_calc f_calc

let hd_globalavg_reckless_dups sface args =
  (** perform a search based on this estimated f function on a domain
      with few duplicates.  Never update f estimates or resort queues *)
  let bound = Search_args.get_float
    "Tqs_rev_fast.hd_globalavg_reckless_dups" args 0 in
  let h_calc,f_calc = Rev_fd.make_unbounded_correction
    (fun n -> n.Three_queue_search_rev_v2.g)
    (fun n -> sface.Search_interface.rev_hd n.Three_queue_search_rev_v2.data)
    (fun n -> n.Three_queue_search_rev_v2.h)
    (fun n -> n.Three_queue_search_rev_v2.d)
    (fun n -> n.Three_queue_search_rev_v2.depth)
  in
    Three_queue_search_rev_v2.dups sface bound
      Timers.reckless h_calc f_calc

let hd_globalavg_reckless_delay sface args =
  (** perform a search based on this estimated f function on a domain
      with few duplicates.  Never update f estimates or resort queues *)
  let bound = Search_args.get_float
    "Tqs_rev_fast.hd_globalavg_reckless_delay" args 0 in
  let h_calc,f_calc = Rev_fd.make_unbounded_correction
    (fun n -> n.Three_queue_search_rev_v2.g)
    (fun n -> sface.Search_interface.rev_hd n.Three_queue_search_rev_v2.data)
    (fun n -> n.Three_queue_search_rev_v2.h)
    (fun n -> n.Three_queue_search_rev_v2.d)
    (fun n -> n.Three_queue_search_rev_v2.depth)
  in
    Three_queue_search_rev_v2.delay sface bound
      Timers.reckless h_calc f_calc


let hd_reckless_no_dups sface args =
  (** perform a search based on this estimated f function on a domain
      with few duplicates.  Never update f estimates or resort queues *)
  let bound = Search_args.get_float "Tqs_rev_fast.hd_reckless_no_dups"
    args 0 in
  let h_calc,f_calc = Rev_fd_nomemory.make_unbounded_correction
    (fun n -> n.Three_queue_search_rev_v2.g)
    (fun n -> sface.Search_interface.rev_hd n.Three_queue_search_rev_v2.data)
    (fun n -> n.Three_queue_search_rev_v2.h)
    (fun n -> n.Three_queue_search_rev_v2.d)
    (fun n -> n.Three_queue_search_rev_v2.depth)
  in
    Three_queue_search_rev_v2.no_dups sface bound
      Timers.reckless h_calc f_calc

let hd_reckless_dups sface args =
  (** perform a search based on this estimated f function on a domain
      with few duplicates.  Never update f estimates or resort queues *)
  let bound = Search_args.get_float "Tqs_rev_fast.hd_reckless_dups" args 0 in
  let h_calc,f_calc = Rev_fd_nomemory.make_unbounded_correction
    (fun n -> n.Three_queue_search_rev_v2.g)
    (fun n -> sface.Search_interface.rev_hd n.Three_queue_search_rev_v2.data)
    (fun n -> n.Three_queue_search_rev_v2.h)
    (fun n -> n.Three_queue_search_rev_v2.d)
    (fun n -> n.Three_queue_search_rev_v2.depth)
  in
    Three_queue_search_rev_v2.dups sface bound
      Timers.reckless h_calc f_calc

let hd_reckless_delay sface args =
  (** perform a search based on this estimated f function on a domain
      with few duplicates.  Never update f estimates or resort queues *)
  let bound = Search_args.get_float "Tqs_rev_fast.hd_reckless_delay" args 0 in
  let h_calc,f_calc = Rev_fd_nomemory.make_unbounded_correction
    (fun n -> n.Three_queue_search_rev_v2.g)
    (fun n -> sface.Search_interface.rev_hd n.Three_queue_search_rev_v2.data)
    (fun n -> n.Three_queue_search_rev_v2.h)
    (fun n -> n.Three_queue_search_rev_v2.d)
    (fun n -> n.Three_queue_search_rev_v2.depth)
  in
    Three_queue_search_rev_v2.delay sface bound
      Timers.reckless h_calc f_calc
(* EOF *)
