open Three_queue_search_rev

let h_globalavg_reckless_no_dups sface args =
  (** perform a search based on this estimated f function on a domain
      with few duplicates.  Never update f estimates or resort queues *)
  let bound = Search_args.get_float "Tqs_rev.h_globalavg_reckless_no_dups"
    args 0 in
  let h_calc,f_calc = Rev_h_globalavg.make_unbounded_correction
    (fun n -> n.g) (fun n -> n.rev_h) (fun n -> n.h) in
    no_dups sface bound Timers.reckless h_calc f_calc


let h_globalavg_reckless_dups sface args =
  (** perform a search based on this estimated f function on a domain
      with few duplicates.  Never update f estimates or resort queues *)
  let bound = Search_args.get_float "Tqs_rev.h_globalavg_reckless_dups"
    args 0 in
  let h_calc,f_calc = Rev_h_globalavg.make_unbounded_correction
    (fun n -> n.g) (fun n -> n.rev_h) (fun n -> n.h) in
    dups sface bound Timers.reckless h_calc f_calc


let vector sface args =
  let bound = Search_args.get_float "Tqs_rev.vector" args 0
  and vector = Search_args.get_float_array "Tqs_rev.vector"
    (Array.sub args 1 ((Array.length args) - 1)) in
  let h_calc,f_calc = Vector_h_ss.make_unbounded_correction
    (fun node ->
       [|node.h;node.d;node.g;float_of_int node.depth;node.rev_h;node.rev_d|])
    vector in
    no_dups ~node_get:no_fh_getnode sface bound Timers.reckless h_calc f_calc


let vector_dups sface args =
  let bound = Search_args.get_float "Tqs_rev.vector" args 0
  and vector = Search_args.get_float_array "Tqs_rev.vector"
    (Array.sub args 1 ((Array.length args) - 1)) in
  let h_calc,f_calc = Vector_h_ss.make_unbounded_correction
    (fun node ->
       [|node.h;node.d;node.g;float_of_int node.depth;node.rev_h;node.rev_d|])
    vector in
    dups ~node_get:no_fh_getnode sface bound Timers.reckless h_calc f_calc

(* EOF *)
