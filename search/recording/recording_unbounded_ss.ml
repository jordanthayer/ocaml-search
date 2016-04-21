open Recording_clamped

let h_ss_reckless_dups_exp sface  args =
  (** perform a search based on this estimated f function on a domain
      with many duplicates.  Never update f estimates or resort queues *)
  let bound = Search_args.get_float
    "Recording_unbounded_ss.h_ss_reckless_dups_exp" args 0 in
  let h_calc,f_calc = Global_h_ss.make_unbounded_correction (fun n -> n.g)
    (fun n -> n.h) (fun n -> n.d) in
    expand_recorder_dups sface bound Timers.reckless h_calc f_calc


let h_ss_reckless_dups_queue sface  args =
  (** perform a search based on this estimated f function on a domain
      with many duplicates.  Never update f estimates or resort queues *)
  let bound = Search_args.get_float
    "Recording_unbounded_ss.h_ss_reckless_dups_queue" args 0 in
  let h_calc,f_calc = Global_h_ss.make_unbounded_correction (fun n -> n.g)
    (fun n -> n.h) (fun n -> n.d) in
    queue_recorder_dups sface bound Timers.reckless h_calc f_calc


let h_ss_reckless_dd_exp sface  args =
  (** perform a search based on this estimated f function on a domain
      with many duplicates.  Never update f estimates or resort queues *)
  let bound = Search_args.get_float
    "Recording_unbounded_ss.h_ss_reckless_dd_exp" args 0 in
  let h_calc,f_calc = Global_h_ss.make_unbounded_correction (fun n -> n.g)
    (fun n -> n.h) (fun n -> n.d) in
    expand_recorder_dd sface bound Timers.reckless h_calc f_calc


let h_ss_reckless_dd_queue sface  args =
  (** perform a search based on this estimated f function on a domain
      with many duplicates.  Never update f estimates or resort queues *)
  let bound = Search_args.get_float
    "Recording_unbounded_ss.h_ss_reckless_dd_queue" args 0 in
  let h_calc,f_calc = Global_h_ss.make_unbounded_correction (fun n -> n.g)
    (fun n -> n.h) (fun n -> n.d) in
    queue_recorder_dd sface bound Timers.reckless h_calc f_calc

(* EOF *)
