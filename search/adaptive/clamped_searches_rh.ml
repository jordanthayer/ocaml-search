open Rev_heuristics


let h_globalavg_reckless_no_dups sface bound =
  (** perform a search based on this estimated f function on a domain
      with few duplicates.  Never update f estimates or resort queues *)
  let h_calc,f_calc = Rev_h_globalavg.make_clamped_correction
    (fun n -> n.g) (fun n -> n.rev_h) (fun n -> n.h) bound in
    no_dups sface bound Timers.reckless h_calc f_calc


let h_globalavg_reckless_dups sface bound =
  (** perform a search based on this estimated f function on a domain
      with many duplicates.  Never update f estimates or resort queues *)
  let h_calc,f_calc = Rev_h_globalavg.make_clamped_correction
    (fun n -> n.g) (fun n -> n.rev_h) (fun n -> n.h) bound in
    dups sface bound Timers.reckless h_calc f_calc


let h_globalavg_reckless_dd sface bound =
  (** perform a search based on this estimated f function on a domain
      with many duplicates.  Never update f estimates or resort queues *)
  let h_calc,f_calc = Rev_h_globalavg.make_clamped_correction
    (fun n -> n.g) (fun n -> n.rev_h) (fun n -> n.h) bound in
    drop sface bound Timers.reckless h_calc f_calc


let h_mockss_reckless_no_dups sface bound =
  (** perform a search based on this estimated f function on a domain
      with few duplicates.  Never update f estimates or resort queues *)
  let h_calc,f_calc = Rev_h_mock_singlestep.make_clamped_correction
    (fun n -> n.g) (fun n -> n.rev_h) (fun n -> n.h) (fun n -> n.d)
    (fun n -> n.depth) bound in
    no_dups sface bound Timers.reckless h_calc f_calc


let h_mockss_reckless_dups sface bound =
  (** perform a search based on this estimated f function on a domain
      with many duplicates.  Never update f estimates or resort queues *)
  let h_calc,f_calc = Rev_h_mock_singlestep.make_clamped_correction
    (fun n -> n.g) (fun n -> n.rev_h) (fun n -> n.h) (fun n -> n.d)
    (fun n -> n.depth) bound in
    dups sface bound Timers.reckless h_calc f_calc


let h_mockss_reckless_dd sface bound =
  (** perform a search based on this estimated f function on a domain
      with many duplicates.  Never update f estimates or resort queues *)
  let h_calc,f_calc = Rev_h_mock_singlestep.make_clamped_correction
    (fun n -> n.g) (fun n -> n.rev_h) (fun n -> n.h) (fun n -> n.d)
    (fun n -> n.depth) bound in
    drop sface bound Timers.reckless h_calc f_calc


let h_nomemory_no_dups sface bound =
  (** perform a search based on this estimated f function on a domain
      with few duplicates.  Never update f estimates or resort queues *)
  let h_calc,f_calc = Rev_h_nomemory.make_clamped_correction
    (fun n -> n.g) (fun n -> n.rev_h) (fun n -> n.h) bound in
    no_dups sface bound Timers.reckless h_calc f_calc


let h_nomemory_dups sface bound =
  (** perform a search based on this estimated f function on a domain
      with many duplicates.  Never update f estimates or resort queues *)
  let h_calc,f_calc = Rev_h_nomemory.make_clamped_correction
    (fun n -> n.g) (fun n -> n.rev_h) (fun n -> n.h) bound in
    dups sface bound Timers.reckless h_calc f_calc


let h_nomemory_dd sface bound =
  (** perform a search based on this estimated f function on a domain
      with many duplicates.  Never update f estimates or resort queues *)
  let h_calc,f_calc = Rev_h_nomemory.make_clamped_correction
    (fun n -> n.g) (fun n -> n.rev_h) (fun n -> n.h) bound in
    drop sface bound Timers.reckless h_calc f_calc


let hd_mockss_reckless_no_dups sface bound =
  (** perform a search based on this estimated f function on a domain
      with few duplicates.  Never update f estimates or resort queues *)
  let h_calc,f_calc = Rev_hd_mock_singlestep.make_clamped_correction
    (fun n -> n.g) (fun n -> n.rev_h) (fun n -> n.rev_d) (fun n -> n.h)
    (fun n -> n.d) (fun n -> n.depth) bound in
    no_dups sface bound Timers.reckless h_calc f_calc


let hd_mockss_reckless_dups sface bound =
  (** perform a search based on this estimated f function on a domain
      with many duplicates.  Never update f estimates or resort queues *)
  let h_calc,f_calc = Rev_hd_mock_singlestep.make_clamped_correction
    (fun n -> n.g) (fun n -> n.rev_h) (fun n -> n.rev_d) (fun n -> n.h)
    (fun n -> n.d) (fun n -> n.depth) bound in
    dups sface bound Timers.reckless h_calc f_calc


let hd_mockss_reckless_dd sface bound =
  (** perform a search based on this estimated f function on a domain
      with many duplicates.  Never update f estimates or resort queues *)
  let h_calc,f_calc = Rev_hd_mock_singlestep.make_clamped_correction
    (fun n -> n.g) (fun n -> n.rev_h) (fun n -> n.rev_d) (fun n -> n.h)
    (fun n -> n.d) (fun n -> n.depth) bound in
    drop sface bound Timers.reckless h_calc f_calc


(* EOF *)
