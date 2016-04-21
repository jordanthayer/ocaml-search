(**

    @author jtd7
    @since 2010-09-13

   Use inadmissible heuristics to construct hhat online
*)

open Three_queue_search_ss_v2


let hd_ss_reckless_dups sface args =
  (** perform a search based on this estimated f function on a domain
      with few duplicates.  Never update f estimates or resort queues *)
  let bound = Search_args.get_float "Tqs_fast_v2.hd_ss_reckless_dups" args 0 in
  let h_inadmiss = wrap sface.Search_interface.h
  and d_inadmiss = wrap sface.Search_interface.d in
  let h_calc,f_calc = (Global_fd_ss.make_unbounded_correction (fun n -> n.g)
			 h_inadmiss (fun n -> n.f) d_inadmiss) in
    dups sface bound Timers.reckless h_calc f_calc


(* EOF *)
