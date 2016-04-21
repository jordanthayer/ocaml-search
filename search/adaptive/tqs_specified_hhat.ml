(** Three Queue Search allowing for a user supplied hhat funciton
    Assumes that the hd function in the search interface is your standard
    admissible hd function, and the stand alone h function is the supplied hhat
 **)

let h_calc _ _ _ = ()
and hd_calc _ _ _ = ()


let make_fcalc sface =
  let hhat = Three_queue_search_ss_fast.wrap sface.Search_interface.h in
    (fun node ->
       (node.Three_queue_search_ss_fast.g +. (hhat node)))


let make_fdcalc sface =
  let hhat = Three_queue_search_ss_v2.wrap sface.Search_interface.h
  and dhat = Three_queue_search_ss_v2.wrap sface.Search_interface.d in
    (fun node ->
       (node.Three_queue_search_ss_v2.g +. (hhat node)),
       dhat node)

(* Only h correction three queue search calls *)

let h_no_dups sface args =
  let bound = Search_args.get_float "Tqs_specified_hhat.h_no_dups" args 0 in
  Three_queue_search_ss_fast.no_dups sface bound
    Timers.reckless h_calc (make_fcalc sface)

let h_dups sface args =
  let bound = Search_args.get_float "Tqs_specified_hhat.h_dups" args 0 in
  Three_queue_search_ss_fast.dups sface bound
    Timers.reckless h_calc (make_fcalc sface)

let h_dd sface args =
  let bound = Search_args.get_float "Tqs_specified_hhat.h_dd" args 0 in
  Three_queue_search_ss_fast.delay sface bound
    Timers.reckless h_calc (make_fcalc sface)

let h_hard_delay sface args =
  let bound = Search_args.get_float "Tqs_specified_hhat.h_hard_delay" args 0 in
  Three_queue_search_ss_fast.delay sface bound
    Timers.reckless h_calc (make_fcalc sface)

(* h and d correction three queue search calls *)

let hd_no_dups sface args =
  let bound = Search_args.get_float "Tqs_specified_hhat.hd_no_dups" args 0 in
  Three_queue_search_ss_v2.no_dups sface bound
    Timers.reckless hd_calc (make_fdcalc sface)

let hd_dups sface args =
  let bound = Search_args.get_float "Tqs_specified_hhat.hd_dups" args 0 in
  Three_queue_search_ss_v2.dups sface bound
    Timers.reckless hd_calc (make_fdcalc sface)

let hd_dd sface args =
  let bound = Search_args.get_float "Tqs_specified_hhat.hd_dd" args 0 in
  Three_queue_search_ss_v2.delay sface bound
    Timers.reckless hd_calc (make_fdcalc sface)

let hd_hard_delay sface args =
  let bound = Search_args.get_float "Tqs_specified_hhat.hd_hard_delay" args 0 in
  Three_queue_search_ss_v2.delay sface bound
    Timers.reckless hd_calc (make_fdcalc sface)

(**** Anytime Variants of the specified hd search ******)


let anytime_hd_ss_reckless_no_dups sface args =
  (** perform a search based on this estimated f function on a domain
      with few duplicates.  Never update f estimates or resort queues *)
  let bound = Search_args.get_float "Tqs_fast_v2.hd_ss_reckless_no_dups"
    args 0 in
    Three_queue_search_ss_v2.anytime_no_dups
      sface bound Timers.reckless h_calc (make_fdcalc sface)

let anytime_hd_ss_reckless_dups sface args =
  (** perform a search based on this estimated f function on a domain
      with few duplicates.  Never update f estimates or resort queues *)
  let bound = Search_args.get_float "Tqs_fast_v2.hd_ss_reckless_no_dups"
    args 0 in
    Three_queue_search_ss_v2.anytime_dups
      sface bound Timers.reckless h_calc (make_fdcalc sface)

(* EOF *)
