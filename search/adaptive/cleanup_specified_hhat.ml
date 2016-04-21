(** Cleanup based searches where the user knew enough to hand us a crafted
    h^ function. *)

let h_calc _ _ _ = ()
and hd_calc _ _ _ = ()


let make_fcalc sface bound =
  let hhat = Single_step_cleanup.wrap sface.Search_interface.h in
    (fun node ->
       (node.Single_step_cleanup.g +. (hhat node)))

let make_fpcalc sface bound =
  let hhat = Single_step_cleanup.wrap sface.Search_interface.h in
    (fun node ->
       (node.Single_step_cleanup.g +. bound *. (hhat node)))


let cleanup_no_dups sface args =
  let bound = Search_args.get_float "CLeanup_specified_hhat.cleanup_no_dups"
    args 0 in
  let f_calc = make_fcalc sface bound in
    Single_step_cleanup.no_dups sface bound Timers.reckless h_calc f_calc

and cleanup_dups sface args =
  let bound = Search_args.get_float "CLeanup_specified_hhat.cleanup_dups"
    args 0 in
  let f_calc = make_fcalc sface bound in
    Single_step_cleanup.dups sface bound Timers.reckless h_calc f_calc


and cleanup_fp_no_dups sface args =
  let bound = Search_args.get_float "CLeanup_specified_hhat.cleanup_fp_no_dups"
    args 0 in
  let f_calc = make_fpcalc sface bound in
    Single_step_cleanup.no_dups sface bound Timers.reckless h_calc f_calc

and cleanup_fp_dups sface args =
  let bound = Search_args.get_float "CLeanup_specified_hhat.cleanup_fp_dups"
    args 0 in
  let f_calc = make_fpcalc sface bound in
    Single_step_cleanup.dups sface bound Timers.reckless h_calc f_calc
(* EOF *)
