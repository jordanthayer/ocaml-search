(** Clamped Adaptive allowing for a user supplied hhat funciton
    Assumes that the hd function in the search interface is your standard
    admissible hd function, and the stand alone h function is the supplied hhat
    d is the supplied dhat, if that were applicable.
 **)

let h_calc _ _ _ = ()

let make_fcalc bound sface =
  let hhat = Single_step.wrap sface.Search_interface.h in
    (fun node ->
       let max = bound *. (node.Single_step.g +. node.Single_step.h) in
	 Math.fmin max
	   (node.Single_step.g +. (hhat node)))


let no_dups sface args =
  let bound = Search_args.get_float "Clamped_specified_hhat.no_dups" args 0 in
    Single_step.no_dups sface Timers.reckless h_calc (make_fcalc bound sface)

let dups sface args =
  let bound = Search_args.get_float "Clamped_specified_hhat.dups" args 0 in
  Single_step.dups sface Timers.reckless h_calc (make_fcalc bound sface)

(* EOF *)
