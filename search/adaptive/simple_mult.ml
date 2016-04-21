(** Used for the limited enthusiasm search *)


let make_clamped_correction get_g get_h get_d bound opt =
  (** Uses the [bound] to create a correction to h based on single step
      error.  The corrected heuristic value will still obey admissibility,
      but may not be consistent.  Make correction returns two functions.
      [h_calc] takes the parent node, the best node, and a list of all
      children in order to update the correction.  [f_calc] returns the
      estimated total cost of the node *)
  let h_calc parent best children = ()

   and f_calc n =
     let wf = bound *. ((get_g n) +. (get_h n)) in
     let fhat = (get_g n) +. opt *. (get_h n) in
       (*Verb.pe Verb.debug "wf: %f\tf^: %f\n" wf fhat;*)
       Math.fmin wf fhat
   in
     h_calc, f_calc


(* EOF *)
