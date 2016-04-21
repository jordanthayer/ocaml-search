(** Single Step error, learned via various lms techniques.
    Added in based on the estimated number of steps to go
*)

let default_lr = Lms_h.default_lr
let alt_col_name = "correction"

let output_col_hdr () =
  Datafile.write_alt_colnames stdout alt_col_name ["observations";
						   "h_step";
						   "h_step_err";]

let output_row samples h_step h_err =
  Datafile.write_alt_row_prefix stdout alt_col_name;
  Verb.pr Verb.always "%i\t%f\t%f\n" samples h_step h_err


let output_geometric_sched ?(duration = 2) output =
  let i = ref 0
  and next = ref duration in
    (fun samples hest err ->
       if !i >= !next
       then (i := !i + 1;
	     next := (!next * 15) / 10;
	     output samples hest err)
       else i := !i + 1)


let is_infinite v =
  (v = infinity) ||  (v = neg_infinity)



let make_unbounded_correction get_g get_h get_d =
  (** Uses the [bound] to create a correction to h based on single step
      error.  The corrected heuristic value will still obey admissibility,
      but may not be consistent.  Make correction returns two functions.
      [h_calc] takes the parent node, the best node, and a list of all
      children in order to update the correction.  [f_calc] returns the
      estimated total cost of the node *)
  output_col_hdr ();
  let record = output_geometric_sched output_row
  and show_ex, estimate, get_weights =
    Lms.init_lms ~initial_weight:0. ~learning_rate:(default_lr) 1
  and num_samp = ref 0 in
  let h_calc parent best children =
    let tv = estimate [|(get_d best)|] +. (get_h best) +. ((get_g best) -. (get_g parent))  -. (get_h parent)
    and dv = get_d parent in
      if (is_infinite dv) || (is_infinite tv)
      then ()
      else(let est,err = show_ex [|dv|] tv
	   and wt = match get_weights () with [|v|] -> v | _ -> -.1. in
	     (*Verb.pe Verb.debug "target: %f\t" tv;
	     Verb.pe Verb.debug "real: %f\test: %f\n" (get_h parent) est;*)
	     num_samp := !num_samp + 1;
	     record !num_samp wt err)
  and f_calc n =
    (get_g n) +. (get_h n) +. (estimate [|get_d n|])
  in
    h_calc,f_calc


let make_clamped_correction get_g get_h get_d bound =
  (** Uses the [bound] to create a correction to h based on single step
      error.  The corrected heuristic value will still obey admissibility,
      but may not be consistent.  Make correction returns two functions.
      [h_calc] takes the parent node, the best node, and a list of all
      children in order to update the correction.  [f_calc] returns the
      estimated total cost of the node *)
  output_col_hdr ();
  let record = output_geometric_sched output_row
  and show_ex, estimate, get_weights =
    Lms.init_lms ~initial_weight:0. ~learning_rate:(default_lr) 1
  and num_samp = ref 0 in
  let h_calc parent best children =
    let tv = (estimate [|(get_d best)|] +. (get_h best)) +.
      ((get_g best) -. (get_g parent)) -. (get_h parent)
    and dv = get_d parent in
      if (is_infinite dv) || (is_infinite tv)
      then ()
      else(let est,err = show_ex [|dv|] tv
	   and wt = match get_weights () with [|v|] -> v | _ -> -.1. in
	     (*Verb.pe Verb.debug "target: %f\t" tv;
	     Verb.pe Verb.debug "real: %f\test: %f\n" (get_h parent) est;*)
	     num_samp := !num_samp + 1;
	     record !num_samp wt err)
  and f_calc n =
    let wf = bound *. ((get_g n) +. (get_h n)) in
    Math.fmin wf ((get_g n) +. (get_h n) +. (estimate [|get_d n|]))
  in
    h_calc,f_calc


let make_clamped_fhp_correction get_g get_h get_d bound =
  (** Uses the [bound] to create a correction to h based on single step
      error.  The corrected heuristic value will still obey admissibility,
      but may not be consistent.  Make correction returns two functions.
      [h_calc] takes the parent node, the best node, and a list of all
      children in order to update the correction.  [f_calc] returns the
      estimated total cost of the node *)
  output_col_hdr ();
  let record = output_geometric_sched output_row
  and show_ex, estimate, get_weights =
    Lms.init_lms ~initial_weight:0. ~learning_rate:(default_lr) 1
  and num_samp = ref 0 in
  let h_calc parent best children =
    let tv = estimate [|(get_d best)|] +. (get_h best) +. ((get_g best) -. (get_g parent))  -. (get_h parent)
    and dv = get_d parent in
      if (is_infinite dv) || (is_infinite tv)
      then ()
      else(let est,err = show_ex [|dv|] tv
	   and wt = match get_weights () with [|v|] -> v | _ -> -.1. in
	     (*Verb.pe Verb.debug "target: %f\t" tv;
	     Verb.pe Verb.debug "real: %f\test: %f\n" (get_h parent) est;*)
	     num_samp := !num_samp + 1;
	     record !num_samp wt err)
  and f_calc n =
    let wf = bound *. ((get_g n) +. (get_h n)) in
    Math.fmin wf ((get_g n) +. bound *. ((get_h n) +. (estimate [|get_d n|])))
  in
    h_calc,f_calc

(* EOF *)
