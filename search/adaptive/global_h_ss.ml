(** Single Step error, lumps error in the cost to go heuristic into
    a giant bucketed average.  Added in based on the estimated number
    of steps to go.  This is a clamped adaptive search *)

let alt_col_name = "correction"

let output_col_hdr () =
  Datafile.write_alt_colnames stdout alt_col_name ["observations";
						   "h_step";]

let output_row samples h_err =
  Datafile.write_alt_row_prefix stdout alt_col_name;
  Verb.pr Verb.always "%i\t%f\n" samples (h_err /. (float_of_int samples))


let output_geometric_sched ?(duration = 2) output =
  let i = ref 0
  and next = ref duration in
    (fun samples h_err ->
       if !i >= !next
       then (i := !i + 1;
	     next := (!next * 15) / 10;
	     output samples h_err)
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
  let num_samples = ref 0
  and h_err = ref 0.
  and record = output_geometric_sched output_row in

  let h_calc parent best children =
    let h_step = (((get_g best) +. (get_h best)) -.
		    ((get_g parent) +. (get_h parent))) in
      if not (is_infinite h_step)
      then (h_err := !h_err +. h_step;
	    num_samples := !num_samples + 1;
	    record !num_samples !h_err;)

  and f_calc n =
    let herr = Math.fmax (!h_err /. (float_of_int !num_samples)) 0. in
    let fh = (get_g n) +. (get_h n) +. (herr *. (get_d n)) in
    let f = (get_g n) +. (get_h n) in
      (*Verb.pe Verb.debug "Admiss:   %f\n" f;
      Verb.pe Verb.debug "Estimate: %f\n" fh;
      Verb.pe Verb.debug "Herror:   %f\n" (!h_err /. (float_of_int !num_samples));*)
      (assert (fh >= f));
      fh
  in
    h_calc, f_calc


let make_clamped_correction get_g get_h get_d bound =
  (** Uses the [bound] to create a correction to h based on single step
      error.  The corrected heuristic value will still obey admissibility,
      but may not be consistent.  Make correction returns two functions.
      [h_calc] takes the parent node, the best node, and a list of all
      children in order to update the correction.  [f_calc] returns the
      estimated total cost of the node *)
  output_col_hdr ();
  let num_samples = ref 0
  and h_err = ref 0.
  and record = output_geometric_sched output_row in

  let h_calc parent best children =
    let h_step = (((get_g best) +. (get_h best)) -.
                    ((get_g parent) +. (get_h parent))) in
      if not (is_infinite h_step)
      then ((*Verb.pe Verb.debug "h_step: %f\n" h_step;*)
	    h_err := !h_err +. h_step;
	    num_samples := !num_samples + 1;
	    record !num_samples !h_err)

   and f_calc n =
     let wf = bound *. ((get_g n) +. (get_h n))
     and herr = Math.fmax (!h_err /. (float_of_int !num_samples)) 0. in
     let fhat = (get_g n) +. (get_h n) +. (herr *. (get_d n)) in
       (*Verb.pe Verb.debug "wf: %f\tf^: %f\n" wf fhat;*)
       Math.fmin wf fhat
   in
     h_calc, f_calc


 let make_clamped_fhp_correction ?(wt = None) get_g get_h get_d bound =
   (** Uses the [bound] to create a correction to h based on single step
       error.  The corrected heuristic value will still obey admissibility,
       but may not be consistent.  Make correction returns two functions.
       [h_calc] takes the parent node, the best node, and a list of all
       children in order to update the correction.  [f_calc] returns the
       estimated total cost of the node *)
  Verb.pe Verb.always "Making correction\n";
   output_col_hdr ();
   let num_samples = ref 0
   and h_err = ref 0.
   and wt = match wt with None -> bound | Some f -> f
   and record = output_geometric_sched output_row in

   let h_calc parent best children =
     let h_step = (((get_g best) +. (get_h best)) -.
		     ((get_g parent) +. (get_h parent))) in
       if not (is_infinite h_step)
       then (h_err := !h_err +. h_step;
	     num_samples := !num_samples + 1;
	     record !num_samples !h_err;)

   and f_calc n =
     let wf = bound *. ((get_g n) +. (get_h n))
     and herr = Math.fmax (!h_err /. (float_of_int !num_samples)) 0. in
     let fhat = (get_g n) +. wt *. ((get_h n) +. (herr *. (get_d n))) in
      (*Verb.pe Verb.debug "wf: %f\tf^: %f\n" wf fhat;*)
      Math.fmin wf fhat
  in
    h_calc, f_calc


let make_h_only_correction get_g get_h get_d =
  (** Uses the [bound] to create a correction to h based on single step
      error.  The corrected heuristic value will still obey admissibility,
      but may not be consistent.  Make correction returns two functions.
      [h_calc] takes the parent node, the best node, and a list of all
      children in order to update the correction.  [f_calc] returns the
      estimated total cost of the node *)
  output_col_hdr ();
  let num_samples = ref 0
  and h_err = ref 0.
  and record = output_geometric_sched output_row in

  let h_calc parent best children =
    let h_step = (((get_g best) +. (get_h best)) -.
		    ((get_g parent) +. (get_h parent))) in
      if not (is_infinite h_step)
      then (h_err := !h_err +. h_step;
	    num_samples := !num_samples + 1;
	    record !num_samples !h_err;)

  and f_calc n =
    let herr = Math.fmax (!h_err /. (float_of_int !num_samples)) 0. in
      herr *. (get_d n) +. (get_h n)
  in
    h_calc, f_calc


let make_hrecur_correction get_g get_h get_d =

  let num_samples = ref 0
  and h_err = ref 0. in

  let h_calc parent best children =
    let c = (get_g best) -. (get_g parent) in
    let h_step = ((get_h best) +. c -. (get_h parent)) /. c in
      if not (is_infinite h_step)
      then (h_err := !h_err +. h_step;
	    num_samples := !num_samples + 1)

  and f_calc n =
    let eh = Math.fmin 1. (!h_err /. (float !num_samples)) in
      (get_h n) /. (1. -. eh)
  in
    h_calc, f_calc


let make_h_fact get_g get_h get_d =
  let num_samples = ref 0
  and h_err = ref 0. in

  let h_calc parent best children =
    let c = (get_g best) -. (get_g parent) in
    let fact = ((get_h best) +. c) /. (get_h parent) in
      h_err := !h_err +. fact;
      num_samples := !num_samples + 1;

  and f_calc n =
    let herr = Math.fmax (!h_err /. (float_of_int !num_samples)) 0. in
      herr *. (get_h n)
  in
    h_calc, f_calc


(* EOF *)
