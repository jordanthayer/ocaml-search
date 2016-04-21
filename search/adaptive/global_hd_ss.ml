(** Single Step error, lumps error in the cost to go heuristic into
    a giant bucketed average.  Added in based on the estimated number
    of steps to go
*)

let alt_col_name = "correction"

let output_col_hdr () =
  Datafile.write_alt_colnames stdout alt_col_name ["observations";
						   "h_step";
						   "d_step";]

let output_row samples h_err d_err =
  Datafile.write_alt_row_prefix stdout alt_col_name;
  Verb.pr Verb.always "%i\t%f\t%f\n"
    samples (h_err /. (float_of_int samples)) (d_err /. (float_of_int samples))


let output_geometric_sched ?(duration = 2) output =
  let i = ref 0
  and next = ref duration in
    output_col_hdr ();
    (fun samples h_err d_err ->
       if !i >= !next
       then (i := !i + 1;
	     next := (!next * 15) / 10;
	     output samples h_err d_err)
       else i := !i + 1)


let eps = 0.0001


let make_unbounded_correction get_g get_h get_d =
  (** Much like global_h_ss but corrects for both the cost to go function
      h and the distance to go estimate d.  Uses the same technique for
      updating h and d.  Produces functions with the same footprints to
      be used later in the same search framework.  *)
  let num_samples = ref 0
  and h_err = ref 0.
  and d_err = ref 0.
  and record = output_geometric_sched output_row in

  let h_calc parent best children =
    let h_step = ((get_g best) +. (get_h best) -.
		    ((get_g parent) +. (get_h parent)))
    and d_step = ((get_d best) +. 1.) -. (get_d parent) in
      if ((Math.finite_p h_step) && (Math.finite_p d_step))
      then (h_err := !h_err +. h_step;
	    d_err := !d_err +. d_step;
	    num_samples := !num_samples + 1;
	    record !num_samples !h_err !d_err;)

  and f_calc n =
    let herr = Math.fmax (!h_err /. (float_of_int !num_samples)) 0.
    and derr = Math.fmin (!d_err /. (float_of_int !num_samples)) 1. in
    let nd = (get_d n) /. (1. -. derr) in
      (get_g n) +. (get_h n) +. (herr *. nd)
  in
    h_calc, f_calc


let make_clamped_correction get_g get_h get_d bound =
  (** Much like global_h_ss but corrects for both the cost to go function
      h and the distance to go estimate d.  Uses the same technique for
      updating h and d.  Produces functions with the same footprints to
      be used later in the same search framework.  *)
  let num_samples = ref 0
  and h_err = ref 0.
  and d_err = ref 0.
  and record = output_geometric_sched output_row in

  let h_calc parent best children =
    let h_step = (((get_g best) +. (get_h best)) -.
		    ((get_g parent) +. (get_h parent)))
    and d_step = ((get_d best) +. 1.) -. (get_d parent) in
(*      if h_step <> 0. || d_step <> 0. then
      Verb.pe Verb.always "dstep: %f\thstep: %f\n" d_step h_step;*)
      if ((Math.finite_p h_step) && (Math.finite_p d_step))
      then (h_err := !h_err +. h_step;
	    d_err := !d_err +. d_step;
	    num_samples := !num_samples + 1;
	    record !num_samples !h_err !d_err;)

  and f_calc n =
    let wf = bound *. ((get_g n)+. (get_h n))
    and herr = Math.fmax (!h_err /. (float_of_int !num_samples)) 0.
    and derr = Math.fmax (!d_err /. (float_of_int !num_samples)) 0. in
    let nd = (get_d n) /. (1. -. derr) in
    let fhat = (get_g n) +. (get_h n) +. (herr *. nd) in
      Math.fmin wf fhat
  in
    h_calc, f_calc


let make_clamped_fhp_correction ?(wt = None) get_g get_h get_d bound =
  (** Much like global_h_ss but corrects for both the cost to go function
      h and the distance to go estimate d.  Uses the same technique for
      updating h and d.  Produces functions with the same footprints to
      be used later in the same search framework.  *)
  let num_samples = ref 0
  and h_err = ref 0.
  and d_err = ref 0.
  and wt = match wt with None -> bound | Some f -> f
  and record = output_geometric_sched output_row in

  let h_calc parent best children =
    let h_step = (((get_g best) +. (get_h best)) -.
		    ((get_g parent) +. (get_h parent)))
    and d_step =  ((get_d best) +. 1.) -. (get_d parent) in
      if ((Math.finite_p h_step) && (Math.finite_p d_step))
      then (h_err := !h_err +. h_step;
	    d_err := !d_err +. d_step;
	    num_samples := !num_samples + 1;
	    record !num_samples !h_err !d_err;)

  and f_calc n =
    let wf = bound *. ((get_g n)+. (get_h n))
    and herr = Math.fmax (!h_err /. (float_of_int !num_samples)) 0.
    and derr = Math.fmax (!d_err /. (float_of_int !num_samples)) 0. in
    let nd = (get_d n) /. (1. -. derr) in
    let fhat = (get_g n) +. wt *. ((get_h n) +. (herr *. nd)) in
      Math.fmin wf fhat
  in
    h_calc, f_calc


let make_unbounded_fhp_correction ?(wt = None) get_g get_h get_d bound =
  (** Much like global_h_ss but corrects for both the cost to go function
      h and the distance to go estimate d.  Uses the same technique for
      updating h and d.  Produces functions with the same footprints to
      be used later in the same search framework.  *)
  let num_samples = ref 0
  and h_err = ref 0.
  and d_err = ref 0.
  and wt = match wt with None -> bound | Some f -> f
  and record = output_geometric_sched output_row in

  let h_calc parent best children =
    let h_step = (((get_g best) +. (get_h best)) -.
		    ((get_g parent) +. (get_h parent)))
    and d_step = (get_d parent) -. (get_d best) -. 1. in
      if ((Math.finite_p h_step) && (Math.finite_p d_step))
      then (h_err := !h_err +. h_step;
	    d_err := !d_err +. d_step;
	    num_samples := !num_samples + 1;
	    record !num_samples !h_err !d_err;)

  and f_calc n =
    let herr = Math.fmax (!h_err /. (float_of_int !num_samples)) 0.
    and derr = Math.fmin (!d_err /. (float_of_int !num_samples)) 1. in
    let nd = (get_d n) /. (1. -. derr) in
    let fhat = (get_g n) +. wt *. ((get_h n) +. (herr *. nd)) in
      fhat
  in
    h_calc, f_calc




let make_unbounded_correction_ignore_neg get_g get_h get_d =
  (** Much like global_h_ss but corrects for both the cost to go function
      h and the distance to go estimate d.  Uses the same technique for
      updating h and d.  Produces functions with the same footprints to
      be used later in the same search framework.  *)
  let num_samples = ref 0
  and h_err = ref 0.
  and d_err = ref 0.
  and record = output_geometric_sched output_row in

  let h_calc parent best children =
    let h_step = ((get_g best) +. (get_h best) -.
		    ((get_g parent) +. (get_h parent)))
    and d_step = ((get_d parent) -. (get_d best)) -. 1. in
      if ((Math.finite_p h_step) && (Math.finite_p d_step) &&
	    (h_step >= 0.) && (d_step >= 0.))
      then (h_err := !h_err +. h_step;
	    d_err := !d_err +. d_step;
	    num_samples := !num_samples + 1;
	    record !num_samples !h_err !d_err;)

  and f_calc n =
    let herr = Math.fmax (!h_err /. (float_of_int !num_samples)) 0.
    and derr = Math.fmax (!d_err /. (float_of_int !num_samples)) 0. in
    let nd = (get_d n) *. (1. +. derr) in
      (get_g n) +. (get_h n) +. (herr *. nd)
  in
    h_calc, f_calc



let make_wted_clamped_fhp_correction  get_g get_h get_d =
  (** Much like global_h_ss but corrects for both the cost to go function
      h and the distance to go estimate d.  Uses the same technique for
      updating h and d.  Produces functions with the same footprints to
      be used later in the same search framework.  *)
  let num_samples = ref 0
  and h_err = ref 0.
  and d_err = ref 0.
  and record = output_geometric_sched output_row in

  let h_calc parent best children =
    let h_step = (((get_g best) +. (get_h best)) -.
		    ((get_g parent) +. (get_h parent)))
    and d_step = ((get_d parent) -. (get_d best)) -. 1. in
      if ((Math.finite_p h_step) && (Math.finite_p d_step))
      then (h_err := !h_err +. h_step;
	    d_err := !d_err +. d_step;
	    num_samples := !num_samples + 1;
	    record !num_samples !h_err !d_err;)

  and f_calc n wt =
    let wf = wt *. ((get_g n)+. (get_h n))
    and herr = Math.fmax (!h_err /. (float_of_int !num_samples)) 0.
    and derr = Math.fmax (!d_err /. (float_of_int !num_samples)) 0. in
    let nd = (get_d n) *. (1. +. derr) in
    let fhat = (get_g n) +. wt *. ((get_h n) +. (herr *. nd)) in
      (*Verb.pe Verb.always "@%f , %f > %f ?\n" wt fhat wf;
      if fhat > wf then Verb.pe Verb.always "Clamping!\n"
      else Verb.pe Verb.always "Not Clamping\n";*)
      Math.fmin wf fhat
  in
    h_calc, f_calc


let make_wted_unbounded_fhp_correction get_g get_h get_d =
  (** Much like global_h_ss but corrects for both the cost to go function
      h and the distance to go estimate d.  Uses the same technique for
      updating h and d.  Produces functions with the same footprints to
      be used later in the same search framework.  *)
  let num_samples = ref 0
  and h_err = ref 0.
  and d_err = ref 0.
  and record = output_geometric_sched output_row in

  let h_calc parent best children =
    let h_step = (((get_g best) +. (get_h best)) -.
		    ((get_g parent) +. (get_h parent)))
    and d_step = ((get_d parent) -. (get_d best)) -. 1. in
      if ((Math.finite_p h_step) && (Math.finite_p d_step))
      then (h_err := !h_err +. h_step;
	    d_err := !d_err +. d_step;
	    num_samples := !num_samples + 1;
	    record !num_samples !h_err !d_err;)

  and f_calc n wt =
    let f = ((get_g n)+. (get_h n))
    and herr = Math.fmax (!h_err /. (float_of_int !num_samples)) 0.
    and derr = Math.fmax (!d_err /. (float_of_int !num_samples)) 0. in
    let nd = (get_d n) *. (1. +. derr) in
    let fhat = (get_g n) +. wt *. ((get_h n) +. (herr *. nd)) in
      Math.fmax f fhat
  in
    h_calc, f_calc


let make_greedy_correction get_g get_h get_d =
  (** Much like global_h_ss but corrects for both the cost to go function
      h and the distance to go estimate d.  Uses the same technique for
      updating h and d.  Produces functions with the same footprints to
      be used later in the same search framework.  *)
  let num_samples = ref 0
  and h_err = ref 0.
  and d_err = ref 0.
  and record = output_geometric_sched output_row in

  let h_calc parent best children =
    let h_step = ((get_g best) +. (get_h best) -.
		    ((get_g parent) +. (get_h parent)))
    and d_step = (get_d best) -. (get_d parent) +. 1. in
      if ((Math.finite_p h_step) && (Math.finite_p d_step))
      then (h_err := !h_err +. h_step;
	    d_err := !d_err +. d_step;
	    num_samples := !num_samples + 1;
	    record !num_samples !h_err !d_err;)

  and f_calc n =
    let herr = Math.fmax (!h_err /. (float_of_int !num_samples)) 0.
    (*and derr = Math.fmin(!d_err /. (float_of_int !num_samples)) 1. in*)
    and derr = (!d_err /. (float_of_int !num_samples)) in
    let nd = (get_d n) /. (1. -. derr) in
    if Math.finite_p nd then (get_h n) +. (herr *. nd) else infinity
  in
    h_calc, f_calc


let make_speedy_correction get_g get_h get_d =
  (** Much like global_h_ss but corrects for both the cost to go function
      h and the distance to go estimate d.  Uses the same technique for
      updating h and d.  Produces functions with the same footprints to
      be used later in the same search framework.  *)
  let num_samples = ref 0
  and d_err = ref 0.
  and record = output_geometric_sched output_row in

  let h_calc parent best children =
    let d_step = ((get_d best) +. 1.) -. (get_d parent) in
      if Math.finite_p d_step
      then (d_err := !d_err +. d_step;
	    num_samples := !num_samples + 1;
	    record !num_samples nan !d_err;)

  and f_calc n =
    let derr = Math.fmax (!d_err /. (float_of_int !num_samples)) 0. in
      (get_d n) /. (1. -. derr) in
    h_calc, f_calc


let make_seeded_correction weight hseed dseed get_g get_h get_d =
  (** Much like global_h_ss but corrects for both the cost to go function
      h and the distance to go estimate d.  Uses the same technique for
      updating h and d.  Produces functions with the same footprints to
      be used later in the same search framework.  *)
  let num_samples = ref weight
  and h_err = ref (hseed *. (float weight))
  and d_err = ref (dseed *. (float weight))
  and record = output_geometric_sched output_row in

  let h_calc parent best children =
    let h_step = ((get_g best) +. (get_h best) -.
		    ((get_g parent) +. (get_h parent)))
    and d_step = (get_d best) -. (get_d parent) +. 1. in
      if ((Math.finite_p h_step) && (Math.finite_p d_step))
      then (h_err := !h_err +. h_step;
	    d_err := !d_err +. d_step;
	    num_samples := !num_samples + 1;
	    record !num_samples !h_err !d_err;)

  and f_calc n =
    let herr = Math.fmax (!h_err /. (float_of_int !num_samples)) 0.
    (*and derr = Math.fmin(!d_err /. (float_of_int !num_samples)) 1. in*)
    and derr = (!d_err /. (float_of_int !num_samples)) in
    let nd = (get_d n) /. (1. -. derr) in
    if Math.finite_p nd then (get_h n) +. (herr *. nd) else infinity
  in
    h_calc, f_calc

(* EOF *)
