(** Global estimated f, estimated d calculator for use in three queue search
    version 2.  Lumps error in cost to go and distance to go into a global
    average. *)

let alt_col_name = "correction"

let output_col_hdr () =
  Datafile.write_alt_colnames stdout alt_col_name ["observations";
						   "h_step";
						   "d_step";]

let output_row samples h_err d_err =
  Datafile.write_alt_row_prefix stdout alt_col_name;
  Verb.pr Verb.always "%f\t%f\t%f\n"
    samples (h_err /. samples) (d_err /. samples)

let output_geometric_sched ?(duration = 2) output =
  let i = ref 0
  and next = ref duration in
    (fun samples h_err d_err ->
       if !i >= !next
       then (i := !i + 1;
	     next := (!next * 15) / 10;
	     output samples h_err d_err)
       else i := !i + 1)


let eps = 0.01
let one_minus_eps = 1. -. eps

let make_unbounded_correction get_g get_h get_f get_d =
  (** Much like global_h_ss but corrects for both the cost to go function
      h and the distance to go estimate d.  Uses the same technique for
      updating h and d.  Produces functions with the same footprints to
      be used later in the same search framework.  *)
  output_col_hdr ();
  let num_samples = ref 0.
  and h_err = ref 0.
  and d_err = ref 0.
  and record = output_geometric_sched output_row in

  let h_calc parent best children =
    let h_step = (get_f best) -. (get_f parent)
    and d_step = (get_d best) -. (get_d parent) +. 1. in

      if (Math.finite_p h_step) && (Math.finite_p d_step)
      then (h_err := !h_err +. h_step;
	    d_err := !d_err +. d_step;
	    num_samples := !num_samples +. 1.;
	    record !num_samples !h_err !d_err;)

  and fd_calc n =
    let herr = (!h_err /. !num_samples)
    and derr = (!d_err /. !num_samples) in
    let based = get_d n
    and baseh = get_h n in
    let nd = Math.fmax based (if derr >= one_minus_eps
			      then based /. eps
			      else based /. (1. -. derr)) in
    let nf =
      (get_g n) +. (Math.fmax baseh (baseh +. (herr *. nd))) in
	nd,nf
  in
    h_calc, fd_calc


let make_unbounded_correction_oldschool get_g get_h get_d =
  (** Much like global_h_ss but corrects for both the cost to go function
      h and the distance to go estimate d.  Uses the same technique for
      updating h and d.  Produces functions with the same footprints to
      be used later in the same search framework.  *)
  output_col_hdr ();
  let num_samples = ref 0.
  and h_err = ref 0.
  and d_err = ref 0.
  and record = output_geometric_sched output_row in

  let h_calc parent best children =
    let h_step = ((get_g best) +. (get_h best) -.
		    ((get_g parent) +. (get_h parent)))
    and d_step = (get_d best) -. (get_d parent) +. 1. in

      if (Math.finite_p h_step) && (Math.finite_p d_step)
      then (h_err := !h_err +. h_step;
	    d_err := !d_err +. d_step;
	    num_samples := !num_samples +. 1.;
	    record !num_samples !h_err !d_err;)

  and fd_calc n =
    let herr = (!h_err /. !num_samples)
    and derr = (!d_err /. !num_samples)
    and based = get_d n
    and baseh = get_h n in
    let nd = Math.fmax based ((1. +. derr) *. based) in
      (get_g n) +. (Math.fmax baseh (baseh +. (herr *. nd))), nd
  in
    h_calc, fd_calc

let make_unbounded_correction_prime bound get_g get_h get_d =
  (** Much like global_h_ss but corrects for both the cost to go function
      h and the distance to go estimate d.  Uses the same technique for
      updating h and d.  Produces functions with the same footprints to
      be used later in the same search framework.  *)
  output_col_hdr ();
  let num_samples = ref 0.
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
	    num_samples := !num_samples +. 1.;
	    record !num_samples !h_err !d_err;)

  and fd_calc n =
    let herr =  Math.fmax (!h_err /. !num_samples) 0.
    and derr =  Math.fmin (!d_err /. !num_samples) 1. in
    let nd = (if derr >= 1.
	      then (get_d n) /. eps
	      else (get_d n) /. (1. -. derr)) in
      (get_g n) +. (((get_h n) +. (herr *. nd)) *. bound), nd
  in
    h_calc, fd_calc



let make_unbounded_correction_prime_oldschool bound get_g get_h get_d =
  (** Much like global_h_ss but corrects for both the cost to go function
      h and the distance to go estimate d.  Uses the same technique for
      updating h and d.  Produces functions with the same footprints to
      be used later in the same search framework.  *)
  output_col_hdr ();
  let num_samples = ref 0.
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
	    num_samples := !num_samples +. 1.;
	    record !num_samples !h_err !d_err;)

  and fd_calc n =
    let herr =  Math.fmax (!h_err /. !num_samples) 0.
    and derr =  Math.fmin (!d_err /. !num_samples) 1. in
    let nd = (get_d n) *. (1. +. derr) in
      (get_g n) +. (((get_h n) +. (herr *. nd)) *. bound), nd
  in
    h_calc, fd_calc


let make_unbounded_correction_prime_noneg bound get_g get_h get_d =
  (** Much like global_h_ss but corrects for both the cost to go function
      h and the distance to go estimate d.  Uses the same technique for
      updating h and d.  Produces functions with the same footprints to
      be used later in the same search framework.  *)
  output_col_hdr ();
  let num_samples = ref 0.
  and h_err = ref 0.
  and d_err = ref 0.
  and record = output_geometric_sched output_row in

  let h_calc parent best children =
    let h_step = ((get_g best) +. (get_h best) -.
		    ((get_g parent) +. (get_h parent)))
    and d_step = (get_d best) -. (get_d parent) +. 1. in
      if ((Math.finite_p h_step) && (Math.finite_p d_step)
	&& (d_step >= 0.) && (h_step >= 0.))
      then (h_err := !h_err +. h_step;
	    d_err := !d_err +. d_step;
	    num_samples := !num_samples +. 1.;
	    record !num_samples !h_err !d_err;)

  and fd_calc n =
    let herr = Math.fmax (!h_err /. !num_samples) 0.
    and derr = Math.fmin (!d_err /. !num_samples) 1. in
    let nd = (get_d n) /. (Math.fmax (1. -. derr) eps) in
    let min_h = get_h n in
      (get_g n) +. (Math.fmax min_h (min_h +. (herr *. nd))) *. bound, nd
  in
    h_calc, fd_calc

(* EOF *)
