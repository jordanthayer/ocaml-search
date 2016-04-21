(** Uses backwards looking heuristics which mirror the forwards looking
    heuristics in order to predict how much error is present in the
    forward looking heuristics. *)

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

let make_unbounded_correction get_g get_rh get_rd get_h get_d get_depth =
  (** Produces a corrected heuristic estimate which when used for a search
      will still return results which are within the [bound] desired.
      This correction uses backwards looking h error and depth to create
      an estimate of single step error which mimics that of the single step
      errors using only single expansions.  Error is collected into a
      global average.  Error in d is also estimated and used in the complete
      correction. *)
  output_col_hdr ();
  let num_samples = ref 0.
  and h_err = ref 0.
  and d_err = ref 0.
  and record = output_geometric_sched output_row in

  let h_calc parent best children =
    h_err := !h_err +. (((get_g best) /. (get_rh best)) /.
			  (float_of_int (get_depth best)));
    d_err := !d_err +.
      (((float_of_int (get_depth best)) /. (get_rd best)) /.
	 (float_of_int (get_depth best)));
    num_samples := !num_samples +. 1.;
    record !num_samples !h_err !d_err

  and f_calc n =
    let herr = Math.fmax (!h_err /. !num_samples) 0.
    and derr = Math.fmax (!d_err /. !num_samples) 0. in
      (get_g n) +. (get_h n) +. (herr *. ((get_d n) *. (1. +. derr)))
  in
    h_calc, f_calc


let make_clamped_correction get_g get_rh get_rd get_h get_d get_depth bound =
  (** Produces a corrected heuristic estimate which when used for a search
      will still return results which are within the [bound] desired.
      This correction uses backwards looking h error and depth to create
      an estimate of single step error which mimics that of the single step
      errors using only single expansions.  Error is collected into a
      global average.  Error in d is also estimated and used in the complete
      correction. *)
  output_col_hdr ();
  let num_samples = ref 0.
  and h_err = ref 0.
  and d_err = ref 0.
  and record = output_geometric_sched output_row in

  let h_calc parent best children =
    h_err := !h_err +. (((get_g best) /. (get_rh best)) /.
			  (float_of_int (get_depth best)));
    d_err := !d_err +.
      (((float_of_int (get_depth best)) /. (get_rd best)) /.
	 (float_of_int (get_depth best)));
    num_samples := !num_samples +. 1.;
    record !num_samples !h_err !d_err

  and f_calc n =
    let wf = bound *. ((get_g n) +. (get_h n))
    and herr = Math.fmax (!h_err /. !num_samples) 0.
    and derr = Math.fmax (!d_err /. !num_samples) 0. in
    let fhat = (get_g n) +. (get_h n) +. (herr *.
					    ((get_d n) *. (1. +. derr))) in
      Math.fmin wf fhat
  in
    h_calc, f_calc


let make_clamped_fhp_correction ?(wt = None) get_g get_rh get_rd get_h get_d
    get_depth bound =
  (** Produces a corrected heuristic estimate which when used for a search
      will still return results which are within the [bound] desired.
      This correction uses backwards looking h error and depth to create
      an estimate of single step error which mimics that of the single step
      errors using only single expansions.  Error is collected into a
      global average.  Error in d is also estimated and used in the complete
      correction. *)
  output_col_hdr ();
  let num_samples = ref 0.
  and h_err = ref 0.
  and d_err = ref 0.
  and wt = match wt with None -> bound | Some f -> f
  and record = output_geometric_sched output_row in

  let h_calc parent best children =
    h_err := !h_err +. (((get_g best) /. (get_rh best)) /.
			  (float_of_int (get_depth best)));
    d_err := !d_err +.
      (((float_of_int (get_depth best)) /. (get_rd best)) /.
	 (float_of_int (get_depth best)));
    num_samples := !num_samples +. 1.;
    record !num_samples !h_err !d_err

  and f_calc n =
    let wf = bound *. ((get_g n) +. (get_h n))
    and herr = Math.fmax (!h_err /. !num_samples) 0.
    and derr = Math.fmax (!d_err /. !num_samples) 0. in
    let fhat = (get_g n) +.
      ((get_h n) +. (herr *. ((get_d n) *. (1. +. derr)))) *. wt in
      Math.fmin wf fhat
  in
    h_calc, f_calc
(* EOF *)
