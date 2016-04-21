(** Uses backwards looking heuristics which mirror the forwards looking
    heuristics in order to predict how much error is present in the
    forward looking heuristics. *)

let alt_col_name = "correction"

let output_col_hdr () =
  Datafile.write_alt_colnames stdout alt_col_name ["observations";
						   "h_step";]

let output_row samples h_err =
  Datafile.write_alt_row_prefix stdout alt_col_name;
  Verb.pr Verb.always "%f\t%f\n" samples (h_err /. samples)


let output_geometric_sched ?(duration = 2) output =
  let i = ref 0
  and next = ref duration in
    (fun samples h_err ->
       if !i >= !next
       then (i := !i + 1;
	     next := (!next * 15) / 10;
	     output samples h_err)
       else i := !i + 1)


let make_unbounded_correction get_g get_rh get_h =
  (** Compares rev_h to g of the current node in order to learn a coefficient
      to apply to h of the node going forward.  Error is assumed to be the
      same for all nodes and is therefore kept in a global average *)
  output_col_hdr ();
  let num_samples = ref 0.
  and factor = ref 0.
  and record = output_geometric_sched output_row in

  let h_calc parent best children =
    factor := !factor +. ((get_g best) /. (get_rh best));
    num_samples := !num_samples +. 1.;
    record !num_samples !factor

  and f_calc n =
    let fact = !factor /. !num_samples in
      (get_g n) +. (get_h n) *. fact
  in
    h_calc, f_calc


let make_clamped_correction get_g get_rh get_h bound =
  (** Compares rev_h to g of the current node in order to learn a coefficient
      to apply to h of the node going forward.  Error is assumed to be the
      same for all nodes and is therefore kept in a global average *)
  output_col_hdr ();
  let num_samples = ref 0.
  and factor = ref 0.
  and record = output_geometric_sched output_row in

  let h_calc parent best children =
    factor := !factor +. ((get_g best) /. (get_rh best));
    num_samples := !num_samples +. 1.;
    record !num_samples !factor

  and f_calc n =
    let wf = bound *. ((get_g n) +. (get_h n))
    and fact = !factor /. !num_samples in
    let fhat = (get_g n) +. (get_h n) *. fact in
      Math.fmin wf fhat
  in
    h_calc, f_calc


let make_clamped_fhp_correction ?(wt = None) get_g get_rh get_h bound =
  (** Compares rev_h to g of the current node in order to learn a coefficient
      to apply to h of the node going forward.  Error is assumed to be the
      same for all nodes and is therefore kept in a global average *)
  output_col_hdr ();
  let num_samples = ref 0.
  and factor = ref 0.
  and wt = match wt with None -> bound | Some f -> f
  and record = output_geometric_sched output_row in

  let h_calc parent best children =
    factor := !factor +. ((get_g best) /. (get_rh best));
    num_samples := !num_samples +. 1.;
    record !num_samples !factor

  and f_calc n =
    let wf = bound *. ((get_g n) +. (get_h n))
    and fact = !factor /. !num_samples in
    let fhat = (get_g n) +. ((get_h n) *. fact) *. wt in
      Math.fmin wf fhat
  in
    h_calc, f_calc



let make_greedy_correction get_g get_rh get_h =
  (** Compares rev_h to g of the current node in order to learn a coefficient
      to apply to h of the node going forward.  Error is assumed to be the
      same for all nodes and is therefore kept in a global average *)
  output_col_hdr ();
  let num_samples = ref 0.
  and factor = ref 0.
  and record = output_geometric_sched output_row in

  let h_calc parent best children =
    factor := !factor +. ((get_g best) /. (get_rh best));
    num_samples := !num_samples +. 1.;
    record !num_samples !factor

  and f_calc n =
    let fact = !factor /. !num_samples in
      (get_g n) +. (get_h n) *. fact
  in
    h_calc, f_calc

(* EOF *)
