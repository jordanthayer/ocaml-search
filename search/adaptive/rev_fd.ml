(** Corrects h and d based on the reverse h and d heuristics *)

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


let make_unbounded_correction get_g get_rhd get_h get_d get_depth =
  (** Produces a corrected heuristic estimate which when used for a search
      will still return results which are within the [bound] desired.
      This correction uses backwards looking h error and depth to create
      an estimate of single step error which mimics that of the single step
      errors using only single expansions.  Error is collected into a
      global average.  Error in d is also estimated and used in the complete
      correction. *)
  output_col_hdr ();
  let num_samples = ref 0.
  and h_fact = ref 0.
  and d_fact = ref 0.
  and record = output_geometric_sched output_row in
  let h_calc parent best children =
    let rh,rd = get_rhd parent in
      (*Verb.pe Verb.always "%f\t%f\n" rh rd;*)
    let v_h =  (get_g parent) /. rh
    and v_d = (float_of_int (get_depth parent)) /. rd in
      if Math.finite_p v_h && Math.finite_p v_d
      then
	(h_fact := !h_fact +. v_h;
	 d_fact := !d_fact +. v_d;
	 num_samples := !num_samples +. 1.;
	 record !num_samples !h_fact !d_fact)

  and f_calc n =
    let hfact = Math.fmax (!h_fact /. !num_samples) 0.
    and dfact = Math.fmax (!d_fact /. !num_samples) 0. in
    let new_d = (get_d n) *. dfact in

      (get_g n) +. (get_h n) *. hfact, new_d
  in
    h_calc, f_calc



(* EOF *)
