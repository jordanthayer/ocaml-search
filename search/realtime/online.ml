(**

   Functions for on-line evaluation of algorithms.

*)

let alt_col_name = "actions"

let output_col_hdr () =
  (** Outputs header for alt_col containing time (from start) that an
      action was issued and duration in some problem-specific
      units. *)
  Datafile.write_alt_colnames stdout alt_col_name ["time";
						   "duration";]

let output_row alg_start duration =
  (** Outputs time and duration for a chosen action or plan. Time is
      calculated starting from [alg_start] and duration is in
      problem-specific units. *)
  Datafile.write_alt_row_prefix stdout alt_col_name;
  Verb.pr Verb.always "%f\t%f\n"
    ((Wrsys.walltime ()) -. alg_start) duration
