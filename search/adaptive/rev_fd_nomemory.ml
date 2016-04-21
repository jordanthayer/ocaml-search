(*    that all calculations are local to a node, no data is shared.  As a result
      an update schedule makes no sense for this search, so it should always
      be used with Timers.rekcless *)

let alt_col_name = "correction"

let output_col_hdr () =
  Datafile.write_alt_colnames stdout alt_col_name ["h_step";]

let output_row h_err =
  Datafile.write_alt_row_prefix stdout alt_col_name;
  Verb.pr Verb.always "%f\n" h_err


let output_geometric_sched ?(duration = 2) output =
  let i = ref 0
  and next = ref duration in
    (fun h_err ->
       if !i >= !next
       then (i := !i + 1;
	     next := (!next * 15) / 10;
	     output h_err)
       else i := !i + 1)


let make_unbounded_correction get_g get_rhd get_h get_d get_depth =
  output_col_hdr ();
  let record = output_geometric_sched output_row in
  let h_calc parent best children = ()
  and f_calc n =
    let rh,rd = get_rhd n in
    let fact = (get_g n) /. rh
    and fact_d = (float_of_int (get_depth n)) /. rd in
      record fact;
      (get_g n) +. (get_h n) *. fact, fact_d *. (get_d n)

  in
    h_calc, f_calc
