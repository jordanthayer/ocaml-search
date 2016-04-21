(** Uses reverse h to calculate an update at a node, has no memory, meaning
    that all calculations are local to a node, no data is shared.  As a result
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


let make_unbounded_correction get_g get_rh get_h bound =
  output_col_hdr ();
  let record = output_geometric_sched output_row in
  let h_calc parent best children = ()
  and f_calc n =
    let fact = (get_g n) /. (get_rh n) in
      record fact;
      (get_g n) +. (get_h n) *. fact
  in
    h_calc, f_calc


let make_clamped_correction get_g get_rh get_h bound =
  output_col_hdr ();
  let record = output_geometric_sched output_row in
  let h_calc parent best children = ()
  and f_calc n =
    let wf = bound *. ((get_g n) +. (get_h n))
    and fact = (get_g n) /. (get_rh n) in
    let fhat = (get_g n) +. (get_h n) *. fact in
      record fact;
      Math.fmin wf fhat
  in
    h_calc, f_calc


let make_clamped_correction ?(wt = None) get_g get_rh get_h bound =
  output_col_hdr ();
  let record = output_geometric_sched output_row in
  let wt = match wt with None -> bound | Some f -> f in
  let h_calc parent best children = ()
  and f_calc n =
    let wf = bound *. ((get_g n) +. (get_h n))
    and fact = (get_g n) /. (get_rh n) in
    let fhat = (get_g n) +. ((get_h n) *. fact) *. wt in
      record fact;
      Math.fmin wf fhat
  in
    h_calc, f_calc

let make_greedy_correction get_g get_rh get_h =
  output_col_hdr ();
  let record = output_geometric_sched output_row in
  let h_calc parent best children = ()
  and f_calc n =
    let g = get_g n
    and rev_h = get_rh n in
    let fact = g /. rev_h in
      record fact;
      (get_h n) *. fact
  in
    h_calc, f_calc


(* EOF *)
