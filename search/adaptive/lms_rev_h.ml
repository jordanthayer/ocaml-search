(** Single Step error, learned via various lms techniques.
    Added in based on the estimated number of steps to go,
    uses reverse heuristics *)


let alt_col_name = "correction"
let default_lr = Lms_h.default_lr
let learner = Lms.init_lms

let output_col_hdr () =
  Datafile.write_alt_colnames stdout alt_col_name ["observations";
						   "h_wt";
						   "h_err";]

let output_row h_est h_err =
  Datafile.write_alt_row_prefix stdout alt_col_name;
  Verb.pr Verb.always "%f\t%f\n" h_est h_err


let output_geometric_sched ?(duration = 2) output =
  let i = ref 0
  and next = ref duration in
    (fun hest err ->
       if !i >= !next
       then (i := !i + 1;
	     next := (!next * 15) / 10;
	     output hest err)
       else i := !i + 1)


let is_infinite v =
  (v = infinity) ||  (v = neg_infinity)



let make_unbounded_correction get_g get_rh  get_h =
  (** Uses the [bound] to create a correction to h based on single step
      error.  The corrected heuristic value will still obey admissibility,
      but may not be consistent.  Make correction returns two functions.
      [h_calc] takes the parent node, the best node, and a list of all
      children in order to update the correction.  [f_calc] returns the
      estimated total cost of the node *)
  output_col_hdr ();
  let record = output_geometric_sched output_row
  and show_ex, estimate, get_weights =
    learner ~initial_weight:1. ~learning_rate:default_lr 1 in
  let h_calc parent best children =
    let est,err = show_ex [|(get_rh best)|] (get_g best) in record est err
  and f_calc n =
    (get_g n) +. (estimate [|get_h n|])
  in
    h_calc,f_calc


let make_bounded_correction bound get_g get_rh  get_h =
  (** Uses the [bound] to create a correction to h based on single step
      error.  The corrected heuristic value will still obey admissibility,
      but may not be consistent.  Make correction returns two functions.
      [h_calc] takes the parent node, the best node, and a list of all
      children in order to update the correction.  [f_calc] returns the
      estimated total cost of the node *)
  output_col_hdr ();
  let record = output_geometric_sched output_row
  and show_ex, estimate, get_weights =
    learner ~initial_weight:1. ~learning_rate:default_lr 1 in
  let h_calc parent best children =
    let est,err = show_ex [|(get_rh best)|] (get_g best) in record est err
  and f_calc n =
    let fhat = (get_g n) +. (estimate [|get_h n|]) in
      min fhat (bound *. ((get_g n) +. get_h n))
  in
    h_calc,f_calc


let make_greedy_correction maxes features f_feat i_feat target get_h =
  (** Uses the [bound] to create a correction to h based on single step
      error.  The corrected heuristic value will still obey admissibility,
      but may not be consistent.  Make correction returns two functions.
      [h_calc] takes the parent node, the best node, and a list of all
      children in order to update the correction.  [f_calc] returns the
      estimated total cost of the node *)

  let show_ex, estimate, get_weights = learner ~initial_weights:(Some i_feat)
    ~learning_rate:default_lr (Array.length i_feat) in

  let norm f = Wrarray.map2 (fun a b -> a /. b) f maxes in

  let h_calc parent best children =
    let t = target parent
    and feat = features parent in
      ignore (show_ex (norm feat) (t /. maxes.(0)))

  and f_calc n =
    (estimate (norm (f_feat n))) *. maxes.(0)

  in
    h_calc,f_calc, get_weights




(* EOF *)
