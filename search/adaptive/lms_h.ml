(** Single Step error, learned via various lms techniques.
    Added in based on the estimated number of steps to go
*)

let default_lr = 0.1
let alt_col_name = "correction"

let output_col_hdr () =
  Datafile.write_alt_colnames stdout alt_col_name ["observations";
						   "h_wt";
						   "h_err";]

let output_row samples h_wt h_err =
  Datafile.write_alt_row_prefix stdout alt_col_name;
  Verb.pr Verb.always "%i\t%f\t%f\n" samples h_wt h_err


let output_geometric_sched ?(duration = 2) output =
  let i = ref 0
  and next = ref duration in
    (fun samples hest err ->
       if !i >= !next
       then (i := !i + 1;
	     next := (!next * 15) / 10;
	     output samples hest err)
       else i := !i + 1)


let make_unbounded_correction get_g get_h get_d =
  (** Uses the [bound] to create a correction to h based on single step
      error.  The corrected heuristic value will still obey admissibility,
      but may not be consistent.  Make correction returns two functions.
      [h_calc] takes the parent node, the best node, and a list of all
      children in order to update the correction.  [f_calc] returns the
      estimated total cost of the node *)
  output_col_hdr ();
  let record = output_geometric_sched output_row
  and show_ex, estimate, get_weights =
    Lms.init_lms ~initial_weight:1. ~learning_rate:(default_lr) 1
  and num_samp = ref 0 in
  let h_calc parent best children =
    let tv = estimate [|(get_h best)|] +. ((get_g best) -. (get_g parent))
    and hv = get_h parent in
      if (Math.finite_p hv) && (Math.finite_p tv)
      then
	(let est,err = show_ex [|hv|] tv
	 and wt = match get_weights ()  with [|v|] -> v | _ -> -.1. in
	   (*Verb.pe Verb.debug "target: %f\t" tv;
	   Verb.pe Verb.debug "real: %f\test: %f\n" (get_h parent) est;*)
	   num_samp := !num_samp + 1;
	   record !num_samp wt err)
  and f_calc n =
    (get_g n) +. (estimate [|get_h n|])
  in
    h_calc,f_calc


let make_clamped_correction
    i_weights max_g max_h max_d get_g get_h get_d bound =
  let show_ex, estimate, get_weights =
    Lms.init_lms ~initial_weights:(Some i_weights)
      ~learning_rate:(default_lr) 4 in
  let h_calc parent best children =
    (*let best = Wrlist.random_elt children in*)
    let c = (get_g best) -. (get_g parent) in

    let target = ((get_h best) +. c ) /. max_h
    and hv = get_h parent in
      if (Math.finite_p hv) && (Math.finite_p target)
      then ignore (show_ex [|hv /. max_h;
			     (get_d parent) /. max_d;
			     (get_g parent) /. max_g; 1.|] target)
  and f_calc n =
    let max = (get_h n) +. (get_g n) *. bound
    and h' =
      (estimate [|get_h n /. max_h ; get_d n /. max_d; get_g n /. max_g; 1.|])
      *. max_h in
    let est_f = (get_g n) +. h' in
      Math.fmin max est_f
  in
    h_calc,f_calc


let make_clamped_fhp_correction
    i_weights max_g max_h max_d get_g get_h get_d bound =
  let show_ex, estimate, get_weights =
    Lms.init_lms ~initial_weights:(Some i_weights)
      ~learning_rate:(default_lr) 4 in
  let h_calc parent best children =
    (*let best = Wrlist.random_elt children in*)
    let c = (get_g best) -. (get_g parent) in

    let target = ((get_h best) +. c ) /. max_h
    and hv = get_h parent in
      if (Math.finite_p hv) && (Math.finite_p target)
      then ignore (show_ex [|hv /. max_h;
			     (get_d parent) /. max_d;
			     (get_g parent) /. max_g; 1.|] target)
  and f_calc n =
    let max = (get_h n) +. (get_g n) *. bound
    and h' =
      (estimate [|get_h n /. max_h ; get_d n /. max_d; get_g n /. max_g; 1.|])
      *. max_h in
    let est_f = (get_g n) +. bound *. h' in
      (*Verb.pe Verb.debug "%f vs %f\n" max est_f;*)
      Math.fmin max est_f
  in
    h_calc,f_calc


let make_fhp_correction
    i_weights max_g max_h max_d get_g get_h get_d bound =
  let show_ex, estimate, get_weights =
    Lms.init_lms ~initial_weights:(Some i_weights)
      ~learning_rate:(default_lr) 4 in
  let h_calc parent best children =
    (*let best = Wrlist.random_elt children in*)
    let c = (get_g best) -. (get_g parent) in

    let target = ((get_h best) +. c ) /. max_h
    and hv = get_h parent in
      if (Math.finite_p hv) && (Math.finite_p target)
      then ignore (show_ex [|hv /. max_h;
			     (get_d parent) /. max_d;
			     (get_g parent) /. max_g; 1.|] target)
  and f_calc n =
    (get_g n) +.
      (estimate [|get_h n /. max_h ; get_d n /. max_d; get_g n /. max_g; 1.|])
    *. max_h *. bound, nan
  in
    h_calc,f_calc


let make_greedy_correction i_weights max_g max_h max_d get_g get_h get_d =
  let show_ex, estimate, get_weights =
    Lms.init_lms ~initial_weights:(Some i_weights)
      ~learning_rate:(default_lr) 4 in
  let h_calc parent best children =
    (*let best = Wrlist.random_elt children in*)
    let c = (get_g best) -. (get_g parent) in

    let target = ((get_h best) +. c ) /. max_h
    and hv = get_h parent in
      if (Math.finite_p hv) && (Math.finite_p target)
      then ignore (show_ex [|hv /. max_h;
			     (get_d parent) /. max_d;
			     (get_g parent) /. max_g; 1.|] target)
  and f_calc n =
    (estimate [|get_h n /. max_h ; get_d n /. max_d; get_g n /. max_g; 1.|])
    *. max_h
  in
    h_calc,f_calc, get_weights


let make_greedy_wdepth i_weights max_g max_h max_d max_dep
    get_g get_h get_d get_dep =
  let show_ex, estimate, get_weights =
    Lms.init_lms ~initial_weights:(Some i_weights)
      ~learning_rate:(default_lr) 5 in
  let h_calc parent best children =
    (*let best = Wrlist.random_elt children in*)
    let c = (get_g best) -. (get_g parent) in

    let target = ((get_h best) +. c ) /. max_h
    and hv = get_h parent in
      if (Math.finite_p hv) && (Math.finite_p target)
      then ignore (show_ex [|hv /. max_h;
			     (get_d parent) /. max_d;
			     (get_g parent) /. max_g;
			     (get_dep parent) /. max_dep;
			     1.|] target)
  and f_calc n =
    (estimate [|get_h n /. max_h ;
		get_d n /. max_d;
		get_g n /. max_g;
		get_dep n /. max_dep;
		1.|])
    *. max_h
  in
    h_calc,f_calc, get_weights


let make_greedy_justh i_weights max_g max_h get_g get_h =
  let show_ex, estimate, get_weights =
    Lms.init_lms ~initial_weights:(Some i_weights)
      ~learning_rate:(default_lr) 3 in
  let h_calc parent best children =
    (*let best = Wrlist.random_elt children in*)
    let c = (get_g best) -. (get_g parent) in

    let target = ((get_h best) +. c ) /. max_h
    and hv = get_h parent in
      if (Math.finite_p hv) && (Math.finite_p target)
      then ignore (show_ex [|hv /. max_h;
			     (get_g parent) /. max_g;
			     1.|] target)
  and f_calc n =
    (estimate [|get_h n /. max_h ;
		get_g n /. max_g;
		1.|])
    *. max_h
  in
    h_calc,f_calc, get_weights

(* EOF *)
