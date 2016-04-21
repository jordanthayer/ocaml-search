(** Single step error learned via lms streaming regression *)

let alt_col_name = "correction"
let default_lr = Lms_h.default_lr
let learner = Lms.init_nlms_momentum


let alt_col_name = "correction"

let output_col_hdr h_len d_len  =
  let h_ar = Array.init h_len (fun i -> Wrutils.str "h_%i" (i+1))
  and d_ar = Array.init d_len (fun i -> Wrutils.str "d_%i" (i+1)) in
  Datafile.write_alt_colnames stdout alt_col_name
    ("observations"::
       (Array.to_list h_ar) @ (Array.to_list d_ar))


let output_row samples h_ar d_ar =
  Datafile.write_alt_row_prefix stdout alt_col_name;
  Verb.pr Verb.always "%i\t" samples;
  Array.iter (fun e -> Verb.pr Verb.always "%f\t" e) h_ar;
  Array.iter (fun e -> Verb.pr Verb.always "%f\t" e) d_ar;
  Verb.pr Verb.always "\n"


let output_geometric_sched ?(duration = 2) output =
  let i = ref 0
  and next = ref duration in
    (fun h_ar d_ar ->
       if !i >= !next
       then (i := !i + 1;
	     next := (!next * 15) / 10;
	     output !i h_ar d_ar)
       else i := !i + 1)


let print_vector ar1 target1 ar2 target2 =
  Array.iter (fun e -> Verb.pe Verb.always "%f\t" e) ar1;
  Verb.pe Verb.always "%f\t" target1;
  Array.iter (fun e -> Verb.pe Verb.always "%f\t" e) ar2;
  Verb.pe Verb.always "%f\n" target2


let gen_make_unbounded_correction (get_h_array,h_length) (get_d_array,d_length)
    get_g get_h get_d =
  (** Uses the [bound] to create a correction to h based on single step
      error.  The corrected heuristic value will still obey admissibility,
      but may not be consistent.  Make correction returns two functions.
      [h_calc] takes the parent node, the best node, and a list of all
      children in order to update the correction.  [f_calc] returns the
      estimated total cost of the node *)
  output_col_hdr h_length d_length;
  let record = output_geometric_sched output_row
  and show_hex, hestimate, get_hweights =
    learner ~initial_weights:(Some (Array.init h_length
				      (fun i -> if i = 0 then 1. else 0.)))
      ~learning_rate:(default_lr) h_length
  and show_dex, destimate, get_dweights =
    learner ~initial_weights:(Some (Array.init d_length
				      (fun i -> if i = 0 then 1. else 0.)))
      ~learning_rate:(default_lr) d_length
  and num_samp = ref 0 in
  let h_calc parent best children =
    let h_ar = get_h_array parent
    and d_ar = get_d_array parent in
    let thv = hestimate (get_h_array best) +. ((get_g best) -. (get_g parent))
    and tdv = destimate (get_d_array best) +. 1.
    and hv = get_h parent
    and dv = get_d parent in
      if (Math.finite_p hv) && (Math.finite_p dv) && (Math.finite_p thv) &&
	(Array.fold_left (fun accum ele ->
			    accum && Math.finite_p ele) true h_ar) &&
	(Array.fold_left (fun accum ele ->
			    accum && Math.finite_p ele) true d_ar)
      then
	((*print_vector h_ar thv d_ar tdv;*)
	 ignore (show_hex h_ar thv);
	 ignore (show_dex d_ar tdv);
	 num_samp := !num_samp + 1;
	 record (get_hweights()) (get_dweights()))
  and f_calc n =
    ((get_g n) +. (hestimate (get_h_array n)),
     (destimate (get_d_array n)))

  in
    h_calc,f_calc


let add_dhat h_array d_hat =
  Array.append h_array [|d_hat|]


let make_unbounded_correction_wdh (get_h_array,h_length) (get_d_array,d_length)
    get_g get_h get_d =
  (** Uses the [bound] to create a correction to h based on single step
      error.  The corrected heuristic value will still obey admissibility,
      but may not be consistent.  Make correction returns two functions.
      [h_calc] takes the parent node, the best node, and a list of all
      children in order to update the correction.  [f_calc] returns the
      estimated total cost of the node *)
  output_col_hdr h_length d_length;
  let record = output_geometric_sched output_row
  and show_hex, hestimate, get_hweights =
    learner ~initial_weight:1. ~learning_rate:(default_lr) (h_length + 1)
  and show_dex, destimate, get_dweights =
    learner ~initial_weight:1. ~learning_rate:(default_lr) d_length
  and num_samp = ref 0 in
  let h_calc parent best children =
    let h_ar = get_h_array parent
    and d_ar = get_d_array parent in
    let tdv = destimate (get_d_array best) +. 1. in
    let thv = (hestimate (add_dhat (get_h_array best) tdv) +.
		 ((get_g best) -. (get_g parent)))
  and hv = get_h parent and dv = get_d parent in
      if (Math.finite_p hv) && (Math.finite_p dv) && (Math.finite_p thv) &&
	(Array.fold_left (fun accum ele ->
			    accum && Math.finite_p ele) true h_ar) &&
	(Array.fold_left (fun accum ele ->
			    accum && Math.finite_p ele) true d_ar)
      then
	(print_vector h_ar thv d_ar tdv;
	 let dest,_ = show_dex d_ar tdv in
	   ignore (show_hex (add_dhat h_ar dest) thv);
	   num_samp := !num_samp + 1;
	   record (get_hweights()) (get_dweights()))
  and f_calc n =
    let est_d = Math.fmax (destimate (get_d_array n)) (get_d n) in
    ((get_g n) +. (hestimate (add_dhat (get_h_array n) est_d)),
     est_d)
  in
    h_calc,f_calc

(* EOF *)
