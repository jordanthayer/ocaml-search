(** A batched learner, instead of your standard stream learners for
    attempting to improve both h and d during the course of a search. *)

open Format
open Bigarray
open Lacaml.Impl.D
open Lacaml.Io

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



let pairs_to_mats val_array t_array =
  (Mat.of_array val_array),
  (Mat.of_array t_array)


let get_least_squares (v_mat, t_mat) ar =
  (* This destroys v_mat and t_mat!! *)
  (** [v_mat] - value matrix
      [t_mat] - target matrix
      [p_count] - length of an entry of [v_mat] *)
  Verb.pe Verb.toplvl "Doing regression\n";
  let p_count = Array.length ar in
    ignore(gelsd v_mat t_mat); (* Finds the least squares solution *)
    Verb.pe Verb.toplvl "Regression done\n";
    for i = 0 to (p_count - 1)
    do
      ar.(i) <- t_mat.{(i+1),1}
    done



let make_estimate feature_array weight_array =
  Wrarray.fold_left2
    (fun accum feat weight ->
	accum +. feat *. weight) 0. feature_array weight_array


let print_vector ar1 target1 ar2 target2 =
  Array.iter (fun e -> Verb.pe Verb.always "%f\t" e) ar1;
  Verb.pe Verb.always "%f\t" target1;
  Array.iter (fun e -> Verb.pe Verb.always "%f\t" e) ar2;
  Verb.pe Verb.always "%f\n" target2


let make_unbounded_correction ?(time = 100)
    (get_h_array,h_length) (get_d_array,d_length) get_g get_h get_d =
  output_col_hdr h_length d_length;
  let target_hdata = ref []
  and target_ddata = ref []
  and h_observations = ref []
  and d_observations = ref []
  and obs_count = ref 0
  and h_weights = Array.init h_length (fun i -> if i > 0 then 0. else 1.)
  and d_weights = Array.init d_length (fun i -> if i > 0 then 0. else 1.) in
  let record = output_geometric_sched output_row in
  let h_calc parent best children =
    let h_ar = get_h_array parent
    and d_ar = get_d_array parent in
    let target_hv = ((make_estimate (get_h_array best) h_weights) +.
		       ((get_g best) -. (get_g parent)))
    and target_dv = (make_estimate (get_d_array best) d_weights) +. 1. in
      if Math.finite_p target_hv && Math.finite_p target_dv &&
	(Array.fold_left (fun accum ele ->
			    accum && Math.finite_p ele) true h_ar) &&
	(Array.fold_left (fun accum ele ->
			    accum && Math.finite_p ele) true d_ar)
      then (* it appears our new datapoint is solid *)
	(print_vector h_ar target_hv d_ar target_dv;
	 target_hdata := (get_h_array best,
			  (get_g best) -. (get_g parent)) :: !target_hdata;
	 target_ddata := (get_d_array best) :: !target_ddata;
	 h_observations := h_ar :: !h_observations;
	 d_observations := d_ar :: !d_observations;
	 obs_count := !obs_count + 1;
	 if !obs_count > time
	 then (obs_count := 0;
	       (get_least_squares
		  (pairs_to_mats
		     (Array.of_list !h_observations)
		     (Array.map
			(fun (a,b) ->
			   [|((make_estimate a h_weights) +. b)|])
			(Array.of_list !target_hdata)))
		     h_weights);
	       (get_least_squares
		  (pairs_to_mats
		     (Array.of_list !d_observations)
		     (Array.map
			(fun a ->
			   [|((make_estimate a d_weights)+.1.)|])
			(Array.of_list !target_ddata))) d_weights));
	 record h_weights d_weights)
 and f_calc n =
    (get_g n) +. (Math.fmax (get_h n)
		    (make_estimate (get_h_array n) h_weights)),
 (Math.fmax (get_d n) (make_estimate (get_d_array n) d_weights))
 in h_calc, f_calc


(* EOF *)
