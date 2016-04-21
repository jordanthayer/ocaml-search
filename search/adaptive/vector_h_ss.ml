(** Imitates a single step update, but is in truth using a fixed vector
    search *)

let h_ind = 0
and d_ind = 1
and g_ind = 2
and depth_ind = 3
and h_rev = 4
and d_rev = 5

let alt_col_name = "wt_vector"
let output_col_hdr () =
  Datafile.write_alt_colnames stdout alt_col_name
    ["h"; "d"; "g"; "depth"; "rev_h"; "rev_d";]

let output_vector v =
  Datafile.write_alt_row_prefix stdout alt_col_name;
  Verb.pr Verb.always "%f\t%f\t%f\t%f\t%f\t%f\n"
    v.(h_ind) v.(d_ind) v.(g_ind) v.(depth_ind) v.(h_rev) v.(d_rev)


let output v =
  output_col_hdr ();
  output_vector v

let h_calc _ _ _ = ()

let do_calc wts features =
  let v,_ =
  Array.fold_left (fun (product,index) wt ->
		     (product +. wt *. features.(index), index + 1)) (0.,0) wts
  in v

let make_unbounded_correction make_vector wt_vector =
  output wt_vector;
  let calc = do_calc wt_vector in
    h_calc,
  (fun node ->
     let vect = make_vector node in
       vect.(g_ind) +. (vect.(h_ind) +. (calc vect)))


let make_bounded_correction make_vector wt_vector bound =
  output wt_vector;
  let calc = do_calc wt_vector in
    h_calc,
  (fun node ->
     let vect = make_vector node in
     let wf = bound *. (vect.(g_ind) +. vect.(h_ind)) in
       Math.fmin wf (vect.(g_ind) +. (vect.(h_ind) +. (calc vect))))


let make_bounded_fhp_correction make_vector wt_vector bound =
  output wt_vector;
  let calc = do_calc wt_vector in
    h_calc,
  (fun node ->
     let vect = make_vector node in
     let wf = bound *. (vect.(g_ind) +. vect.(h_ind)) in
     let fhat = (vect.(g_ind) +. bound *. (vect.(h_ind) +. (calc vect))) in
       (*Verb.pe Verb.debug "%f vs %f\n" wf fhat;*)
       Math.fmin wf fhat)


(* EOF *)


