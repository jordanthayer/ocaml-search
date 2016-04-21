(**

    @author jtd7
    @since 2011-09-28
   This is about the point I started to wonder what I was doing with my life.
*)

let alt_col_name = "correction"
let lr = 0.001


let alt_col_name = "correction"

let output_col_hdr h_len  =
  let h_ar = Array.init h_len (fun i -> Wrutils.str "h_%i" (i+1)) in
  Datafile.write_alt_colnames stdout alt_col_name
    ("observations"::
       (Array.to_list h_ar))


let output_row samples h_ar =
  Datafile.write_alt_row_prefix stdout alt_col_name;
  Verb.pr Verb.always "%i\t" samples;
  Array.iter (fun e -> Verb.pr Verb.always "%f\t" e) h_ar;
  Verb.pr Verb.always "\n"


let output_geometric_sched ?(duration = 2) output =
  let i = ref 0
  and next = ref duration in
    (fun h_ar ->
       if !i >= !next
       then (i := !i + 1;
	     next := (!next * 15) / 10;
	     output !i h_ar)
       else i := !i + 1)


let combine maxes weights features =
  let sum = ref 0. in
  for i = 0 to ((Array.length weights) - 1) do
    sum := !sum +. weights.(i) *. features.(i) /. maxes.(i)
  done;
    !sum

let combine_nm weights features =
  let sum = ref 0. in
  for i = 0 to ((Array.length weights) - 1) do
    sum := !sum +. weights.(i) *. features.(i)
  done;
    !sum


let make_unbounded_correction (get_h_feat,h_length) get_g maxes =
  assert(lr < 1.);
  let h_array = Array.create h_length 0. in
  let combine = combine maxes h_array in
  let combine_nm = combine_nm h_array in
  let output = output_geometric_sched output_row in
  let calculate_hhat n =
    let phi = get_h_feat n in
    let h = phi.(0) in
    let v = (combine phi) in
      Math.fmax h v
  and calculate_c ~p ~bc = (get_g bc) -. (get_g p) in
  let update_h_wts p bc _ = (* _ would be children, but we can ignore it *)
    let c = (calculate_c ~p ~bc)
    and phi1 = get_h_feat p
    and phi2 = get_h_feat bc in
    let diffs = Array.init h_length (fun i ->
				       (phi1.(i) -. phi2.(i)) /. maxes.(i)) in
    let error = c -. (combine_nm diffs) in
      assert(c >= 0.);
      if Math.finite_p error
      then (let update = error *. lr in
	      Array.iteri (fun i e -> h_array.(i) <- e +. diffs.(i) *. update)
		h_array);
      output h_array in
    h_array.(0) <- 1.;
    output_col_hdr h_length;
    update_h_wts, calculate_hhat,  (fun _ -> h_array)


let make_unbounded_lms_correction (get_h_feat,h_length) get_g maxes =
  let h_array = Array.create h_length 0. in
    h_array.(0) <- 1.;
  let show_ex, estimate, get_weights =
    Lms.init_nlms_momentum ~initial_weights:(Some h_array) ~learning_rate:lr h_length in
  let output = output_geometric_sched output_row in
  let calculate_hhat n =
    let phi = get_h_feat n in
    let h = phi.(0) in
      Array.iteri (fun i e -> phi.(i) <- e /. maxes.(i)) phi;
      Math.fmax h (estimate phi)
  and calculate_c ~p ~bc = (get_g bc) -. (get_g p) in
  let update_h_wts p bc _ = (* _ would be children, but we can ignore it *)
    let c = (calculate_c ~p ~bc)
    and phi1 = get_h_feat p
    and phi2 = get_h_feat bc in
    let diffs = Array.init h_length (fun i ->
				       (phi1.(i) -. phi2.(i)) /. maxes.(i)) in
      ignore (show_ex diffs c);
      output (get_weights ()) in
    output_col_hdr h_length;
    update_h_wts, calculate_hhat,  get_weights


(* EOF *)
