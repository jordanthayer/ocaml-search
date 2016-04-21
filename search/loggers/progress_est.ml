(**

    @author jtd7


   Tool for logging deadline estimations
*)

let alt_col_name = "progress_estimate"

let output_col_hdr () =
  Datafile.write_alt_colnames stdout alt_col_name
    ["expansion"; "depth"; "estimated_remaining"; "estimated_percent"]


let output_row ~expansion ~depth (estimate,percent) =
  if (Math.finite_p estimate) && (Math.finite_p percent)
  then
    (Datafile.write_alt_row_prefix stdout alt_col_name;
     Verb.pr Verb.always "%i\t%i\t%f\t%f\n" expansion depth estimate percent)


let make_output_fixed ?(dur = 50) output =
  let i = ref 0 in
  let do_output ~expansion ~depth (estimate, percent) =
    if !i == dur
    then (output ~expansion ~depth (estimate, percent);
	  i := 0)
    else i := !i + 1 in
    output_col_hdr ();
    do_output


let make_output_geo ?(dur = 2) output =
  let i = ref 0
  and next = ref dur in
  let do_output ~expansion ~depth (estimate, percent) =
    if !i == dur
    then (output ~expansion ~depth (estimate, percent);
	  next := (!next * 15) / 10)
    else i := !i + 1 in
    output_col_hdr ();
    do_output


let make_output_p ?(p = 0.01) output =
  let do_output ~expansion ~depth get_est =
    if Math.true_with_prob p
    then (let estimate,percent = get_est true in
	    output ~expansion ~depth (estimate,percent))
    else ignore (get_est false) in
    output_col_hdr ();
    do_output


let make_output_stream ?(num_samp = 250) output =
  let cons, pi, get_sample, _ = Sample.make_stream_sampler_prime num_samp in
  let sample ~expansion ~depth get_est =
    if cons ()
    then (let ep = get_est true in
	    pi (expansion, depth, ep))
    else ignore (get_est false) in
  let final_output () =
    let ar = get_sample() in
      Array.sort (fun (e,_,_) (e',_,_) -> compare e e') ar;
      Array.iter (fun (expansion,depth, foo) ->
		    output ~expansion ~depth foo) ar in
    output_col_hdr ();
    sample, final_output, output



(* EOF *)
