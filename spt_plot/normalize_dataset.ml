(**

    @author jtd7
    @since 2010-05-28
*)


let norm_by_alg group_key norm_key ?(wkey = norm_key) norm_to datasets =
  List.map (Dataset.transform_with group_key norm_to norm_key ~with_key:wkey
	      (fun a b -> b /. a)) datasets


let norm_fn norm_by_fn ?(norm_fn = (/.)) group_key norm_key datasets =
  let by_group = Dataset.group_by [| group_key |] (Dataset.merge datasets) in
  let group_by_min_list = List.map
    (fun ds -> (truncate (float_of_string
			      (Dataset.get_group_value group_key ds)),
		norm_by_fn norm_key ds)) by_group  in
    List.map
      (Dataset.transform norm_key
	 (fun df vl ->
	    let gkey = truncate (float_of_string
				   (Datafile.get_val df group_key)) in
	      try
		(norm_fn vl (List.assoc gkey group_by_min_list))
	      with Not_found -> (Verb.pe Verb.debug
				   "Couldn't find %i in assoc list of %s\n%!"
				   gkey group_key;
				 Verb.pe Verb.debug "[\n";
				 List.iter
				   (fun (a,_) -> Verb.pe Verb.debug "%i; " a)
				   group_by_min_list;
				 Verb.pe Verb.debug "]\n%!";
				 raise Not_found)))
      datasets

let norm a b =
  (* b is the best / max / min of everyone *)
  let fa = Math.finite_p a
  and fb = Math.finite_p b in
    if(not fa && not fb) then 1.
    else if a = 0. then 1.
    else (if (not fb) then 1.
	  else (if fa then b /. a
		else 0.))

let norm_min = norm_fn Dataset.get_min ~norm_fn:norm

and norm_max = norm_fn Dataset.get_max ~norm_fn:norm

and filter_nan = norm_fn Dataset.get_max ~norm_fn:(fun a b ->
						     if Math.finite_p a
						     then a
						     else b)

(* EOF *)
