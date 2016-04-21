(**

    @author jtd7
    @since 2011-03-21
*)

let order = Laso_record.order

let update_br alpha get_features nfeatures beam_width weights update_cost
    (key,_,expand,initial) proper_nodes =
  (** runs through a single search trace updating the weights as we go along*)
  let zar = Array.create nfeatures 0. in
  let beam = ref [initial]
  and comp_beam = ref []
  and sum_ars = List.fold_left (fun ac ar -> Wrarray.map2 (+.) ac ar) zar
  and fbw = float beam_width in
  let p = Array.of_list proper_nodes in
    Verb.pe Verb.debug "Starting Update_br...%!";
    for depth = 1 to ((Array.length p) - 1) do
      (let proper_nodes = p.(depth) in
       let expanded_beam =  (List.fold_left (fun accum n ->
					    (expand n) @ accum) [] !beam) in
	 List.iter update_cost expanded_beam;
	 comp_beam := Wrlist.remove_dups key (List.sort order expanded_beam);
	 beam := fst (Wrlist.split !comp_beam beam_width);
	 if (Wrlist.intersection_by key !beam proper_nodes) = []
	 then (let proper_in_c = (Wrlist.intersection_by key
				    !comp_beam proper_nodes) in
	       let mag_pinc = float (List.length proper_in_c) in
	       let features_pinc = List.fold_left
		 (fun accum n -> get_features n::accum) [] proper_in_c
	       and features_beam = List.fold_left
		 (fun accum n -> get_features n :: accum) [] !beam in
	       let features_pinc = sum_ars features_pinc
	       and features_beam = sum_ars features_beam in
		 if (mag_pinc = 0.)
		 then failwith (Printf.sprintf "properinc 0 at depth %i" depth)
		 else (for w_index = 0 to (nfeatures - 1) do
			 weights.(w_index) <- weights.(w_index) +.
			   alpha *. (features_pinc.(w_index) /. mag_pinc -.
				       features_beam.(w_index) /. fbw)
		       done;
		       beam := proper_in_c)))
    done;
    Verb.pe Verb.debug "Update_br done!\n%!"


let learn_weight_vector ?(max_iteration = 1_000_000) ?(min_change = 0.0001)
    ?(alpha = 0.01) sface_list_by_proper_node_list get_features nfeatures
    update_cost beam_width =
  (** Learn weight vector iterates over all of the problems in the instance
      set, calling update_br until either max iterations are exceeded or
      until the delta between weights in an iteration is below some threshold *)
  let weights = Array.create nfeatures 0.
  and i = ref 0
  and last_weights = Array.create nfeatures infinity in
  let update_br = (update_br alpha get_features nfeatures
		     beam_width weights (update_cost weights)) in
    while !i < max_iteration &&
      (Laso_record.delta weights last_weights) > min_change do
	(Verb.pe Verb.always "Iteration: %i\tWeights:" !i;
	 Array.iter (Verb.pe Verb.always " %f") weights;
	 Verb.pe Verb.always "\n%!";
	 Array.iteri (fun i e -> last_weights.(i) <- e) weights;
	 List.iter (fun (sface,pnl) -> update_br sface pnl)
	   sface_list_by_proper_node_list;
	 i := !i + 1;)
    done;
    weights


let learn ?(fleng = 6) tdata =
  let k_by_brfs_nodes = Laso_record.kegi_by_brfs_nodes tdata
  and sf = fst (List.hd tdata) in
  let features = Laso_record.make_get_standard_features sf in
  let update_cost = Laso_record.make_update_cost features in
  learn_weight_vector k_by_brfs_nodes features fleng update_cost

(* EOF *)
