(**

    @author jtd7
    @since 2011-03-21
*)

let order = Laso_record.order


let update_bst alpha get_features nfeatures beam_width weights
    update_cost (key,goal_p,expand,initial) pb =
  let zar = Array.create nfeatures 0.
  and goal = ref false
  and fbw = float beam_width
  and beam = ref [initial] in
  let sum_ars = List.fold_left (fun ac ar -> Wrarray.map2 (+.) ac ar) zar
  and p_bar = Hashtbl.create 50
  and closed = Hashtbl.create 100 in
  let depth = ref 0 in
  let intersect = List.filter (fun n -> Hashtbl.mem p_bar (key n)) in
  (*let intersect_dep depth beam = (let l = pb.(depth) in
				    Wrlist.intersection_by key l beam) in *)
    Array.iter (List.iter (fun n -> Hashtbl.add p_bar (key n) n)) pb;
    while (not !goal) && ((intersect !beam) <> []) do
      (Verb.pe Verb.debug "Beam width: %i\t" (List.length !beam);
       Verb.pe Verb.debug "Remaining in p_bar %i\n%!" (Hashtbl.length p_bar);
       depth := !depth + 1;
       match !beam with
	 | hd::tl ->
	     if goal_p hd
	     then goal := true
	     else
	       (Hashtbl.remove p_bar (key hd);
		let children = expand hd in
		let kids = List.fold_left
		  (fun accum c ->
		     let state = key c in
		       try
			 let prev = Hashtbl.find closed state in
			   if (get_features prev).(0) < (get_features c).(0)
			   then accum
			   else (Hashtbl.replace closed state c; c::accum)
		       with _ -> (Hashtbl.add closed state c;
				  c::accum)) tl children in
		  List.iter update_cost kids;
		  (* stable sort, fifo preserved *)
		  let c = List.stable_sort compare kids in
		    (* grabs the first bw elemeants, order preserved *)
		    beam := fst (Wrlist.split c beam_width);
		    if (intersect !beam) = []
		    then (let proper_in_c = intersect c in
			  let mag_pinc = float (List.length proper_in_c) in
			  let features_pinc = List.map get_features proper_in_c
			  and features_beam = List.map get_features !beam in
			  let features_pinc = sum_ars features_pinc
			  and features_beam = sum_ars features_beam in
			    Verb.pe Verb.toplvl "Learning!\n%!";
			    assert(mag_pinc > 0.);
			    for w_index = 0 to (nfeatures - 1) do
			      weights.(w_index) <- weights.(w_index) +.
				alpha *. (features_pinc.(w_index) /. mag_pinc -.
					    features_beam.(w_index) /. fbw)
			    done;
			    beam := proper_in_c)
		    else Verb.pe Verb.debug "Beam contains good nodes!\n%!")
	 | [] -> failwith "Unpossible")
    done


let learn_weight_vector ?(max_iteration = 1_000_000) ?(min_change = 0.0001)
    ?(alpha = 0.01) kge_list_by_proper_node_list get_features nfeatures
    update_cost beam_width =
  (** Learn weight vector iterates over all of the problems in the instance
      set, calling update_br until either max iterations are exceeded or
      until the delta between weights in an iteration is below some threshold *)
  let weights = Array.init nfeatures (fun i -> 0.)
  and i = ref 0
  and last_weights = Array.create nfeatures infinity in
  let update_bst = (update_bst alpha get_features nfeatures
		      beam_width weights (update_cost weights)) in
    while !i < max_iteration &&
      (Laso_record.delta weights last_weights) > min_change do
	(Verb.pe Verb.always "Iteration: %i\tWeights:" !i;
	 Array.iter (Verb.pe Verb.always " %f") weights;
	 Verb.pe Verb.always "\n%!";
	 Array.iteri (fun i e -> last_weights.(i) <- e) weights;
	 List.iter (fun (kge,pnl) -> update_bst kge pnl)
	   kge_list_by_proper_node_list;
	 i := !i + 1)
    done;
    weights


let learn ?(fleng = 6) tdata =
  let k_by_bfs_nodes = Laso_record.kegi_by_bfs_nodes tdata
  and sf = fst (List.hd tdata) in
  let features = Laso_record.make_get_standard_features sf in
  let update_cost = Laso_record.make_update_cost features in
    learn_weight_vector k_by_bfs_nodes features fleng update_cost

(* EOF*)
