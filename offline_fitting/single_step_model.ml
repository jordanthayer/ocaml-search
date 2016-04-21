(**

    @author jtd7
    @since 2010-11-10
*)

module RR = Recorded_run


let standard_features n = [| RR.get_h n; RR.get_d n; RR.get_g n; 1.|]

let rev_features n = [| RR.get_rh n; RR.get_rd n; 1.|]



let maxes features run = Batched_regression.get_maxes_run features run


let get_features_targets feat run =
  let max_cost, max_feat  = maxes feat run in
  let pairs = RR.get_best_pairs run in
  let features = List.rev
    (List.rev_map (fun (p,_) -> Wrarray.map2 (fun a b -> a /. b)
		     (feat p) max_feat) pairs)
  and targets = List.rev
    (List.rev_map (fun (p,c) ->
		     ((RR.get_h c) +.
			((RR.get_g c) -. (RR.get_g p))) /. max_cost)
       pairs) in
    Verb.pe Verb.always "%i examples\n" (List.length features);
    features,targets


let get_features_targets_m2 feat run =
  let max_cost, max_feat  = maxes feat run in
  let pairs = RR.get_best_pairs run in
  let features = List.rev
    (List.rev_map (fun (p,c) -> Wrarray.map2 (fun a b -> a /. b)
		     (Wrarray.map2 (-.) (feat p) (feat c)) max_feat) pairs)
  and targets =
    List.rev
    (List.rev_map (fun (p,c) ->
		     ((RR.get_g c) -. (RR.get_g p)) /. max_cost)
       pairs) in
    Verb.pe Verb.always "%i examples\n" (List.length features);
    features,targets


let rev_features_targets feat run =
  let max_cost, max_feat  = maxes feat run in
  let pairs = RR.get_best_pairs run in
  let features = List.rev
    (List.rev_map (fun (p,_) -> Wrarray.map2 (fun a b -> a /. b)
		     (feat p) max_feat) pairs)
  and targets = List.rev
    (List.rev_map (fun (p,c) -> (RR.get_g p) /. max_cost) pairs) in
    Verb.pe Verb.always "%i examples\n" (List.length features);
    features,targets


let streamed_lms (features,targets) =
  let show_ex, est, _ = (Lms.init_lms
			   (*~initial_weights:(Some [|1.; 0.; 0.; 0.|])*) 4) in
    List.iter2 (fun f t -> ignore (show_ex f t)) features targets;
    est


let streamed_ann (features,targets) =
  let network = Ann.two_layer 4 3 in
  let rec get_first n = function
    | [] -> [],[]
    | hd::tl -> (if n > 0
		 then (let (f,s) = get_first (n-1) tl in
			 hd::f,s)
		 else [],tl) in
  let first_100_feat,rest_feat = get_first 100 features
  and first_100_targ,rest_targ = get_first 100 targets in
    Ann.batch_train_twolayer network
      (Array.of_list first_100_feat)
      (Array.of_list first_100_targ);
    List.iter2 (fun f t -> ignore
		  (Ann.show_example_twolayer network f t))
      rest_feat
      rest_targ;
    (Ann.two_layer_output network)


let global_ss run =
  let pairs = RR.get_best_pairs run
  and total_eh = ref 0.
  and total_ed = ref 0. in
  let count = float (List.length pairs) in
    List.iter (fun (p,c) ->
		 total_eh := !total_eh +.
		   ((RR.get_h c) +. ((RR.get_g c) -. (RR.get_g p)) -.
		      (RR.get_h p));
		 total_ed := !total_ed +. ((RR.get_d c) +. 1. -. (RR.get_d p)))
      pairs;
    let avg_eh = !total_eh /. count
    and avg_ed = !total_ed /. count in
    (fun n ->
       let dhat = (RR.get_d n) /. (1. -. avg_ed) in
       (RR.get_h n) +.  dhat *. avg_eh)


let path_ss run =
  (fun n ->
     let path = RR.get_path run.RR.run n in
     let path = List.rev (List.fold_left (fun accum e ->
					    try
					      (RR.get_node run e)::accum
					    with Not_found -> accum) [] path) in
     let avg_ed = (RR.path_accum 0. (+.)
		     (fun p c -> (RR.get_d c) -. (RR.get_d p) +. 1.) path)
     and avg_eh = (RR.path_accum 0. (+.)
		     (fun p c -> (RR.get_h c) -. (RR.get_h p) +.
			((RR.get_g c) -. (RR.get_g p))) path) in
     let dhat = (RR.get_d n) /. (1. -. avg_ed) in
       (RR.get_h n) +.  dhat *. avg_eh)


let justh_ss run =
  (fun n ->
     let path = RR.get_path run.RR.run n in
     let path = List.rev (List.fold_left (fun accum e ->
					    try
					      (RR.get_node run e)::accum
					    with Not_found -> accum) [] path) in
     let avg_eh = (RR.path_accum 0. (+.)
		     (fun p c ->
			let cost = (RR.get_g c) -. (RR.get_g p) in
			  ((RR.get_h c) +. cost -. (RR.get_h p)) /. cost)
		     path) in
       (RR.get_h n) /. (1. -. avg_eh))


let global_justh run =
  let pairs = RR.get_best_pairs run
  and total_eh = ref 0. in
  let count = float (List.length pairs) in
    List.iter (fun (p,c) ->
		 let cost = ((RR.get_g c) -. (RR.get_g p)) in
		 total_eh := !total_eh +.
		   ((RR.get_h c) +.  cost -. (RR.get_h p)) /. cost;)
      pairs;
    let avg_eh = !total_eh /. count in
    (fun n ->
       (RR.get_h n) /. (1. -. avg_eh))

(* EOF *)
