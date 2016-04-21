(**

   @author jtd7
   @since 2010-10-23
*)

module RR = Recorded_run


let get_maxes features target =
  let max_features = Array.copy (features.(0))
  and max_target = ref target.(0) in
    for i = 0 to (Array.length features) - 1
    do
      (for j = 0 to (Array.length features.(0)) - 1
       do
	 (if Math.finite_p features.(i).(j)
	  then max_features.(j) <- Math.fmax max_features.(j) features.(i).(j))
       done;
       if Math.finite_p target.(i)
       then max_target := Math.fmax !max_target target.(i))
    done;
    !max_target, max_features


let get_maxes_run features run =
  let root = RR.get_node run run.RR.sequence.(0) in
  let f = features root in
    f.(2) <- f.(0);
    let f = Array.map (fun a -> a *. 2.5) f in
    f.(3) <- 1.;
    f.(0), f


let norm maxes features =
  Wrarray.map2 (fun a b -> a /. b) features maxes


let lms ?(ethresh = 0.001) ?(nepoch = 100) bias run get_features target =
  let num_points = Hashtbl.length run.RR.run in
    Verb.pe Verb.always "Number of data points: %i\n%!" num_points;
    let features = Array.create num_points [||]
    and targets = Array.create num_points nan
    and index = ref 0 in
      Hashtbl.iter (fun _ node ->
		      features.(!index) <- get_features node;
		      targets.(!index) <- target node;
		      index := !index + 1;) run.RR.run;
      Wrarray.permute_pair features targets;
      let max_target, max_features = get_maxes_run get_features run in
      let num_features = Array.length features.(0) in
      let i_weights = Array.init num_features (fun i -> Random.float 1.) in
	(*i_weights.(0) <- 1.;*)
	Verb.pe Verb.always "Initial Weights: ";
	Array.iter (fun e -> Verb.pe Verb.always "%f\t" e) i_weights;
	Verb.pe Verb.always "\n%!";
	let show_ex, est, get = (Lms.init_lms ~learning_rate: 0.001
				   ~initial_weights:(Some i_weights)
				   ~bias
				   num_features) in
	let avg_error = ref infinity
	and count = ref 0 in
	  while ((not (Math.finite_p !avg_error)) || !avg_error > ethresh) &&
	    !count < nepoch
	  do
	    (avg_error := 0.;
	     count := !count + 1;
	     for i = 0 to (num_points - 1) do
	       (let f = features.(i) and t = targets.(i) in
		  if Math.finite_p t && (Array.fold_left
					   (fun accum e -> accum &&
					      (Math.finite_p e)) true f)
		  then
		    let step_error = abs_float
		      (snd (show_ex
			      (norm max_features features.(i))
			      (targets.(i) /. max_target)))
		    in avg_error := !avg_error +. (step_error /.
						     (float num_points)))
	     done;
	     Verb.pe Verb.always "Count: %i MaxError: %f\n%!"
	       !count !avg_error;)
	  done;
	  let wts = get() in
	    Verb.pe Verb.always "Learned Weights: ";
	    Array.iter (fun e -> Verb.pe Verb.always "%f\t" e) wts;
	    Verb.pe Verb.always "\n%!";
	    (fun n -> (est (norm max_features (get_features n))) *.
	       max_target), get


let ann ?(hidden = 3) ?(ethresh = 20.) ?(nepoch = 100) run get_features target =
  let num_points = Hashtbl.length run.RR.run in
    Verb.pe Verb.always "Number of data points: %i\n%!" num_points;
    let features = Array.create num_points [||]
    and targets = Array.create num_points nan
    and index = ref 0 in
      Hashtbl.iter (fun key node ->
		      features.(!index) <- get_features node;
		      targets.(!index) <- target node;
		      index := !index + 1;) run.RR.run;
      Wrarray.permute_pair features targets;
      let max_target, max_features = get_maxes_run get_features run in
      let features = Array.map (norm max_features) features
      and targets = Array.map (fun a -> a /. max_target) targets in
      let network = Ann.two_layer (Array.length (features.(0))) hidden in
	Ann.batch_train_twolayer ~ethresh ~nepoch network features targets;
	(fun n -> (Ann.two_layer_output network (norm max_features
						   (get_features n))) *.
	   max_target), network


(****************************************************************************)
(** This is some code for doing batched regressions over many runs.  The idea
    is that by comparing the differences in what we learn on one instance versus
    what we learn in aggregate, we'll be able to show that there are some
    problems that vary so dramatically instance to instance that learning across
    all of them is going to be worse than the individually learned heuristics *)


let batched_lms ?(ethresh = 0.001) ?(nepoch = 100) runs get_features target =
  let features = ref [||]
  and targets = ref [||]
  and index = ref 0 in
    List.iter (fun run ->
		 let run = run () in
		 let max_target, max_features =
		   get_maxes_run get_features run in
		   Verb.pe Verb.always
		     "Max target: %f Max features: " max_target;
		   Array.iter
		     (fun e -> Verb.pe Verb.always "%f\t" e) max_features;
		   Verb.pe Verb.always "\n%!";
		   index := 0;
		   let num_points = Hashtbl.length run.RR.run in
		     Verb.pe Verb.always "Number of data points: %i\n%!"
		       num_points;
		     let f = Array.create num_points [||]
		     and t = Array.create num_points nan in
		       Hashtbl.iter (fun _ node ->
				       f.(!index) <- (norm max_features
							(get_features node));
				       t.(!index) <- target node /. max_target;
				       index := !index + 1;) run.RR.run;
		       features := Array.append !features f;
		       targets := Array.append !targets t;) runs;
    Wrarray.permute_pair !features !targets;
      (* I may want to consider doing the next two steps in place *)
    let num_features = Array.length !features.(0)
    and num_points = Array.length !features in
    let i_weights = Array.init num_features (fun i -> Random.float 1.) in
      (*i_weights.(0) <- 1.;*)
      Verb.pe Verb.always "Initial Weights: ";
      Array.iter (fun e -> Verb.pe Verb.always "%f\t" e) i_weights;
      Verb.pe Verb.always "\n%!";
      let show_ex, est, get = (Lms.init_lms ~learning_rate: 0.001
				 ~initial_weights:(Some i_weights)
				 num_features) in
      let avg_error = ref infinity
      and count = ref 0 in
	while ((not (Math.finite_p !avg_error)) || !avg_error > ethresh) &&
	  !count < nepoch
	do
	  (avg_error := 0.;
	   count := !count + 1;
	   for i = 0 to (num_points - 1) do
	     (let f = !features.(i) and t = !targets.(i) in
		if Math.finite_p t && (Array.fold_left
					 (fun accum e -> accum &&
					    (Math.finite_p e)) true f)
		then
		  let step_error = abs_float
		    (snd (show_ex !features.(i) !targets.(i)))
		  in avg_error := Math.fmax !avg_error step_error)
	   done;
	   Verb.pe Verb.always "Count: %i MaxError: %f\n%!"
	     !count !avg_error;)
	done;
	let wts = get() in
	  Verb.pe Verb.always "Learned Weights: ";
	  Array.iter (fun e -> Verb.pe Verb.always "%f\t" e) wts;
	  Verb.pe Verb.always "\n%!";
	  est,get


let batched_ann ?(hidden = 3) ?(ethresh = 0.001) ?(nepoch = 100) runs
    get_features target =
  (* the runs are actually thunks for loading runs *)
  let features = ref [||]
  and targets = ref [||]
  and index = ref 0 in
    List.iter (fun run ->
		 let run = run () in
		 let max_target, max_features =
		   get_maxes_run get_features run in
		   index := 0;
		   let num_points = Hashtbl.length run.RR.run in
		     Verb.pe Verb.always "Number of data points: %i\n%!"
		       num_points;
		     let f = Array.create num_points [||]
		     and t = Array.create num_points nan in
		       Hashtbl.iter (fun key node ->
				       f.(!index) <- norm max_features
					 (get_features node);
				       t.(!index) <- target node /. max_target;
				       index := !index + 1;) run.RR.run;
		       features := Array.append !features f;
		       targets := Array.append !targets t;) runs;
    Wrarray.permute_pair !features !targets;
      (* I may want to consider doing the next two steps in place *)
    let network = Ann.two_layer (Array.length (!features.(0))) hidden in
      Ann.batch_train_twolayer ~ethresh ~nepoch network !features !targets;
      network

(* EOF *)
