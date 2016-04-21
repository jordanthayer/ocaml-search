(**

    @author jtd7
    @since 2010-10-15

   Uses our learning algorithms to do learning
*)

module RR = Recorded_run


let lms bias run get_features target =
  let num_points = Hashtbl.length run.RR.run in
    Verb.pe Verb.always "Number of data points: %i\n%!" num_points;
    let features = Array.create num_points [||]
    and targets = Array.create num_points nan
    and index = ref 0 in
      Hashtbl.iter (fun key node ->
		      features.(!index) <- get_features node;
		      targets.(!index) <- target node;
		      index := !index + 1;) run.RR.run;
      let num_features = Array.length features.(0) in
      let i_weights = Array.init num_features (fun i -> Random.float 1.) in
	  Verb.pe Verb.always "Initial Weights: ";
	  Array.iter (fun e -> Verb.pe Verb.always "%f\t" e) i_weights;
	  Verb.pe Verb.always "\n%!";
      let show_ex, est, get = (Lms.init_lms ~learning_rate: 0.00001
				 ~initial_weights:(Some i_weights)
				 ~bias
				 num_features) in
	for i = 0 to (num_points - 1) do
	  (let f = features.(i) and t = targets.(i) in
	     (*Verb.pe Verb.always "Learning: ";
	     Array.iter (fun e -> Verb.pe Verb.always "%f\t" e) f;
	     Verb.pe Verb.always "-> %f\n%!" t;*)
	     if Math.finite_p t && (Array.fold_left
				      (fun accum e -> accum &&
					 (Math.finite_p e)) true f)
	     then ignore (show_ex features.(i) targets.(i)))
	done;
	let wts = get() in
	  Verb.pe Verb.always "Learned Weights: ";
	  Array.iter (fun e -> Verb.pe Verb.always "%f\t" e) wts;
	  Verb.pe Verb.always "\n%!";
	  (fun n -> est (get_features n)), get


let lms_truth ?(bias = Fn.identity) run get_features =
  lms bias run get_features RR.get_ht


(* EOF *)
