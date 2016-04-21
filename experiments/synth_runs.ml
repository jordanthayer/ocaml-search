(** Run file for the synthetic instances *)

let data_root = Experiments.data_root ^ "synth"
and instance_root = Experiments.instance_root ^ "synthetic_instances"
and solver_binary = ref (Experiments.bin_root ^ "synthetic_solver")


let make_rst_instance scale depth branch max_edge_cost seed i =
  let t = Rst_tree.make_tree scale max_edge_cost branch depth seed in
  let path = Rdb.path_for instance_root (("type", "instance")::
					   (Rst_tree.config_to_pairs t)) in
    if Sys.file_exists path && Datafile.seems_complete path then
      Wrutils.pr "%s exists, skipping...\n%!" path
    else
      Wrio.with_outfile path (fun ch -> Rst_tree.write_pairs t ch)


let make_rst_instances scale depth branch max_edge_cost n =
  Random.self_init();
  for i = 1 to n
  do
    make_rst_instance scale depth branch max_edge_cost
      (truncate (Random.float Random_tree.my_max)) i
  done


 (* branching factor 5 & 7 trees are too large to use more than 10, 5 nodes *)
let depths = [5; 10; 20;]
and branches = [2; 5; 7;]
and max_edge_cost = [1.;]
and scale = [Rst_tree.Uniform;
	      Rst_tree.Linear_Increase;
	      Rst_tree.Linear_Decrease]
and models = [(*"rst";*)"stu"; (* "silvia"*)]
and heuristics = [(*"constant_percent";*) "random_percent"; "random_median"; "scaled_error"]
and corruptions = [(*0.1; 0.2; *) (*0.3;*) 0.5; (*0.7; 0.9*)] (* 0 is uninteresting. *)


let truth_pairs = [["truth", 0.];]

let pairs = (*truth_pairs @ *)(List.map (fun str -> List.map
				       (fun num -> str,num)
				       corruptions)
			     heuristics)


let caching = ref true

let make_rst_batch count =
  List.iter
    (fun d ->
       List.iter
	 (fun b ->
	    List.iter
	      (fun mec ->
		 List.iter
		   (fun scl -> make_rst_instances scl d b mec count)
		   scale) max_edge_cost) branches) depths



let make_stu_instance scale max_edge optimal branch decisive seed i =
  (*
     scale doesn't do anything but would vary
     max_edge maxiumum possible cost of a transition
     optimal cost of the optimal solution
     branch branching factor
     decisive float larger than 0 that tells how costly siblins are
     relative to optimal sibling
     seed random seed to use to generate the tree
     i instance number but isn't used at the moment=
*)
  let t = Stu_tree.make_tree scale max_edge branch optimal seed decisive in
  let path = Rdb.path_for instance_root
    (Rdb.filter_attrs ["seed"] ((("type", "instance")::
				   (Stu_tree.config_to_pairs t))
				@ ["num", string_of_int i])) in
    if Sys.file_exists path && Datafile.seems_complete path then
      Wrutils.pr "%s exists, skipping...\n%!" path
    else
      Wrio.with_outfile path (fun ch -> Stu_tree.write_pairs t ch)


let make_silvia_instance scale max_edge optimal branch decisive seed i =
  (*
     scale doesn't do anything but would vary
     max_edge maxiumum possible cost of a transition
     optimal cost of the optimal solution
     branch branching factor
     decisive float larger than 0 that tells how costly siblins are
     relative to optimal sibling
     seed random seed to use to generate the tree
     i instance number but isn't used at the moment=
*)
  let t = Silvia_tree.make_tree branch optimal seed in
  let path = Rdb.path_for instance_root
    (Rdb.filter_attrs ["seed"] ((("type", "instance")::
				   (Silvia_tree.config_to_pairs t))
				@ ["num", string_of_int i])) in
    if Sys.file_exists path && Datafile.seems_complete path then
      Wrutils.pr "%s exists, skipping...\n%!" path
    else
      Wrio.with_outfile path (fun ch -> Silvia_tree.write_pairs t ch)


let make_stu_instances scale max_edge optimal branch decisive n =
  Random.self_init();
  for i = 1 to n
  do
    make_stu_instance scale max_edge optimal branch decisive
      (truncate (Random.float Random_tree.my_max)) i
  done


let optimals = [(*5.; 10.;*) 15.; 25.; (*50.;*)]
and branching = [(*2; 4; 10;*) 15; 20; 25;]
and max_edge_cost = [1.;]
and scales = [Stu_tree.Uniform;
	      (*Stu_tree.Linear_Increase;*)
	      (*Stu_tree.Linear_Decrease;*)]
and decisive = [(*0.01; 0.05;*) 0.1; (*0.2; 0.25;0.5;*)]


let exclude_run branch optimal scale =
  false


let make_stu_batch count =
  List.iter
    (fun d ->
       (List.iter
	  (fun opt ->
	     List.iter
	       (fun b ->
		  List.iter
		    (fun mec ->
		       List.iter
			 (fun scl -> make_stu_instances scl mec opt b d count)
			 scales) max_edge_cost) branching) optimals))
    decisive



let call_solver time_limit node_limit args prob_path attrs heuristic outch =
  (** calls the external solver on [args] and [board_name], redirecting its
      output to [outch].  all logging is done within here.  returns cost, time
      pair *)
  let cache = if !caching then "--cache " else "" in
  let limit_args =
    match time_limit with
	Some(t) ->  Wrutils.str "%s-limit %f %s %f %s" cache t (fst heuristic) (snd heuristic) args
      | _ -> Wrutils.str "%s%s %f %s" cache (fst heuristic) (snd heuristic) args in
  let attrs = ("type", "run"):: (Rdb.filter_attrs ["type"] attrs) in
    (Verb.pr Verb.debug "%s %s < %s" !solver_binary limit_args prob_path);
    let go () =
      Wrsys.with_subprocess_pipes
	(Wrutils.str "%s %s < %s" !solver_binary limit_args prob_path)
	(fun to_solver from_solver ->
	   Datafile.write_header_pairs outch;
	   Datafile.write_pairs outch attrs;
	   Datafile.write_pairs outch ["attrs", (Wrstr.encode_pairs attrs)];
	   let res = Datafile.pipe_data from_solver outch in
	     Datafile.write_trailer_pairs outch;
	     res) in
      Notify.try_and_email_on_fail go "Synth_runs"


let do_run overwrite time_limit node_limit args prob_path attrs heuristic =
  (** sets up run file and calls solver on board_name *)
  let limits = [(match time_limit with None -> Limit.Never
		   | Some t -> Limit.Time t);
		(match node_limit with None -> Limit.Never
		   | Some n -> Limit.Generated n)] in
  let run_file = Rdb.path_for data_root (Rdb.filter_attrs ["type"] attrs) in
    Verb.pe Verb.debug "Writing to %s\n" run_file;
    if Sys.file_exists run_file &&
      Datafile.seems_complete run_file  &&
      (*Datafile.run_finished run_file  &&*)
      not (Limit.has_new_limits run_file limits) &&
      not overwrite
    then
      Wrutils.pr "%s exists - skipping!\n%!" run_file
    else
      (Wrio.with_outfile run_file
	 (fun outch ->
	    Wrutils.pr "Running %s on %s...%!"
	      args (Wrfname.make_relative instance_root prob_path);
	    let c,t =
	      call_solver time_limit node_limit args prob_path attrs heuristic
		outch in
	      Wrutils.pr "%.1f in %.3f.\n%!" c t))


let call_do_rst_run overwrite tl nl attrs args model scale branch depth mec
    heuristic =
  let prob_attrs = ["type", "instance";
		    "scale", Rst_tree.scale_to_string scale;
		    "branching factor", string_of_int branch;
		    "tree depth", string_of_int depth;
		    "maximum edge cost", string_of_float mec;] in
    List.iter (fun problem ->
		 let problem_path = Rdb.path_for instance_root problem in
	       do_run overwrite tl nl args problem_path
		 (("heuristic", (fst heuristic))::
		    ("corruption", string_of_float (snd heuristic))::
		    (attrs@problem)) heuristic)
      (Rdb.matching_attrs instance_root prob_attrs)


let call_do_stu_run overwrite tl nl attrs args model scale branch decisive
    optimal mec heuristic =
  let prob_attrs = ["model", "stu";
		    "type", "instance";
		    "scale", Stu_tree.scale_to_string scale;
		    "branching factor", string_of_int branch;
		    "optimal", string_of_float optimal;
		    "decisive", string_of_float decisive;
		    "maximum edge cost", string_of_float mec;] in
    List.iter (fun problem ->
		 let problem_path = Rdb.path_for instance_root problem in
	       do_run overwrite tl nl args problem_path
		 (("heuristic", (fst heuristic))::
		    ("corruption", string_of_float (snd heuristic))::
		    (attrs@problem)) heuristic)
      (Rdb.matching_attrs instance_root prob_attrs)


let call_do_silvia_run overwrite tl nl attrs args model scale branch decisive
    optimal mec heuristic =
  let prob_attrs = ["model", "silvia_v2";
		    "type", "instance";
		    "branching factor", string_of_int branch;
		    "optimal", string_of_float optimal;] in
    List.iter (fun problem ->
		 let problem_path = Rdb.path_for instance_root problem in
	       do_run overwrite tl nl args problem_path
		 (("heuristic", (fst heuristic))::
		    ("corruption", string_of_float (snd heuristic))::
		    (attrs@problem)) heuristic)
      (Rdb.matching_attrs instance_root prob_attrs)


let do_rst_runs overwrite time_limit node_limit args alg_attrs =
  (* model, scale, branch, depth, mec, mac *)
  List.iter
    (fun pair ->
       List.iter
	 (fun d ->
	    List.iter
	      (fun b ->
		 List.iter
		   (fun mec ->
		      List.iter
			(fun scl ->
			   List.iter
			     (fun m ->
				List.iter
				  (fun heuristic ->
				     call_do_rst_run overwrite time_limit
				       node_limit alg_attrs args m scl b d
				       mec heuristic) pair)
			     models) scale)
		   max_edge_cost)
	      branches)
	 depths)
    pairs


let do_stu_runs overwrite time_limit node_limit args alg_attrs =
  (* model, scale, branch, depth, mec, mac *)
  List.iter
    (fun opt ->
       List.iter
	 (fun pair ->
	    List.iter
	      (fun d ->
		 List.iter
		   (fun b ->
		      List.iter
			(fun mec ->
			   List.iter
			     (fun scl ->
				List.iter
				  (fun m ->
				     List.iter
				       (fun heuristic ->
					  if not (exclude_run b opt scl)
					  then
					    (* todo: fix quick and dirty*)
					  call_do_stu_run overwrite time_limit
					    node_limit alg_attrs args m scl b d
					    opt mec heuristic) pair)
				  models)
			     scales)
			max_edge_cost)
		   branching)
	      decisive)
	 pairs)
    optimals

let do_runs overwrite time_limit node_limit args alg_attrs =
 do_stu_runs overwrite time_limit node_limit args alg_attrs


let do_basic_batch ?(overwrite = false)  ?(time_limit = Some (Experiments.default_time_limit))
    ?(node_limit = Some (Experiments.default_node_limit))  alg =
  (** greedy, speedy, a_star *)
  Notify.start_batchtime();
  do_runs overwrite time_limit node_limit alg ["alg", alg];
  Notify.send_batch_completed_mail "Rucksack_runs" "Basic" alg



let do_wted_batch overwrite time_limit node_limit alg wt =
  (** wted_astar, anytime_a_star, dyn_wted_a_star, a_star_eps *)
  do_runs overwrite time_limit node_limit
    (Wrutils.str "%s %f" alg wt) ["alg", alg;
				"wt", string_of_float wt;]


let do_beam_batch overwrite time_limit node_limit alg beam_width =
  (** wted_astar, anytime_a_star, dyn_wted_a_star, a_star_eps *)
  do_runs overwrite time_limit node_limit
    (Wrutils.str "%s %d" alg
       beam_width) ["alg", alg;
				"beam_width", string_of_int beam_width;]

let do_wted_batches ?(overwrite = false) ?(time_limit = Some (Experiments.default_time_limit))
    ?(node_limit = Some (Experiments.default_node_limit))
    ?(weights = Experiments.low_res_weights) alg =
  Notify.start_batchtime();
  List.iter (fun w ->
	       do_wted_batch overwrite time_limit node_limit alg w) weights;
  Notify.send_batch_completed_mail "Rucksack_runs" "Weighted" alg



let do_beam_batches ?(overwrite = false) ?(time_limit = Some (Experiments.default_time_limit))
    ?(node_limit = Some (Experiments.default_node_limit))
    ?(beam_widths = Experiments.full_beams) alg =
  Notify.start_batchtime();
  List.iter (fun w ->
	       do_beam_batch overwrite time_limit node_limit alg w) beam_widths;
  Notify.send_batch_completed_mail "Rucksack_runs" "Beam" alg



let do_optimistic_batch overwrite time_limit node_limit alg wt opt =
  assert (opt >= 1.);
  do_runs overwrite time_limit node_limit
    (Wrutils.str "%s %f %f" alg wt opt) ["alg", alg;
(* order really matters here*)	       "wt", string_of_float wt;
(* wt (bound) must come first*)	       "optimism", string_of_float opt]


let do_optimistic_batches ?(overwrite = false) ?(time_limit = Some (Experiments.default_time_limit))
    ?(node_limit = Some (Experiments.default_node_limit))
    ?(weights = Experiments.low_res_weights)
    ?(opt = Experiments.optimisms) alg =
  Notify.start_batchtime();
  List.iter
    (fun opt ->
       List.iter (fun w -> do_optimistic_batch overwrite time_limit node_limit alg w opt)
	 weights) opt;
  Notify.send_batch_completed_mail "Rucksack_runs" "Optimistic" alg

(* EOF *)
