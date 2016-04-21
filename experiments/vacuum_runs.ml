(** Top level tools for vacuum worlds, mostly instance generation
    and running on instances *)


let data_root = Experiments.data_root ^ "vacuum"
and instance_root = Experiments.instance_root ^ "vacuum_instances"
and solver_binary = ref (Experiments.bin_root ^ "vacuum_solver")


let with_path6 (s, e, g, p, m, d) =
  (match s with
    None -> None
  | Some (n, f) -> Some (n, f)), e, g, p, m, d


let validator =
  Experiments.feasible_p_dups with_path6 Vacuum_interfaces.symmetric_interface


(**** Making instances *****)


let make_uniform_instances width height num prob dirts =
  Random.self_init ();
  for i = 1 to num do
    let attrs = [ "obstacles", "uniform" ] @
		  [ "width", string_of_int width;
		    "height", string_of_int height;
		    "obst_prob", string_of_float prob;
		    "dirt", string_of_int dirts;
		    "num", string_of_int i; ] in
    let path = Rdb.path_for instance_root attrs in
      if Sys.file_exists path then
	Wrutils.pr "%s exists, skipping...\n%!" path
      else
	let b = Vacuum_instance.uniform_random_board
	  ~dirts:dirts width height prob in
	  Vacuum_instance.save b path;
	  Wrutils.pr "Wrote board %d.\n%!" i
  done



(********************* doing experiments **********************)
(* you may want to set the solver binary when doing runs
   Runs.solver_binary := desired path *)

let call_solver limits args prob_path attrs outch =
  (** calls the external solver on [args] and [board_name], redirecting its
    output to [outch].  all logging is done within here.  returns cost, time
    pair *)


  let limit_args = Limit.to_string limits in
    (* Printf.fprintf stderr "%s" (Wrutils.str "%s %s < %s" !solver_binary
       limit_args prob_path);
    *)
    Verb.pr Verb.debug "calling: %s %s %s < %s"
      !solver_binary args limit_args prob_path;
  let go () =
    let call_str = (Wrutils.str "%s --memory max %s %s < %s"
		      !solver_binary limit_args args prob_path) in
      Verb.pe Verb.debug "\n%s\n%!" call_str;
      Wrsys.with_subprocess_pipes
	call_str
      (fun to_solver from_solver ->
	 Datafile.write_header_pairs outch;
	 Datafile.write_pairs outch attrs;
	 Datafile.write_pairs outch ["attrs", (Wrstr.encode_pairs attrs)];
	 let res = Datafile.pipe_data from_solver outch in
	   Datafile.write_trailer_pairs outch;
	   res)in
    Notify.try_and_email_on_fail go "Vacuum_runs"


let finished df =
  try
    (let df = Datafile.load df in
       (Datafile.get_val df "found solution") <> "no")
  with _ -> true


let do_run overwrite limits args prob_path attrs =
  (** sets up run file and calls solver on board_name *)
  let run_file = Rdb.path_for data_root
    (Rdb.filter_attrs ["type"] attrs) in
    if Sys.file_exists run_file &&
      Datafile.seems_complete run_file  &&
      (*valid_df (Datafile.load run_file)  &&*)
      not ((Limit.has_new_limits run_file limits) && not (finished run_file)) &&
      not overwrite
    then
      Wrutils.pr "%s exists - skipping!\n%!" run_file
    else
      (Wrio.with_outfile run_file
	 (fun outch ->
	    Wrutils.pr "Running %s on %s...%!" args
	      (Wrfname.make_relative instance_root prob_path);
	    let c,t =
	      call_solver limits args prob_path attrs outch in
	      Wrutils.pr "%.1f in %.3f.\n%!" c t))


let do_uni_runs overwrite time_limit node_limit w h p d alg_attrs args =
  (** runs a basic alg on all boards with [prob_attrs] *)
  let l = match time_limit with  None -> [] | Some t -> [Limit.Time t] in
  let limit = match node_limit with
    | None -> l
    | Some n -> (Limit.Generated n) :: l
  in
  let prob_attrs = [ "obstacles", "uniform";
		     "obst_prob", string_of_float p;
		     "width", string_of_int w;
 		     "height", string_of_int h;
		     "dirt", string_of_int d
		   ] in
  let probs = (Rdb.matching_attrs instance_root prob_attrs) in
    Verb.pe Verb.always "Doing uniform %i runs...\n" (List.length probs);
    List.iter (fun board_attrs ->
		 let prob_path = Rdb.path_for instance_root board_attrs
		 and attrs = alg_attrs @ board_attrs in
		   do_run overwrite limit args prob_path attrs)
      probs


let do_maze_runs overwrite ?(inst_attrs=[]) limits w h p d alg_attrs args =
  (** runs a basic alg on all boards with [prob_attrs] *)
  let default_prob_attrs = [ "obstacles", "maze";
			     "cycle_prob", string_of_float p;
			     "width", string_of_int w;
 			     "height", string_of_int h;
			     "dirt", string_of_int d
			   ] in
  let prob_attrs = Rdb.merge_attrs default_prob_attrs inst_attrs in
  let probs = Rdb.matching_attrs instance_root prob_attrs in
    Verb.pe Verb.always "Doing maze %i runs...\n" (List.length probs);
    List.iter (fun board_attrs ->
		 let prob_path = Rdb.path_for instance_root board_attrs
		 and attrs = alg_attrs @ board_attrs in
		   do_run overwrite limits args prob_path attrs)
      probs


let do_runs overwrite time_limit node_limit w h p d alg_attrs args =
  do_uni_runs overwrite time_limit node_limit w h p d alg_attrs args


let bc =
  (*[(200, 200, 0.2, 5);
   (200, 200, 0.1, 5);
   (200, 200, 0.0, 5);
   (200, 200, 0.2 , 10);
   (200, 200, 0.1 , 10);
   (200, 200, 0.05, 10);
   (200, 200, 0.0 , 10);]*)
  [
(*    (500,500,0.35,10);*)
(500,500,0.35,20);
 (200,200,0.35,5);
]

let heavy_bc = [ (200,200,0.35,10)]

let batch_configs = ref heavy_bc


let batch_iter ?(b = !batch_configs) f =
  List.iter (fun (w, h, p, d) -> f w h p d) b


let do_batch ?(memo = false)
    heuristics cost overwrite time_limit node_limit args attrs =
  Verb.pe Verb.always "Doing batch...\n";
  List.iter (fun heur ->
	       batch_iter
		 (fun w h p d ->
		    do_runs overwrite
		      time_limit node_limit w h p d
		      (attrs @ ["heuristic",heur;
				"cost", cost;
				"memo", string_of_bool memo;
			       ])
		      (if memo
		       then Wrutils.str "--heuristic \"%s\" --memo --cost %s %s"
			 heur cost args
		       else Wrutils.str "--heuristic \"%s\" --cost %s %s"
			 heur cost args)
		 ))
    heuristics


let do_basic_batch ?(overwrite = false)
    ?(attrs = [])
    ?(time_limit = Some (Experiments.default_time_limit))
    ?(node_limit = Some (Experiments.default_node_limit))
    ?(heuristics = ["chris"]) ?(cost = "unit") alg =
  (** greedy, speedy, a_star *)
  Notify.start_batchtime();
  do_batch heuristics cost overwrite time_limit node_limit alg
    (["alg", alg] @ attrs);
  Notify.send_batch_completed_mail "Vacuum_runs" "Basic" alg



let do_wted_batch heuristics cost overwrite time_limit node_limit alg wt =
  (** wted_astar, anytime_a_star, dyn_wted_a_star, a_star_eps *)
  do_batch heuristics cost overwrite time_limit node_limit
    (Wrutils.str "%s %f" alg wt) ["alg", alg;
				  "wt", string_of_float wt;]


let do_wted_batches ?(overwrite = false)
    ?(time_limit = Some (Experiments.default_time_limit))
    ?(node_limit = Some (Experiments.default_node_limit))
    ?(heuristics = ["symmetric"]) ?(cost = "unit")
    ?(weights = Experiments.low_res_weights) alg =
  Notify.start_batchtime();
  List.iter (fun w ->
	       do_wted_batch heuristics cost overwrite
		 time_limit node_limit alg w)
    (List.filter (fun w -> w >= 1.1) (weights));
  Notify.send_batch_completed_mail "Vacuum_runs" "Weighted" alg


let do_beam_batch heuristics cost overwrite
    time_limit node_limit alg beam_width =
  (** wted_astar, anytime_a_star, dyn_wted_a_star, a_star_eps *)
  do_batch heuristics cost overwrite time_limit node_limit
    (Wrutils.str "%s %d" alg
       beam_width) ["alg", alg;
		    "beam_width", string_of_int beam_width;]


let do_beam_batches ?(overwrite = false)
    ?(time_limit = Some (Experiments.default_time_limit))
    ?(node_limit = Some (Experiments.default_node_limit))
    ?(beam_widths = Experiments.full_beams)
    ?(heuristics = ["chris"])
    ?(cost = "unit") alg =
  Notify.start_batchtime();
  List.iter
    (fun w -> do_beam_batch heuristics cost overwrite
       time_limit node_limit alg w)
    beam_widths;
  Notify.send_batch_completed_mail "Vacuum_runs" "Beam" alg


let do_new_beam_batch heuristics cost overwrite time_limit node_limit alg
    beam_width sort_predicate=
  (** wted_astar, anytime_a_star, dyn_wted_a_star, a_star_eps *)
  do_batch heuristics cost overwrite time_limit node_limit
    (Wrutils.str "%s %d %s" alg beam_width sort_predicate) ["alg", alg;
		    "beam_width", string_of_int beam_width;
		    "sort_predicate",sort_predicate;]


let do_new_beam_batches
    ?(overwrite = false)
    ?(time_limit = Some (Experiments.default_time_limit))
    ?(node_limit = Some (Experiments.default_node_limit))
    ?(sort_predicate = "f")
    ?(beam_widths = Experiments.full_beams)
    ?(heuristics = ["chris"]) ?(cost = "unit") alg =
  Notify.start_batchtime();
  List.iter (fun w ->
	       do_new_beam_batch heuristics cost
		 overwrite time_limit node_limit
		 alg w sort_predicate) beam_widths;
  Notify.send_batch_completed_mail "Vacuum_runs" "Beam" alg



let do_cab_batch heuristics overwrite time_limit node_limit alg
    (sort_predicate:string) =
  (** wted_astar, anytime_a_star, dyn_wted_a_star, a_star_eps *)
  do_batch heuristics "unit" overwrite time_limit node_limit
    (Wrutils.str "%s %s" alg
       sort_predicate) ["alg", alg;
			"sort_predicate", sort_predicate;]

let do_cab_batches ?(overwrite = false)
    ?(time_limit = Some (Experiments.default_time_limit))
    ?(node_limit = Some (Experiments.default_node_limit))
    ?(heuristics = ["chris"]) alg
    (sort_predicate:string) =
  Notify.start_batchtime();
  List.iter (fun w ->
	       do_cab_batch heuristics overwrite time_limit node_limit alg w)
    [sort_predicate];
  Notify.send_batch_completed_mail "Vacuum_runs" "cab" alg



let do_optimistic_batch heuristics cost overwrite time_limit node_limit
    alg wt opt =
  assert (opt >= 1.);
  do_batch heuristics cost overwrite time_limit node_limit
    (Wrutils.str "%s %f %f" alg wt opt) ["alg", alg;
					 "wt", string_of_float wt;
					 "optimism", string_of_float opt]

let do_restarting_beam_batches ?(heuristic = "manhattan") ?(overwrite = false)
    ?(time_limit = Some (Experiments.default_time_limit))
    ?(node_limit = Some (Experiments.default_node_limit))
    ?(sort_predicate = "f")
    ?(beam_widths = Experiments.full_beams)
    ?(heuristics = ["chris"])
    ?(cost = "unit")  alg =
  let restart_pred = ["f";"h";"i_max";"i_min";"i_all";] in
    Notify.start_batchtime();
    List.iter
      (fun bw ->
	 List.iter
	   (fun rp ->
	      do_batch heuristics cost overwrite time_limit node_limit
		(Wrutils.str "%s %d %s %s" alg bw sort_predicate rp)
		["alg", alg;
		 "beam_width",string_of_int bw;
		 "sort_predicate",sort_predicate;
		 "backup_sort_predicate",rp;]) restart_pred) beam_widths;
    Notify.send_batch_completed_mail "Vacuum_runs" "restarting_beam" alg


let do_msc_k_wted_batches ?(overwrite = false)
    ?(time_limit = Some (Experiments.default_time_limit))
    ?(node_limit = Some (Experiments.default_node_limit))
    ?(c_size = Experiments.full_beams) ?(ks = Experiments.ks)
    ?(weights = Experiments.low_res_weights)
    ?(heuristics = ["chris"]) alg =
  Notify.start_batchtime();
  List.iter
    (fun c ->
       List.iter
	 (fun k ->
	    List.iter
	      (fun wt ->
		 do_batch heuristics "unit" overwrite time_limit node_limit
		   (Wrutils.str "%s %f %i %i" alg wt k c)
		   ["alg", alg;
		    "commit", string_of_int c;
		    "k", string_of_int k;
		    "wt", string_of_float wt;]) weights)
	 ks) c_size;
  Notify.send_batch_completed_mail "Vacuum_runs" "MSC_K_WTED" alg


let do_optimistic_batches ?(overwrite = false)
    ?(time_limit = Some (Experiments.default_time_limit))
    ?(node_limit = Some (Experiments.default_node_limit))
    ?(weights = Experiments.low_res_weights)
    ?(opt = Experiments.optimisms)
    ?(heuristics = ["chris"]) ?(cost = "unit") alg =
  Notify.start_batchtime();
  List.iter
    (fun opt ->
       List.iter (fun w -> do_optimistic_batch heuristics cost overwrite
		    time_limit node_limit alg w opt) weights) opt;
  Notify.send_batch_completed_mail "Vacuum_runs" "Optimistic" alg


let do_im_batch ?(overwrite = false) ?(inst_attrs=[])
    ?(alg="idastar_im") limits w h prob dirt bins control =
  (** [do_online_model_batch ?overwrite ?inst_attrs limits w h prob
      dirt bins control] runs the idastar_online_model algorithm on
      some vacuum maze problems. *)
  Notify.start_batchtime();
  do_maze_runs overwrite ~inst_attrs limits w h prob dirt
    ["alg", alg;
     "max-bins", string_of_int bins;
     "control-factor", string_of_float control;
    ]
    (Wrutils.str "idastar_im %d %f" bins control);
  Notify.send_batch_completed_mail "Vacuum_runs" "Basic" "idastar_im"



let do_cr_batch ?(overwrite = false) ?(inst_attrs=[])
    limits w h prob dirt control =
  (** [do_cr_batch ?overwrite ?inst_attrs limits w h prob dirt
      control] runs the idastar_cr algorithm on some vacuum maze
      problems. *)
  Notify.start_batchtime();
  do_maze_runs overwrite ~inst_attrs limits w h prob dirt
    ["alg", "idastar_cr";
     "max-bins", "100";
     "control-factor", string_of_float control;
    ]
    (Wrutils.str "idastar_cr 100 %f" control);
  Notify.send_batch_completed_mail "Vacuum_runs" "Basic" "idastar_cr"


(******************************)
let do_fixed ?(heuristic = ["spanning tree";])
    ?(overwrite = false) ?(time_limit = Some (Experiments.default_time_limit))
    ?(node_limit = Some (Experiments.default_node_limit)) () =
  (** greedy, speedy, a_star *)
  Notify.start_batchtime();
  do_batch heuristic "unit" overwrite time_limit node_limit
    (*"-- greedy_lms_fixed 0.759613 0.527754 -0.107183 0.085170"*)
    (*"greedy_path_adapt"*)
    (*"greedy_lms_adapt"*)
    (*"greedy_adapt"*)
    (*"greedier_adapt"*)
    (*"greedy_ann_adapt"*)
    (*"greedy_ann_fixed vacuum"*)
    "greedier_ann_fixed vacuum"
    (*"greedy"*)
    (*"astar"*)
    ["alg", "greedier_ann_fixed"];
  Notify.send_batch_completed_mail "Vac_runs" "Basic" "greedy_fixed"



let do_wted_fixed
    ?(weights = Experiments.low_res_weights)
    ?(overwrite = false) ?(time_limit = Some (Experiments.default_time_limit))
    ?(node_limit = Some (Experiments.default_node_limit)) () =
  (** greedy, speedy, a_star *)
  Notify.start_batchtime();
  List.iter
    (fun wt ->
       do_batch ["spanning tree"] "unit" overwrite time_limit
	 node_limit
	 (*(Wrutils.str "wted_astar %f" wt)*)
	 (Wrutils.str "clamped_fixed %f 0.759613 0.527754 -0.107183 0.085170"
	   wt)
	 (*(Wrutils.str "clamped_hd_reckless %f" wt)*)
	 (*(Wrutils.str "clamped_lms_reckless %f" wt)*)
	 (*(Wrutils.str "clamped_hdp_path %f" wt)*)
	 ["alg", "clamped_fixed";
	  "wt", string_of_float wt])
    weights;
  Notify.send_batch_completed_mail "Grid_runs" "Basic" "greedy_fixed"


let model_on_instance alg =
  let base_instance_attrs = ["obstacles", "uniform";
			     "width", "200";
			     "height", "200";
			     "obst_prob", "0.35";
			     "dirt", "10";
			     "cost", "heavy";
			     "heuristic", "improved d";
			     "alg", "greedier_adapt";] in
    for i = 1 to 99 do
      (let iat = ("num", string_of_int i)::base_instance_attrs in
       let old_run = (Dataset.load_from_rdb_with_domain ~domain:"vacuum" iat
			~name:"oldrun") in
       let herr = (Dataset.get_values float_of_string
		     ~sort:Dataset.Last "h_step" old_run).(0)
       and derr = (Dataset.get_values float_of_string
		     ~sort:Dataset.Last "d_step" old_run).(0) in
	 do_run false [Limit.Time 300.]
	   (Wrutils.str "--heuristic \"improved d\" --cost heavy -- %s %f %f"
	      alg herr derr)
	   (Wrutils.str
	      "./group/data/vacuum_instances/uniform/200/200/0.35/10/%i" i)
	   ["obstacles", "uniform";
	    "width", "200";
	    "height", "200";
	    "obst_prob", "0.35";
	    "dirt", "10";
	    "heuristic", "improved d";
	    "cost", "heavy";
	    "num", string_of_int i;
	    "alg", (Wrutils.str "%s_%s" alg "this_instance")])
    done

let model_on_next_instance alg =
  let base_instance_attrs = ["obstacles", "uniform";
			     "width", "200";
			     "height", "200";
			     "obst_prob", "0.35";
			     "dirt", "10";
			     "cost", "heavy";
			     "heuristic", "improved d";
			     "alg", "greedier_adapt";] in
    for i = 1 to 99 do
      (let iat = ("num", string_of_int ((i mod 99)+1))::base_instance_attrs in
       let old_run = (Dataset.load_from_rdb_with_domain ~domain:"vacuum" iat
			~name:"oldrun") in
       let herr = (Dataset.get_values float_of_string
		     ~sort:Dataset.Last "h_step" old_run).(0)
       and derr = (Dataset.get_values float_of_string
		     ~sort:Dataset.Last "d_step" old_run).(0) in
	 do_run false [Limit.Time 300.]
	   (Wrutils.str "--heuristic \"improved d\" --cost heavy -- %s %f %f"
	      alg herr derr)
	   (Wrutils.str
	      "./group/data/vacuum_instances/uniform/200/200/0.35/10/%i" i)
	   ["obstacles", "uniform";
	    "width", "200";
	    "height", "200";
	    "obst_prob", "0.35";
	    "dirt", "10";
	    "cost", "heavy";
	    "heuristic", "improved d";
	    "num", string_of_int i;
	    "alg", (Wrutils.str "%s_%s" alg "next_instance")])
    done

(* EOF *)
