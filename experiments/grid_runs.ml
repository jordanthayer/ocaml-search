(* $Id: runs.ml,v 1.1 2004/12/22 20:18:00 ruml Exp ruml $
  running experiments *)

open Printf

let data_root = Experiments.data_root ^ "grid"
and instance_root = Experiments.instance_root ^ "grid_instances"
and solver_binary = ref (Experiments.bin_root ^ "grid_path_solver")


(* A collection of board configuration parameters *)
let costs    = [Grid.Unit; Grid.Life;]
and moves    = [Grid.Fourway; Grid.Eightway;]
and variants = ["lines"; "maze"; "uniform_region"; "game"; "many_goal";
		"uniform"; (*"benchmark";*)]
and probs    = [0.15; 0.2; 0.25; 0.3; 0.35; 0.4; 0.45; 0.5; 0.6]
and lines    = [10; 25; 50; 75;]
and games    = ["battleground"; "blastedlands"; "darkforest"; "moonglade";]
and buckets  = ["100"; "110"; "120"; "130"; "140"; "150"; "160"; "170"; "180";
		"190";]

let no_arg_h = ["manhattan";
		"canonical";
		"canonical_max";
		"online_truth";
		"countblocked";
		"scaled";]
and arg_h = ["corrupted_truth";"online_corrupted";]

let heuristics = no_arg_h @ arg_h

let no_arg_hstring hname =
  Wrutils.str "--heuristic %s" hname

let validator = Experiments.feasible_p_dups
  Grid_algs.with_path6 Grid_interfaces.default_interface


let batch_attrs ?(tipe="instance") costs ways p =
  [ "type", tipe;
    "costs", (Grid_instance.tag_of_costs costs);
    "moves", (Grid_instance.tag_of_moves ways);
    "prob", string_of_float p ]


let ha_attrs  =
  [ "obstacles", "ha"; ]



let line_attrs costs ways l =
  [ "obstacles", "lines";
    "type", "instance";
    "costs", (Grid_instance.tag_of_costs costs);
    "moves", (Grid_instance.tag_of_moves ways);
    "prob", string_of_int l ]


let game_attrs board bucket =
  [ "obstacles", "game";
    "type", "instance";
    "board", board;
    "bucket", bucket;]



let mg_attrs costs ways start_p end_p goals =
  [ "obstacles", "many_goal";
    "type", "instance";
    "costs", (Grid_instance.tag_of_costs costs);
    "moves", (Grid_instance.tag_of_moves ways);
    "start_prob", string_of_float start_p;
    "end_prob", string_of_float end_p;
    "goals", string_of_int goals;]


let maze_attrs width height cycle_p =
  [ "obstacles", "maze";
    "type", "instance";
    "width", string_of_int width;
    "height", string_of_int height;
    "cycle_prob", string_of_float cycle_p;]


let scaled_maze_attrs width height cycle_p w =
  [ "obstacles", "scaled_maze";
    "type", "instance";
    "width", string_of_int width;
    "height", string_of_int height;
    "cycle_prob", string_of_float cycle_p;
    "hallway_size", string_of_int w;]


let scaled_attrs width height cycle_p =
  [ "obstacles", "scaled_maze";
    "type", "instance";
    "width", string_of_int width;
    "height", string_of_int height;
    "cycle_prob", string_of_float cycle_p;]



(* 0.4's are taking too long to generate.  We'll do them up after all the others are made. *)
let batch_tuples = [Grid.Life, [ Grid.Fourway,  [0.25; 0.3; 0.35;];
				 Grid.Eightway, [0.35; 0.4; 0.45]];
		    Grid.Unit, [ Grid.Fourway,  [0.25; 0.3; 0.35; 0.4;0.5;];
				 Grid.Eightway, [0.35; 0.4; 0.45; 0.5; 0.55; 0.65; 0.75]]]

let limited_tuples = [Grid.Life, [ Grid.Fourway,  [0.35;];];
		      Grid.Unit, [ Grid.Fourway,  [0.35;];]]

let unit_tuples = [Grid.Unit, [ Grid.Fourway,  [0.35;];]]

let obst_tuples = [Grid.Unit, [ Grid.Fourway, [0.; 0.1; 0.2; 0.3; 0.4; 0.5; 0.6; 0.7; 0.8; 0.9;]];
		   Grid.Life, [ Grid.Fourway, [0.; 0.1; 0.2; 0.3; 0.4; 0.5; 0.6; 0.7; 0.8; 0.9;]]]

let size_scale = [Grid.Unit, [Grid.Fourway, [0.35;]];]


(* set batches := limited_tuples for a nice quick test run *)
let batches = ref [Grid.Life, [Grid.Fourway, [0.35]];
		   Grid.Unit, [Grid.Fourway, [0.35]];]
  (*ref limited_tuples*)

let batch_fold f init =
  List.fold_left (fun a (costs, way_pairs) ->
		    List.fold_left (fun a (ways, ps) ->
				      List.fold_left (fun a p ->
							f a costs ways p)
				      a ps)
		    a way_pairs)
    init !batches

let batch_iter f =
  batch_fold (fun () c w p -> f c w p; ()) ()

let batch_map f =
  List.rev (batch_fold (fun a c w p -> (f c w p)::a) [])


let size_fold f init =
  List.fold_left (fun a (costs, way_pairs) ->
		    List.fold_left (fun a (ways, ps) ->
				      List.fold_left (fun a p ->
							f a costs ways p)
				      a ps)
		    a way_pairs)
    init size_scale


let size_iter f =
  size_fold (fun () c w p -> f c w p; ()) ()


(********************* doing experiments **********************)
(* you may want to set the solver binary when doing runs
   Runs.solver_binary := desired path *)

let call_solver heuristic ~time_limit ~node_limit ?exp_limit
    args prob_path attrs outch =
  (** calls the external solver on [args] and [board_name], redirecting its
    output to [outch].  all logging is done within here.  returns cost, time
    pair *)
  let time_limit = match time_limit with None -> Limit.Never
    | Some t -> Limit.Time t
  and node_limit = match node_limit with None -> Limit.Never
    | Some n -> Limit.Generated n
  and exp_limit = match exp_limit with None -> Limit.Never
    | Some n -> Limit.Expanded n in
  let limit_string = Limit.to_string [time_limit; node_limit; exp_limit] in
  let limit_args =  Wrutils.str "%s %s" limit_string args in
  let go () =
    Wrsys.with_subprocess_pipes
      (Wrutils.str "%s %s %s < %s"
	 !solver_binary (no_arg_hstring heuristic) limit_args prob_path)
      (fun to_solver from_solver ->
	 Datafile.write_header_pairs outch;
	 Datafile.write_pairs outch attrs;
	 Datafile.write_pairs outch ["attrs", (Wrstr.encode_pairs attrs)];
	 let res = Datafile.pipe_data from_solver outch in
	   Datafile.write_trailer_pairs outch;
	   res) in
    Notify.try_and_email_on_fail go "Grid_runs"

let valid_df df =
  try
    (Datafile.get_val df "found solution") <> "no"
  with _ -> true


let do_run heuristic overwrite ~time_limit ~node_limit ?exp_limit
    args prob_path attrs =
  (** sets up run file and calls solver on board_name *)
  let attrs = ("heuristic", heuristic) :: attrs in
  let limits = [(match time_limit with None -> Limit.Never
		   | Some t -> Limit.Time t);
		(match node_limit with None -> Limit.Never
		   | Some n -> Limit.Generated n);
		(match exp_limit with None -> Limit.Never
		   | Some n -> Limit.Expanded n) ] in
  let run_file = Rdb.path_for data_root
    (Rdb.filter_attrs ["type"] attrs) in
    if Sys.file_exists run_file &&
      Datafile.seems_complete run_file  &&
      (*valid_df (Datafile.load run_file)  &&*)
      not overwrite &&
      not (Limit.has_new_limits run_file limits)
    then
      Wrutils.pr "%s exists - skipping!\n%!" run_file
    else
      (
      Wrio.with_outfile run_file
	(fun outch ->
	   Wrutils.pr "Running %s on %s...%!" args
	     (Wrfname.make_relative instance_root
		prob_path);
	   let c,t =
	     call_solver heuristic ~time_limit:time_limit
	       ~node_limit:node_limit ?exp_limit args
	       prob_path attrs outch in
	     Wrutils.pr "%.1f in %.3f.\n%!" c t))


let do_maze_runs heuristic overwrite time_limit node_limit w h p alg_attrs args =
  (** runs a basic alg on all boards with [prob_attrs] *)
  let prob_attrs = scaled_attrs w h p in
    List.iter (fun board_attrs ->
		 let prob_path = Rdb.path_for instance_root board_attrs
		 and attrs = alg_attrs @ board_attrs in
		   do_run heuristic overwrite time_limit node_limit args prob_path attrs)
      (Rdb.matching_attrs instance_root prob_attrs)


let do_ha_runs heuristic overwrite time_limit node_limit alg_attrs args =
  (** runs a basic alg on all boards with [prob_attrs] *)
  let prob_attrs = ha_attrs in
    List.iter (fun board_attrs ->
		 let prob_path = Rdb.path_for instance_root board_attrs
		 and attrs = alg_attrs @ board_attrs in
		   do_run
		     heuristic
		     overwrite
		     time_limit
		     node_limit
		     args
		     prob_path
		     attrs)
      (Rdb.matching_attrs instance_root prob_attrs)




let do_region_runs heuristic overwrite time_limit node_limit w h alg_attrs args
    ways costs p =
  (** runs a basic alg on all boards with [prob_attrs] *)
  let costs = costs in
  let prob_attrs = ([ "obstacles", "uniform_region"] @
		      (batch_attrs costs ways p) @
		      ["width", string_of_int w;
 		       "height", string_of_int h;]) in
    List.iter (fun board_attrs ->
		 let prob_path = Rdb.path_for instance_root board_attrs
		 and attrs = alg_attrs @ board_attrs in
		   do_run heuristic overwrite time_limit node_limit args prob_path attrs)
      (Rdb.matching_attrs instance_root prob_attrs)


let get_oc board_attrs heuristic =
  let opt_attrs = (("alg", "astar")::("heuristic",heuristic)::
		     board_attrs) in
  let opt_attrs = Rdb.filter_attrs ["type"] opt_attrs in
  let opt_path = Rdb.path_for data_root opt_attrs in
  let opt_df = Datafile.load opt_path in
    float_of_string (Datafile.get_val opt_df "final sol cost")


let do_uni_runs ?(use_oc = false) ?tipe heuristic overwrite ~time_limit
    ~node_limit ?exp_limit w h alg_attrs args ways costs p =
  (** runs a basic alg on all boards with [prob_attrs] *)
  let costs = costs in
  let prob_attrs = [ "obstacles", "uniform"] @
    (batch_attrs ?tipe costs ways p) @ (* comment next out for size runs *)
    ["width", string_of_int w;
     "height", string_of_int h;] in
    List.iter (fun board_attrs ->
		 let opt_cost = if use_oc then get_oc board_attrs heuristic else nan in
		 let args = (if use_oc then Wrutils.str "%s %f" args opt_cost
			     else args) in
		let prob_path = Rdb.path_for instance_root board_attrs
		and attrs = alg_attrs @ board_attrs in
		  do_run heuristic overwrite ~time_limit:time_limit
		    ~node_limit:node_limit ?exp_limit args prob_path attrs)
      (Rdb.matching_attrs instance_root prob_attrs)


let do_line_runs heuristic overwrite time_limit node_limit w h alg_attrs args ways costs =
  (** runs a basic alg on all boards with [prob_attrs]
      why isn't it folding over the line numbers? *)
  let prob_attrs =
    [ "obstacles", "lines";
      "type", "instance";
      "costs", (Grid_instance.tag_of_costs costs);
      "moves", (Grid_instance.tag_of_moves ways);
      "width", string_of_int w;
      "height", string_of_int h;] in

    List.iter (fun board_attrs ->
		 let prob_path = Rdb.path_for instance_root board_attrs
		 and attrs = alg_attrs @ board_attrs in
		   do_run heuristic overwrite time_limit node_limit args
		     prob_path attrs)
      (Rdb.matching_attrs instance_root prob_attrs)


let do_mg_runs heuristic overwrite time_limit node_limit w h alg_attrs args ways costs sp ep g =
  (** runs a basic alg on all boards with [prob_attrs] *)
  let prob_attrs = mg_attrs costs ways sp ep g in
    List.iter (fun board_attrs ->
		 let prob_path = Rdb.path_for instance_root board_attrs
		 and attrs = alg_attrs @ board_attrs in
		   do_run heuristic overwrite time_limit node_limit args prob_path attrs)
      (Rdb.matching_attrs instance_root prob_attrs)


let do_game_runs heuristic overwrite ~time_limit ~node_limit alg_attrs args =
  (** runs a basic alg on all boards with [prob_attrs] *)
  let prob_attrs = ["obstacles", "game";] in
    List.iter (fun board_attrs ->
		 let prob_path = Rdb.path_for instance_root board_attrs
		 and attrs = alg_attrs @ board_attrs in
		   do_run heuristic overwrite ~node_limit:node_limit ~time_limit:time_limit args prob_path attrs)
      (Rdb.matching_attrs instance_root prob_attrs)


let do_runs ?(use_oc = false) heuristic overwrite ~time_limit
    ~node_limit ?exp_limit
    ?(w = 2000) ?(h = 1200) alg_attrs args ways
    costs p =
  (* overwrite is whether or not to overwrite
     time limit is when to justquit
     node limit is hopw many nodes to allow alg to expand
     h and w are height and width
     alg_attrs are the alg's parameters
     args is the algorithm's parameters in string form
     ways is directions can move on the graph
     costs is cost function
     p is how much blocked*)
  do_uni_runs ~use_oc heuristic overwrite
    ~time_limit:time_limit
    ~node_limit:node_limit
    ?exp_limit w h alg_attrs args ways costs p


let do_batch ?(w = 2000) ?(h = 1200) ?(use_oc = false) heuristic overwrite
    ~time_limit ~node_limit args attrs =
  (* This is why things are getting called more than once.
     You'll need to rewrite this. *)
  batch_iter (fun costs ways p ->
		do_runs ~use_oc ~w ~h heuristic overwrite
		  ~time_limit:time_limit
		  ~node_limit:node_limit attrs
		  args ways costs p)
  (*do_uni_runs heuristic overwrite time_limit node_limit 5000 5000 attrs args Grid.Eightway Grid.Unit 0.45*)
  (*do_game_runs heuristic overwrite ~time_limit:time_limit ~node_limit:node_limit attrs args*)
(*do_uni_runs heuristic overwrite time_limit node_limit 5000 5000 attrs args Grid.Fourway Grid.Life 0.35;
    do_maze_runs heuristic overwrite time_limit node_limit 500 500 0. attrs args*)


let deadline_contract_batch ?(overwrite = false) alg_name =
  Notify.start_batchtime();
  let deadlines =
    [60.; 30.; 15.; 7.5; 3.75; 1.875; 0.9375; 0.46875; 0.234375; 0.1171875;
     0.05859375; 0.029296875; 0.0146484375; 0.00732421875;] in
  let w_res = List.map (fun a -> a, 20000) deadlines in
    List.iter
      (fun (deadline,res) ->
	 do_batch "manhattan" overwrite ~time_limit:(Some deadline)
	   ~node_limit:None
	   (Printf.sprintf "%s %f %i" alg_name deadline res)
	   ["alg", alg_name;
	    "deadline", string_of_float deadline;
	    "res", string_of_int res;])
      (List.rev w_res)


let do_basic_batch
    ?(w = 2000)
    ?(h = 1200)
    ?(heuristic = ["manhattan";])
    ?(overwrite = false) ?(time_limit = Some (Experiments.default_time_limit))
    ?(node_limit = Some (Experiments.default_node_limit)) ?(append = "") alg =
  (** greedy, speedy, a_star *)
  Notify.start_batchtime();
  List.iter
    (fun heur ->
       do_batch ~w ~h heur overwrite ~time_limit:time_limit
	 ~node_limit:node_limit (Printf.sprintf "%s%s" alg append) ["alg", alg])
    heuristic;
  Notify.send_batch_completed_mail "Grid_runs" "Basic" alg


let do_basic_batch_oc
    ?(w = 2000)
    ?(h = 1200)
    ?(heuristic = ["manhattan";])
    ?(overwrite = false) ?(time_limit = Some (Experiments.default_time_limit))
    ?(node_limit = Some (Experiments.default_node_limit)) ?(append = "") alg =
  (** greedy, speedy, a_star *)
  Notify.start_batchtime();
  List.iter
    (fun heur ->
       do_batch ~use_oc:true ~w ~h heur overwrite ~time_limit:time_limit
	 ~node_limit:node_limit (Printf.sprintf "%s%s" alg append) ["alg", alg])
    heuristic;
  Notify.send_batch_completed_mail "Grid_runs" "Basic" alg


let do_corruption_batch ?(w = 2000) ?(h = 1200)
    ?(overwrite = false) ?(time_limit = Some (Experiments.default_time_limit))
    ?(node_limit = Some (Experiments.default_node_limit)) ?(append = "") alg =
  let corruptions = ["0.1"; "0.2"; "0.3"; (*"0.4"; "0.5"; "0.5"; "0.6"; "0.7";
		     "0.75"; "0.8"; "0.85"; "0.9"; "0.95";*)] in
    Notify.start_batchtime();
    List.iter (fun cr -> do_batch ~w ~h "online_corrupted" overwrite
		 ~time_limit ~node_limit (Printf.sprintf "%s --corruption %s"
					    alg cr) ["alg", alg;
						     "corrupt", cr])
      corruptions



let do_bandwidth_batch ?(w = 2000) ?(h = 1200)
    ?(overwrite = false) ?(time_limit = Some (Experiments.default_time_limit))
    ?(node_limit = Some (Experiments.default_node_limit)) ?(append = "")
    ?(alg = "astar") () =
  let corruptions = List.rev (List.map string_of_float Experiments.bc_grid) in
    Notify.start_batchtime();
    List.iter (fun cr -> do_batch ~w ~h "bandwidth" overwrite
		 ~time_limit ~node_limit (Printf.sprintf "%s --corruption %s"
					    alg cr) ["alg", alg;
						     "corrupt", cr])
      corruptions


let do_beam_corruption_batch ?(w = 2000) ?(h = 1200)  ?(overwrite = false)
    ?(wts = Experiments.low_res_weights)
    ?(time_limit = None)
    ?(node_limit = None) ?(append = "")
    ?(beams = Experiments.reduced_beams)
    ?(corruptions = ["0."; "0.1"; "0.2"; "0.3"; "0.4"; "0.5"; "0.5"; "0.6";
		     "0.7"; "0.75"; "0.8"; "0.85"; "0.9"; "0.95";])
    alg =
    Notify.start_batchtime();
  List.iter (fun bw ->
	       List.iter
		 (fun cr ->
		       do_batch ~w ~h "online_corrupted" overwrite
			 ~time_limit ~node_limit
			 (Printf.sprintf "%s %i --corruption %s"
			    alg bw cr) ["alg", alg;
					"beam_width", string_of_int bw;
					"corrupt", cr])
		 corruptions)
    beams



let do_wted_corruption_batch ?(w = 2000) ?(h = 1200)  ?(overwrite = false)
    ?(wts = Experiments.low_res_weights)
    ?(time_limit = None)
    ?(node_limit = None) ?(append = "")
    ?(corruptions = ["0."; "0.1"; "0.2"; "0.3"; "0.4"; "0.5"; "0.5"; "0.6";
		     "0.7"; "0.75"; "0.8"; "0.85"; "0.9"; "0.95";])
    alg =
    Notify.start_batchtime();
    List.iter
      (fun cr -> List.iter
	 (fun wt ->
	    do_batch ~w ~h "online_corrupted" overwrite
	      ~time_limit ~node_limit (Printf.sprintf "%s %f --corruption %s"
					 alg wt cr) ["alg", alg;
						     "wt", string_of_float wt;
						     "corrupt", cr])
	 wts) corruptions




let do_wted_batch ?(w = 2000) ?(h = 1200)
    heuristic overwrite time_limit node_limit alg wt =
  (** wted_astar, anytime_a_star, dyn_wted_a_star, a_star_eps *)
  do_batch ~w ~h heuristic overwrite ~time_limit ~node_limit
    (Wrutils.str "%s %f" alg wt) ["alg", alg;
				  "wt", string_of_float wt;]


let do_beam_batch ?(append = "") heuristic overwrite
    time_limit node_limit alg beam_width =
  (** wted_astar, anytime_a_star, dyn_wted_a_star, a_star_eps *)
  do_batch heuristic overwrite ~time_limit ~node_limit
    (Wrutils.str "%s %d%s" alg beam_width append)
    ["alg", alg;
     "beam_width", string_of_int beam_width;]


let do_cab_batch heuristic overwrite time_limit node_limit alg
    (sort_predicate:string) =
  (** wted_astar, anytime_a_star, dyn_wted_a_star, a_star_eps *)
  do_batch heuristic overwrite time_limit node_limit
    (Wrutils.str "%s %s" alg
       sort_predicate) ["alg", alg;
			"sort_predicate", sort_predicate;]

let do_cab_batches ?(heuristic = "manhattan") ?(overwrite = false)
    ?(time_limit = Some (Experiments.default_time_limit))
    ?(node_limit = Some (Experiments.default_node_limit)) alg
    (sort_predicate:string) =
  Notify.start_batchtime();
  List.iter (fun w ->
	       do_cab_batch heuristic overwrite time_limit node_limit alg w)
    [sort_predicate];
  Notify.send_batch_completed_mail "Grid_runs" "cab" alg



let do_bulb_batch heuristic overwrite time_limit node_limit alg beam_width =
  (** bulb *)
  let node_limit_number = match node_limit with
      None -> 100000000
    | Some n -> n in
  do_batch heuristic overwrite time_limit node_limit
    (Wrutils.str "%s %d %d" alg
       beam_width node_limit_number) ["alg", alg;
				      "beam_width", string_of_int beam_width;
				      "node_capacity", string_of_int node_limit_number;]

let do_bulb_batches ?(heuristic = "manhattan") ?(overwrite = false)
    ?(time_limit = Some (Experiments.default_time_limit))
    ?(node_limit = None)
    ?(beam_widths = Experiments.full_beams) alg =
  Notify.start_batchtime();
  List.iter (fun w ->
	       do_bulb_batch heuristic overwrite time_limit node_limit alg w)
    beam_widths;
  Notify.send_batch_completed_mail "Grid_runs" "Bulb" alg


let do_ib_batch heuristic overwrite time_limit node_limit alg
    beam_width backup_pred
    =
  (** ib *)
  do_batch heuristic overwrite time_limit node_limit
    (Wrutils.str "%s %d %s" alg
       beam_width backup_pred) ["alg", alg;
				"backup_predicate", backup_pred;
				"beam_width", string_of_int beam_width;
			       ]

let do_ib_batches ?(heuristic = "manhattan") ?(overwrite = false)
    ?(time_limit = Some (Experiments.default_time_limit))
    ?(node_limit = None)
    ?(beam_widths = Experiments.full_beams) alg bp =
  Notify.start_batchtime();
  List.iter (fun w ->
	       do_ib_batch heuristic overwrite time_limit node_limit
		 alg w bp)
    beam_widths;
  Notify.send_batch_completed_mail "Grid_runs" "IB" alg



let do_cutoff_run ?(heuristic = "") ?(overwrite = false)
    ?(time_limit = Some (Experiments.default_time_limit))
    ?(node_limit = Some (Experiments.default_node_limit)) alg wt cutoff =
 do_batch heuristic overwrite time_limit node_limit
   (Wrutils.str "%s %f %f" alg wt cutoff)
   ["alg", alg;
    "wt", string_of_float wt;
    "cutoff", string_of_float cutoff;]


let do_wted_batches ?(heuristic = ["manhattan"; (*"canonical"; "canonical_max";*)])
    ?(overwrite = false) ?(time_limit = Some (Experiments.default_time_limit))
    ?(node_limit = Some (Experiments.default_node_limit))
    ?(w = 2000)
    ?(h = 1200)
    ?(weights = Experiments.low_res_weights) alg =
  Notify.start_batchtime();
  List.iter
    (fun heur ->
       List.iter (fun wt ->
		    do_wted_batch ~w ~h
		      heur overwrite time_limit node_limit alg wt) weights)
    heuristic;
  Notify.send_batch_completed_mail "Grid_runs" "Weighted" alg


let do_new_beam_batches ?(heuristic = "manhattan") ?(overwrite = false)
    ?(time_limit = Some (Experiments.default_time_limit))
    ?(node_limit = Some (Experiments.default_node_limit))
    ?(sort_predicate = "f")
    ?(beam_widths = Experiments.full_beams) alg =
    Notify.start_batchtime();
    List.iter
      (fun bw ->
	      do_batch heuristic overwrite time_limit node_limit
		(Wrutils.str "%s %d %s" alg bw sort_predicate)
		["alg", alg;
		 "beam_width",string_of_int bw;
		 "sort_predicate",sort_predicate;]) beam_widths;
    Notify.send_batch_completed_mail "Grid_runs" "restarting_beam" alg


let do_restarting_beam_batches ?(heuristic = "manhattan") ?(overwrite = false)
    ?(time_limit = Some (Experiments.default_time_limit))
    ?(node_limit = Some (Experiments.default_node_limit))
    ?(sort_predicate = "f")
    ?(restart_pred = ["f";"h";"i_max";"i_min";"i_all";"i_d"])
    ?(beam_widths = Experiments.full_beams) alg =
  Notify.start_batchtime();
  List.iter
    (fun bw ->
       List.iter
	 (fun rp ->
	    do_batch heuristic overwrite time_limit node_limit
	      (Wrutils.str "%s %d %s %s" alg bw sort_predicate rp)
	      ["alg", alg;
	       "beam_width",string_of_int bw;
	       "sort_predicate",sort_predicate;
	       "backup_sort_predicate",rp;]) restart_pred) beam_widths;
  Notify.send_batch_completed_mail "Grid_runs" "restarting_beam" alg



let do_msc_k_wted_batches ?(heuristic = "manhattan") ?(overwrite = false)
    ?(time_limit = Some (Experiments.default_time_limit))
    ?(node_limit = Some (Experiments.default_node_limit))
    ?(c_size = Experiments.full_beams) ?(ks = Experiments.ks)
    ?(weights = Experiments.low_res_weights) alg =
  Notify.start_batchtime();
  List.iter
    (fun c ->
       List.iter
	 (fun k ->
	    List.iter
	      (fun wt ->
		 do_batch heuristic overwrite time_limit node_limit
		   (Wrutils.str "%s %f %i %i" alg wt k c)
		   ["alg", alg;
		    "commit", string_of_int c;
		    "k", string_of_int k;
		    "wt", string_of_float wt;]) weights) ks) c_size;
  Notify.send_batch_completed_mail "Grid_runs" "MSC_K_WTED" alg


let do_beam_batches ?(heuristic = "manhattan") ?(overwrite = false)
    ?(time_limit = Some (Experiments.default_time_limit))
    ?(node_limit = Some (Experiments.default_node_limit))
    ?(beam_widths = Experiments.full_beams) ?(append = "") alg =
  Notify.start_batchtime();
  List.iter (fun w ->
	       do_beam_batch ~append heuristic overwrite
		 time_limit node_limit alg w)
    beam_widths;
  Notify.send_batch_completed_mail "Grid_runs" "Beam" alg


let do_optimistic_batch heuristic overwrite time_limit node_limit alg wt opt =
  assert (opt >= 1.);
  do_batch heuristic overwrite time_limit node_limit
    (Wrutils.str "%s %f %f" alg wt opt)
    ["alg", alg;
     "wt", string_of_float wt;        (* order really matters here*)
     "optimism", string_of_float opt] (* wt (bound) must come first*)


let do_wted_beam_batch heuristic overwrite time_limit node_limit alg wt beam =
  do_batch heuristic overwrite time_limit node_limit
    (Wrutils.str "%s %f %i" alg wt beam)
    ["alg", alg;
     "wt", string_of_float wt;        (* order really matters here*)
     "beam", string_of_int beam] (* wt (bound) must come first*)



let do_optimistic_batches ?(heuristic = ["manhattan";(* "canonical"; "canonical_max";*)])
    ?(overwrite = false) ?(time_limit = Some (Experiments.default_time_limit))
    ?(node_limit = Some (Experiments.default_node_limit))
    ?(weights = Experiments.low_res_weights)
    ?(opt = Experiments.optimisms) alg =
  Notify.start_batchtime();
    List.iter
      (fun h ->
	 List.iter
	   (fun opt ->
	      List.iter (fun w -> do_optimistic_batch h overwrite time_limit
			   node_limit alg w opt)
		weights) opt) heuristic;
  Notify.send_batch_completed_mail "Grid_runs" "Optimistic" alg


let do_wted_beam_batches ?(heuristic = ["manhattan";])
    ?(overwrite = false) ?(time_limit = Some (Experiments.default_time_limit))
    ?(node_limit = Some (Experiments.default_node_limit))
    ?(weights = Experiments.low_res_weights)
    ?(beams = [1; 5; 10; 100; 1000]) alg =
  Notify.start_batchtime();
    List.iter
      (fun h ->
	 List.iter
	   (fun beam ->
	      List.iter
		(fun w -> do_wted_beam_batch h overwrite time_limit
		   node_limit alg w beam)
		weights) beams) heuristic;
    Notify.send_batch_completed_mail "Grid_runs" "Optimistic" alg


let do_ara_star_batch ?(heuristic = "") ?(overwrite = false)
    ?(time_limit = Some (Experiments.default_time_limit))
    ?(node_limit = Some (Experiments.default_node_limit))
    wt decr =
  let alg = "ara_star" in
    do_batch heuristic overwrite time_limit node_limit
      (Wrutils.str "%s %f %f" alg wt decr)
      ["alg", alg;
       "wt", string_of_float wt;
       "decr", string_of_float decr;]


let utility_tuples =
  (* cost_coeff, time_coeff *)
  let r secs_per_cost =
    1., (1. /. secs_per_cost)
  in
    [ 0., 1.0;
      (*r 0.0000001; *)
      r 0.000001;
      r 0.000005;
      r 0.00001;
      r 0.00005;
      r 0.0001;
      r 0.0005;
      r 0.001;
      r 0.005;
      r 0.01;
      r 0.05;
      r 0.1;
      1., 0.0; ]


let do_bugsy_batch ?(heuristic = "") ?(overwrite = false)
    ?(time_limit = Some (Experiments.default_time_limit))
    ?(node_limit = Some (Experiments.default_node_limit)) alg =
  (* bugsy, bugsy_coeff *)
  List.iter (fun (cost, time) ->
	       do_batch heuristic overwrite time_limit node_limit
		 (Wrutils.str "%s %f %f" alg cost time)
		 [ "alg", alg;
		   "cost_coeff", string_of_float cost;
		   "time_coeff", string_of_float time; ])
    utility_tuples


let do_partial_astar ?(overwrite=false) p0 p_rate m_rate exps =
  do_runs "manhattan" overwrite ~time_limit:None ~node_limit:None
    ~exp_limit:exps ~w:5000 ~h:5000 ["alg", "partial_astar";
				     "p0", string_of_float p0;
				     "p_rate", string_of_float p_rate;
				     "m_rate", string_of_int m_rate;
				     "expansions", string_of_int exps;
				    ]
    (Printf.sprintf "partial_astar %f %f %d" p0 p_rate m_rate)
    Grid.Fourway Grid.Unit 0.35


let do_continuing_par_astar overwrite w h ways costs p p0 p_rate frac_time =
  let alg_attrs =
    ["alg", "continuing_par_astar";
     "initial p", string_of_float p0;
     "p decrease rate", string_of_float p_rate;
     "fraction of astar time", string_of_float frac_time;
    ] in
  let prob_attrs =
    [ "obstacles", "uniform"]
    @ (batch_attrs costs ways p)
    @ ["width", string_of_int w; "height", string_of_int h;]
  in
  Notify.start_batchtime();
    List.iter
      (fun board_attrs ->
	 let prob_path = Rdb.path_for instance_root board_attrs
	 and attrs = alg_attrs @ (Rdb.filter_attrs ["type"] board_attrs) in
	 let opt_data_path =
	   Rdb.path_for data_root
	     (("alg", "astar") :: ("heuristic", "manhattan")
	      :: (Rdb.filter_attrs ["type"] board_attrs))
	 in
	 let opt_df = Datafile.load opt_data_path in
	 let opt_cost = Datafile.get_val opt_df "final sol cost" in
	 let opt_time = Datafile.get_val opt_df "total raw cpu time" in
	 let time_limit = Some (frac_time *. (float_of_string opt_time)) in
	 let args =
	   sprintf "continuing_par_astar %f %f %s" p0 p_rate opt_cost
	 in
	   do_run "manhattan" overwrite ~time_limit ?node_limit:None
	     args prob_path attrs)
      (Rdb.matching_attrs instance_root prob_attrs);
  Notify.send_batch_completed_mail "Grid_runs"
    "Continuing Partial A*" "continuing_par_astar"


let do_restarting_par_astar overwrite w h ways costs p p0 p_rate frac_time =
  let alg_attrs =
    ["alg", "restarting_par_astar";
     "initial p", string_of_float p0;
     "p decrease rate", string_of_float p_rate;
     "fraction of astar time", string_of_float frac_time;
    ] in
  let prob_attrs =
    [ "obstacles", "uniform"]
    @ (batch_attrs costs ways p)
    @ ["width", string_of_int w; "height", string_of_int h;]
  in
  Notify.start_batchtime();
    List.iter
      (fun board_attrs ->
	 let prob_path = Rdb.path_for instance_root board_attrs
	 and attrs = alg_attrs @ (Rdb.filter_attrs ["type"] board_attrs) in
	 let opt_data_path =
	   Rdb.path_for data_root
	     (("alg", "astar") :: ("heuristic", "manhattan")
	      :: (Rdb.filter_attrs ["type"] board_attrs))
	 in
	 let opt_df = Datafile.load opt_data_path in
	 let opt_cost = Datafile.get_val opt_df "final sol cost" in
	 let opt_time = Datafile.get_val opt_df "total raw cpu time" in
	 let time_limit = Some (frac_time *. (float_of_string opt_time)) in
	 let args =
	   sprintf "restarting_par_astar %f %f %s" p0 p_rate opt_cost
	 in
	   do_run "manhattan" overwrite ~time_limit ?node_limit:None
	     args prob_path attrs)
      (Rdb.matching_attrs instance_root prob_attrs);
  Notify.send_batch_completed_mail "Grid_runs"
    "Restarting Partial A*" "restarting_par_astar"


let do_knuth_sampling overwrite w h ways costs p frac_time =
  let alg_attrs =
    ["alg", "knuth_sampling";
     "fraction of astar time", string_of_float frac_time;
    ] in
  let prob_attrs =
    [ "obstacles", "uniform"]
    @ (batch_attrs costs ways p)
    @ ["width", string_of_int w; "height", string_of_int h;]
  in
  Notify.start_batchtime();
    List.iter
      (fun board_attrs ->
	 let prob_path = Rdb.path_for instance_root board_attrs
	 and attrs = alg_attrs @ (Rdb.filter_attrs ["type"] board_attrs) in
	 let opt_data_path =
	   Rdb.path_for data_root
	     (("alg", "astar") :: ("heuristic", "manhattan")
	      :: (Rdb.filter_attrs ["type"] board_attrs))
	 in
	 let opt_df = Datafile.load opt_data_path in
	 let opt_cost = Datafile.get_val opt_df "final sol cost" in
	 let opt_time = Datafile.get_val opt_df "total raw cpu time" in
	 let time_limit = Some (frac_time *. (float_of_string opt_time)) in
	 let args =
	   sprintf "knuth_sampling %s" opt_cost
	 in
	   do_run "manhattan" overwrite ~time_limit ?node_limit:None
	     args prob_path attrs)
      (Rdb.matching_attrs instance_root prob_attrs);
  Notify.send_batch_completed_mail "Grid_runs" "Knuth Sampling"
    "knuth_sampling"


let do_count_subtrees overwrite w h ways costs p =
  let alg_attrs =
    ["alg", "count_subtrees";
    ] in
  let prob_attrs =
    [ "obstacles", "uniform"]
    @ (batch_attrs costs ways p)
    @ ["width", string_of_int w; "height", string_of_int h;]
  in
  Notify.start_batchtime();
    List.iter
      (fun board_attrs ->
	 let prob_path = Rdb.path_for instance_root board_attrs
	 and attrs = alg_attrs @ (Rdb.filter_attrs ["type"] board_attrs) in
	 let opt_data_path =
	   Rdb.path_for data_root
	     (("alg", "astar") :: ("heuristic", "manhattan")
	      :: (Rdb.filter_attrs ["type"] board_attrs))
	 in
	 let opt_df = Datafile.load opt_data_path in
	 let opt_cost = Datafile.get_val opt_df "final sol cost" in
	 let args =
	   sprintf "count_subtrees %s" opt_cost
	 in
	   do_run "manhattan" overwrite ?time_limit:None ?node_limit:None
	     args prob_path attrs)
      (Rdb.matching_attrs instance_root prob_attrs);
  Notify.send_batch_completed_mail "Grid_runs" "Knuth Sampling"
    "count_subtrees"

(*****************************)


let do_fixed ?(heuristic = ["manhattan";])
    ?(overwrite = false) ?(time_limit = Some 300.)
    ?(node_limit = None) () =
  (** greedy, speedy, a_star *)
  Notify.start_batchtime();
  List.iter
    (fun h ->
       do_batch ~w:200 ~h:200 h overwrite ~time_limit:time_limit
	 ~node_limit:node_limit
	 (*" -- greedy_lms_fixed 1.024316 0.099249 -0.042736 0.76587"*)
	 (*"greedy_adapt"*)
	 (*"greedy_h_adapt"*)
	 "greedy_hpath_adapt"
	 (*"greedy_lms_adapt"*)
	 (*"greedy_ann_adapt"*)
	 (*"greedy"*)
	 (*"astar"*)
	 (*"-- greedy_ann_fixed l4grid"*)
	 (*"-- greedier_ann_fixed l4grid"*)
	 (*" -- greedier_lms_fixed 1.024316 0.099249 -0.042736 0.76587"*)
	 (*"greedier_adapt"*)
	 (*"greedier_lms_adapt"*)
	 (*"greedier_ann_adapt"*)
	 (*"greedier_adapt"*)
	 (*"greedier"*)
	 (*"astar"*)
	 (*"-- greedy_ann_fixed l4grid"*)

	 ["alg", "greedy_hpath_adapt"])
    heuristic;
  Notify.send_batch_completed_mail "Grid_runs" "Basic" "greedy_fixed"


let do_wted_fixed
    ?(weights = Experiments.low_res_weights)
    ?(overwrite = false) ?(time_limit = Some (Experiments.default_time_limit))
    ?(node_limit = Some (Experiments.default_node_limit)) () =
  (** greedy, speedy, a_star *)
  Notify.start_batchtime();
  List.iter
    (fun wt ->
       do_batch ~w:200 ~h:200 "manhattan" overwrite ~time_limit:time_limit
	 ~node_limit:node_limit
	 (Wrutils.str
	   " -- clamped_fixed %f 1.024316 0.099249 -0.042736 0.76587" wt)
	 (*(Wrutils.str "clamped_hd_reckless %f" wt)*)
	 (*(Wrutils.str "clamped_lms_reckless %f" wt)*)
	 (*(Wrutils.str "clamped_hdp_path %f" wt)*)
	 ["alg", "clamped_fixed";
	  "wt", string_of_float wt])
    weights;
  Notify.send_batch_completed_mail "Grid_runs" "Basic" "greedy_fixed"


(*** I'm really sorry - JTD7 **********)

let model_on_instance alg =
  let base_instance_attrs = ["obstacles", "uniform";
			     "width", "2000";
			     "height", "1200";
			     "heuristic", "manhattan";
			     "costs", "Life";
			     "moves", "Four-way";
			     "prob", "0.35";
			     "alg", "greedier_adapt";] in
    for i = 1 to 20 do
      (let iat = ("num", string_of_int i)::base_instance_attrs in
       let old_run = (Dataset.load_from_rdb_with_domain ~domain:"grid" iat
			~name:"oldrun") in
       let herr = (Dataset.get_values float_of_string
		     ~sort:Dataset.Last "h_step" old_run).(0)
       and derr = (Dataset.get_values float_of_string
		     ~sort:Dataset.Last "d_step" old_run).(0) in
	 do_run "manhattan" false ~time_limit:(Some 300.) ~node_limit:None
	   (Wrutils.str " -- %s %f %f"
	      alg herr derr)
	   (Wrutils.str
	      "./group/data/grid_instances/uniform/instance/Life/Four-way/0.35/2000/1200/%i" i)
	   ["obstacles", "uniform";
	    "width", "2000";
	    "height", "1200";
	    "heuristic", "manhattan";
	    "costs", "Life";
	    "moves", "Four-way";
	    "prob", "0.35";
	    "num", string_of_int i;
	    "alg", (Wrutils.str "%s_%s" alg "this_instance")])
    done


let model_on_next_instance alg =
  let base_instance_attrs = ["obstacles", "uniform";
			     "width", "2000";
			     "height", "1200";
			     "heuristic", "manhattan";
			     "costs", "Life";
			     "moves", "Four-way";
			     "prob", "0.35";
			     "alg", "greedier_adapt";] in
    for i = 1 to 20 do
      (let iat = ("num", string_of_int ((i mod 20)+1))::base_instance_attrs in
       let old_run = (Dataset.load_from_rdb_with_domain ~domain:"grid" iat
			~name:"oldrun") in
       let herr = (Dataset.get_values float_of_string
		     ~sort:Dataset.Last "h_step" old_run).(0)
       and derr = (Dataset.get_values float_of_string
		     ~sort:Dataset.Last "d_step" old_run).(0) in
	 do_run "manhattan" false ~time_limit:(Some 300.) ~node_limit:None
	   (Wrutils.str " -- %s %f %f"
	      alg herr derr)
	   (Wrutils.str
	      "./group/data/grid_instances/uniform/instance/Life/Four-way/0.35/2000/1200/%i" i)
	   ["obstacles", "uniform";
	    "width", "2000";
	    "height", "1200";
	    "heuristic", "manhattan";
	    "costs", "Life";
	    "moves", "Four-way";
	    "prob", "0.35";
	    "num", string_of_int i;
	    "alg", (Wrutils.str "%s_%s" alg "next_instance")])
    done


(* EOF *)
