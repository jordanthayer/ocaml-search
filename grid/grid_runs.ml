(* $Id: runs.ml,v 1.1 2004/12/22 20:18:00 ruml Exp ruml $
   running experiments *)


(********************* solver main **************************)

let run_alg alg b limit =
  (** writes results to stdout. assumes that header stuff like problem
    attrs are already written.  This is the main function for the stand-alone
    solver. *)
  Datafile.write_colnames stdout
    ["sol cost"; "sol length";
     "nodes expanded"; "nodes generated"; "raw cpu time"];
  let (s, e, g, p, m, d), t = Wrsys.with_time (fun () -> alg b limit) in
    (match s with
	 None -> Datafile.write_pairs stdout ["found solution", "no"]
       | Some (p,f) -> Datafile.write_pairs stdout ["found solution", "yes"]);
  let cost, len = (match s with
		     None -> infinity, 0
		       (* path_length checks solution *)
		   | Some (p, f) ->
			 (Grid.check_path b p f), (List.length p)) in
    (* write last row of cols *)
    Wrutils.pr "%f\t%d\t%d\t%d\t%f\n" cost len e g t;
    let trail_pairs = ["final sol cost", string_of_float cost;
		       "final sol length", string_of_int len;
		       "total raw cpu time", string_of_float t;
		       "total nodes expanded", string_of_int e;
		       "total nodes generated", string_of_int g] in
      Datafile.write_pairs stdout trail_pairs


(********************* instances **************************)


let data_root = Experiments.data_root ^ "grid"
and instance_root = "./group/data/grid_instances"

let batch_attrs costs ways p =
  [ "costs", (Grid_instance.tag_of_costs costs);
    "moves", (Grid_instance.tag_of_moves ways);
    "prob", string_of_float p ]

let line_attrs costs ways l =
  [ "costs", (Grid_instance.tag_of_costs costs);
    "moves", (Grid_instance.tag_of_moves ways);
    "prob", string_of_int l ]

let mg_attrs costs ways start_p end_p goals =
  [ "costs", (Grid_instance.tag_of_costs costs);
    "moves", (Grid_instance.tag_of_moves ways);
    "start_prob", string_of_float start_p;
    "end_prob", string_of_float end_p;
    "goals", string_of_int goals;]

let box_attrs costs ways p =
  [ "costs", (Grid_instance.tag_of_costs costs);
    "moves", (Grid_instance.tag_of_moves ways);
    "prob", string_of_float p ]


let make_instances ?(w = 2000) ?(h = 1200) ?(n = 20) costs ways p =
  Random.self_init ();
  for i = 1 to n do
    let attrs = [ "type", "instance";
		  "obstacles", "uniform" ] @
      (batch_attrs costs ways p) @
		  [ "width", string_of_int w;
		    "height", string_of_int h;
		    "num", string_of_int i; ] in
    let path = Rdb.path_for instance_root attrs in
      (** TODO: test to see if file is complete **)
      if Sys.file_exists path && Datafile.seems_complete path then
	Wrutils.pr "%s exists, skipping...\n%!" path
      else
	let b = Grid_instance.feasible_board w h p costs ways in
	  Grid_instance.save path b;
	  Wrutils.pr "Wrote board %d.\n%!" i
  done


(* segment boards 25, 35, 45? *)
(* 25 50 75 sounds better *)

let make_line_instances ?(w = 2000) ?(h = 1200) ?(n = 20) costs ways lines =
  Random.self_init ();
  for i = 1 to n do
    let attrs = [ "type", "instance";
		  "obstacles", "lines" ] @
      (line_attrs costs ways lines) @
		  [ "width", string_of_int w;
		    "height", string_of_int h;
		    "num", string_of_int i; ] in
    let path = Rdb.path_for instance_root attrs in
      (** TODO: test to see if file is complete **)
      if Sys.file_exists path && Datafile.seems_complete path then
	Wrutils.pr "%s exists, skipping...\n%!" path
      else
	let b = LineSegGrid.feasible_board w h costs ways lines in
	  Grid_instance.save path b;
	  Wrutils.pr "Wrote board %d.\n%!" i
  done


let make_game_instances path brd_name =
  let rec write_bucket i b list =
    match list with
	[] -> ()
      | h::tl ->
	  (let attrs = [ "type", "instance";
			 "obstacles", "game";
			 "board", brd_name;
			 "bucket", string_of_int (100 + b * 10);
			 "num", string_of_int i;] in
	   let path = Rdb.path_for instance_root attrs in
	     if Sys.file_exists path && Datafile.seems_complete path then
	       Wrutils.pr "%s exists, skipping...\n%!" path
	     else
	       (Grid_instance.save path h.GameGrid.b;
		Wrutils.pr "Wrote board\n%!";
		write_bucket (i+1) b tl)) in
    Random.self_init ();
    let bar = GameGrid.load_bucket path 100 200 10 40 in
      Verb.pe Verb.toplvl "bar loaded\n%!";
      for i = 0 to ((Array.length bar.GameGrid.bar) - 1)
      do
	write_bucket 0 i bar.GameGrid.bar.(i)
      done



let make_fshad_instances ?(w = 2000) ?(h = 1200) ?(n = 20) costs ways prob =
  Random.self_init ();
  for i = 1 to n do
    let attrs = [ "type", "instance";
		  "obstacles", "uniform_region" ] @
      (box_attrs costs ways prob) @
		  [ "width", string_of_int w;
		    "height", string_of_int h;
		    "num", string_of_int i; ] in
    let path = Rdb.path_for instance_root attrs in
      (** TODO: test to see if file is complete **)
      if Sys.file_exists path && Datafile.seems_complete path then
	Wrutils.pr "%s exists, skipping...\n%!" path
      else
	let b = Grid_demo_fshadow.feasible_board w h prob costs ways in
	  Grid_instance.save path b;
	  Wrutils.pr "Wrote board %d.\n%!" i
  done


let make_many_instances ?(w = 1000) ?(h = 1000) ?(n = 20) costs ways
    start_p end_p goals =
  Random.self_init ();
  for i = 1 to n do
    let attrs = [ "type", "instance";
		  "obstacles", "many_goal" ] @
      (mg_attrs costs ways start_p end_p goals) @
		  [ "width", string_of_int w;
		    "height", string_of_int h;
		    "num", string_of_int i; ] in
    let path = Rdb.path_for instance_root attrs in
      (** TODO: test to see if file is complete **)
      if Sys.file_exists path && Datafile.seems_complete path then
	Wrutils.pr "%s exists, skipping...\n%!" path
      else
	let b = Many_goal.feasible_board w h costs ways start_p end_p goals in
	  Grid_instance.save path b;
	  Wrutils.pr "Wrote board %d.\n%!" i
  done



let make_bench_instance1 ?(w = 8000) ?(h = 4800) ?(n = 1) costs ways p =
  Random.self_init ();
  for i = 1 to n do
    let attrs = [ "type", "instance";
		  "obstacles", "uniform" ] @
      (batch_attrs costs ways p) @
		  [ "width", string_of_int w;
		    "height", string_of_int h;
		    "num", string_of_int i; ] in
    let path = Rdb.path_for instance_root attrs in
      (** TODO: test to see if file is complete **)
      if Sys.file_exists path && Datafile.seems_complete path then
	Wrutils.pr "%s exists, skipping...\n%!" path
      else
	let b = Grid_instance.feasible_board w h p costs ways in
	  Grid_instance.save path b;
	  Wrutils.pr "Wrote board %d.\n%!" i
  done


let batch_tuples = [Grid.Life, [ Grid.Fourway,  [0.25; 0.3; 0.35;];
				 Grid.Eightway, [0.35; 0.4; 0.45]];
		    Grid.Unit, [ Grid.Fourway,  [0.25; 0.3; 0.35;];
				 Grid.Eightway, [0.35; 0.4; 0.45]]]

let size_scale = [Grid.Unit, [Grid.Fourway, [0.35;]];]

let batch_fold f init =
  List.fold_left (fun a (costs, way_pairs) ->
		    List.fold_left (fun a (ways, ps) ->
				      List.fold_left (fun a p ->
							f a costs ways p)
				      a ps)
		    a way_pairs)
    init batch_tuples

let batch_iter f =
  batch_fold (fun () c w p -> f c w p; ()) ()

let batch_map f =
  List.rev (batch_fold (fun a c w p -> (f c w p)::a) [])


let make_all_instances () =
  batch_iter make_instances

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

let make_size_scale () =
  for x = 1 to 10
  do
    Verb.pe Verb.toplvl "Run %d of 100\n\n%!" x;
    size_iter (make_instances ~w:(x*100) ~h:(x*100))
  done


(********************* doing experiments **********************)
(* you may want to set the solver binary when doing runs
   Runs.solver_binary := desired path *)
let solver_binary = ref "./research/grid/grid_path.linux-gnu"

let call_solver ?(time_limit= (Some 300)) args prob_path attrs outch =
  (** calls the external solver on [args] and [board_name], redirecting its
    output to [outch].  all logging is done within here.  returns cost, time
    pair *)
  let limit_args =
    match time_limit with
	Some(t) ->  Wrutils.str "-limit %i %s" t args
      | _ -> args in
    Wrsys.with_subprocess_pipes
      (Wrutils.str "%s %s < %s" !solver_binary limit_args prob_path)
      (fun to_solver from_solver ->
	 Datafile.write_header_pairs outch;
	 Datafile.write_pairs outch attrs;
	 Datafile.write_pairs outch ["attrs", (Wrstr.encode_pairs attrs)];
	 let res = Datafile.pipe_data from_solver outch in
	   Datafile.write_trailer_pairs outch;
	   res)

let valid_df df =
  try
    (Datafile.get_val df "found solution") <> "no"
  with _ -> true

(* replace default time limit of 600 with actual time limit *)
let do_run args prob_path attrs =
  (** sets up run file and calls solver on board_name *)
  let run_file = Rdb.path_for data_root
    (Rdb.filter_attrs ["type"] attrs) in
    if Sys.file_exists run_file &&
      Datafile.seems_complete run_file  &&
      valid_df (Datafile.load run_file)
    then
      Wrutils.pr "%s exists - skipping!\n%!" run_file
    else
      (
      Wrio.with_outfile run_file
	(fun outch ->
	   Wrutils.pr "Running on %s...%!" (Wrfname.make_relative instance_root
					    prob_path);
	   let c,t = call_solver args prob_path attrs outch in
	     Wrutils.pr "%.1f in %.3f.\n%!" c t)
      )


let do_uni_runs w h alg_attrs args ways costs p =
  (** runs a basic alg on all boards with [prob_attrs] *)
  let costs = costs in
  let prob_attrs = [ "obstacles", "uniform";
		     "prob", string_of_float p;
		     "costs", (Grid_instance.tag_of_costs costs);
		     "moves", (Grid_instance.tag_of_moves ways);
		     "width", string_of_int w;
 		     "height", string_of_int h;
		   ] in
    List.iter (fun board_attrs ->
		 let prob_path = Rdb.path_for instance_root board_attrs
		 and attrs = alg_attrs @ board_attrs in
		   do_run args prob_path attrs)
      (Rdb.matching_attrs instance_root prob_attrs)


let do_line_runs w h alg_attrs args ways costs =
  (** runs a basic alg on all boards with [prob_attrs] *)
  let costs = costs in
  let prob_attrs = [ "obstacles", "lines";
		     "costs", (Grid_instance.tag_of_costs costs);
		     "moves", (Grid_instance.tag_of_moves ways);
		     "width", string_of_int w;
 		     "height", string_of_int h;
		   ] in
    List.iter (fun board_attrs ->
		 let prob_path = Rdb.path_for instance_root board_attrs
		 and attrs = alg_attrs @ board_attrs in
		   do_run args prob_path attrs)
      (Rdb.matching_attrs instance_root prob_attrs)


let do_mg_runs w h alg_attrs args ways costs sp ep g =
  (** runs a basic alg on all boards with [prob_attrs] *)
  let costs = costs in
  let prob_attrs = [ "obstacles", "lines";
		     "costs", (Grid_instance.tag_of_costs costs);
		     "moves", (Grid_instance.tag_of_moves ways);
		     "width", string_of_int w;
 		     "height", string_of_int h;
		     "start_prob", string_of_float sp;
		     "end_prob" , string_of_float ep;
		     "goals", string_of_int g
		   ] in
    List.iter (fun board_attrs ->
		 let prob_path = Rdb.path_for instance_root board_attrs
		 and attrs = alg_attrs @ board_attrs in
		   do_run args prob_path attrs)
      (Rdb.matching_attrs instance_root prob_attrs)


let do_game_runs alg_attrs args =
  (** runs a basic alg on all boards with [prob_attrs] *)
  let prob_attrs = ["obstacles", "game";] in
    List.iter (fun board_attrs ->
		 let prob_path = Rdb.path_for instance_root board_attrs
		 and attrs = alg_attrs @ board_attrs in
		   do_run args prob_path attrs)
      (Rdb.matching_attrs instance_root prob_attrs)


let do_runs ?(w = 2000) ?(h = 1200) alg_attrs args ways costs p =
  do_uni_runs w h alg_attrs args ways costs p;
  do_line_runs w h alg_attrs args ways costs;
  do_game_runs alg_attrs args

let do_batch args attrs =
  batch_iter (fun costs ways p ->
		do_runs attrs args ways costs p)


let do_size_run args attrs =
  for i = 1 to 100 do
    batch_iter
      (fun costs ways p ->
	 do_runs ~w:(i*10) ~h:(i*10) attrs args ways costs p)
  done

let do_basic_batch alg =
  (** greedy, speedy, a_star *)
  do_batch alg ["alg", alg]


let do_george_batch ()=
 do_basic_batch "george_global_err";
 do_basic_batch "george_path_err";
 do_basic_batch "george_level_err";
 do_basic_batch "george_window_err";
 do_basic_batch "george_classic_w"

let do_wted_batch alg wt =
  (** wted_astar, anytime_a_star, dyn_wted_a_star, a_star_eps *)
  do_batch (Wrutils.str "%s %f" alg wt) ["alg", alg;
				       "wt", string_of_float wt;]
(*  do_size_run (Wrutils.str "%s %f" alg wt) ["alg", alg;
				       "wt", string_of_float wt;]*)

let do_cutoff_run alg wt cutoff =
 do_batch (Wrutils.str "%s %f %f" alg wt cutoff)
   ["alg", alg;
    "wt", string_of_float wt;
    "cutoff", string_of_float cutoff;
   ]


let do_george_cutoff alg cutoff =
  do_batch (Wrutils.str "%s %f" alg cutoff)
    ["alg", alg;
     "cutoff", string_of_float cutoff;
    ]


let weights =
  [ (*100000.;
    100.;
    50.;
    20.;
    15.;
    10.;
    7.;*)
    5.;
    4.;
    3.;
    2.5; 2.;
    1.75; 1.5; 1.3; 1.2; 1.15; 1.1;1.05; 1.01; 1.001; 1.0005; 1.0;]

(*let weights = List.rev weights*)

let cutoffs =
  [ 5.0;
    4.0;
    3.5;
    3.0;
    2.5;
    2.0;
    1.75;
    1.5;
    1.3;
    1.2;
    1.1;
    1.05;
    1.03;
    1.01;
    1.0
  ]


let do_wted_batches alg =
  List.iter (fun w ->
	       do_wted_batch alg w)
    weights


let do_anytime_george alg =
  List.iter (fun co -> do_george_cutoff alg co) weights


let do_ara_star_batch wt decr =
  let alg = "ara_star" in
    do_batch
      (Wrutils.str "%s %f %f" alg wt decr)
      ["alg", alg;
       "wt", string_of_float wt;
       "decr", string_of_float decr;]


let do_anytime_georges ()=
 do_anytime_george "anytime_george_global_err";
 do_anytime_george "anytime_george_path_err";
 do_anytime_george "anytime_george_level_err";
 do_anytime_george "anytime_george_window_err";
 do_anytime_george "anytime_george_classic_w"


let do_batches () =
 do_basic_batch "greedy";
 do_basic_batch "speedy";
 do_basic_batch "a_star";
 do_basic_batch "speedier";
 do_basic_batch "greedier";
 do_wted_batches "wted_a_star";
 do_anytime_george "anytime_astar_cutoff_2x";
 do_george_batch()

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


let do_bugsy_batch alg =
  (* bugsy, bugsy_coeff *)
  List.iter (fun (cost, time) ->
	       do_batch (Wrutils.str "%s %f %f" alg cost time)
	       [ "alg", alg;
		 "cost_coeff", string_of_float cost;
		 "time_coeff", string_of_float time; ])
    utility_tuples
(* EOF *)
