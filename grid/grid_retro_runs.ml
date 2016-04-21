(* $Id: runs.ml,v 1.1 2004/12/22 20:18:00 ruml Exp ruml $
   running experiments*)


(********************* solver main **************************)

let run_retro alg b limit=
  (** writes results to stdout. assumes that header stuff like problem
    attrs are already written.  This is the main function for the stand-alone
    solver. *)
  (* CHEATING HERE : cols are filled in by first log output *)
  (*                 Also, uses comma seperator insetead of tab *)
  output_string stdout "#cols ";
  let (s, e, g, p, m, d), t = Wrsys.with_time (fun () -> alg b limit) in
    (match s with
	 None -> Datafile.write_pairs stdout ["found solution", "no"]
       | Some (p,f) -> Datafile.write_pairs stdout ["found solution", "yes"]);
  let cost, len = (match s with
		     None -> infinity, 0
		       (* path_length checks solution *)
		   | Some (p, f) ->
		       ignore(Grid.check_path b p f);
		       f, (List.length p)) in
    let trail_pairs = ["final sol cost", string_of_float cost;
		       "final sol length", string_of_int len;
		       "total raw cpu time", string_of_float t;
		       "total nodes expanded", string_of_int e;
		       "total nodes generated", string_of_int g] in
      Datafile.write_pairs stdout trail_pairs


(********************* instances **************************)

let data_root = "./jtd7/grid/data"

let batch_attrs costs ways p =
  [ "costs", (Grid_instance.tag_of_costs costs);
    "moves", (Grid_instance.tag_of_moves ways);
    "prob", string_of_float p ]


let make_instances costs ways p =
  Random.self_init ();
  let w = 2000
  and h = 1200
  and n = 20 in
    for i = 1 to n do
      let attrs = [ "type", "instance";
		    "obstacles", "uniform" ] @
		  (batch_attrs costs ways p) @
		  [ "width", string_of_int w;
		    "height", string_of_int h;
		    "num", string_of_int i; ] in
      let path = Rdb.path_for data_root attrs in
	(** TODO: test to see if file is complete **)
	if Sys.file_exists path && Datafile.seems_complete path then
	  Wrutils.pr "%s exists, skipping...\n%!" path
	else
	  let b = Grid_instance.feasible_board w h p costs ways in
	    Grid_instance.save path b;
	    Wrutils.pr "Wrote board %d.\n%!" i
    done

let make_bench_instance1 costs ways p =
  Random.self_init ();
  let w = 4000
  and h = 2400
  and n = 1 in
    for i = 1 to n do
      let attrs = [ "type", "instance";
		    "obstacles", "uniform" ] @
		  (batch_attrs costs ways p) @
		  [ "width", string_of_int w;
		    "height", string_of_int h;
		    "num", string_of_int i; ] in
      let path = Rdb.path_for data_root attrs in
	(** TODO: test to see if file is complete **)
	if Sys.file_exists path && Datafile.seems_complete path then
	  Wrutils.pr "%s exists, skipping...\n%!" path
	else
	  let b = Grid_instance.feasible_board w h p costs ways in
	    Grid_instance.save path b;
	    Wrutils.pr "Wrote board %d.\n%!" i
    done


(* 0.4's are taking too long to generate.  We'll do them up after all the others are made. *)
let batch_tuples = [ Grid.Life, [ Grid.Fourway,  [(*0.25; 0.3;*) 0.35;];
				  Grid.Eightway, [(*0.35; 0.4;*) 0.45]];
		     Grid.Unit, [ Grid.Fourway,  [(*0.25; 0.3;*) 0.35;];
				  Grid.Eightway, [(*0.35; 0.4;*) 0.45]]]


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


(********************* doing experiments **********************)
(* you may want to set the solver binary when doing runs
   Runs.solver_binary := desired path *)
let solver_binary = ref "./grid/src/grid_retro"

let call_solver ?(time_limit=None) args prob_path attrs outch =
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

(* replace default time limit of 600 with actual time limit *)
let do_run args prob_path attrs =
  (** sets up run file and calls solver on board_name *)
  let run_file = Rdb.path_for data_root
		   (Rdb.override_attrs ["type", "retro_log"] attrs) in
    if Sys.file_exists run_file && Datafile.seems_complete run_file then
      Wrutils.pr "%s exists - skipping!\n%!" run_file
    else
      (
      Wrio.with_outfile run_file
	(fun outch ->
	   Wrutils.pr "Running on %s...%!" (Wrfname.make_relative data_root
					    prob_path);
	   let c,t = call_solver (* ~time_limit:(Some(300)) *)
             args prob_path attrs outch in
	       Wrutils.pr "%.1f in %.3f.\n%!" c t)
      )

let do_runs alg_attrs args ways costs p =
  (** runs a basic alg on all boards with [prob_attrs] *)
  let w = 2000
  and h = 1200
  and costs = costs in
  let prob_attrs = [ "obstacles", "uniform";
		     "costs", (Grid_instance.tag_of_costs costs);
		     "moves", (Grid_instance.tag_of_moves ways);
		     "width", string_of_int w;
		     "height", string_of_int h;
		     "prob", string_of_float p;] in
    List.iter (fun board_attrs ->
		 let prob_path = Rdb.path_for data_root board_attrs
		 and attrs = alg_attrs @ board_attrs in
		   do_run args prob_path attrs)
      (Rdb.matching_attrs data_root (Rdb.merge_attrs ["type", "instance"]
				       prob_attrs))


let do_batch args attrs =
  batch_iter (fun costs ways p ->
		do_runs attrs args ways costs p)

let do_basic_batch alg =
  (** greedy, speedy, a_star *)
  do_batch alg ["alg", alg]

(*
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
  [ 100000.;
    100.;
    50.;
    20.;
    15.;
    10.;
    7.;
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
*)
(* EOF *)
