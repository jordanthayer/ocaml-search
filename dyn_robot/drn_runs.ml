(* $Id: runs.ml,v 1.1 2006/06/30 22:41:52 ruml Exp $

   doing runs
*)


(************** basic interactive tests **************)


let test_alg a w =
  let (s, e, g,_,_,_),t = Wrsys.with_time (fun () -> a w) in
    match s with
	None -> failwith "No solution!"
      | Some (n,cost) ->
	  let sol,_ = Dynamic.extract_path s in
	    Wrutils.pr "Finished search.\n";
	    Drn_instance.check_sol w sol cost;
	    Wrutils.pr "Path of %d steps with time %f found in %.3f secs.\n"
	      (List.length sol) cost t;
	    Wrutils.pr "%d nodes expanded (%.1f per sec), %d nodes generated.\n%!"
	      e ((float e) /. t) g;
	    Drn_instance.print_sol sol;
	    Drn_instance.show_sol w sol true


(*
  for cell_size = 2:
    Runs.test1 160 100 0.1;;
*)
(*let test1 ?(seed = (Random.self_init (); Random.int 10000)) x y p =
  let w = Math.with_random_state
	    (fun () ->
	       Drn_instance.make_instance Drn_instance.make_clumpy x y p)
	    (Math.random_state_from seed) in
    Wrutils.pr "Starting search.\n%!";
    test_alg Non_dynamic.shortest_path w
*)

(*
  Runs.test2 ~seed:6553 25 25 0.2 3.;;

  for clumpy:
  Runs.test2 ~seed:1011 250 250 0.1 3.;;

  for liney:
  Runs.test2 250 250 20. 1.5;;
  Seed is 7576.

  Runs.test2 250 250 20. 1.5;;
  Seed is 4699.

*)
let test2 ?(seed = (Random.self_init (); Random.int 10000)) x y p wt =
  Wrutils.pr "Seed is %d.\n" seed;
  let w = Math.with_random_state
	    (fun () ->
	       Drn_instance.make_instance Drn_instance.make_liney x y p)
	    (Math.random_state_from seed) in
    (let gx, gy, _ = w.Drn_instance.goal in
       Wrutils.pr "Starting search to %d,%d.\n%!" gx gy);
    test_alg (fun n -> Drn_algs.wted_a_star wt n [Limit.Never]) w


let test3 i a =
  let p = Drn_instance.load (Wrutils.str "/tilde/ruml/projects/path/drn/data/instance/liney/250/250/20/%d" i) in
    test_alg a p


(************** stand-along solver **************)


let run_alg ?(limit = [Limit.Time 300.]) alg prob =
  (** writes results to stdout. assumes that header stuff like problem
      attrs are already written.  This is the main function for the
      stand-alone solver. *)
  Datafile.write_colnames stdout
    ["sol cost"; "nodes expanded"; "nodes generated"; "raw cpu time"];
  let (s,e,g,p,m,d),t = Wrsys.with_time (fun () -> alg prob limit) in
  let cost,len = (match s with
		      None -> infinity, 0
		    | Some (p,f) ->
			(let pth, t = Dynamic.extract_path s in
			   f, (List.length pth))) in
    Wrutils.pr "%f\t%d\t%d\t%f\n" cost e g t;
    let trail_pairs = ["final sol cost", string_of_float cost;
		       "final sol length", string_of_int len;
		       "total raw cpu time", string_of_float t;
		       "total nodes expanded", string_of_int e;
		       "total nodes generated", string_of_int g] in
      Datafile.write_pairs stdout trail_pairs


(************** instances **************)

let data_root = "./research/data/dyn_robot"
and instance_root = "./group/data/dyn_robot_instances"

let if_not_exists attrs i f =
  let path = Rdb.path_for instance_root attrs in
    if Sys.file_exists path then
      Wrutils.pr "%s exists, skipping...\n%!" path
    else
      let p = f () in
	Drn_instance.save path p;
	Wrutils.pr "Wrote problem %d.\n%!" i


let make_liney_instances x y l n =
  for i = 1 to n do
    let attrs = [ "type", "instance";
		  "obstacles", "liney";
		  "width", string_of_int x;
		  "height", string_of_int y;
		  "num_lines", string_of_int l;
		  "num", string_of_int i; ] in
      if_not_exists attrs i
	(fun () ->
	   Wrutils.eval_until (fun () ->
			       Drn_instance.make_instance
			       Drn_instance.make_liney x y (float l))
	   Drn_algs.feasible_p)
  done


let make_uniform_instances x y p n =
  for i = 1 to n do
    let attrs = [ "type", "instance";
		  "obstacles", "uniform";
		  "width", string_of_int x;
		  "height", string_of_int y;
		  "num_lines", string_of_float p;
		  "num", string_of_int i; ] in
      if_not_exists attrs i
	(fun () ->
	   Wrutils.eval_until (fun () ->
			       Drn_instance.make_instance
			       Drn_instance.make_uniform x y p)
	   Drn_algs.feasible_p)
  done




let make_all_instances () =
  Random.self_init ();
  make_liney_instances 250 250 20 20


(************** running experiments **************)


let solver_binary = "drn.linux"


let call_solver args prob_path attrs outch =
  (** calls the external solver on [args] and [board_name], redirecting its
    output to [outch].  all logging is done within here.  returns cost, time
    pair *)
  Wrsys.with_subprocess_pipes
    (Wrutils.str "%s %s < %s" solver_binary args prob_path)
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

let do_run args prob_path attrs =
  (** sets up run file and calls solver on board_name *)
  let run_file = Rdb.path_for data_root
		   (Rdb.override_attrs ["type", "run"] attrs) in
    if ((Sys.file_exists run_file) &&
	(Datafile.seems_complete run_file)) then
      Wrutils.pr "%s exists - skipping!\n%!" run_file
    else
      Wrio.with_outfile run_file
	(fun outch ->
	   Wrutils.pr "Running on %s...%!" (Wrfname.make_relative data_root
					    prob_path);
	   let c,t = call_solver args prob_path attrs outch in
	     Wrutils.pr "%.1f in %.3f.\n%!" c t)



let batch_tuples = [ [ "obstacles", "liney"] ;
		     (*[ "obstacles", "uniform"]*)
		   ]


let do_batches alg_attrs args =
  (** runs an alg on all problems *)
  List.iter (fun batch_attrs ->
	       List.iter (fun prob_attrs ->
			    let prob_path = Rdb.path_for instance_root prob_attrs
			    and attrs = alg_attrs @ prob_attrs in
			      do_run args prob_path attrs)
	       (Rdb.matching_attrs instance_root
		  (Rdb.merge_attrs ["type", "instance"] batch_attrs)))
    batch_tuples


let do_basic_batch alg =
  (* a_star, greedy, speedy *)
  do_batches ["alg", alg] alg


let do_wted_batch alg wt =
  (* wted_a_star, anytime_a_star *)
  do_batches ["alg", alg;
	      "wt", string_of_float wt;]
    (Wrutils.str "%s %f" alg wt)

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


let do_wted_batches alg =
  List.iter (do_wted_batch alg) weights


let do_ara_star_batch () =
  (* Likhachev et al start at 2.5 and don't say what they decrement by *)
  let alg = "ara_star"
  and start = 3.
  and decr = 0.2
  and final = 1. in
    do_batches ["alg", alg;
		"start", string_of_float start;
		"decr", string_of_float decr;
		"final", string_of_float final; ]
      (Wrutils.str "%s %f %f %f" alg start decr final)


let utility_tuples =
  (* cost_coeff, time_coeff *)
  let r secs_per_cost =
    1., (1. /. secs_per_cost)
  in
    [
      0., 1.0;
      r 0.00001;
      r 0.0001;
      r 0.001;
      r 0.01;
      r 0.1;
      r 1.0;
      r 10.;
      r 100.;
      (* 1., 0.0; *)
    ]


let do_bugsy_batch () =
  let alg = "bugsy" in
    List.iter (fun (cost, time) ->
		 let attrs = ["alg", alg;
			      "cost_coeff", string_of_float cost;
			      "time_coeff", string_of_float time; ]
		 and args = Wrutils.str "%s %f %f" alg cost time in
		   do_batches attrs args)
      utility_tuples


(* EOF *)
