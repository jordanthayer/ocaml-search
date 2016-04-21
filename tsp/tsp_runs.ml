(* $Id: runs.ml,v 1.1 2006/06/30 19:42:43 ruml Exp ruml $

   doing runs
*)
open Tsp


let print_results w s e g t =
  (match s with
    None ->
      Wrutils.pr "No feasible tour found!\n";
  | Some (n, g) ->
      Wrutils.pr "Found tour of length = %d with cost %.4f: \n" n.depth g);
  Wrutils.pr "Search took %.3f seconds.\n" t;
  Wrutils.pr "%d nodes expanded (%d per second)\n"
    e (Math.round ((float e) /. t));
  Wrutils.pr "%d nodes generated (%d per second)\n"
    g (Math.round ((float g) /. t));
  Wrutils.pr "Branching factor of %f.\n" (Math.div g e)

(************** instances for experiments **************)
let data_root = "./research/data/tsp"
and instance_root = "./group/data/tsp_instances"


let generate_prob symm m size =
  (fun () ->
     if m = "pkhard" then
       Tsp_instances.p_and_k ~symm:symm size
     else
       Tsp_instances.uniform_usquare size)


let if_not_exists symm attrs i f =
  let path = Rdb.path_for instance_root attrs in
    if Sys.file_exists path then
      Wrutils.pr "%s exists, skipping...\n%!" path
    else
      let p = f () in
	Wrio.with_outfile path
	  (fun ch -> Tsp_instances.write p symm ch);
	Wrutils.pr "Wrote problem %d.\n%!" i


let make_instances symm model size num =
  for i = 1 to num do
    let attrs = [ "type", "instance";
		  "model", model;
		  "symmetric", string_of_bool symm;
		  "size", string_of_int size;
		  "num", string_of_int i; ] in
      if_not_exists symm attrs i (generate_prob symm model size)
  done


let instance_specs =
  [ 1, 12, 40;
    1, 19, 40;
    1, 25, 40;
    1, 30, 40;
    1, 40, 40;
    1, 100, 100;
    1, 100, 100;
  ]

let batch_tuples =
  [ "model" , "pkhard" ;
    "model" , "usquare" ]

let make_all_instances () =
  List.iter2 (fun (m,v) (symm, size, num) ->
		if symm == 1 then
		  make_instances true v size num
		else
		  make_instances false v size num)
    batch_tuples instance_specs



(************** running experiments **************)

let solver_binary = "./research/tsp/tsp.linux"


let call_solver ?(time_limit= Some 60) args prob_path attrs outch =
  (** calls the external solver on [args] and [board_name], redirecting its
    output to [outch].  all logging is done within here.  returns cost, time
    pair *)
  let limit_args =
    match time_limit with
	Some(t) -> Wrutils.str "-limit %i %s" t args
      | _ -> args in
    Wrsys.with_subprocess_pipes
      (Wrutils.str "%s %s < %s" solver_binary limit_args prob_path)
      (fun to_solver from_solver ->
	 Datafile.write_header_pairs outch;
	 Datafile.write_pairs outch attrs;
	 Datafile.write_pairs outch ["attrs", (Wrstr.encode_pairs attrs)];
	 let res = Datafile.pipe_data from_solver outch in
	   Datafile.write_trailer_pairs outch;
	   res)


let do_run args prob_path attrs =
  (** sets up run file and calls solver on problem *)
  let run_file = Rdb.path_for data_root
		   (Rdb.filter_attrs ["type"] attrs) in
    if ((Sys.file_exists run_file) &&
	(Datafile.seems_complete run_file)) then
      Wrutils.pr "%s exists - skipping!\n%!" run_file
    else
      Wrio.with_outfile run_file
	(fun outch ->
	   Wrutils.pr "Running on %s...%!" (Wrfname.make_relative instance_root
					    prob_path);
	   let c,t = call_solver args prob_path attrs outch in
	     Wrutils.pr "%.5f in %.3f.\n%!" c t)


let b_attrs =
  [ ["model", "pkhard";
     "size", "12"];
(*
    ["model", "pkhard";
     "size", "20"];
    ["model", "pkhard";
     "size", "25"];
    ["model", "pkhard";
     "size", "40"];*)
    ["model", "usquare";
     "size", "19";];
    ["model", "usquare";
     "size", "25";];
    ["model", "usquare";
     "size", "30";];
    ["model", "usquare";
     "size", "40";];
    ["model", "usquare";
     "size", "100";];
    ["model", "pkhard";
     "size", "100";];
]


let do_batches alg_attrs args =
  (** runs an alg on all problems *)
  List.iter2 (fun (symm,size,_) (batch_attrs) ->
		List.iter (fun prob_attrs ->
			     let prob_path =
			       Rdb.path_for instance_root prob_attrs
			     and attrs = alg_attrs @ prob_attrs in
			       do_run args prob_path attrs)
		(Rdb.matching_attrs instance_root batch_attrs))
    instance_specs b_attrs



let do_basic_batch alg =
  (* a_star, greedy, speedy *)
  do_batches ["alg", alg] alg


let do_wted_batch alg wt =
  (* wted_a_star, anytime_a_star *)
  (*let wt = 3. in*)
  do_batches ["alg", alg;
	      "wt", string_of_float wt;]
    (Wrutils.str "%s %f" alg wt)


let weights =
  [(*100000.;
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
   1.75; 1.5; 1.3; 1.2; 1.15; 1.1; 1.05; 1.01; 1.001; 1.0005; 1.0]

let weights = List.rev weights


let do_wted_batches alg =
  List.iter (fun w ->
	       do_wted_batch alg w)
    (List.rev weights)



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
      r 0.0001;
      r 0.0005;
      r 0.001;
      r 0.005;
      r 0.01;
      r 0.05;
      r 0.1;
      r 0.5;
      r 1.;
      r 5.;
      r 10.;
      1., 0.0;
    ]


let do_bugsy_batch alg =
  List.iter (fun (cost, time) ->
	       let attrs = ["alg", alg;
			    "cost_coeff", string_of_float cost;
			    "time_coeff", string_of_float time; ]
	       and args = Wrutils.str "%s %f %f" alg cost time in
		 do_batches attrs args)
    utility_tuples





let run_all_algs () =
  do_basic_batch "a_star";
  do_basic_batch "greedy";
  do_wted_batches "wted_a_star";
  do_wted_batches "a_star_eps";
  do_wted_batches "anytime_a_star";
  do_wted_batches "anytime_aseps"
  (*do_ara_star_batch ()*)


(* EOF *)
