(* $Id: runs.ml,v 1.1 2008/01/19 21:40:40 ruml Exp $

   doing runs
*)


(************** basic interactive tests **************)


let test_alg a i =
  let (s, e, g, p, q, d),t = Wrsys.with_time (fun () -> a i [Limit.Never]) in
    match s with
	None -> Wrutils.pr "no solution?!"
      | Some (s, c) ->
	  Msa.print_trace (Msa.check_goal i s c);
	  Wrutils.pr "Score of %d in %.3f secs (%d expanded, %d generated).\n" c t e g


let test_two a i =
    test_alg a i;
    let score,t = Wrsys.with_time
		    (fun () -> Msa_dp.pair_optimal_cost i.(0) i.(1)) in
      Wrutils.pr "DP gets %d in %.3f secs.\n" score t

(*
let test2 alpha n prob_same =
  let i = Msa_instance.similar prob_same n 2 alpha in
    test_two Msa.a_star i

      (*
let test3 alpha k n =
  let i = Msa_instance.random n k alpha in
    test_alg (Msa.bugsy 1. 0.) i
      *)
let test4 alpha k n prob_same =
  let i = Msa_instance.similar prob_same n k alpha in
    test_alg Msa.a_star i

let test5 n prob_subst prob_del =
  let k = 3
  and alpha = 20 in
  let i = Msa_instance.morphed prob_subst prob_del n k alpha in
    test_alg Msa.a_star i
*)

(************** stand-along solver **************)


let run_alg ?(lim = [Limit.Never]) alg prob =
  (** writes results to stdout. assumes that header stuff like problem
      attrs are already written.  This is the main function for the
      stand-alone solver. *)
  Datafile.write_colnames stdout
    ["sol cost"; "nodes expanded"; "nodes generated"; "raw cpu time"];
  let (s, e, g, p, q, d),t = Wrsys.with_time (fun () -> alg prob lim) in
    (match s with
	 None -> Datafile.write_pairs stdout ["found solution", "no"]
       | Some (sol, cost) ->
	   ignore (Msa.check_goal prob sol cost);
	   Wrutils.pr "%d\t%d\t%d\t%f\n" cost e g t;
	   Datafile.write_pairs stdout ["found solution", "yes";
					"final sol cost", string_of_int cost]);
    let trail_pairs = [ "total raw cpu time", string_of_float t;
			"total nodes expanded", string_of_int e;
			"total nodes generated", string_of_int g] in
      Datafile.write_pairs stdout trail_pairs


(************** instances **************)


let data_root = "./msa/data"


let if_not_exists attrs i f =
  let path = Rdb.path_for data_root attrs in
    if Sys.file_exists path then
      Wrutils.pr "%s exists, skipping...\n%!" path
    else
      let p = f () in
	Msa.save path p;
	Wrutils.pr "Wrote problem %d.\n%!" i


let make_morphed_instances p_subst p_del n num =
  let k = 3
  and alpha = 20 in
  for i = 1 to num do
    let attrs = [ "type", "instance";
		  "model", "morphed";
		  "seq_len", string_of_int n;
		  "prob_subst", string_of_float p_subst;
		  "prob_del", string_of_float p_del;
		  "num_seqs", string_of_int k;
		  "alphabet", string_of_int alpha;
		  "num", string_of_int i; ] in
      if_not_exists attrs i
	(fun () -> Msa_instance.morphed p_subst p_del n k alpha)
  done


let make_all_instances () =
  let num = 5
  and sizes = [100; 200; 300; 400; 500; 600; 700; 800; 900; 1000] in
    Random.self_init ();
    List.iter (fun len ->
		 make_morphed_instances 0.25 0.25 len num)
      sizes


let make_real_instances () =
  List.iter (fun attrs ->
	       let in_path = Rdb.path_for data_root attrs in
	       let p = Msa_instance.load_real_raw in_path in
		 Wrutils.pr "loaded %s.\n%!" in_path;
		 Msa.save (Rdb.path_for data_root
			     (Rdb.override_attrs ["model", "real"] attrs))
		   p)
    (Rdb.matching_attrs data_root ["type", "instance";
				   "model", "real-raw";])


(************** running experiments **************)

(*I CHANGED THIS TARGET TEMPORARILY BECAUSE I DON'T REALLY
  UNDERSTAND SYMBOLIC LINKS. ORIGINALLY HARD MSA/BIN/MSA.LINUX *)
let solver_binary = "./msa/src/msa.linux"


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



let batch_tuples =
  [ ["model", "real" ] ]
  (*List.map (fun n ->
	      [ "model", "morphed";
		"seq_len", n; ])
    ["500"; "600"; "700"; "800"; "900"; "1000"] *)


let do_batches alg_attrs args =
  (** runs an alg on all problems *)
  List.iter (fun batch_attrs ->
	       List.iter (fun prob_attrs ->
			    let prob_path = Rdb.path_for data_root prob_attrs
			    and attrs = alg_attrs @ prob_attrs in
			      do_run args prob_path attrs)
	       (Rdb.matching_attrs data_root
		  (Rdb.merge_attrs ["type", "instance"] batch_attrs)))
    batch_tuples


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
   15.;*)
   10.;
   7.;
   5.;
   4.;
   3.;
   2.5; 2.;
   1.75; 1.5; 1.3; 1.2; 1.15; 1.1; 1.05; 1.01; 1.001; 1.0005; 1.0]


let do_wted_batches alg =
  List.iter (fun w ->
	     do_wted_batch alg w)
    weights



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
      (* r 0.0001;
      r 0.0005; *)
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


(* EOF *)
