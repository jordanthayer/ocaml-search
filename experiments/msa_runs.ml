(* $Id: runs.ml,v 1.1 2008/01/19 21:40:40 ruml Exp $

   doing runs
*)


let data_root = Experiments.data_root ^ "grid"
and instance_root = Experiments.instance_root ^ "grid_instances"
and solver_binary = ref (Experiments.bin_root ^ "msa_solver")


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

(************** instances **************)


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


let call_solver time_limit node_limit args prob_path attrs outch =
  (** calls the external solver on [args] and [board_name], redirecting its
      output to [outch].  all logging is done within here.  returns cost, time
      pair *)
  let go () =
    Wrsys.with_subprocess_pipes
      (Wrutils.str "%s %s < %s" !solver_binary args prob_path)
      (fun to_solver from_solver ->
	 Datafile.write_header_pairs outch;
	 Datafile.write_pairs outch attrs;
	 Datafile.write_pairs outch ["attrs", (Wrstr.encode_pairs attrs)];
	 let res = Datafile.pipe_data from_solver outch in
	   Datafile.write_trailer_pairs outch;
	   res) in
    Notify.try_and_email_on_fail go "Msa_runs"


let do_run time_limit node_limit args prob_path attrs =
  (** sets up run file and calls solver on board_name *)
  let run_file = Rdb.path_for data_root
    (Rdb.override_attrs ["type", "run"] attrs) in
    if ((Sys.file_exists run_file) &&
	  (Datafile.seems_complete run_file)) then
      Wrutils.pr "%s exists - skipping!\n%!" run_file
    else
      Wrio.with_outfile run_file
	(fun outch ->
	   Wrutils.pr "Running %s on %s...%!" args (Wrfname.make_relative data_root
						      prob_path);
	   let c,t =
	     call_solver time_limit node_limit args prob_path attrs outch in
	     Wrutils.pr "%.1f in %.3f.\n%!" c t)



let batch_tuples =
  [ ["model", "real" ] ]
  (*List.map (fun n ->
	      [ "model", "morphed";
		"seq_len", n; ])
    ["500"; "600"; "700"; "800"; "900"; "1000"] *)


let do_batches time_limit node_limit alg_attrs args =
  (** runs an alg on all problems *)
  List.iter (fun batch_attrs ->
	       List.iter (fun prob_attrs ->
			    let prob_path = Rdb.path_for data_root prob_attrs
			    and attrs = alg_attrs @ prob_attrs in
			      do_run time_limit node_limit
				args prob_path attrs)
	       (Rdb.matching_attrs data_root
		  (Rdb.merge_attrs ["type", "instance"] batch_attrs)))
    batch_tuples


let do_basic_batch ?(time_limit = (Some Experiments.default_time_limit))
    ?(node_limit = (Some Experiments.default_node_limit))
    ?(heuristic = "foo") alg =
  (* a_star, greedy, speedy *)
  Notify.start_batchtime();
  do_batches time_limit node_limit
    ["alg", alg;
     "heuristic", heuristic]
    (Wrutils.str "--heuristic %s %s" heuristic alg);
  Notify.send_batch_completed_mail "Msa_runs" "Basic" alg


let do_wted_batch time_limit node_limit heuristic alg wt =
  do_batches time_limit node_limit ["alg", alg;
				    "heuristic", heuristic;
				    "wt", string_of_float wt;]
    (Wrutils.str "--heuristic %s %s %f" heuristic alg wt)


let do_optimistic_batch time_limit node_limit heuristic alg wt opt =
  assert (opt >= 1.);
  do_batches time_limit node_limit
    ["alg", alg;
     "heuristic", heuristic;
     "wt", string_of_float wt;
     "optimism", string_of_float opt]
    (Wrutils.str "--heuristic %s %s %f %f" heuristic alg wt opt)


let do_optimistic_batches ?(time_limit = (Some Experiments.default_time_limit))
    ?(node_limit = (Some Experiments.default_node_limit))
    ?(weights = Experiments.low_res_weights)
    ?(opt = Experiments.optimisms)
    ?(heuristic = "foo") alg =
  Notify.start_batchtime();
  List.iter
    (fun opt ->
       List.iter (fun w -> do_optimistic_batch time_limit node_limit
		    heuristic alg w opt)
	 weights) opt;
  Notify.send_batch_completed_mail "Msa_runs" "Optimistic" alg


let do_wted_batches ?(time_limit = (Some Experiments.default_time_limit))
    ?(node_limit = (Some Experiments.default_node_limit))
    ?(weights = Experiments.low_res_weights)
    ?(heuristic = "foo")  alg =
  Notify.start_batchtime();
  List.iter (fun w ->
	     do_wted_batch time_limit node_limit heuristic alg w) weights;
  Notify.send_batch_completed_mail "Msa_runs" "Weighted" alg


let do_beam_batch time_limit node_limit heuristic alg beam_width =
  do_batches time_limit node_limit ["alg", alg;
				    "heuristic", heuristic;
				    "beam_width", string_of_int beam_width;]
    (Wrutils.str "--heuristic %s %s %d" heuristic alg beam_width)


let do_beam_batches ?(overwrite = false)
    ?(time_limit = (Some Experiments.default_time_limit))
    ?(node_limit = (Some Experiments.default_node_limit))
    ?(beam_widths = Experiments.full_beams) ?(heuristic = "foo") alg =
  Notify.start_batchtime();
  List.iter (fun w ->
	       do_beam_batch time_limit node_limit heuristic alg w) beam_widths;
  Notify.send_batch_completed_mail "Msa_runs" "Beam" alg


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


let do_bugsy_batch time_limit node_limit alg =
  List.iter (fun (cost, time) ->
	       let attrs = ["alg", alg;
			    "cost_coeff", string_of_float cost;
			    "time_coeff", string_of_float time; ]
	       and args = Wrutils.str "%s %f %f" alg cost time in
		 do_batches time_limit node_limit attrs args)
    utility_tuples


(* EOF *)
