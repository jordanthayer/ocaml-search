(* $Id: runs.ml,v 1.1 2006/06/30 22:41:52 ruml Exp $

   doing runs
*)


(************** instances **************)

let data_root = Experiments.data_root ^ "dyn_robot"
and instance_root = Experiments.instance_root ^ "dyn_robot_instances"
and solver_binary = Experiments.bin_root ^ "dyn_robot_solver"

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
			       (Drn_instance.make_liney 0.67) x y (float l))
	   Dynamic.feasible_p)
  done


let make_uniform_instances x y p n =
  for i = 1 to n do
    let attrs = [ "type", "instance";
		  "obstacles", "uniform";
		  "width", string_of_int x;
		  "height", string_of_int y;
		  "density", string_of_float p;
		  "num", string_of_int i; ] in
      if_not_exists attrs i
	(fun () ->
	   Wrutils.eval_until (fun () ->
			       Drn_instance.make_instance
			       Drn_instance.make_uniform x y p)
	   Dynamic.feasible_p)
  done


let make_all_instances () =
  Random.self_init ();
  make_liney_instances 250 250 20 20


(************** running experiments **************)


let call_solver time_limit node_limit args prob_path attrs outch =
  (** calls the external solver on [args] and [board_name], redirecting its
      output to [outch].  all logging is done within here.  returns cost, time
      pair *)
  let limit_args =
    match time_limit with
	Some(t) ->  Wrutils.str "--time %f %s" t args
      | _ -> args in
  let go () =
    Wrsys.with_subprocess_pipes
      (Wrutils.str "%s %s < %s" solver_binary limit_args prob_path)
      (fun to_solver from_solver ->
	 Datafile.write_header_pairs outch;
	 Datafile.write_pairs outch attrs;
	 Datafile.write_pairs outch ["attrs", (Wrstr.encode_pairs attrs)];
	 let res = Datafile.pipe_data from_solver outch in
	   Datafile.write_trailer_pairs outch;
	   res) in
    Notify.try_and_email_on_fail go "Drn_runs"


let valid_df time_limit node_limit df =
  try
    (Datafile.get_val df "found solution") <> "no"
  with _ -> true


let do_run overwrite time_limit node_limit args prob_path attrs =
  (** sets up run file and calls solver on board_name *)
  let limits = [(match time_limit with None -> Limit.Never
		   | Some t -> Limit.Time t);
		(match node_limit with None -> Limit.Never
		   | Some n -> Limit.Generated n)] in
  let run_file = Rdb.path_for data_root
    (Rdb.override_attrs ["type", "run"] attrs) in
    if (Sys.file_exists run_file) &&
      (Datafile.seems_complete run_file) &&
      not ((Limit.has_new_limits run_file limits) &&
	           not (Datafile.run_finished run_file)) &&
      not (overwrite)
    then
      Wrutils.pr "%s exists - skipping!\n%!" run_file
    else
      Wrio.with_outfile run_file
	(fun outch ->
	   Wrutils.pr "Running %s on %s...%!" args (Wrfname.make_relative data_root
						    prob_path);
	   let c,t =
	     call_solver time_limit node_limit args prob_path attrs outch in
	     Wrutils.pr "%.1f in %.3f.\n%!" c t)



let batch_tuples = [(*["obstacles", "liney"; "num_lines", "25"; "width", "200";];
		    ["obstacles", "liney"; "num_lines", "25"; "width", "300";];
		    ["obstacles", "liney"; "num_lines", "25"; "width", "400";];
		    ["obstacles", "liney"; "num_lines", "25"; "width", "500";];
		    [ "obstacles", "uniform"]*)
		    ["obstacles", "liney"; "num_lines", "75"; "width", "500";];
		   ]


let do_batches overwrite time_limit node_limit alg_attrs args =
  (** runs an alg on all problems *)
  List.iter (fun batch_attrs ->
	       List.iter
		 (fun prob_attrs ->
		    let prob_path = Rdb.path_for instance_root prob_attrs
		    and attrs = alg_attrs @ prob_attrs in
		      do_run overwrite time_limit node_limit args
			prob_path attrs)
		 (Rdb.matching_attrs instance_root
		    (Rdb.merge_attrs ["type", "instance"] batch_attrs)))
    batch_tuples


let deadline_contract_batch ?(overwrite = false) alg_name =
  Notify.start_batchtime();
  let deadlines =
    [60.; 30.; 15.; 7.5; 3.75; 1.875; 0.9375; 0.46875; 0.234375; 0.1171875;
     0.05859375; 0.029296875; 0.0146484375; 0.00732421875;] in
  let w_res = List.map (fun a -> a, 1000) deadlines in
    List.iter
      (fun (deadline,res) ->
	 do_batches overwrite (Some deadline) None
	   ["alg", alg_name;
	    "deadline", string_of_float deadline;
	    "res", string_of_int res;]
	   (Printf.sprintf "%s %f %i" alg_name deadline res))
      (List.rev w_res)


let do_basic_batch ?(time_limit = (Some Experiments.default_time_limit))
    ?(node_limit = (Some Experiments.default_node_limit)) ?(overwrite = false)
    ?(heuristic = "static") alg =
  (* a_star, greedy, speedy *)
  Notify.start_batchtime();
  do_batches overwrite time_limit node_limit
    ["alg", alg;
     "heuristic", heuristic;] alg;
  Notify.send_batch_completed_mail "Drn_runs" "Basic"
    (Wrutils.str "--heuristic %s %s" heuristic alg)


let do_wted_batch overwrite time_limit node_limit heuristic alg wt =
  (* wted_a_star, anytime_a_star *)
  do_batches overwrite time_limit node_limit
    ["alg", alg;
     "heuristic", heuristic;
     "wt", string_of_float wt;]
    (Wrutils.str "--heuristic %s %s %f" heuristic alg wt)


let do_wted_batches ?(time_limit = (Some Experiments.default_time_limit))
    ?(node_limit = (Some Experiments.default_node_limit))
    ?(weights = Experiments.low_res_weights) ?(overwrite = false)
    ?(heuristic = "static")  alg =
  Notify.start_batchtime();
  List.iter (do_wted_batch overwrite time_limit node_limit heuristic alg)
    weights;
  Notify.send_batch_completed_mail "Drn_runs" "Weighted" alg


let do_cab_batch overwrite time_limit node_limit alg sort_predicate =
  (* wted_a_star, anytime_a_star *)
  do_batches overwrite time_limit node_limit ["alg", alg;
					      "sort_predicate", sort_predicate;]
    (Wrutils.str "%s %s" alg sort_predicate)


let do_cab_batches ?(time_limit = (Some Experiments.default_time_limit))
    ?(node_limit = (Some Experiments.default_node_limit))
    ?(overwrite = false) ?(heuristic = "static") alg sort_predicate=
  Notify.start_batchtime();
  List.iter (do_cab_batch overwrite time_limit node_limit alg) [sort_predicate];
  Notify.send_batch_completed_mail "Drn_runs" "cab" alg


let do_restarting_beam_batches ?(heuristic = "static") ?(overwrite = false)
    ?(time_limit = Some (Experiments.default_time_limit))
    ?(node_limit = Some (Experiments.default_node_limit))
    ?(sort_predicate = "f")
    ?(beam_widths = Experiments.full_beams)
    ?(restart_pred = ["f";"h";"i_max";"i_min";"i_all";])
    ?(heuristic = "static")
    alg =

  Notify.start_batchtime();
  List.iter
    (fun bw ->
       List.iter
	 (fun rp ->
	    do_batches overwrite time_limit node_limit
	      ["alg", alg;
	       "heuristic", heuristic;
	       "beam_width",string_of_int bw;
	       "sort_predicate",sort_predicate;
	       "backup_sort_predicate",rp;]
	      (Wrutils.str "%s %d %s %s" alg bw sort_predicate rp)
	 ) restart_pred) beam_widths;
  Notify.send_batch_completed_mail "Drn_runs" "restarting_beam" alg



let do_msc_k_wted_batches ?(overwrite = false)
    ?(time_limit = Some (Experiments.default_time_limit))
    ?(node_limit = Some (Experiments.default_node_limit))
    ?(c_size = Experiments.full_beams) ?(ks = Experiments.ks)
    ?(weights = Experiments.low_res_weights) ?(heuristic = "static") alg =
  Notify.start_batchtime();
  List.iter
    (fun c ->
       List.iter
	 (fun k ->
	    List.iter
	      (fun wt ->
		 do_batches overwrite time_limit node_limit
		   ["alg", alg;
		    "heuristic", heuristic;
		    "commit", string_of_int c;
		    "k", string_of_int k;
		    "wt", string_of_float wt;]
		   (Wrutils.str "%s %f %i %i" alg wt k c)) weights) ks) c_size;
  Notify.send_batch_completed_mail "Drn_runs" "MSC_K_WTED" alg


let do_beam_batch overwrite time_limit node_limit alg beam_width =
  (* beam *)
  do_batches overwrite time_limit node_limit ["alg", alg;
	      "beam_width", string_of_int beam_width;]
    (Wrutils.str "%s %d" alg beam_width)


let do_beam_batches ?(overwrite = false) ?(time_limit = (Some Experiments.default_time_limit))
    ?(node_limit = (Some Experiments.default_node_limit))
    ?(beam_widths = Experiments.full_beams)
    ?(heuristic = "static") alg =
  Notify.start_batchtime();
  List.iter (do_beam_batch overwrite time_limit node_limit alg) beam_widths;
  Notify.send_batch_completed_mail "Drn_runs" "Beam" alg


let do_optimistic_batch overwrite time_limit node_limit heuristic alg wt opt =
  assert (opt >= 1.);
  do_batches overwrite time_limit node_limit
    ["alg", alg;
     "heuristic", heuristic;
     "wt", string_of_float wt;
     "optimism", string_of_float opt]
    (Wrutils.str "--heuristic %s %s %f %f" heuristic alg wt opt)


let do_optimistic_batches ?(time_limit = (Some Experiments.default_time_limit))
    ?(node_limit = (Some Experiments.default_node_limit))
    ?(weights = Experiments.low_res_weights)
    ?(opt = Experiments.optimisms) ?(overwrite = false)
    ?(heuristic = "static") alg =
  Notify.start_batchtime();
  List.iter
    (fun opt ->
       List.iter (fun w -> do_optimistic_batch overwrite time_limit node_limit
		    heuristic alg w opt) weights) opt;
  Notify.send_batch_completed_mail "Drn_runs" "Optimistic" alg


let do_bulb_batch overwrite time_limit node_limit alg beam_width =
  (** bulb *)
  let node_limit_number = match node_limit with
      None -> 10000000
    | Some n -> n in
    do_batches overwrite time_limit node_limit

      ["alg", alg;
       "beam_width", string_of_int beam_width;
       "node_capacity", string_of_int node_limit_number;]
      (Wrutils.str "%s %d %d" alg beam_width node_limit_number)


let do_new_beam_batches ?(heuristic = "static") ?(overwrite = false)
    ?(time_limit = Some (Experiments.default_time_limit))
    ?(node_limit = Some (Experiments.default_node_limit))
    ?(sort_predicate = "f")
    ?(beam_widths = Experiments.full_beams) alg =
  Notify.start_batchtime();
  List.iter
    (fun bw ->
       do_batches overwrite time_limit node_limit
	 ["alg", alg;
	  "beam_width",string_of_int bw;
	  "sort_predicate",sort_predicate;]
	 (Wrutils.str "%s %d %s" alg bw sort_predicate)
    ) beam_widths;
  Notify.send_batch_completed_mail "Grid_runs" "restarting_beam" alg


let do_bulb_batches ?(overwrite = false)
    ?(time_limit = Some (Experiments.default_time_limit))
    ?(node_limit = Some (Experiments.default_node_limit))
    ?(beam_widths = Experiments.full_beams) alg =
  Notify.start_batchtime();
  List.iter (fun w ->
	       do_bulb_batch overwrite time_limit node_limit alg w)
    beam_widths;
  Notify.send_batch_completed_mail "Drn_runs" "Bulb" alg


let do_ib_batch overwrite time_limit node_limit alg beam_width backup_predicate=
  (** ib *)
    do_batches overwrite time_limit node_limit
      ["alg", alg;
       "backup_predicate", backup_predicate;
       "beam_width", string_of_int beam_width;
      ]
      (Wrutils.str "%s %d %s" alg beam_width backup_predicate)

let do_ib_batches ?(overwrite = false)
    ?(time_limit = Some (Experiments.default_time_limit))
    ?(node_limit = Some (Experiments.default_node_limit))
    ?(beam_widths = Experiments.full_beams) alg backup_pred=
  Notify.start_batchtime();
  List.iter (fun w ->
	       do_ib_batch overwrite time_limit node_limit alg w backup_pred)
    beam_widths;
  Notify.send_batch_completed_mail "Drn_runs" "ib" alg



let do_ara_star_batch ?(overwrite = false) time_limit node_limit =
  (* Likhachev et al start at 2.5 and don't say what they decrement by *)
  let alg = "ara_star"
  and start = 3.
  and decr = 0.2
  and final = 1. in
    do_batches overwrite time_limit node_limit
      ["alg", alg;
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


let do_bugsy_batch ?(overwrite = false) time_limit node_limit =
  let alg = "bugsy" in
    List.iter (fun (cost, time) ->
		 let attrs = ["alg", alg;
			      "cost_coeff", string_of_float cost;
			      "time_coeff", string_of_float time; ]
		 and args = Wrutils.str "%s %f %f" alg cost time in
		   do_batches overwrite time_limit node_limit attrs args)
      utility_tuples


(*** I'm really sorry - JTD7 **********)

let model_on_instance alg =
  let base_instance_attrs = ["obstacles", "liney";
			     "width", "500";
			     "height", "500";
			     "num_lines", "75";
			     "line_length", "0.1";
			     "heuristic", "static";
			     "alg", "greedier_adapt";] in
    for i = 1 to 40 do
      (let iat = ("num", string_of_int i)::base_instance_attrs in
       let old_run = (Dataset.load_from_rdb_with_domain ~domain:"dyn_robot" iat
			~name:"oldrun") in
       let herr = (Dataset.get_values float_of_string
		     ~sort:Dataset.Last "h_step" old_run).(0)
       and derr = (Dataset.get_values float_of_string
		     ~sort:Dataset.Last "d_step" old_run).(0) in
	 do_run false (Some 300.) None
	   (Wrutils.str " -- %s %f %f"
	      alg herr derr)
	   (Wrutils.str
	      "./group/data/dyn_robot_instances/instance/liney/500/500/75/0.1/%i" i)
	   ["type", "run";
	    "obstacles", "liney";
	    "width", "500";
	    "height", "500";
	    "num_lines", "75";
	    "line_length", "0.1";
	    "heuristic", "static";
	    "num", string_of_int i;
	    "alg", (Wrutils.str "%s_%s" alg "this_instance")])
    done


let model_on_next_instance alg =
  let base_instance_attrs = ["obstacles", "liney";
			     "width", "500";
			     "height", "500";
			     "num_lines", "75";
			     "line_length", "0.1";
			     "heuristic", "static";
			     "alg", "greedier_adapt";] in
    for i = 1 to 40 do
      let ni = (i mod 40) + 1 in
      (let iat = ("num", string_of_int ni)::base_instance_attrs in
       let old_run = (Dataset.load_from_rdb_with_domain ~domain:"dyn_robot" iat
			~name:"oldrun") in
       let herr = (Dataset.get_values float_of_string
		     ~sort:Dataset.Last "h_step" old_run).(0)
       and derr = (Dataset.get_values float_of_string
		     ~sort:Dataset.Last "d_step" old_run).(0) in
	 do_run false (Some 300.) None
	   (Wrutils.str " -- %s %f %f"
	      alg herr derr)
	   (Wrutils.str
	      "./group/data/dyn_robot_instances/instance/liney/500/500/75/0.1/%i" i)
	   ["type", "run";
	    "obstacles", "liney";
	    "width", "500";
	    "height", "500";
	    "num_lines", "75";
	    "line_length", "0.1";
	    "heuristic", "static";
	    "num", string_of_int i;
	    "alg", (Wrutils.str "%s_%s" alg "next_instance")])
    done
(* EOF *)
