(** Meant for creating and running rucksack problems *)

let data_root = Experiments.data_root ^ "rucksack"
and instance_root = Experiments.instance_root ^ "rucksack"
and solver_binary = ref (Experiments.bin_root ^ "rucksack_solver")

let rucksack_size = [0.4; 0.5; 0.6] (* bag is total * x in size *)
and models = ["random_fixed_total"; "random_max_val"]
and items = [200;100;50;10]
and weights = [200.]
and values = [200.]


let make_instances count =
  for i = 0 to (count - 1)
  do
    List.iter
      (fun items ->
	 List.iter
	   (fun rss ->
	      List.iter
		(fun model ->
		   List.iter
		     (fun weight ->
			List.iter
			  (fun value ->
			     let t =
			       match model with
				 | "random_fixed_total" ->
				     Zero_one_graph.make_random_fixed
				       ~seed:(Random.int (Math.int_exp 2 25))
				       value weight items rss
				 | "random_max_val" ->
				     Zero_one_graph.make_random_max
				       ~seed:(Random.int (Math.int_exp 2 25))
				       value weight items rss
				 | _ -> failwith "unrecognized model"
			     in
			     let path = Rdb.path_for instance_root
			       (["type", "instance";
				 "model", model;
				 "value", string_of_float value;
				 "weight", string_of_float weight;
				 "sack size", string_of_float rss;
				 "item count", string_of_int items;
				 "num", string_of_int i]) in
			       if Sys.file_exists path &&
				 Datafile.seems_complete path
			       then Wrutils.pr
				 "%s exists, skipping...\n%!" path
			       else Wrio.with_outfile path
				 (fun ch -> Zero_one_graph.write_instance t ch))
			  values)
		     weights)
		models)
	   rucksack_size)
      items
  done


let call_solver time_limit node_limit args prob_path attrs outch =
  (** calls the external solver on [args] and [board_name], redirecting its
      output to [outch].  all logging is done within here.  returns cost, time
      pair *)
  if Machine_usage.is_idle () then begin
    let limit_args =
      match time_limit with
	  Some(t) -> Wrutils.str "--time %f %s" t args
	| _ -> args in
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
      Notify.try_and_email_on_fail go "Rucksack_runs"
  end else
    failwith "Machine is in use"


let do_run overwrite time_limit node_limit args prob_path attrs =
  (** sets up run file and calls solver on problem *)
  let limits = [(match time_limit with None -> Limit.Never
		   | Some t -> Limit.Time t);
		(match node_limit with None -> Limit.Never
		   | Some n -> Limit.Generated n)] in
  let run_file = Rdb.path_for data_root
    (Rdb.filter_attrs ["type"] attrs) in
    if (Sys.file_exists run_file) &&
      (Datafile.run_finished run_file) &&
      not (Limit.has_new_limits run_file limits) &&
      not overwrite
    then
      Wrutils.pr "%s exists - skipping!\n%!" run_file
    else
      Wrio.with_outfile run_file
	(fun outch ->
	   Wrutils.pr "Running %s on %s...%!" args (Wrfname.make_relative instance_root
						    prob_path);
	   let c,t =
	     call_solver time_limit node_limit args prob_path attrs outch in
	     Wrutils.pr "%.5f in %.3f.\n%!" c t)


let do_batches overwrite time_limit node_limit alg_attrs args =
  (** runs an alg on all problems *)
  List.iter (fun prob_attrs ->
	       let prob_path =
		 Rdb.path_for instance_root prob_attrs
	       and attrs = alg_attrs @ prob_attrs in
		 do_run overwrite time_limit node_limit args prob_path attrs)
    (Rdb.matching_attrs instance_root [])


let do_basic_batch ?(time_limit = (Some Experiments.default_time_limit))
    ?(node_limit = (Some Experiments.default_node_limit)) ?(overwrite = true)
    alg =
  (** Runs algorithms that take no arguments (a_star, greedy, speedy) on
      all of the probelms *)
  Notify.start_batchtime();
  do_batches overwrite time_limit node_limit ["alg", alg] alg;
  Notify.send_batch_completed_mail "Rucksack_runs" "Basic" alg


let do_wted_batch overwrite time_limit node_limit alg wt =
  (** Runs algorithms that take a single argument (wted_astar) on all of the
      problems *)
  do_batches overwrite time_limit node_limit
    ["alg", alg;
     "wt", string_of_float wt;]
    (Wrutils.str "%s %f" alg wt)


let do_wted_batches ?(time_limit = (Some Experiments.default_time_limit))
    ?(node_limit = (Some Experiments.default_node_limit))
    ?(weights = Experiments.low_res_weights) ?(overwrite = true) alg =
  (** Calls [do wted_batch] on the standard set of weights *)
  Notify.start_batchtime();
  List.iter (fun w ->
	       do_wted_batch overwrite time_limit node_limit alg w) weights;
  Notify.send_batch_completed_mail "Rucksack_runs" "Weighted" alg


let do_beam_batch overwrite time_limit node_limit alg beam_width =
  (** Runs algorithms that take a single int argument (beam search) on
      all of the problems *)
  do_batches overwrite time_limit node_limit
    ["alg", alg;
     "beam_width", string_of_int beam_width;]
    (Wrutils.str "%s %d" alg beam_width)


let do_msc_k_wted_batches ?(overwrite = false)
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
		 do_batches overwrite time_limit node_limit
		   ["alg", alg;
		    "commit", string_of_int c;
		    "k", string_of_int k;
		    "wt", string_of_float wt;]
		   (Wrutils.str "%s %f %i %i" alg wt k c)) weights) ks) c_size;
  Notify.send_batch_completed_mail "Rucksack_runs" "MSC_K_WTED" alg


let do_beam_batches ?(overwrite = false) ?(time_limit = (Some Experiments.default_time_limit))
    ?(node_limit = (Some Experiments.default_node_limit))
    ?(beam_widths = Experiments.full_beams) alg =
  (** Calls [do wted_batch] on the standard set of weights *)
  Notify.start_batchtime();
  List.iter (fun w ->
	       do_beam_batch overwrite time_limit node_limit alg w) beam_widths;
  Notify.send_batch_completed_mail "Rucksack_runs" "Beam" alg


let do_optimistic_batch overwrite time_limit node_limit alg wt opt =
  (** Runs algorithms that take two arguments (optimistic search) against all
      of the problems *)
  assert (opt >= 1.);
  do_batches overwrite time_limit node_limit
    ["alg", alg;
     "wt", string_of_float wt;
     "optimism", string_of_float opt]
    (Wrutils.str "%s %f %f" alg wt opt)


let do_optimistic_batches ?(time_limit = (Some Experiments.default_time_limit))
    ?(node_limit = (Some Experiments.default_node_limit))
    ?(weights = Experiments.low_res_weights)
    ?(opt = Experiments.optimisms) ?(overwrite = true) alg =
  (** Calls [do_optimistic_batch] across all optimisms and a restricted set of
      weights (a subset of the full weights) *)
  Notify.start_batchtime();
  List.iter
    (fun opt ->
       List.iter (fun w -> do_optimistic_batch overwrite time_limit node_limit alg w opt)
	 weights) opt;
  Notify.send_batch_completed_mail "Rucksack_runs" "Optimistic" alg


(** Runs an alg on all problems with the 'max value' model. *)
let do_max_val_batches overwrite time_limit node_limit alg_attrs args =
  List.iter (fun prob_attrs ->
	       let prob_path =
		 Rdb.path_for instance_root prob_attrs
	       and attrs = alg_attrs @ prob_attrs in
		 do_run overwrite time_limit node_limit args prob_path attrs)
    (Rdb.matching_attrs instance_root ["model", "random_max_val"; ])

(* EOF *)
