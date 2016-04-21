(* $Id: runs.ml,v 1.1 2006/06/30 19:42:43 ruml Exp ruml $
   doing runs
*)

open Tsp
open Printf
open Fn

let data_root = Experiments.data_root ^ "tsp"
and instance_root = Experiments.instance_root ^ "tsp_instances"
and solver_binary = ref (Experiments.bin_root ^ "tsp_solver")

let generate_prob symm m size =
  (** Creates new problems for the travelling salesman domain.
      [symm] Whether or not the problem is symmetric.  In symmetric problems,
             traveling from a to b is as expensive as traveling from b to a.
      [m]    The model, uniform and pearl and kim hard are acceptable
      [size] The number of cities *)
  (fun () ->
     if m = "pkhard" then
       Tsp_instances.p_and_k ~symm:symm size
     else
       Tsp_instances.uniform_usquare size)


let if_not_exists symm attrs i f =
  (** Checks to see if a problem with the given arguments exists, and generates
      one if it does not.
      [symm] Is the problem symmetric
      [attrs] Problem attribute list
      [i] The instance number
      [f] Generates the problem when given a unit argument *)
  let path = Rdb.path_for instance_root attrs in
    if Sys.file_exists path then
      Wrutils.pr "%s exists, skipping...\n%!" path
    else
      (Wrutils.pr "Saving to %s\n%!" path;
       let p = f () in
	 Wrio.with_outfile path
	   (fun ch -> Tsp_instances.write p symm ch);
	 Wrutils.pr "Wrote problem %d.\n%!" i)


let make_instances symm model size num =
  (** Generates a set of problems with the given parameters.
      [symm] is the problem symmetric
      [model] uniform or pkhard
      [size] Number of cities to tour in the problem
      [num] The number of problems to create *)
  for i = 1 to num do
    let attrs = [ (*"type", "instance";*)
		  "model", model;
		  "symmetric", string_of_bool symm;
		  "size", string_of_int size;
		  "num", string_of_int i; ] in
      if_not_exists symm attrs i (generate_prob symm model size)
  done


let instance_specs =
(* will probably need to run with cutoffs somewhere around the two minute
   mark. *)
  [ (*1, 12, 40;*)

    1, 100, 40;
    1, 100, 40;

    (*1, 500, 40;*) (* too big. *) ]


let batch_tuples =
  [ "model" , "pkhard" ;
    "model" , "usquare" ]


let make_all_instances () =
  (** Generates a large set of basic instances by folding the [make_instances]
      function over [batch_tuples] and [instance_specs] *)
  List.iter2 (fun (m,v) (symm, size, num) ->
		if symm == 1 then
		  make_instances true v size num
		else
		  make_instances false v size num)
    batch_tuples instance_specs



(************** running experiments **************)

(** calls the external solver on [args] and [board_name],
    redirecting its output to [outch].  all logging is done within
    here.  returns cost, time pair *)
let call_solver time_limit node_limit args prob_path attrs outch =
  if Machine_usage.is_idle ()
  then
    (let limit_args =
       match time_limit with
	 | Some(t) -> Wrutils.str "--time %f %s" t args
	 | _ -> args in
     let limit_args =
       match node_limit with
	 | Some(t) -> Wrutils.str "--exp %i %s" t args
	 | _ -> limit_args in
     let go () =
       Wrsys.with_subprocess_all_pipes
	 (Wrutils.str "%s --memory max %s < %s"
	    !solver_binary limit_args prob_path)
	 (fun to_solver from_solver err_solver ->
	    let buf = Buffer.create 16 in
	      (try
		 while true do
		   Buffer.add_channel buf err_solver 1
		 done
	       with End_of_file -> ());
	      let errors = (Buffer.contents buf) in
		if((String.length errors) = 0)
		then ()
		else
		  Verb.pe Verb.always
		    "\nErrors: ***start here***\n%s\n***end here***\n"
		    (Buffer.contents buf);
		Datafile.write_header_pairs outch;
		Datafile.write_pairs outch attrs;
		Datafile.write_pairs outch
		  ["attrs", (Wrstr.encode_pairs attrs)];
		let res = Datafile.pipe_data from_solver outch in
		  Datafile.write_trailer_pairs outch;
		  res) in
       Notify.try_and_email_on_fail go "Tsp_runs")
  else failwith "Machine is in use"


let do_run overwrite time_limit node_limit args prob_path attrs =
  (** sets up run file and calls solver on problem *)
  let limits = [(match time_limit with None -> Limit.Never
		   | Some t -> Limit.Time t);
		(match node_limit with None -> Limit.Never
		   | Some n -> Limit.Generated n)] in
  let run_file = Rdb.path_for data_root
    (Rdb.filter_attrs ["type"] attrs) in
    if (Sys.file_exists run_file) &&
      (Datafile.seems_complete run_file) &&
      not ((Limit.has_new_limits run_file limits) &&
	     not (Datafile.run_finished run_file)) &&
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


let b_attrs =
  (** Simple set of batch attributes to be used by the do_batches call *)
  [
(*
    ["model", "usquare";
     "size", "500";];
*)
    ["model", "pkhard";
     "size", "100";];
    ["model", "usquare";
     "size", "100";];
(*
    ["model", "pkhard";
     "size", "20"];
    ["model", "pkhard";
     "size", "25"];
    ["model", "pkhard";
     "size", "40"];
    ["model", "usquare";
     "size", "25";];
    ["model", "usquare";
     "size", "30";];
    ["model", "usquare";
     "size", "40";]
*)
]


let do_batches overwrite time_limit node_limit alg_attrs args =
  (** runs an alg on all problems *)
  List.iter
    (fun (batch_attrs) ->
       List.iter (fun prob_attrs ->
		    let prob_path =
		      Rdb.path_for instance_root prob_attrs
		    and attrs = alg_attrs @ prob_attrs in
		      do_run overwrite time_limit node_limit
			args prob_path attrs)
	 (Rdb.matching_attrs instance_root batch_attrs))
    b_attrs


let do_basic_batch ?(time_limit = (Some Experiments.default_time_limit))
    ?(node_limit = (Some Experiments.default_node_limit))
    ?(overwrite = false)
    ?(heuristic = "spanning_tree")
    alg =
  (** Runs algorithms that take no arguments (a_star, greedy, speedy) on
      all of the probelms *)
  Notify.start_batchtime();
  do_batches overwrite time_limit node_limit
    ["alg", alg; "heuristic", heuristic]
    (Wrutils.str "--heuristic %s %s" heuristic alg);
  Notify.send_batch_completed_mail "Tsp_runs" "Basic" alg



let do_wted_batch overwrite time_limit node_limit heuristic alg wt =
  (** Runs algorithms that take a single argument (wted_astar) on all of the
      problems *)
  do_batches overwrite time_limit node_limit
    ["alg", alg;
     "heuristic", heuristic;
     "wt", string_of_float wt;]
    (Wrutils.str "--heuristic %s %s %f" heuristic alg wt)


let do_wted_batches ?(time_limit = (Some Experiments.default_time_limit))
    ?(node_limit = (Some Experiments.default_node_limit))
    ?(weights = Experiments.low_res_weights) ?(overwrite = false)
    ?(heuristic = "spanning_tree") alg =
  (** Calls [do wted_batch] on the standard set of weights *)
  Notify.start_batchtime();
  List.iter (fun w ->
	       do_wted_batch overwrite time_limit node_limit heuristic alg w)
    (List.filter (fun n -> n >= 1.2) weights);
  Notify.send_batch_completed_mail "Tsp_runs" "Weighted" alg



let do_beam_batch overwrite time_limit node_limit heuristic alg beam_width =
  (** Runs algorithms that take a single int argument (beam search) on
      all of the problems *)
  do_batches overwrite time_limit node_limit
    ["alg", alg;
     "heuristic", heuristic;
     "beam_width", string_of_int beam_width;]
    (Wrutils.str "--heuristic %s %s %d" heuristic alg beam_width)


let do_beam_batches ?(overwrite = false)
    ?(time_limit = (Some Experiments.default_time_limit))
    ?(node_limit = (Some Experiments.default_node_limit))
    ?(beam_widths = Experiments.full_beams)
    ?(heuristic = "spanning_tree") alg =
  (** Calls [do wted_batch] on the standard set of weights *)
  Notify.start_batchtime();
  List.iter (fun w ->
	       do_beam_batch overwrite time_limit node_limit heuristic alg w)
    beam_widths;
  Notify.send_batch_completed_mail "Tsp_runs" "Beam" alg


let do_new_beam_batch overwrite time_limit node_limit heuristic alg beam_width
    sort_predicate =
  (** Runs algorithms that take a single int argument (beam search) on
      all of the problems *)
  do_batches overwrite time_limit node_limit
    ["alg", alg;
     "heuristic", heuristic;
     "beam_width", string_of_int beam_width;
     "sort_predicate",sort_predicate]
    (Wrutils.str "--heuristic %s %s %d %s" heuristic alg
       beam_width sort_predicate)


let do_new_beam_batches ?(overwrite = false) ?(time_limit = (Some Experiments.default_time_limit))
    ?(node_limit = (Some Experiments.default_node_limit))
    ?(beam_widths = Experiments.full_beams)
    ?(sort_predicate = "f") ?(heuristic = "spanning_tree") alg =
  (** Calls [do wted_batch] on the standard set of weights *)
  Notify.start_batchtime();
  List.iter (fun w ->
	       do_new_beam_batch overwrite time_limit node_limit heuristic alg w
		 sort_predicate) beam_widths;
  Notify.send_batch_completed_mail "Tsp_runs" "Beam" alg


let do_msc_k_wted_batches ?(overwrite = false)
    ?(time_limit = Some (Experiments.default_time_limit))
    ?(node_limit = Some (Experiments.default_node_limit))
    ?(c_size = Experiments.full_beams) ?(ks = Experiments.ks)
    ?(weights = Experiments.low_res_weights)
    ?(heuristic = "spanning_tree") alg =
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
		   (Wrutils.str "--heuristic %s %s %f %i %i"
		      heuristic alg wt k c))
	      weights)
	 ks) c_size;
  Notify.send_batch_completed_mail "Tsp_runs" "MSC_K_WTED" alg


let do_optimistic_batch overwrite time_limit node_limit heuristic alg wt opt =
  (** Runs algorithms that take two arguments (optimistic search) against all
      of the problems *)
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
    ?(heuristic = "spanning_tree") alg =
  (** Calls [do_optimistic_batch] across all optimisms and a restricted set of
      weights (a subset of the full weights) *)
  Notify.start_batchtime();
  List.iter
    (fun opt ->
       List.iter (fun w -> do_optimistic_batch overwrite time_limit node_limit
		    heuristic alg w opt)
	 weights) opt;
  Notify.send_batch_completed_mail "Tsp_runs" "Optimistic" alg


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


let do_bugsy_batch ?(overwrite = true) time_limit node_limit alg =
  List.iter (fun (cost, time) ->
	       let attrs = ["alg", alg;
			    "cost_coeff", string_of_float cost;
			    "time_coeff", string_of_float time; ]
	       and args = Wrutils.str "%s %f %f" alg cost time in
		 do_batches overwrite time_limit node_limit attrs args)
    utility_tuples


(** Runs a bounded-depth algorithm on some problem batches. *)
let do_batches_bd ?(choose=constantly1 true) overwrite alg_attrs args =
  solver_binary := Experiments.bin_root ^ "tsp_bounded_depth_solver";
  List.iter
    (fun (batch_attrs, time_limit) ->
       List.iter (fun prob_attrs ->
		    if choose prob_attrs then begin
		      let prob_path = Rdb.path_for instance_root prob_attrs in
		      let attrs = alg_attrs @ prob_attrs in
			do_run overwrite time_limit None args prob_path attrs
		    end)
	 (Rdb.matching_attrs instance_root batch_attrs))
    [
(*
      ["model", "usquare"; "size", "500";], Some 300.;
*)
      ["model", "usquare"; "size", "50";], Some 30.;
      ["model", "pkhard"; "size", "100";], Some 30.;
      ["model", "usquare"; "size", "100";], Some 30.;
    ]

(*
  do_batches_bd overwrite
    ["alg", "indecision_cr2_bottom"; "max-bins", "100"; ]
  "indecision_cr2_bottom 100";
  do_batches_bd overwrite
    ["alg", "indecision_cr_bottom"; "max-bins", "100"; ]
  "indecision_cr_bottom 100";
  do_batches_bd overwrite
    ["alg", "wds_bottom"; "max-bins", "100"; ]
   "wds_bottom 100";;

  do_batches_bd overwrite
    ["alg", "indecision_sum_bottom"; "norm", "false"; "max-bins", "2000"; ]
  "indecision_sum_bottom 2000";
  do_batches_bd overwrite
    ["alg", "indecision_sum_top"; "norm", "false"; "max-bins", "2000"; ]
  "indecision_sum_top 2000";;

  do_batches_bd overwrite ["alg", "dfs"; ] "dfs";
  do_batches_bd overwrite ["alg", "dds"; ] "dds";
  do_batches_bd overwrite ["alg", "ilds_bottom"; ] "ilds_bottom";
  do_batches_bd overwrite ["alg", "ilds_top"; ] "ilds_top";;

  do_batches_bd overwrite
  ["alg", "blfs_poly"; "learner", "nlms_flow_2"; "learning rate", "0.01";
  "degree", "2"; ] "blfs_poly nlms_flow_2 0.01 2";
  do_batches_bd overwrite
  ["alg", "blfs_poly"; "learner", "nlms_flow_2"; "learning rate", "0.05";
  "degree", "2"; ] "blfs_poly nlms_flow_2 0.05 2";;

*)


(** Trains on instances on the even numbered instances. *)
let train_windecision overwrite ?(traversal="probe")
    ?(degree=2) ?(nleaves=512) () =
  let train_attrs =
    [ "degree", string_of_int degree;
      "leaves per inst", string_of_int nleaves;
      "traversal", traversal; ]
  in
  let alg_attrs = ("alg", "train_windecision") :: train_attrs in
  let sf_attrs = ("alg", "windecision_samples") :: train_attrs in
  let attrs = sf_attrs in
  let sf = Rdb.path_for data_root attrs in
  let args =
    sprintf "train_windecision %d %d %s %s" degree nleaves traversal sf
  in
  let choose attrs =
    let n = int_of_string (List.assoc "num" attrs) in
      n mod 2 = 0
  in
    do_batches_bd ~choose overwrite alg_attrs args


let windecision_rbfs ?(traversal="probe") ?(degree=2) ?(nleaves=512) () =
  let sample_attrs =
    [ "alg", "windecision_samples";
      "degree", string_of_int degree;
      "leaves per inst", string_of_int nleaves;
      "traversal", traversal;
    ] in
  let sf = Rdb.path_for data_root sample_attrs in
  let coeffs = Offline_windecision.learn_coeffs sf in
  let coeff_string = Offline_windecision.string_of_coeffs coeffs in
  let args = sprintf "windecision_rbfs \"%s\"" coeff_string in
  let alg_attrs = [ "alg", "windecision_rbfs";
		    "coeffs", coeff_string;
		    "degree", string_of_int degree;
		    "leaves per inst", string_of_int nleaves;
		    "traversal", traversal; ] in
    Verb.pr Verb.debug "args: [%s]\n" args;
    do_batches_bd false alg_attrs args


let windecision_sum_bottom ?(traversal="probe") ?(degree=2) ?(nleaves=512)
    bins =
  let sample_attrs =
    [ "alg", "windecision_samples";
      "degree", string_of_int degree;
      "leaves per inst", string_of_int nleaves;
      "traversal", traversal;
    ] in
  let sf = Rdb.path_for data_root sample_attrs in
  let coeffs = Offline_windecision.learn_coeffs sf in
  let coeff_string = Offline_windecision.string_of_coeffs coeffs in
  let args = sprintf "windecision_sum_bottom \"%s\" %d" coeff_string bins in
  let alg_attrs = [ "alg", "windecision_sum_bottom";
		    "coeffs", coeff_string;
		    "degree", string_of_int degree;
		    "leaves per inst", string_of_int nleaves;
		    "traversal", traversal;
		    "max-bins", string_of_int bins; ] in
    Verb.pr Verb.debug "args: [%s]\n" args;
    do_batches_bd false alg_attrs args


let windecision_sum_top ?(traversal="probe") ?(degree=2) ?(nleaves=512)
    bins =
  let sample_attrs =
    [ "alg", "windecision_samples";
      "degree", string_of_int degree;
      "leaves per inst", string_of_int nleaves;
      "traversal", traversal;
    ] in
  let sf = Rdb.path_for data_root sample_attrs in
  let coeffs = Offline_windecision.learn_coeffs sf in
  let coeff_string = Offline_windecision.string_of_coeffs coeffs in
  let args = sprintf "windecision_sum_top \"%s\" %d" coeff_string bins in
  let alg_attrs = [ "alg", "windecision_sum_top";
		    "coeffs", coeff_string;
		    "degree", string_of_int degree;
		    "leaves per inst", string_of_int nleaves;
		    "traversal", traversal;
		    "max-bins", string_of_int bins; ] in
    Verb.pr Verb.debug "args: [%s]\n" args;
    do_batches_bd false alg_attrs args

(* EOF *)
