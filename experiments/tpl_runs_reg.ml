(* $Id: runs.ml,v 1.1 2005/04/06 00:39:05 ruml Exp ruml $
   search algorithms for tplan
*)

let prob_path = Experiments.instance_root ^ "tplan"
and dom_path  = Experiments.instance_root ^ "tplan"
and data_root = Experiments.data_root ^ "tplan"
and solver_binary = Experiments.bin_root ^ "tplan_reg_solver"


let call_planner time_limit node_limit args domain_path prob_path attrs outch =
  (** calls the external solver on [args] and [board_name], redirecting its
    output to [outch].  all logging is done within here.  returns cost, time
    pair *)
  let limit_args = match time_limit with
    | None -> args
    | Some i -> Wrutils.str "--time %f %s" i args in
    Verb.pe Verb.debug "\nCalling Planner\n";
  Wrsys.with_subprocess_pipes
    (Wrutils.str "cat %s %s | %s %s" domain_path prob_path solver_binary limit_args)
    (fun to_solver from_solver ->
       Datafile.write_header_pairs outch;
       Datafile.write_pairs outch attrs;
       Datafile.write_pairs outch ["attrs", (Wrstr.encode_pairs attrs)];
       let res = Datafile.pipe_data from_solver outch in
	 Datafile.write_trailer_pairs outch;
	 flush outch;
	 res)

let do_run overwrite time_limit node_limit args domain_path prob_path attrs =
  (** sets up run file and calls solver on problem *)
  let limits = [(match time_limit with None -> Limit.Never
		   | Some t -> Limit.Time t);
		(match node_limit with None -> Limit.Never
		   | Some n -> Limit.Generated n)] in
  let run_file = Rdb.path_for data_root
    (Rdb.filter_attrs["type"] attrs) in
    if (Sys.file_exists run_file) &&
      (Datafile.seems_complete run_file) &&
      (*(Datafile.run_finished run_file) &&*)
      not (Limit.has_new_limits run_file limits) &&
      not (overwrite)
    then
      Wrutils.pr "%s exists - skipping!\n%!" run_file
    else
      Wrio.with_outfile run_file
	(fun outch ->
	   Wrutils.pr "Running on %s...%!" (Wrfname.make_relative data_root
					    prob_path);
	   let c,t =
	     call_planner time_limit node_limit args domain_path prob_path attrs outch in
	     Wrutils.pr "%.1f in %.3f.\n%!" c t)


let do_batches overwrite time_limit node_limit alg_attrs args =
  (** runs an alg on all problems *)
  List.iter
    (fun domain_attrs ->
       let domain_path = Rdb.path_for dom_path domain_attrs in
	 List.iter
	   (fun prob_attrs ->
	      let prob_path = Rdb.path_for prob_path prob_attrs
	      and attrs = alg_attrs @ prob_attrs in
		do_run overwrite time_limit node_limit args domain_path
		  prob_path attrs)
	   (Rdb.matching_attrs prob_path
	      (Rdb.override_attrs ["type", "instance"] domain_attrs)))
    (Rdb.matching_attrs dom_path ["type", "domain"])


let do_domain overwrite time_limit node_limit alg_attrs args domainname =
  (** runs an alg on all problems *)
  List.iter
    (fun domain_attrs ->
       let domain_path = Rdb.path_for dom_path domain_attrs in
	 List.iter
	   (fun prob_attrs ->
	      let prob_path = Rdb.path_for data_root prob_attrs
	      and attrs = alg_attrs @ prob_attrs in
		do_run overwrite time_limit node_limit args domain_path prob_path attrs)
	   (Rdb.matching_attrs prob_path
	      (Rdb.override_attrs ["type", "instance"] domain_attrs)))
    (Rdb.matching_attrs dom_path ["type", "domain";
				  "domainname", domainname;])


(***
  a_star, greedy, speedy,
  wted_a_star, a_star_eps
  anytime_a_star, ara_star
  bugsy
***)

let do_basic_batch ?(time_limit = (Some Experiments.default_time_limit))
    ?(node_limit = (Some Experiments.default_node_limit)) ?(overwrite = false)
    alg =
  (* a_star, greedy *)
  do_batches overwrite time_limit node_limit ["alg", alg] alg


let do_wted_batch ?(time_limit = (Some Experiments.default_time_limit))
    ?(node_limit = (Some Experiments.default_node_limit))
    ?(weights = Experiments.low_res_weights) ?(overwrite = false) alg =
  (* wted_a_star, a_star_eps, anytime_a_star *)
  List.iter (fun wt ->
	       do_batches overwrite time_limit node_limit
		 ["alg", alg;
		  "wt", string_of_float wt;]
		 (Wrutils.str "%s %f" alg wt))
    weights


let do_beam_batch ?(time_limit = (Some Experiments.default_time_limit))
    ?(node_limit = (Some Experiments.default_node_limit))
    ?(beam_widths = Experiments.full_beams) ?(overwrite = false) alg =
  (* beam search and other int parameter algs *)
  List.iter (fun beam_width ->
	       do_batches overwrite time_limit node_limit
		 ["alg", alg;
		  "beam_width", string_of_int beam_width;]
		 (Wrutils.str "%s %d" alg beam_width))
    beam_widths



let do_optimistic_batch overwrite time_limit node_limit alg wt opt =
  assert (opt >= 1.);
  do_batches overwrite time_limit node_limit
    ["alg", alg;
     "wt", string_of_float wt;
     "optimism", string_of_float opt] (Wrutils.str "%s %f %f" alg wt opt)


let do_optimistic_batches ?(time_limit = (Some Experiments.default_time_limit))
    ?(node_limit = (Some Experiments.default_node_limit))
    ?(weights = Experiments.low_res_weights)
    ?(opt = Experiments.optimisms) ?(overwrite = false) alg =
  List.iter
    (fun opt ->
       List.iter (fun w -> do_optimistic_batch overwrite time_limit node_limit
		    alg w opt) weights) opt


let utility_tuples =
  (* cost_coeff, time_coeff *)
  let r secs_per_cost =
    1., (1. /. secs_per_cost)
  in
    [ (* 0., 1.0; *)
      (* r 0.0000001;
      r 0.000001; *)
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
      r 0.5;
      r 1.;
      r 5.;
      r 10.;
      r 50.;
      1., 0.0; ]


let do_bugsy_batch overwrite time_limit node_limit alg domain =
  (* bugsy, bugsy_coeff.  PLEASE ROUGHLY NORMALIZE COSTS IN DOMAINS *)
  List.iter (fun (cost, time) ->
	       let attrs = ["alg", alg;
			    "cost_coeff", string_of_float cost;
			    "time_coeff", string_of_float time; ]
	       and args = Wrutils.str "%s %f %f" alg cost time
	       in
		 do_domain overwrite time_limit node_limit attrs args domain)
    utility_tuples

(* EOF *)
