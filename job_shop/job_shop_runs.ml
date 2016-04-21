(** Runs batches of job shop scheduling instances

    @author eaburns
    @since 2010-02-22
*)

open Printf

let data_root = User_paths.data_root ^ "job_shop/"
and instance_root = User_paths.instance_root ^ "job_shop"
and solver_binary = ref (User_paths.bin_root ^ "job_shop_solver")


let call_solver limits args prob_path attrs outch =
  (** calls the external solver on [args] and [board_name],
      redirecting its output to [outch].  all logging is done within
      here.  returns cost, time pair *)
  if Machine_usage.is_idle () then begin
    let args =
      List.fold_left (fun s l ->
			Wrutils.str "%s %s" s (Info.arg_string_from_limit l))
	args
	limits
    in
      Wrsys.with_subprocess_pipes
	(Wrutils.str "%s %s < %s 2>&1 | tee output.log"
	   !solver_binary args prob_path)
	(fun to_solver from_solver ->
	   Datafile.write_header_pairs outch;
	   Datafile.write_pairs outch attrs;
	   Datafile.write_pairs outch ["attrs", (Wrstr.encode_pairs attrs)];
	   let res = Datafile.pipe_data from_solver outch in
	     Datafile.write_trailer_pairs outch;
	     res)
  end else
    failwith "Machine is in use"


let do_run overwrite limits args prob_path attrs =
  (** sets up run file and calls solver on board_name *)
  let run_file = Rdb.path_for data_root
    (Rdb.filter_attrs ["type"] attrs) in
    if Sys.file_exists run_file &&
      Datafile.seems_complete run_file  &&
      (*valid_df (Datafile.load run_file)  &&*)
      not overwrite
(*
      &&
      not ((Limit.has_new_limits run_file limits) &&
*)
(*
      && not (Datafile.run_finished run_file)
  )
*)
    then
      Wrutils.pr "%s exists - skipping!\n%!" run_file
    else
      (Wrio.with_outfile run_file
	 (fun outch ->
	    Wrutils.pr "Running %s on %s...%!" args
	      (Wrfname.make_relative instance_root
		 prob_path);
	    let c,t =
	      call_solver limits args prob_path attrs outch in
	      Wrutils.pr "%.1f in %.3f.\n%!" c t))


let do_batch overwrite limits args alg_attrs batch_attrs =
  List.iter
    (fun prob_attrs ->
       do_run
	 overwrite limits args
	 (Rdb.path_for instance_root prob_attrs)
	 (alg_attrs @ prob_attrs))
    (Rdb.matching_attrs instance_root batch_attrs)


let train_windecision overwrite ?(traversal="probe") ?(time=60.)
    ?(degree=2) ?(nleaves=512) n m =
  let train_attrs =
    [ "degree", string_of_int degree;
      "leaves per inst", string_of_int nleaves;
      "traversal", traversal; ]
  in
  let alg_attrs = ("alg", "train_windecision") :: train_attrs in
  let sf_attrs = ("alg", "windecision_samples") :: train_attrs in
  let inst_attrs =
    [ "model", "random-training";
      "num_jobs", string_of_int n; "num_machines", string_of_int m; ]
  in
  let lim = [ Info.Time time ]  in
  let attrs = sf_attrs @ inst_attrs in
  let sf = Rdb.path_for data_root attrs in
  let args =
    sprintf "train_windecision %d %d %s %s"
      degree nleaves traversal sf
  in
    do_batch overwrite lim args alg_attrs inst_attrs


let windecision_rbfs overwrite ?(traversal="probe") ?(degree=2) ?(nleaves=512)
    ?(limit=[Info.Never]) model n m =
  let sample_attrs =
    [ "alg", "windecision_samples";
      "degree", string_of_int degree;
      "leaves per inst", string_of_int nleaves;
      "traversal", traversal;
      "model", "random-training";
      "num_jobs", string_of_int n;
      "num_machines", string_of_int m;
    ] in
  let inst_attrs =
    [ "model", model;
      "num_jobs", string_of_int n; "num_machines", string_of_int m; ] in
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
    do_batch overwrite limit args alg_attrs inst_attrs


let windecision_sum_bottom overwrite ?(traversal="probe") ?(degree=2)
    ?(nleaves=512) ?(limit=[Info.Never]) model n m =
  let sample_attrs =
    [ "alg", "windecision_samples";
      "degree", string_of_int degree;
      "leaves per inst", string_of_int nleaves;
      "traversal", traversal;
      "model", "random-training";
      "num_jobs", string_of_int n;
      "num_machines", string_of_int m;
    ] in
  let inst_attrs =
    [ "model", model;
      "num_jobs", string_of_int n; "num_machines", string_of_int m; ] in
  let sf = Rdb.path_for data_root sample_attrs in
  let coeffs = Offline_windecision.learn_coeffs sf in
  let coeff_string = Offline_windecision.string_of_coeffs coeffs in
  let args = sprintf "windecision_sum_bottom \"%s\" 2000" coeff_string in
  let alg_attrs = [ "alg", "windecision_sum_bottom";
		    "coeffs", coeff_string;
		    "degree", string_of_int degree;
		    "leaves per inst", string_of_int nleaves;
		    "traversal", traversal;
		    "max-bins", "2000"; ] in
    Verb.pr Verb.debug "args: [%s]\n" args;
    do_batch overwrite limit args alg_attrs inst_attrs


let windecision_sum_top overwrite ?(traversal="probe") ?(degree=2)
    ?(nleaves=512) ?(limit=[Info.Never]) model n m =
  let sample_attrs =
    [ "alg", "windecision_samples";
      "degree", string_of_int degree;
      "leaves per inst", string_of_int nleaves;
      "traversal", traversal;
      "model", "random-training";
      "num_jobs", string_of_int n;
      "num_machines", string_of_int m;
    ] in
  let inst_attrs =
    [ "model", model;
      "num_jobs", string_of_int n; "num_machines", string_of_int m; ] in
  let sf = Rdb.path_for data_root sample_attrs in
  let coeffs = Offline_windecision.learn_coeffs sf in
  let coeff_string = Offline_windecision.string_of_coeffs coeffs in
  let args = sprintf "windecision_sum_top \"%s\" 2000" coeff_string in
  let alg_attrs = [ "alg", "windecision_sum_top";
		    "coeffs", coeff_string;
		    "degree", string_of_int degree;
		    "leaves per inst", string_of_int nleaves;
		    "traversal", traversal;
		    "max-bins", "2000"; ] in
    Verb.pr Verb.debug "args: [%s]\n" args;
    do_batch overwrite limit args alg_attrs inst_attrs


let all_insts = [

(*
  ["model", "random"; "num_jobs", "8"; "num_machines", "8"],
  [Info.Time 60.];
*)
  ["model", "random"; "num_jobs", "10"; "num_machines", "10"],
  [Info.Time 60.];
  ["model", "random"; "num_jobs", "15"; "num_machines", "15"],
  [Info.Time 120.];
  ["model", "random"; "num_jobs", "20"; "num_machines", "20"],
  [Info.Time 300.];

  ["model", "taillard"; "num_jobs", "10"; "num_machines", "10"],
  [Info.Time 300.];

  ["model", "taillard"; "num_jobs", "15"; "num_machines", "15"],
  [Info.Time 300.];

  ["model", "taillard"; "num_jobs", "20"; "num_machines", "20"],
  [Info.Time 300.];

(*
  ["model", "taillard"; "num_jobs", "20"; "num_machines", "15"],
  [Info.Time 60.];

  ["model", "taillard-style"; "num_jobs", "10"; "num_machines", "10"],
  [Info.Time 300.];

  ["model", "taillard-style"; "num_jobs", "15"; "num_machines", "15"],
  [Info.Time 300.];

  ["model", "taillard-style"; "num_jobs", "20"; "num_machines", "20"],
  [Info.Time 300.];
*)

]


let do_batches overwrite ?(limits=[]) args alg_attrs =
  List.iter
    (fun (inst, lim) ->
       do_batch overwrite (lim@limits) args alg_attrs inst)
    all_insts


let all_batches overwrite =
(*
  do_batches overwrite "taillard_lb" ["alg", "taillard_lb"];
*)
  do_batches overwrite ~limits:[Info.Leaves 1] "dfs" ["alg", "dfs_first"];
  do_batches overwrite "dfs" ["alg", "dfs"];
  do_batches overwrite "ilds_bottom" ["alg", "ilds_bottom"];
  do_batches overwrite "ilds_top" ["alg", "ilds_top"];

  do_batches overwrite "indecision_rbfs false"
    ["alg", "indecision_rbfs"; "norm", "false"; ];

  do_batches overwrite "indecision_sum_bottom false 2000"
    ["alg", "indecision_sum_bottom"; "norm", "false"; "max-bins", "2000"];

  do_batches overwrite "indecision_sum_top false 2000"
    ["alg", "indecision_sum_top"; "norm", "false"; "max-bins", "2000"];

  do_batches overwrite "indecision_cr_bottom 100"
    ["alg", "indecision_cr_bottom"; "max-bins", "100"; ];

  do_batches overwrite "indecision_cr2_bottom 100"
    ["alg", "indecision_cr2_bottom"; "max-bins", "100"; ];

  do_batches overwrite "wds_bottom 100"
    ["alg", "wds_bottom"; "max-bins", "100"; ];

  do_batches overwrite "blfs_poly nlms_flow_2 0.05 2"
    ["alg", "blfs_poly"; "learner", "nlms_flow_2"; "learning rate", "0.05";
     "degree", "2"; ];
  do_batches overwrite "blfs_poly nlms_flow_2 0.01 2"
    ["alg", "blfs_poly"; "learner", "nlms_flow_2"; "learning rate", "0.01";
     "degree", "2"; ]
(*
  List.iter (fun (attrs, lim) ->
	       let model = List.assoc "model" attrs in
	       let n = int_of_string (List.assoc "num_jobs" attrs) in
	       let m = int_of_string (List.assoc "num_machines" attrs) in
	       windecision_rbfs overwrite ~limit:lim model n m)
    all_insts
*)



let do_learned_batches overwrite =
(*
  do_batches overwrite "blfs_sep nlms_flow_2 0.05"
    ["alg", "blfs_sep"; "learner", "nlms_flow_2"; "learning rate", "0.05"; ];
*)
  do_batches overwrite "blfs_poly nlms_flow_2 0.05 2"
    ["alg", "blfs_poly"; "learner", "nlms_flow_2"; "learning rate", "0.05";
     "degree", "2"; ];
  do_batches overwrite "blfs_poly nlms_flow_2 0.01 2"
    ["alg", "blfs_poly"; "learner", "nlms_flow_2"; "learning rate", "0.01";
     "degree", "2"; ]

(*
  do_batches overwrite "blfs_sep_sampled nlms_flow_2 0.05 4096"
    ["alg", "blfs_sep_sampled"; "learner", "nlms_flow_2";
     "learning rate", "0.05"; "sample-size", "4096"; ]
*)


