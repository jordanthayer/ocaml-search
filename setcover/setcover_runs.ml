(** Run script for weighted set cover.

    @author eaburns
    @since 2011-01-13
*)


open Printf

let data_root = User_paths.data_root ^ "setcover/"
and instance_root = User_paths.instance_root ^ "setcover"
and solver_binary = ref (User_paths.bin_root ^ "setcover_solver")


(** calls the external solver on [args] and [board_name],
    redirecting its output to [outch].  all logging is done within
    here.  returns cost, time pair *)
let call_solver limits args prob_path attrs outch =
  if Machine_usage.is_idle () then begin
    let args =
      List.fold_left (fun s l ->
			Wrutils.str "%s %s" s (Info.arg_string_from_limit l))
	args
	limits
    in
      Wrsys.with_subprocess_pipes
	(Wrutils.str "%s %s < %s" !solver_binary args prob_path)
	(fun to_solver from_solver ->
	   Datafile.write_header_pairs outch;
	   Datafile.write_pairs outch attrs;
	   Datafile.write_pairs outch ["attrs", (Wrstr.encode_pairs attrs)];
	   let res = Datafile.pipe_data from_solver outch in
	     Datafile.write_trailer_pairs outch;
	     res)
  end else
    failwith "Machine is in use"


(** sets up run file and calls solver on board_name *)
let do_run overwrite limits args prob_path attrs =
  let run_file = Rdb.path_for data_root
    (Rdb.filter_attrs ["type"] attrs) in
    if Sys.file_exists run_file
      && Datafile.seems_complete run_file
      && not overwrite
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


let all_insts = [

(*
  [ "model", "uniform"; "objects", "1000"; "subsets", "500";
    "min value", "0."; "max value", "100."; "max proportion", "0.1"; ],
  [Info.Time 120.];

  [ "model", "uniform"; "objects", "1000"; "subsets", "500";
    "min value", "0."; "max value", "100."; "max proportion", "0.01"; ],
  [Info.Time 120.];

  [ "model", "uniform"; "objects", "1000"; "subsets", "500";
    "min value", "0."; "max value", "10."; "max proportion", "0.1"; ],
  [Info.Time 120.];

  [ "model", "uniform"; "objects", "1000"; "subsets", "500";
    "min value", "0."; "max value", "1."; "max proportion", "0.1"; ],
  [Info.Time 120.];

  [ "model", "uniform"; "objects", "1000"; "subsets", "100";
    "min value", "0."; "max value", "1."; "max proportion", "0.1"; ],
  [Info.Time 120.];

  [ "model", "uniform"; "objects", "1000"; "subsets", "1000";
    "min value", "0."; "max value", "1."; "max proportion", "0.1"; ],
  [Info.Time 120.];
*)

  [ "model", "uniform"; "objects", "1000"; "subsets", "500";
    "min value", "9."; "max value", "10."; "max proportion", "0.1"; ],
  [Info.Time 120.];

]


let do_batches overwrite ?(limits=[]) args alg_attrs =
  List.iter
    (fun (inst, lim) ->
       do_batch overwrite (lim@limits) args alg_attrs inst)
    all_insts


let all_batches overwrite =
  do_batches overwrite "dfs" ["alg", "dfs"];
  do_batches overwrite "ilds_bottom" ["alg", "ilds_bottom"];
  do_batches overwrite "ilds_top" ["alg", "ilds_top"];
  do_batches overwrite "blfs_poly nlms_flow_2 0.05 2"
    ["alg", "blfs_poly"; "learner", "nlms_flow_2"; "learning rate", "0.05";
     "degree", "2"; ];
  do_batches overwrite "blfs_poly nlms_flow_2 0.01 2"
    ["alg", "blfs_poly"; "learner", "nlms_flow_2"; "learning rate", "0.01";
     "degree", "2"; ];
  ()
