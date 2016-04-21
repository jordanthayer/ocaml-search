open Printf

let instance_root =
  Filename.concat User_paths.instance_root "manufacturing_instances"

let solver_binary =
  ref (Filename.concat User_paths.bin_root "manufacture-solver")

let call_solver limits args prob_path attrs outch =
  let limit_args = Limit.to_string limits in
  let cmd =
    sprintf "%s %s %s < %s" !solver_binary args limit_args prob_path
  in
    Verb.pe Verb.debug "calling [%s]\n" cmd;
    Wrsys.with_subprocess_all_pipes cmd
      (fun to_solver from_solver _ ->
	 Datafile.write_header_pairs outch;
	 Datafile.write_pairs outch attrs;
	 Datafile.write_pairs outch [ "attrs", (Wrstr.encode_pairs attrs) ];
	 let res = Datafile.pipe_data from_solver outch in
	   Datafile.write_trailer_pairs outch;
	   res)

let do_run overwrite limits args attrs prob_path =
  let data_root =
    Filename.concat User_paths.data_root "manufacturing" in
  let res_file = Rdb.path_for data_root attrs in
    if Sys.file_exists res_file
      && Datafile.seems_complete res_file
      && not (Limit.has_new_limits res_file limits)
      && not overwrite
    then
      printf "%s exists -- skipping!\n%!" res_file
    else begin
      Wrio.with_outfile res_file
	(fun outch ->
	   let rel_path = Wrfname.make_relative instance_root prob_path in
	     printf "Running %s on %s...%!" args rel_path;
	     let c, t = call_solver limits args prob_path attrs outch in
	       printf "%.1f in %.3f.\n%!" c t)
    end

let do_batch ?(overwrite=false) ?(limits=[])
    ~nmachs ~njobs ~nattrs args alg_attrs =
  let edge_mult = 10. in
  let min_proc = 900 in
  let max_proc = 1000 in
  let attrs_per_mach = 2 in
  let ninputs = 2 in
  let noutputs = 2 in
  let batch_attrs = [
    "num machines", string_of_int nmachs;
    "num jobs", string_of_int njobs;
    "num attributes", string_of_int nattrs;
    "num inputs", string_of_int ninputs;
    "num outputs", string_of_int noutputs;
    "edge mult", string_of_float edge_mult;
    "min proc time", string_of_int min_proc;
    "max proc time", string_of_int max_proc;
    "max attributes per machin", string_of_int attrs_per_mach;
  ] in
  let attr_sets = Rdb.matching_attrs instance_root batch_attrs in
    List.iter (fun inst_attrs ->
		 let attrs = alg_attrs @ inst_attrs in
		 let prob_path = Rdb.path_for instance_root inst_attrs in
		   do_run overwrite limits args attrs prob_path)
      attr_sets
