(** Run file for the synthetic instances *)

open Printf

let data_root = Experiments.data_root ^ "synth"
and instance_root = Experiments.instance_root ^ "synthetic_instances"
and solver_binary = ref (Experiments.bin_root ^ "normal_tree_solver")

let call_solver limits prob_path attrs args outch =
  (** [call_solver limits prob_path attrs args outch] calls the
      external solver. *)
  let args' = Limit.to_string ~accum:args limits in
  let go () =
    let cmd = sprintf "%s %s < %s" !solver_binary args' prob_path in
      Datafile.write_header_pairs outch;
      Datafile.write_pairs outch attrs;
      Datafile.write_pairs outch ["attrs", (Wrstr.encode_pairs attrs)];
      Verb.pr Verb.debug "executing [%s]\n%!" cmd;
      let result =
	Wrsys.with_subprocess_pipes cmd
	  (fun _ inch -> Datafile.pipe_data inch outch)
      in
	Datafile.write_trailer_pairs outch;
	result
  in Notify.try_and_email_on_fail go "Uniform_runs"



let do_run overwrite limits prob_path attrs args =
  (** [do_run overwrite limits prob_path attrs args] performs the
      experiment run and collects the results in a datafile. *)
  let data_file = Rdb.path_for data_root attrs in
    if (Sys.file_exists data_file)
      && (Datafile.seems_complete data_file)
      && not overwrite
      && not (Limit.has_new_limits data_file limits)
    then Printf.printf "%s exists -- skipping!\n%!" data_file
    else begin
      Wrio.with_outfile data_file
	(fun outch ->
	   Printf.printf "Running %s and %s ...%!" args
	     (Wrfname.make_relative instance_root prob_path);
	   let c, t = call_solver limits prob_path attrs args outch in
	     Printf.printf "%.1f in %.3f.\n%!" c t)
    end


type insts =
  | All_instances
  | Even_instances
  | Odd_instances
  | These_instances of int list


let should_run insts attrs =
  let num = int_of_string (List.assoc "num" attrs) in
    match insts with
      | All_instances -> true
      | Even_instances -> num mod 2 = 0
      | Odd_instances -> num mod 2 <> 0
      | These_instances lst -> List.mem num lst


let run_batch overwrite insts limits ~batch_attrs args ~alg_attrs =
  (** [run_batch overwrite insts limits ~batch_attrs args ~alg_attrs]
      performs a batch of runs on all matching instances. *)
  Verb.pr Verb.always "Looking for instances in %s\n" instance_root;
  Verb.pr Verb.always "Looking for instances matching: %s\n"
    (Rdb.attrs_str [] batch_attrs);
  List.iter (fun prob_attrs ->
	       if should_run insts prob_attrs then begin
		 let prob_path = Rdb.path_for instance_root prob_attrs in
		 let attrs = alg_attrs @ prob_attrs in
		   Verb.pr Verb.debug "Running on problem %s\n" prob_path;
		   do_run overwrite limits prob_path attrs args
	       end)
    (Rdb.matching_attrs instance_root batch_attrs)


let do_batch
    overwrite ?(insts=All_instances) limits ?(mean=0.)
    ?(stdev=1.5) ?(p=0.75) depth args alg_attrs =
  (** [do_batch overwrite limits ?mean ?stdev ?p depth args alg_attrs]
      performs a batch of runs on all matching instances. *)
  run_batch overwrite insts limits args ~alg_attrs
    ~batch_attrs:[ "model", "normal-tree";
		   "mean", string_of_float mean;
		   "stdev", string_of_float stdev;
		   "p", string_of_float p;
		   "depth", string_of_int depth; ]
