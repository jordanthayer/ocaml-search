(**

   For running orc experiments

*)


let instance_root =
  Filename.concat Experiments.instance_root "orc"

let solver_binary =
  ref (Filename.concat Experiments.bin_root "orc_solver")


let call_solver limits args prob_path attrs outch =
  (** [call_solver limits args prob_path attrs outch] calls the
      external solver binary with [args] and the given limits as the
      arguments and [prob_path] is piped into its input.  The output
      is redirected to [outch]. *)
  let limit_args = Limit.to_string limits in
  let cmd = Printf.sprintf "ulimit -s unlimited\n%s %s %s < %s" !solver_binary args limit_args prob_path in
    Verb.pe Verb.debug "calling::\n%s\n" cmd;
    let run cmd () =
      Verb.pe Verb.debug "calling [%s]\n" cmd;
      Wrsys.with_subprocess_all_pipes cmd
	(fun to_solver from_solver err_solver ->
	   let buf = Buffer.create 16 in
	     (
	       try 
		 while true do
		   Buffer.add_channel buf err_solver 1
		 done
	       with
		   End_of_file -> ()
	     );
	     let errors = (Buffer.contents buf) in
	       if((String.length errors) = 0) then ()
	       else
		 Verb.pe Verb.always "\nErrors: ***start here***\n%s\n***end here***\n" (Buffer.contents buf);
	       
	       Datafile.write_header_pairs outch;
	       Datafile.write_pairs outch attrs;
	       Datafile.write_pairs outch [ "attrs", (Wrstr.encode_pairs attrs) ];
	       let res = Datafile.pipe_data from_solver outch in
		 Datafile.write_trailer_pairs outch;
		 res);
    in
      Notify.try_and_email_on_fail (run cmd) "Orc_runs"


let do_run overwrite limits args attrs prob_path =
  (** [do_run overwrite limits macro args attrs prob_path] performs a
      single run on the given problem.  The results are stored in the
      RDB specified by data_root. *)
  let data_root = Filename.concat Experiments.data_root "orc" in
  let res_file = Rdb.path_for data_root attrs in
    if Sys.file_exists res_file
      && Datafile.seems_complete res_file
      && not (Limit.has_new_limits res_file limits)
      && not overwrite
    then Printf.printf "%s exists -- skipping!\n%!" res_file
    else begin
      Wrio.with_outfile res_file
	(fun outch ->
	   let rel_path = Wrfname.make_relative instance_root prob_path in
	     Printf.printf "Running %s on %s...%!" args rel_path;
	     let c, t = call_solver limits args prob_path attrs outch in
	       Printf.printf "%.1f in %.3f.\n%!" c t)
    end



let do_batch 
    ?(send_mail=true) 
    ?(overwrite=false) ?(limits=[])
    ~model ~x ~y ~prob args alg_attrs =
  (** does a orc run with the specified parameters. *)
  let batch_attrs = [
    "model",model;
    "rows",string_of_int x;
    "cols",string_of_int y;
    "prob",string_of_float prob;
  ] in

  let batch_str = Printf.sprintf "model=%s, size=%dx%d prob %f" 
    model x y prob in
  let attr_sets = Rdb.matching_attrs instance_root 
    (batch_attrs) in
    if(send_mail) then
      Notify.start_batchtime ();

    List.iter (fun (inst_attrs:(string*string) list) ->
		 let other_instance_attrs = 
		   (inst_attrs) in
		 let attrs = alg_attrs @ 
		   other_instance_attrs in
		 let prob_path = Rdb.path_for instance_root inst_attrs in
		   do_run overwrite limits args attrs prob_path)
      attr_sets;
    if(send_mail) then
      Notify.send_batch_completed_mail "Topspin_runs" batch_str args


let do_basic_batch
    ?(limits=[
	(Limit.Time Experiments.default_time_limit);
	(Limit.Generated Experiments.default_node_limit);
      ])
    ?(send_mail=true) 
    ?(overwrite=false) ?(limits=[])
    ~model ~x ~y ~prob alg =
  (* greedy, speedy, a_star *)
  let send_mail = false in
    Notify.start_batchtime();
    do_batch ~send_mail ~overwrite ~limits 
      ~model ~x ~y ~prob alg ["alg", alg];
    Notify.send_batch_completed_mail "Orc_runs" "Basic" alg


let do_wted_batch
    ?(limits=[
	(Limit.Time Experiments.default_time_limit);
	(Limit.Generated Experiments.default_node_limit);
      ])
    ?(send_mail=true) 
    ?(overwrite=false) ?(limits=[])
    ?(weights=Experiments.low_res_weights)
    ~model ~x ~y ~prob alg =
  (* greedy, speedy, a_star *)
  let send_mail = false in
    Notify.start_batchtime();
    List.iter (
      fun wt -> 
      do_batch ~send_mail ~overwrite ~limits 
	~model ~x ~y ~prob 
	(Printf.sprintf "%s %f" alg wt) 
	["alg", alg;"wt",string_of_float wt];
    ) weights;
    Notify.send_batch_completed_mail "Orc_runs" "weighted" alg

let do_new_beam_batch
    ?(limits=[
	(Limit.Time Experiments.default_time_limit);
	(Limit.Generated Experiments.default_node_limit);
      ])
    ?(sort_predicate="f")
    ?(send_mail=true) 
    ?(overwrite=false) ?(limits=[])
    ?(beams=Experiments.full_beams)
    ~model ~x ~y ~prob alg =
  (* greedy, speedy, a_star *)
  let send_mail = false in
    Notify.start_batchtime();
    List.iter (
      fun beam_width ->
	do_batch ~send_mail ~overwrite ~limits 
	  ~model ~x ~y ~prob 
	  (Printf.sprintf "%s %d %s" alg beam_width sort_predicate) 
	  ["alg", alg;"beam_width",string_of_int beam_width;
	   "sort_predicate",sort_predicate];
    ) beams;
    Notify.send_batch_completed_mail "Orc_runs" "new beam" alg

