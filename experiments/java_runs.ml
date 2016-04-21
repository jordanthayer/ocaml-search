



let instance_root name =
  match name with
      "tiles" -> Filename.concat Experiments.instance_root "tiles_instances"
    | "santa_naive" -> Filename.concat Experiments.instance_root "santa"
    | _ -> failwith "invalid instance root selection"

let solver_binary =
  ref ("java -Xmx7000m -Xms7000m -Xss300m -XX:+UseConcMarkSweepGC -jar " 
       ^ (Filename.concat Experiments.bin_root "search.jar "))


let call_solver limits args prob_path attrs outch =
  (** [call_solver limits args prob_path attrs outch] calls the
      external solver binary with [args] and the given limits as the
      arguments and [prob_path] is piped into its input.  The output
      is redirected to [outch]. *)
  let limit_args = Limit.to_string limits in

  let cmd = Printf.sprintf "%s %s %s --type tiles --problem %s" !solver_binary args limit_args prob_path in

    Verb.pe Verb.debug "calling::\n%s\n" cmd;
    let run cmd () =
      Verb.pe Verb.debug "calling [%s]\n" cmd;
      Wrsys.with_subprocess_all_pipes cmd
	(fun to_solver from_solver err_solver ->
	   let buf = Buffer.create 16 in
	     (try
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
      Notify.try_and_email_on_fail (run cmd) "Sliding_tiles_runs"


let do_run ?(lazy_run=false)
    overwrite
    limits
    macro
    args
    attrs
    prob_path 
    prob_type
    =
  (** [do_run overwrite limits macro args attrs prob_path] performs a
      single run on the given problem.  The results are stored in the
      RDB specified by data_root.

      the lazy_run optional parameter makes it so that if the file
      already exists and looks done, it just skips it no questions
      asked.

  *)
  let data_root =
    Filename.concat Experiments.data_root prob_type
  in
  let attrs = ("lang","java_hotspot") :: attrs in
  let res_file = Rdb.path_for data_root attrs in
    if Sys.file_exists res_file
      && Datafile.seems_complete res_file
      && not (Limit.has_new_limits res_file limits)
      && not overwrite
      && not lazy_run
    then Printf.printf "%s exists -- skipping!\n%!" res_file
    else if
      lazy_run
      && Sys.file_exists res_file
      && Datafile.seems_complete res_file
      && (let df = Datafile.load res_file in
	  let found_solution = Datafile.get_val df "found solution" in
	    found_solution = "yes"
	 )
    then Printf.printf "%s exists -- skipping (lazy mode)!\n%!" res_file
    else begin
      Wrio.with_outfile res_file
	(fun outch ->
	   let rel_path = Wrfname.make_relative (instance_root prob_type) prob_path in
	     Printf.printf "Running %s on %s...%!" args rel_path;
	     let c, t = call_solver limits args prob_path attrs outch in
	       Printf.printf "%.1f in %.3f.\n%!" c t)
    end



let do_batch ?(lazy_run = false) ?(send_mail=true)
    ?(threads=1)
    ?(overwrite=false) ?(cost="unit")
    ?(heuristic="manhattan")
    ?(limits=[]) ?(macro=false) ?(battrs =[])
    model ~nrows ~ncols args alg_attrs =
  (** [do_batch ?overwrite ?cost ?limits model ~nrows ~ncols args
      alg_attrs] performs a batch of runs on the given set of tiles
      problems. *)
  let batch_attrs = [ "model", model;
		      "rows", string_of_int nrows;
		      "cols", string_of_int ncols; ] @ battrs
  in
  let batch_str = Printf.sprintf "model=%s, nrows=%d, ncols=%d" model nrows ncols in
  let attr_sets = Rdb.matching_attrs (instance_root "tiles") batch_attrs in
    if(send_mail) then
      Notify.start_batchtime ();

    List.iter (fun inst_attrs ->
		 let cost_attr = [ "cost", cost;"heuristic",heuristic ] in
		 let attrs =  ["threads",string_of_int threads] 
		   @ alg_attrs @ cost_attr @ inst_attrs in
		 let args = Printf.sprintf " %s --threads %d " args
		   threads in

		 let prob_path = Rdb.path_for (instance_root "tiles" ) inst_attrs in
		   do_run ~lazy_run:lazy_run overwrite limits macro
		     args attrs prob_path "tiles")
      attr_sets;
    if(send_mail) then
      Notify.send_batch_completed_mail "Sliding_tiles_runs" batch_str args



let do_basic_batch
    ?(overwrite = false)
    ?(lazy_run = false)
    ?(cost="unit")
    ?(limits=[(Limit.Generated Experiments.default_node_limit);
	      (Limit.Time Experiments.default_time_limit);])
    ?(macro=false)
    ~model ~nrows ~ncols
    alg =
  Notify.start_batchtime();
  do_batch ~lazy_run:lazy_run ~send_mail:false
    ~overwrite:overwrite
    ~cost:cost
    ~limits:limits
    ~macro:macro
    model
    ~nrows
    ~ncols
    (Printf.sprintf " --alg %s " alg)
    (
      [
	"alg",alg;
      ]
    );
    Notify.send_batch_completed_mail "Sliding_tiles_runs" "basic" alg


let do_wted_batch
    ?(overwrite = false)
    ?(lazy_run = false)
    ?(weights = Experiments.low_res_weights)
    ?(cost="unit")
    ?(limits=[(Limit.Generated Experiments.default_node_limit);
	      (Limit.Time Experiments.default_time_limit);])
    ?(macro=false)
    ?(heuristic = "manhattan")
    ~model ~nrows ~ncols
    alg =
  Notify.start_batchtime();
  List.iter (fun wt ->
	       do_batch ~lazy_run:lazy_run ~send_mail:false
		 ~overwrite:overwrite
		 ~heuristic:heuristic
		 ~cost:cost
		 ~limits:limits
		 ~macro:macro
		 model
		 ~nrows
		 ~ncols
		 (Printf.sprintf "--alg %s --rest %f" alg wt)
		 (
		   [
		     "alg",alg;
		     "wt",(string_of_float wt);]
		 )
	    ) weights;
  Notify.send_batch_completed_mail "Sliding_tiles_runs" "wted" alg


let do_beam_batch
    ?(threads = 1)
    ?(overwrite = false)
    ?(lazy_run = false)
    ?(beams = Experiments.full_beams)
    ?(cost="unit")
    ?(limits=[(Limit.Generated Experiments.default_node_limit);
	      (Limit.Time Experiments.default_time_limit);])
    ?(macro=false)
    ?(heuristic = "manhattan")
    ~model ~nrows ~ncols 
    alg =
  Notify.start_batchtime();
  List.iter (fun beam ->
	       do_batch ~threads:threads 
		 ~lazy_run:lazy_run ~send_mail:false
		 ~overwrite:overwrite
		 ~heuristic:heuristic
		 ~cost:cost
		 ~limits:limits
		 ~macro:macro
		 model
		 ~nrows
		 ~ncols
		 (Printf.sprintf "--alg %s --rest %d" alg beam)
		 (
		   [
		     "alg",alg;
		     "beam_width",(string_of_int beam);]
		 )
	    ) beams;
  Notify.send_batch_completed_mail "Sliding_tiles_runs" "beam" alg
