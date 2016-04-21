(** Running sliding tiles experiments.

    @author eaburns
    @since 2010-09-03
*)

open Printf

let instance_root =
  Filename.concat Experiments.instance_root "tiles_instances"

let solver_binary =
  ref (Filename.concat Experiments.bin_root "sliding-tiles-solver")


let call_solver limits args prob_path attrs outch =
  (** [call_solver limits args prob_path attrs outch] calls the
      external solver binary with [args] and the given limits as the
      arguments and [prob_path] is piped into its input.  The output
      is redirected to [outch]. *)
  let limit_args = Limit.to_string limits in
    (*
      let cmd = sprintf "%s %s %s < %s" !solver_binary args limit_args prob_path in
    *)
    (*
      let run cmd () =
      Verb.pe Verb.debug "calling [%s]\n" cmd;
      Wrsys.with_subprocess_all_pipes cmd
      (fun to_solver from_solver err_solver ->
      Datafile.write_header_pairs outch;
      Datafile.write_pairs outch attrs;
      Datafile.write_pairs outch [ "attrs", (Wrstr.encode_pairs attrs) ];
      let res = Datafile.pipe_data from_solver outch in
      Datafile.write_trailer_pairs outch;
      res);
      in
      Notify.try_and_email_on_fail (run cmd) "Sliding_tiles_runs"
    *)

  let cmd = sprintf "%s %s %s < %s" !solver_binary args limit_args prob_path in

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
    prob_path =
  (** [do_run overwrite limits macro args attrs prob_path] performs a
      single run on the given problem.  The results are stored in the
      RDB specified by data_root.

      the lazy_run optional parameter makes it so that if the file
      already exists and looks done, it just skips it no questions
      asked.

  *)
  let data_root =
    if macro
    then Filename.concat Experiments.data_root "macro_sliding_tiles"
    else Filename.concat Experiments.data_root "sliding_tiles"
  in
  let args = if macro then "-m " ^ args else args in
  let res_file = Rdb.path_for data_root attrs in
    if Sys.file_exists res_file
      && Datafile.seems_complete res_file
      && not (Limit.has_new_limits res_file limits)
      && not overwrite
      && not lazy_run
    then printf "%s exists -- skipping!\n%!" res_file
    else if
      lazy_run
      && Sys.file_exists res_file
      && Datafile.seems_complete res_file
      && (let df = Datafile.load res_file in
	  let found_solution = Datafile.get_val df "found solution" in
	    found_solution = "yes"
	 )
    then printf "%s exists -- skipping (lazy mode)!\n%!" res_file
    else begin
      Wrio.with_outfile res_file
	(fun outch ->
	   let rel_path = Wrfname.make_relative instance_root prob_path in
	     printf "Running %s on %s...%!" args rel_path;
	     let c, t = call_solver limits args prob_path attrs outch in
	       printf "%.1f in %.3f.\n%!" c t)
    end


let do_batch ?(lazy_run = false) ?(send_mail=true)
    ?(overwrite=false) ?(cost="unit")
    ?(heuristic="manhattan")
    ?(limits=[]) ?(macro=false) ?(battrs =[])
    model ~nrows ~ncols args alg_attrs =
  (** [do_batch ?overwrite ?cost ?limits model ~nrows ~ncols args
      alg_attrs] performs a batch of runs on the given set of tiles
      problems. *)
  (*string to append to the arguemnts for model*)
  let model_string = match model with
      "glued" -> " --glued "
    | _ -> "" in
  let batch_attrs = [ "model", model;
		      "rows", string_of_int nrows;
		      "cols", string_of_int ncols; ] @ battrs
  in
  let batch_str = sprintf "model=%s, nrows=%d, ncols=%d" model nrows ncols in
  let attr_sets = Rdb.matching_attrs instance_root batch_attrs in
    if(send_mail) then
      Notify.start_batchtime ();

    List.iter (fun inst_attrs ->
		 let cost_attr = [ "cost", cost;"heuristic",heuristic ] in
		 let attrs = alg_attrs @ cost_attr @ inst_attrs in
		 let args = sprintf "-c %s %s %s" cost model_string args in

		 let prob_path = Rdb.path_for instance_root inst_attrs in
		   do_run ~lazy_run:lazy_run overwrite limits macro args attrs prob_path)
      attr_sets;
    if(send_mail) then
      Notify.send_batch_completed_mail "Sliding_tiles_runs" batch_str args


let do_nonunit_batch ?(lazy_run = false) ?(send_mail=true)
    ?(overwrite=false)
    ?(heuristic="manhattan")
    ?(limits=[]) ?(macro=false)
    model
    weight
    ~nrows ~ncols args alg_attrs =
  (** [do_batch ?overwrite ?cost ?limits model ~nrows ~ncols args
      alg_attrs] performs a batch of runs on the given set of tiles
      problems. *)
  (*string to append to the arguemnts for model*)
  let model_string = match model with
      "glued" -> " --glued "
    | _ -> "" in
  let batch_attrs = [ "model", model;
		      "rows", string_of_int nrows;
		      "cols", string_of_int ncols;
		    ]
  in
  let batch_str = sprintf "model=%s, nrows=%d, ncols=%d weight=%d"
    model nrows ncols weight in
  let attr_sets = Rdb.matching_attrs instance_root batch_attrs in
    if(send_mail) then
      Notify.start_batchtime ();
    List.iter (fun inst_attrs ->
		 let cost_attr = [ "cost", (string_of_int weight);"heuristic",heuristic ] in
		 let attrs = alg_attrs @ cost_attr @ inst_attrs in
		 let args = sprintf "--tile_weight %d %s %s" weight model_string args in

		 let prob_path = Rdb.path_for instance_root inst_attrs in
		   do_run ~lazy_run:lazy_run overwrite limits macro args attrs prob_path)
      attr_sets;
    if(send_mail) then
      Notify.send_batch_completed_mail "Sliding_tiles_runs" batch_str args



let do_partial_batch ?(lazy_run = false) ?(send_mail=true)
    ?(overwrite=false) ?(cost="unit")
    ?(heuristic="manhattan")
    ?(limits=[]) ?(macro=false)
    model ~nrows ~ncols ~inst_start ~inst_end args alg_attrs =
  (** [do_batch ?overwrite ?cost ?limits model ~nrows ~ncols args
      alg_attrs] performs a batch of runs on the given set of tiles
      problems. *)
  (*string to append to the arguemnts for model*)
  for instance_number = inst_start to inst_end do
    (
      let model_string = match model with
	  "glued" -> " --glued "
	| _ -> "" in
      let batch_attrs = [ "model", model;
			  "rows", string_of_int nrows;
			  "cols", string_of_int ncols;
			  "num",string_of_int instance_number;
			]
      in
      let batch_str = sprintf "model=%s, nrows=%d, ncols=%d" model nrows ncols in
      let attr_sets = Rdb.matching_attrs instance_root batch_attrs in
	if(send_mail) then
	  Notify.start_batchtime ();

	List.iter (
	  fun inst_attrs ->
	    let cost_attr = [ "cost", cost;"heuristic",heuristic ] in
	    let attrs = alg_attrs @ cost_attr @ inst_attrs in
	    let args = sprintf "-c %s %s %s" cost model_string args in
	    let prob_path = Rdb.path_for instance_root inst_attrs in
	      do_run ~lazy_run:lazy_run
		overwrite limits macro args attrs prob_path)
	  attr_sets;
	if(send_mail) then
	  Notify.send_batch_completed_mail "Sliding_tiles_runs" batch_str args
    )
  done


let do_idastar_opt_batch ?(overwrite=false) ?(cost="unit")
    ?(limits=[])
    model ~nrows ~ncols =
  (** [do_idastar_opt_batch ?overwrite ?cost ?limits model ~nrows
      ~ncols] performs a batch of runs on the given set of tiles
      problems.  The algorithm is 'idastar_with_bound' and it is fed
      the optimal bound. *)
  let batch_attrs = [ "model", model;
		      "rows", string_of_int nrows;
		      "cols", string_of_int ncols; ]
  in
  let opt_alg = match cost with
    | "unit" -> ["alg", "idastar"]
    | "sqrt" -> ["alg", "idastar_im_c_hist";
		 "max-bins", "100";
		 "control-factor", "2."]
    | x -> failwith (sprintf "No known optimal algorithm for %s cost" x)
  in
  let batch_str = sprintf "model=%s, nrows=%d, ncols=%d" model nrows ncols in
  let attr_sets = Rdb.matching_attrs instance_root batch_attrs in
  let alg_attrs = ["alg", "idastar_with_opt_bound"] in
    Notify.start_batchtime ();
    List.iter (fun inst_attrs ->
		 let cost_attr = [ "cost", cost ] in
		 let attrs = alg_attrs @ cost_attr @ inst_attrs in
		 let opt_path =
		   Rdb.path_for
		     (Filename.concat User_paths.data_root "sliding_tiles")
		     (opt_alg @ cost_attr @ inst_attrs)
		 in
		 let opt_df = Datafile.load opt_path in
		 let opt_str = Datafile.get_val opt_df "final sol cost" in
		 let opt_digits = String.length opt_str in
		 let decs = String.index opt_str '.' in
		 let opt_decs = opt_digits - (decs + 1) in
		 let opt_epsilon = 1. /. (10. ** (float opt_decs)) in
		 let opt_cost = (float_of_string opt_str) +. opt_epsilon in
		 let args = sprintf "idastar_with_bound %.15g" opt_cost in
		 let args = sprintf "-c %s %s" cost args in
		 let prob_path = Rdb.path_for instance_root inst_attrs in
		   Verb.pr Verb.debug "opt cost=%g, epsilon=%g\n"
		     opt_cost opt_epsilon;
		   do_run overwrite limits false args attrs prob_path)
      attr_sets;
    Notify.send_batch_completed_mail "Sliding_tiles_runs" batch_str
      "idastar with optimal bound"


let do_beam_batch
    ?(overwrite = false)
    ?(lazy_run = false)
    ?(beams = Experiments.full_beams)
    ?(cost="unit")
    ?(limits=[(Limit.Generated Experiments.default_node_limit);
	      (Limit.Time Experiments.default_time_limit);])
    ?(macro=false)
    ?(heuristic = "manhattan")
    ?(append = "")
    ~model ~nrows ~ncols
    alg =
  Notify.start_batchtime();
  List.iter (fun beam ->
	       do_batch ~lazy_run:lazy_run ~send_mail:false
		 ~overwrite:overwrite
		 ~heuristic:heuristic
		 ~cost:cost
		 ~limits:limits
		 ~macro:macro
		 model
		 ~nrows
		 ~ncols
		 (Printf.sprintf "%s %d%s" alg beam append)
		 (
		   [
		     "alg",alg;
		     "beam_width",(string_of_int beam);]
		 )
	    ) beams;
  Notify.send_batch_completed_mail "Sliding_tiles_runs" "beam" alg


let do_basic_batch
    ?(overwrite = false)
    ?(lazy_run = false)
    ?(cost="unit")
    ?(limits=[(Limit.Generated Experiments.default_node_limit);
	      (Limit.Time Experiments.default_time_limit);])
    ?(macro=false)
    ?(heuristic = "manhattan")
    ~model ~nrows ~ncols
    alg =
  Notify.start_batchtime();
  do_batch ~lazy_run:lazy_run ~send_mail:false
    ~overwrite:overwrite
    ~heuristic:heuristic
    ~cost:cost
    ~limits:limits
    ~macro:macro
    model
    ~nrows
    ~ncols
    (Printf.sprintf "%s" alg)
    (
      [
	"alg",alg;
      ]
    );
    Notify.send_batch_completed_mail "Sliding_tiles_runs" "basic" alg


let do_basic_nonunit_batch
    ?(overwrite = false)
    ?(lazy_run = false)
    ?(limits=[(Limit.Generated Experiments.default_node_limit);
	      (Limit.Time Experiments.default_time_limit);])
    ?(macro=false)
    ?(heuristic = "manhattan")
    weights
    ~model ~nrows ~ncols
    alg =
  Notify.start_batchtime();
  List.iter (fun wt ->
	       do_nonunit_batch ~lazy_run:lazy_run ~send_mail:false
		 ~overwrite:overwrite
		 ~heuristic:heuristic
		 ~limits:limits
		 ~macro:macro
		 model
		 wt
		 ~nrows
		 ~ncols
		 (Printf.sprintf "%s" alg)
		 (
		   [
		     "alg",alg;
		   ]
		 );
	    ) weights;
  Notify.send_batch_completed_mail "Sliding_tiles_runs" "basic" alg



let do_new_beam_batch
    ?(overwrite = false)
    ?(lazy_run = false)
    ?(beams = Experiments.full_beams)
    ?(sort_predicate = "f")
    ?(cost="unit")
    ?(limits=[(Limit.Generated Experiments.default_node_limit);
	      (Limit.Time Experiments.default_time_limit);])
    ?(macro=false)
    ?(heuristic = "manhattan")
    ~model ~nrows ~ncols
    alg =
  Notify.start_batchtime();
  List.iter (fun beam ->
	       do_batch ~lazy_run:lazy_run ~send_mail:false
		 ~overwrite:overwrite
		 ~heuristic:heuristic
		 ~cost:cost
		 ~limits:limits
		 ~macro:macro
		 model
		 ~nrows
		 ~ncols
		 (Printf.sprintf "%s %d %s" alg beam sort_predicate)
		 (
		   [
		     "alg",alg;
		     "beam_width",(string_of_int beam);
		     "sort_predicate",sort_predicate;]
		 )
	    ) beams;
  Notify.send_batch_completed_mail "Sliding_tiles_runs" "new_beam" alg



let do_hbss_batch
    ?(overwrite = false)
    ?(lazy_run = false)
    ?(sort_predicate = "f")
    ?(cost="unit")
    ?(limits=[(Limit.Generated Experiments.default_node_limit);
	      (Limit.Time Experiments.default_time_limit);])
    ?(beams=Experiments.full_beams)
    ?(macro=false)
    ?(heuristic = "manhattan")
    ~model ~nrows ~ncols
    alg node_capacity =
  Notify.start_batchtime();

  let beam_widths = List.map (fun a -> node_capacity / a) beams in
  let biases = ["exp";"log";"lin";"p_2";"p_3";] in
    List.iter (
      fun bias_function ->
	List.iter (
	  fun depth_bound ->
	    do_batch ~lazy_run:lazy_run ~send_mail:false
	      ~overwrite:overwrite
	      ~heuristic:heuristic
	      ~cost:cost
	      ~limits:limits
	      ~macro:macro
	      model
	      ~nrows
	      ~ncols
	      (Printf.sprintf "%s %s %d" alg bias_function depth_bound)
	      (
		[
		  "alg",alg;
		  "beam_width",(string_of_int depth_bound);
		  "bias_function",bias_function;]
	      )
	) beam_widths;) biases;
    Notify.send_batch_completed_mail "Sliding_tiles_runs" "new_beam" alg


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
		 (Printf.sprintf "%s %f" alg wt)
		 (
		   [
		     "alg",alg;
		     "wt",(string_of_float wt);]
		 )
	    ) weights;
  Notify.send_batch_completed_mail "Sliding_tiles_runs" "wted" alg


let do_bulb_batch
    ?(overwrite = false)
    ?(lazy_run = false)
    ?(beams = Experiments.full_beams)
    ?(cost="unit")
    ?(limits=[(Limit.Generated Experiments.default_node_limit);
	      (Limit.Time Experiments.default_time_limit);])
    ?(macro=false)
    ?(heuristic = "manhattan")
    ?(node_range = None)
    ~model
    ~nrows
    ~ncols
    alg node_capacity =
  let do_batch = match node_range with
      None -> do_batch ~battrs:[]
    | Some (inst_start, inst_end) ->
	(do_partial_batch ~inst_start ~inst_end) in
    Notify.start_batchtime();
    List.iter (fun beam ->
		 do_batch ~lazy_run:lazy_run ~send_mail:false
		   ~overwrite:overwrite
		   ~heuristic:heuristic
		   ~cost:cost
		   ~limits:limits
		   ~macro:macro
		   model
		   ~nrows
		   ~ncols
		   (Printf.sprintf "%s %d %d" alg beam node_capacity)
		   (
		     [
		       "alg",alg;
		       "beam_width",(string_of_int beam);
		       "node_capacity",(string_of_int node_capacity);]
		   )
	      ) beams;
    Notify.send_batch_completed_mail "Sliding_tiles_runs" "bulb" alg


let do_ib_batch
    ?(overwrite = false)
    ?(lazy_run= false)
    ?(beams = Experiments.full_beams)
    ?(cost="unit")
    ?(limits=[(Limit.Generated Experiments.default_node_limit);
	      (Limit.Time Experiments.default_time_limit);])
    ?(macro=false)
    ?(sort_predicate = "f")
    ?(reserve_sort_predicate = "f")
    ?(heuristic = "manhattan")
    ~model
    ~nrows
    ~ncols
    alg node_capacity p_beam =
  Notify.start_batchtime();
  List.iter (fun beam ->
	       let depth_bound = node_capacity / beam in
	       let bw_to_use = int_of_float ((float_of_int beam)
					     *. p_beam) in
		 do_batch ~lazy_run:lazy_run ~send_mail:false
		   ~overwrite:overwrite
		 ~heuristic:heuristic
		   ~cost:cost
		   ~limits:limits
		   ~macro:macro
		   model
		   ~nrows
		   ~ncols
		   (Printf.sprintf "%s %d %s %s %d %d"
		      alg
		      bw_to_use
		      sort_predicate
		      reserve_sort_predicate
		      depth_bound
		      node_capacity
		   )
		   (
		     [
		       "alg",alg;
		       "beam_width",(string_of_int bw_to_use);
		       "sort_predicate",sort_predicate;
		       "reserve_sort_predicate",reserve_sort_predicate;
		       "depth_bound",(string_of_int depth_bound);
		       "node_capacity",(string_of_int node_capacity);
		     ]
		   )
	    ) beams;
  Notify.send_batch_completed_mail "Sliding_tiles_runs" "ib_beam" alg



let do_wted_strat_batch
    ?(overwrite = false)
    ?(lazy_run= false)
    ?(beams = Experiments.full_beams)
    ?(cost="unit")
    ?(limits=[(Limit.Generated Experiments.default_node_limit);
	      (Limit.Time Experiments.default_time_limit);])
    ?(macro=false)
    ?(strat = "g")
    ?(buckets = [1.0;])
    ?(weights = [1.0])
    ?(heuristic = "manhattan")
    ~model
    ~nrows
    ~ncols
    alg =
  Notify.start_batchtime();
  List.iter
    (fun bucket ->
       List.iter
	 (fun wt ->
	    (List.iter (fun beam ->
			  do_batch ~lazy_run:lazy_run ~send_mail:false
			    ~overwrite:overwrite
			    ~heuristic:heuristic
			    ~cost:cost
			    ~limits:limits
			    ~macro:macro
			    model
			    ~nrows
			    ~ncols
			    (Printf.sprintf "%s %d %s %f %f"
			       alg
			       beam
			       strat
			       bucket
			       wt
			    )
			    (
			      [
				"alg",alg;
				"beam_width",(string_of_int beam);
				"sort_predicate",strat;
				"bucket_size",(string_of_float bucket);
				"wt",(string_of_float wt);
			      ]
			    )
		       )
	       beams))
	 weights
    ) buckets;
  Notify.send_batch_completed_mail "Sliding_tiles_runs" "wted_strat_beam" alg


let do_wted_beam_batch
    ?(overwrite = false)
    ?(lazy_run= false)
    ?(beams = Experiments.full_beams)
    ?(cost="unit")
    ?(limits=[(Limit.Generated Experiments.default_node_limit);
	      (Limit.Time Experiments.default_time_limit);])
    ?(macro=false)
    ?(strat = "g")
    ?(weights = [1.0])
    ?(heuristic = "manhattan")
    ~model
    ~nrows
    ~ncols
    alg =
  Notify.start_batchtime();
  List.iter
    (fun wt ->
       (List.iter (fun beam ->
		     do_batch ~lazy_run:lazy_run ~send_mail:false
		       ~overwrite:overwrite
		       ~heuristic:heuristic
		       ~cost:cost
		       ~limits:limits
		       ~macro:macro
		       model
		       ~nrows
		       ~ncols
		       (Printf.sprintf "%s %d %s %f"
			  alg
			  beam
			  strat
			  wt
		       )
		       (
			 [
			   "alg",alg;
			   "beam_width",(string_of_int beam);
			   "sort_predicate",strat;
			   "wt",(string_of_float wt);
			 ]
		       )
		  )
	  beams))
    weights;
  Notify.send_batch_completed_mail "Sliding_tiles_runs" "wted_beam" alg



let do_wted_nonunit_batch
    ?(overwrite = false)
    ?(lazy_run = false)
    ?(weights = Experiments.low_res_weights)
    ?(limits=[(Limit.Generated Experiments.default_node_limit);
	      (Limit.Time Experiments.default_time_limit);])
    ?(macro=false)
    ?(heuristic = "manhattan")
    tile_weights
    ~model ~nrows ~ncols
    alg =
  Notify.start_batchtime();
  List.iter (fun tw ->
	       List.iter (fun wt ->
			    do_nonunit_batch ~lazy_run:lazy_run ~send_mail:false
			      ~overwrite:overwrite
			      ~heuristic:heuristic
			      ~limits:limits
			      ~macro:macro
			      model
			      tw
			      ~nrows
			      ~ncols
			      (Printf.sprintf "%s %f" alg wt)
			      (
				[
				  "alg",alg;
				  "wt",(string_of_float wt);]
			      )
			 ) weights;
	    ) tile_weights;
  Notify.send_batch_completed_mail "Sliding_tiles_runs" "wted" alg
