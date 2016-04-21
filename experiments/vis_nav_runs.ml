(**

    @author sna4
    @since 2010-03-15
*)

let data_root = Experiments.data_root ^ "vis_nav"
and instance_root = Experiments.instance_root ^ "vis_nav_instances"
and solver_binary = Experiments.bin_root ^ "vis_nav_solver"

let instance_specs =
  (** this list lets you specify which tile puzzles to run on.  The
      first number says how many there are (if there are more, it
      skips the remaining ones, if there are less, not sure what
      happens).  The next 2 numbers are the size of the problems you
      want to actually run on.   *)
  [ 10, 100, 100, 5;
    10, 100, 100, 10;
    10, 100, 100, 15;
    10, 100, 100, 20;
    10, 100, 100, 25;
    10, 100, 100, 30;
    10, 100, 100, 40;
    10, 100, 100, 50;
    10, 100, 100, 100;]

let if_not_exists attrs i f =
  (** Generates a new problem if one of the given configuration does
      not exist.  [attrs] the problem configuration [i] the instance
      number [f] a function for generating new problems *)
     let _, time =
       Wrsys.with_time (fun () ->
			  let path = Rdb.path_for instance_root attrs in
			    if Sys.file_exists path then
			      Wrutils.pr "%s exists, skipping...\n%!" path
			    else
			      let p = f () in
				Wrio.with_outfile path
				  (fun ch -> Vis_nav_instance.write p ch))
     in Wrutils.pr "Wrote problem %d, time=%f.\n%!" i time

   let feasible_instance width height n_objs =
     (** Generates a single feasible instance of size [widht] by
	 [height] and with [n_objs] objects *)
  Vis_nav_instance.gen_instance width height
    {Vis_nav_instance.x=0.;Vis_nav_instance.y=0.}
    {Vis_nav_instance.x=width;Vis_nav_instance.y=height;} n_objs

let make_instances ?(model = "random") width height n_objs num =
  (** Makes a fixed number of instances of a given size.  [num] the
      number of instances to generate *)
  Random.self_init ();
  (for i = 1 to num do
     let attrs = [ "type", "instance";
		   "model", model;
		   "width", string_of_int (truncate width);
		   "height", string_of_int (truncate height);
		   "n_objs", string_of_int n_objs;
		   "num", string_of_int i; ] in
       if_not_exists attrs i
	 (fun () -> feasible_instance width height n_objs)
   done;
   Notify.send_batch_completed_mail "Vis_nav" "generation" (Wrutils.str "%i,%i %i" (truncate width) (truncate height) n_objs))

(************** running experiments **************)

let call_solver time_limit node_limit args prob_path attrs outch =
  (** calls the external solver on [args] and [board_name],
      redirecting its output to [outch].  all logging is done within
      here.  returns cost, time pair. *)
  let limit_time =
    match time_limit with
	Some(t) -> Wrutils.str " --time %f " t
      | _ -> "" in

  let limit_nodes =
    match node_limit with
       	Some(t) -> Wrutils.str " --gen %d " t
      | _ -> "" in
    (* this prints out the thing that is sent to the command line. *)
  let go () =
    Wrsys.with_subprocess_pipes

      ~sig_on_exception:Sys.sigkill

      (Wrutils.str "%s %s%s%s < %s"
	 solver_binary limit_time limit_nodes args prob_path)

      (fun to_solver from_solver ->
	 Datafile.write_header_pairs outch;
	 Datafile.write_pairs outch attrs;
	 Datafile.write_pairs outch ["attrs", (Wrstr.encode_pairs attrs)];
	 let res = Datafile.pipe_data from_solver outch in
	   Datafile.write_trailer_pairs outch;
	   res) in
    Notify.try_and_email_on_fail go "Vis_nav_runs"

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
      (* has new limits and didn't finish last go around *)
      not ((Limit.has_new_limits run_file limits) &&
	           not (Datafile.run_finished run_file)) &&
      not (overwrite)
    then
      Wrutils.pr "%s exists - skipping!\n%!" run_file
    else
      Wrio.with_outfile run_file
	(fun outch ->
	   Wrutils.pr "Running %s on %s ...%!" args (Wrfname.make_relative instance_root
						      prob_path);
	   (* the number after the some allows people to specify a
	      limit on the number of nodes to be generated. *)

	   let c,t =
	     call_solver
	       time_limit node_limit args prob_path attrs outch in
	     Wrutils.pr "%.1f in %.3f.\n%!" c t)

let do_batches
    overwrite
    time_limit
    node_limit
    alg_attrs
    args model =
  (** runs an alg on all problems *)
  List.iter (fun (_,width,height,n_objs) ->
	       let batch_attrs = ["model", model;
				  "width", string_of_int width;
				  "height", string_of_int height;
				  "n_objs", string_of_int n_objs; ] in
		 List.iter
		   (fun prob_attrs ->
		      let prob_path = Rdb.path_for instance_root prob_attrs
		      and attrs = alg_attrs @
			prob_attrs in
			do_run overwrite time_limit node_limit
			  (args)
			  prob_path attrs)
		   (Rdb.matching_attrs instance_root batch_attrs))
    instance_specs

let make_dta_samples () =
  (** creates random instances for DTA* to learn from *)
  List.iter (fun x ->
	       make_instances ~model:"dta_learning" 100. 100. x 20)
    [5;10;15;20;25;30;40;50;100]

let do_dta_learning ?(time_limit = (Some Experiments.default_time_limit))
    ?(node_limit = (Some Experiments.default_node_limit))
    ?(bound = 10) ?(overwrite = false) () =
  (** Performs DTA* learning on all random 4*4 puzzles in the user's
      data directory and stores the error values in files with the
      extension .learn *)
  let call_learning_solver args prob_path learn_path attrs outch =
    let limit_time =
      match time_limit with
	  Some(t) -> Wrutils.str " --time %f " t
	| _ -> "" in
    let limit_nodes =
      match node_limit with
       	  Some(t) -> Wrutils.str " --gen %d " t
	| _ -> "" in
      (* this prints out the thing that is sent to the command line. *)
      Verb.pe Verb.debug "%s %s %s < %s %s %s" solver_binary args learn_path
	prob_path limit_time limit_nodes;
      print_endline (Wrutils.str "%s %s %s < %s %s %s"
		       solver_binary args learn_path prob_path limit_time
		       limit_nodes);
      Wrsys.with_subprocess_pipes
	~sig_on_exception:Sys.sigkill
	(Wrutils.str "%s %s %s < %s %s %s"
	   solver_binary args learn_path prob_path limit_time limit_nodes)
	(fun to_solver from_solver ->
	   Datafile.write_header_pairs outch;
	   Datafile.write_pairs outch attrs;
	   Datafile.write_pairs outch ["attrs", (Wrstr.encode_pairs attrs)];
	   let res = Datafile.pipe_data from_solver outch in
	     Datafile.write_trailer_pairs outch;
	     res) in
  let do_learning_run args prob_path attrs =
    (** sets up run file and calls solver on problem *)
    let limits = [(match time_limit with None -> Limit.Never
		     | Some t -> Limit.Time t);
		  (match node_limit with None -> Limit.Never
		     | Some n -> Limit.Generated n)] in
    let run_file = Rdb.path_for data_root
      (Rdb.filter_attrs ["type"] attrs) in
      if (Sys.file_exists run_file) &&
	(Datafile.seems_complete run_file) &&
	(* has new limits and didn't finish last go around *)
	not ((Limit.has_new_limits run_file limits) &&
	       not (Datafile.run_finished run_file)) &&
	not (overwrite)
      then
	Wrutils.pr "%s exists - skipping!\n%!" run_file
      else
	Wrio.with_outfile run_file
	  (fun outch ->
	     Wrutils.pr "Running %s on %s ...%!" args
	       (Wrfname.make_relative instance_root
		  prob_path);
	     (* the number after the some allows people to specify a
		limit on the number of nodes to be generated. *)
	     let c,t =
	       call_learning_solver args
		 prob_path (Wrutils.str "%s.learn" run_file) attrs outch in
	       Wrutils.pr "%.1f in %.3f.\n%!" c t) in
  let do_learning_batches alg_attrs args model =
    List.iter (fun (_,width,height,n_objs) ->
		 let batch_attrs = ["model", model;
				    "type", "instance";
				    "width", string_of_int width;
				    "height", string_of_int height;
				    "n_objs", string_of_int n_objs; ] in
		   List.iter
		     (fun prob_attrs ->
			let prob_path = Rdb.path_for data_root prob_attrs
			and attrs = alg_attrs @
			  prob_attrs in
			  do_learning_run
			    args
			    prob_path attrs)
		     (Rdb.matching_attrs data_root batch_attrs))
      instance_specs in
    (let alg = "dtastar_learn" in
       do_learning_batches ["alg", alg]
	 (Wrutils.str "%s %d" alg bound) "dta_learning";
       Notify.send_batch_completed_mail "learning" "random" "dta_learn")

(* pulling in a bunch of things we need to be able to build DTA*
   learning table *)
type prob_table = Dtastar_old.prob_table
let new_value = Dtastar_old.new_value
let create_prob_table = Dtastar_old.create_prob_table
let update_total = Dtastar_old.update_total
let add_sample = Dtastar_old.add_sample
let output_table = Dtastar_old.output_table

let read_learning_file accuracy tbl file_name =
  let infile = open_in file_name in
  let all_lines = Wrio.input_lines infile in
  let branch_factor = float_of_string (Wrlist.last all_lines)
  and lines = Wrlist.butlast all_lines in
  let trunc x = Functions.trunc_to x accuracy in
    (
      close_in infile;
      let handle_line line =
	let feature = (trunc (float_of_string
				   (List.hd (Wrstr.split_white line))))
	and values = List.map (fun x -> (trunc (float_of_string x)))
	  (List.tl (Wrstr.split_white line)) in
	let horizon = List.length values in (* actually 1 more than
					       this *)
	  (update_total tbl feature horizon;
	   (for i = 1 to (List.length values) do
	      add_sample tbl feature i
		(List.nth values (i-1))
	    done)) in
	List.iter handle_line lines
    );
    branch_factor

let condense_learning ?(accuracy = 2) n_objs =
  (** Takes the many .learn files created by do_dta_learning and
      compiles them into a single table of the form h_value, times_seen;
      depth, h_error_value, times_error_value_seen *)
  let alg = "dtastar_learn"
  and tbl = create_prob_table accuracy
  and model = "dta_learning"
  and width = truncate 100.
  and height = truncate 100. in
  let batch_attrs = ["model", model;
		     "type", "instance";
		     "width", string_of_int width;
		     "height", string_of_int height;
		     "n_objs", n_objs ] in
    ((* read in the values and condense them to one table *)
      let branch_factors =
	List.map
	  (fun prob_attrs ->
	     let attrs = [("alg", alg)] @
	       prob_attrs in
	     let run_file = Wrutils.str "%s.learn"
	       (Rdb.path_for data_root
		  (Rdb.filter_attrs ["type"] attrs)) in
	       read_learning_file accuracy tbl run_file)
	  (Rdb.matching_attrs data_root batch_attrs) in
	(* output values to a single file *)
      let prob_attrs = (List.hd
			  (Rdb.matching_attrs data_root batch_attrs)) in
      let attrs = ([("alg", alg)] @
		     prob_attrs) in
      let out_path = Wrutils.str "%s.learn"
	(Rdb.path_for data_root
	   ((Rdb.filter_attrs ["type"; "num"] attrs)@[("num","condensed")])) in
	(tbl.Dtastar_old.branch_factor <-
	   (List.fold_left (+.) 0.
	      branch_factors)/.(float (List.length branch_factors));
	 output_table out_path tbl))

let do_realtime_batch ?(time_limit = (Some Experiments.default_time_limit))
    ?(node_limit = (Some Experiments.default_node_limit))
    ?(bounds = [1;3;5;10]) ?(grains = [1; 3; 5; 10; 20; 50]) ?(r_vals = [0.001;0.01;0.1;0.3;0.5;0.9]) ?(branch_factors = [5.;10.;15.;20.;25.;30.]) ?(overwrite = false) () =
  let do_dta_batches
      overwrite
      time_limit
      node_limit
      alg_attrs
      args model =
    (** runs an alg on all problems *)
    List.iter
      (fun (_,width,height,n_objs) ->
	 let learn_batch_attrs = ["model", "dta_learning";
				  "type", "instance";
				  "width", string_of_int width;
				  "height", string_of_int height;
				  "n_objs", string_of_int n_objs ] in
	 let learn_prob_attrs = (List.hd
				   (Rdb.matching_attrs
				      data_root learn_batch_attrs)) in
	 let learn_attrs = ([("alg", "dtastar_learn")] @
			      learn_prob_attrs) in
	 let learn_path = Wrutils.str "%s.learn"
	   (Rdb.path_for data_root
	      ((Rdb.filter_attrs ["type"; "num"] learn_attrs)@
		 [("num","condensed")])) in
	 let batch_attrs = ["model", model;
			    "width", string_of_int width;
			    "height", string_of_int height;
			    "n_objs", string_of_int n_objs; ] in
	   List.iter
	     (fun prob_attrs ->
		let prob_path = Rdb.path_for instance_root prob_attrs
		and attrs = alg_attrs @
		  prob_attrs in
		  do_run overwrite time_limit node_limit
		    (Wrutils.str "%s %s" args learn_path)
		    prob_path attrs)
	     (Rdb.matching_attrs instance_root batch_attrs))
      instance_specs in
    (** runs real-time algorithms on all instances *)
    (*(let alg = "rtastar" in
       List.iter
	 (fun bound ->
	    (do_batches overwrite time_limit
	       node_limit ["alg", alg;
			   "bound",
			   (string_of_int bound)]
	       (Wrutils.str "%s %d" alg bound) "random"))
	 bounds);
    (let alg = "srtastar" in
       List.iter
	 (fun bound ->
	    (do_batches overwrite time_limit
	       node_limit ["alg", alg;
			   "bound",
			   (string_of_int bound)]
	       (Wrutils.str "%s %d" alg bound) "random"))
	 bounds);*)
    (let alg = "dtastar" in
       List.iter
	      (fun grain ->
		 (List.iter
		    (fun r -> do_dta_batches overwrite time_limit
		       node_limit ["alg", alg;
				   "r", (string_of_float r);
				   "grain_size", (string_of_int grain)]
		       (Wrutils.str "%s %d %d %f" alg grain 10 r)
		       "random") r_vals))
	      grains);
    (let alg = "astar" in
       do_batches overwrite time_limit
	 node_limit ["alg", alg] alg
	 "random");
    Notify.send_batch_completed_mail "realtime" "basic" "all"
