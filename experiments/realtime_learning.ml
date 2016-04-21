(**

    @author snlemons
    @since 2010-11-10
*)

let solver_binary = Tiles_runs.solver_binary
let data_root = Tiles_runs.data_root
let instance_root = Tiles_runs.instance_root
let instance_specs = Tiles_runs.instance_specs

(* -------------------------iDTA* functions -----------------------*)

let do_idta_learning ?(time_limit = (Some Experiments.default_time_limit))
    ?(node_limit = (Some Experiments.default_node_limit))
    ?(bound = 10) ?(overwrite = false) ?(moves = "standard")
    ?(cost = "unit") ?(ideal = false) ?(round = 0) () =
  (** Performs DTA* learning on all random 4*4 puzzles in the user's
      data directory and stores the error values in files with the
      extension .learn *)
  (* Needs updated for 2-round learning approach *)
  let model = "eight_puzzle"
  and rows = 3
  and cols = 3 in
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
      Verb.pe Verb.debug "%s %s %s < %s %s %s" !solver_binary args learn_path
	prob_path limit_time limit_nodes;
      print_endline (Wrutils.str "%s %s %s < %s %s %s"
		       !solver_binary args learn_path prob_path
		       limit_time limit_nodes);
      Wrsys.with_subprocess_pipes
	~sig_on_exception:Sys.sigkill
	(Wrutils.str "%s %s %s < %s %s %s"
	   !solver_binary args learn_path prob_path limit_time limit_nodes)
	(fun to_solver from_solver ->
	   Datafile.write_header_pairs outch;
	   Datafile.write_pairs outch attrs;
	   Datafile
	     .write_pairs outch ["attrs", (Wrstr.encode_pairs attrs)];
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
  let do_learning_batches
      alg_attrs args model =
    List.iter (fun (_,_,_) ->
		 let batch_attrs = ["model", model;
				    "rows", string_of_int rows;
				    "cols", string_of_int cols;
				    "bucket", "9"; ] in
		   List.iter
		     (fun prob_attrs ->
			let prob_path = Rdb.path_for instance_root prob_attrs
			and attrs = alg_attrs @
			  ([("cost", cost); "moves", moves] @ prob_attrs) in
			  do_learning_run
			    (args ^ " --cost " ^ cost ^ " --moves " ^ moves)
			    prob_path attrs)
		     (Rdb.matching_attrs instance_root batch_attrs))
      !instance_specs in
    (let alg = if ideal then
       "idtastar_ideal_learn"
     else
       "idtastar_learn" in
     let prev_path = if (round = 0) then
       "fake.learn"
     else
       let attrs = (List.hd
		      (Rdb.matching_attrs data_root
			 ["alg", alg;
			  "model", model;
			  "rows", string_of_int rows;
			  "cols", string_of_int cols;
			  "bucket", "9";
			  "round", "0"; ])) in
	 Wrutils.str "%s.learn"
	   (Rdb.path_for data_root
	      ((Rdb.filter_attrs
		  ["type"; "num"] attrs)@[("num","condensed")])) in
       do_learning_batches ["alg", alg; "round", string_of_int round;]
	 (Wrutils.str "%s %d %d %s" alg bound round prev_path) model;
       Notify.send_batch_completed_mail "learning" model "dta_learn")

(* Functions for condensing iDTA* learning *)

(* pulling in a bunch of things we need to be able to build iDTA*
   learning table *)
type idta_prob_table = Idtastar_ideal.prob_table
let idta_create_prob_table = Idtastar_ideal.create_prob_table
let idta_update_total = Idtastar_ideal.update_total
let idta_add_sample = Idtastar_ideal.add_sample
let idta_add_dist = Idtastar_ideal.add_dist
let idta_load_table = Idtastar_ideal.load_table
let idta_output_table = Idtastar_ideal.output_table

let idta_read_learning_file accuracy tbl file_name round =
  let infile = open_in file_name in
  let all_lines = Wrio.input_lines infile in
  let branch_factor = float_of_string (Wrlist.last all_lines)
  and lines = Wrlist.butlast all_lines in
  let trunc x = Functions.trunc_to x accuracy in
    (
      close_in infile;
      let handle_line line =
	if (round == 0) then
	  let feature = (trunc (float_of_string
		      (List.hd (Wrstr.split_white line))))
	  and value = (trunc (float_of_string
				(List.nth
				   (Wrstr.split_white line) 1)))
	  in
	    (idta_update_total tbl feature;
	     idta_add_sample tbl feature value)
	else
	  let feature = (trunc (float_of_string
				  (List.hd (Wrstr.split_white line))))
	  and values = List.map (fun x -> (trunc (float_of_string x)))
	    (List.tl (Wrstr.split_white line)) in
	    (for i = 1 to (List.length values) do
	       idta_add_dist tbl feature i
		 (List.nth values (i-1))
	     done) in
	List.iter handle_line lines
    );
    branch_factor

let condense_idta_learning ?(accuracy = 0) ?(moves = "standard")
    ?(cost = "unit") ?(ideal = false) ?(round = 0) () =
  (** Takes the many .learn files created by do_idta_learning and
      compiles them into a single table of the form h_value, times_seen;
      depth, h_error_value, times_error_value_seen *)
  (* Needs modifications for new two-round learning approach! *)
  let alg = if ideal then
    "idtastar_ideal_learn"
  else
    "idtastar_learn"
  and model = "eight_puzzle"
  and rows = 3
  and cols = 3 in
  let batch_attrs = ["alg", alg;
		     "model", model;
		     "rows", string_of_int rows;
		     "cols", string_of_int cols;
		     "bucket", "9";
		     "round", string_of_int round; ] in

  let tbl = if (round = 0) then
    idta_create_prob_table accuracy
  else
    let prev_path =
      let attrs = (List.hd
		     (Rdb.matching_attrs data_root
			["alg", alg;
			 "model", model;
			 "rows", string_of_int rows;
			 "cols", string_of_int cols;
			 "bucket", "9";
			 "round", "0"; ])) in
	Wrutils.str "%s.learn"
	  (Rdb.path_for data_root
	     ((Rdb.filter_attrs
		 ["type"; "num"] attrs)@[("num","condensed")])) in
      idta_load_table (prev_path) in
    ((* read in the values and condense them to one table *)
      let branch_factors =
	List.map
	  (fun prob_attrs ->
	     let attrs = prob_attrs in
	     let run_file =
	       (Rdb.path_for data_root
		  (Rdb.filter_attrs ["type"] attrs)) in
	       idta_read_learning_file accuracy tbl run_file round)
	  (List.filter (fun l ->
			  (Wrlist.count (fun (_, v) ->
					   (Wrstr.contains
					      ".learn" v)) l) > 0)
	     (Rdb.matching_attrs data_root batch_attrs)) in
	(* output values to a single file *)
      let prob_attrs = (List.hd
			  (Rdb.matching_attrs data_root batch_attrs)) in
      let attrs = prob_attrs in
      let out_path = Wrutils.str "%s.learn"
	(Rdb.path_for data_root
	   ((Rdb.filter_attrs ["type"; "num"] attrs)@[("num","condensed")])) in
	(tbl.Idtastar_ideal.branch_factor <-
	   (List.fold_left (+.) 0.
	      branch_factors)/.(float (List.length branch_factors));
	 idta_output_table out_path tbl))


(* ------------------------ DTA* functions -------------------------*)

let do_dta_learning ?(time_limit = (Some Experiments.default_time_limit))
    ?(node_limit = (Some Experiments.default_node_limit))
    ?(bound = 10) ?(overwrite = false) ?(moves = "standard")
    ?(cost = "unit") () =
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
      Verb.pe Verb.debug "%s %s %s < %s %s %s" !solver_binary args learn_path
	prob_path limit_time limit_nodes;
      print_endline (Wrutils.str "%s %s %s < %s %s %s"
		       !solver_binary args learn_path prob_path limit_time limit_nodes);
      Wrsys.with_subprocess_pipes
	~sig_on_exception:Sys.sigkill
	(Wrutils.str "%s %s %s < %s %s %s"
	   !solver_binary args learn_path prob_path limit_time limit_nodes)
	(fun to_solver from_solver ->
	   Datafile.write_header_pairs outch;
	   Datafile.write_pairs outch attrs;
	   Datafile
	     .write_pairs outch ["attrs", (Wrstr.encode_pairs attrs)];
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
  let do_learning_batches
      alg_attrs args model =
    List.iter (fun (_,rows,cols) ->
		 let batch_attrs = ["model", model;
				    "rows", "3";
				    "cols", "3";
				    "bucket", "9"; ] in
		   List.iter
		     (fun prob_attrs ->
			let prob_path = Rdb.path_for instance_root prob_attrs
			and attrs = alg_attrs @
			  ([("cost", cost); "moves", moves] @ prob_attrs) in
			  do_learning_run
			    (args ^ " --cost " ^ cost ^ " --moves " ^ moves)
			    prob_path attrs)
		     (Rdb.matching_attrs instance_root batch_attrs))
      !instance_specs in
    (let alg = "dtastar_learn" in
       do_learning_batches ["alg", alg]
	 (Wrutils.str "%s %d" alg bound) "eight_puzzle";
       Notify.send_batch_completed_mail "learning" "eight_puzzle" "dta_learn")

(* Functions for condensing DTA* learning *)

(* pulling in a bunch of things we need to be able to build DTA*
   learning table *)
type dta_prob_table = Dtastar_old.prob_table
let dta_create_prob_table = Dtastar_old.create_prob_table
let dta_update_total = Dtastar_old.update_total
let dta_add_sample = Dtastar_old.add_sample
let dta_output_table = Dtastar_old.output_table

let dta_read_learning_file accuracy tbl file_name =
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
	   (dta_update_total tbl feature horizon;
	    (for i = 1 to (List.length values) do
	       dta_add_sample tbl feature i
		 (List.nth values (i-1))
	     done)) in
	List.iter handle_line lines
    );
    branch_factor

let condense_dta_learning ?(accuracy = 0) ?(moves = "standard")
    ?(cost = "unit") () =
  (** Takes the many .learn files created by do_dta_learning and
      compiles them into a single table of the form h_value, times_seen;
      depth, h_error_value, times_error_value_seen *)
  let tbl = dta_create_prob_table accuracy in
  let alg = "dtastar_learn"
  and model = "eight_puzzle"
  and rows = 3
  and cols = 3 in
  let batch_attrs = ["model", model;
		     "rows", string_of_int rows;
		     "cols", string_of_int cols;
		     "alg", alg;
		     "cost", cost;
		     "moves", moves;
		     "bucket", "9"] in
    ((* read in the values and condense them to one table *)
      let branch_factors =
	List.map
	  (fun attrs ->
	     let run_file =
	       (Rdb.path_for data_root
		  (Rdb.filter_attrs ["type"] attrs)) in
	       dta_read_learning_file accuracy tbl run_file)
	  (List.filter (fun l ->
			  (Wrlist.count (fun (_, v) ->
					   (Wrstr.contains
					      ".learn" v)) l) > 0)
	     (Rdb.matching_attrs data_root batch_attrs)) in
	(* output values to a single file *)
      let attrs = (List.hd
		     (Rdb.matching_attrs data_root batch_attrs)) in
      let out_path = Wrutils.str "%s.learn"
	(Rdb.path_for data_root
	   ((Rdb.filter_attrs ["type"; "num"] attrs)@[("num","condensed")])) in
	(tbl.Dtastar_old.branch_factor <-
	   (List.fold_left (+.) 0.
	      branch_factors)/.(float (List.length branch_factors));
	 dta_output_table out_path tbl))
