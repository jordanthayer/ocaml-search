(** Runs batches of pancake puzzle instances *)


let data_root = Experiments.data_root ^ "pancake"
and instance_root = Experiments.instance_root ^ "pancake"
and solver_binary = ref (Experiments.bin_root ^ "pancake_solver")


type heuristic = 
    PDB
  | Gap

type db = 
    Dynamic of float
  | Static of int


let call_solver ?(max_mem = false) time_limit node_limit args prob_path attrs outch =
  (** calls the external solver on [args] and [board_name], redirecting its
      output to [outch].  all logging is done within here.  returns cost, time
      pair *)
  let args = match max_mem with
      true -> Wrutils.str "--max_mem %s" args
    | false -> args in
  let limit_args =
    match time_limit with
	Some(t) ->  Wrutils.str "--time %f %s" t args
      | _ -> args in
  let str_to_solver = 
    (try 
       (ignore (List.assoc "pdb" attrs);
	(Wrutils.str "%s --make-pdb %s < %s"
	   !solver_binary limit_args prob_path))
     with Not_found ->
	(Wrutils.str "%s %s < %s"
	   !solver_binary limit_args prob_path)	 
    ) in
    Verb.pe Verb.toplvl "calling this: %s\n" str_to_solver;
  let go () =
    Wrsys.with_subprocess_all_pipes
      str_to_solver 
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
	     Datafile.write_pairs outch ["attrs", (Wrstr.encode_pairs attrs)];
	     let res = Datafile.pipe_data from_solver outch in
	       Datafile.write_trailer_pairs outch;
	       res) in
    Notify.try_and_email_on_fail go "Pancake_runs"


let do_run max_mem overwrite time_limit node_limit args prob_path attrs =
  (** sets up run file and calls solver on board_name *)
  let limits = [(match time_limit with None -> Limit.Never
		   | Some t -> Limit.Time t);
		(match node_limit with None -> Limit.Never
		   | Some n -> Limit.Generated n)] in
  let run_file = Rdb.path_for data_root
    (Rdb.filter_attrs ["type"] attrs) in
    if Sys.file_exists run_file &&
      Datafile.seems_complete run_file  &&
      (*valid_df (Datafile.load run_file)  &&*)
      not overwrite &&
      not ((Limit.has_new_limits run_file limits) &&
	           not (Datafile.run_finished run_file))
    then
      Wrutils.pr "%s exists - skipping!\n%!" run_file
    else
      (Wrio.with_outfile run_file
	 (fun outch ->
	    Wrutils.pr "Running %s on %s...%!" args
	      (Wrfname.make_relative instance_root
		 prob_path);
	    let c,t =
	      call_solver ~max_mem:max_mem 
		time_limit node_limit args prob_path attrs outch in
	      Wrutils.pr "%.1f in %.3f.\n%!" c t))


let standard_costs = ["unit"; "sum"; (*"sqrt_sum"*)]
let standard_pdbsizes = [(*2;*) (* 5; *) 7;]

let do_batch max_mem overwrite time_limit node_limit args alg_attrs batch_attrs =
  List.iter
    (fun prob_attrs ->
       List.iter
	 (fun cost ->
	    List.iter
	      (fun pdbsize ->
		 do_run max_mem overwrite time_limit node_limit
		   (Wrutils.str "--cost %s --pdb-size %s %s"
		      cost (string_of_int pdbsize) args)
		   (Rdb.path_for instance_root prob_attrs)
		   ((alg_attrs @ ["cost", cost;
				  "pdb", string_of_int pdbsize;]
		     @ prob_attrs)))
		   standard_pdbsizes)
	      standard_costs)
	 (Rdb.matching_attrs instance_root batch_attrs)


(************************************************************)
(* Using the gap heuristic.                                 *)
(************************************************************)

let do_gap_batch max_mem overwrite time_limit node_limit args alg_attrs 
    batch_attrs
    =
  let (ncakes) = List.assoc "ncakes" batch_attrs in
  let prob_attrs = [ "type", "instance"; "ncakes", ncakes; ] in
  let insts = Rdb.matching_attrs instance_root prob_attrs in
    List.iter
      (fun inst_attrs ->
	 let path = Rdb.path_for instance_root inst_attrs in
	   do_run 
	     max_mem
	     overwrite 
	     time_limit 
	     node_limit 
	     args 
	     path 
	     (alg_attrs @ inst_attrs))
      insts



(* batch run callers *)
let do_basic_batch ?(overwrite = false)
    ?(time_limit = Some (Experiments.default_time_limit))
    ?(node_limit = Some (Experiments.default_node_limit))
    ?(batch_attrs = ["ncakes", "10"])
    ?(heuristic = PDB)
    ?(max_mem=false)
    alg =
  (* greedy, speedy, a_star *)
  Notify.start_batchtime();
  do_batch max_mem overwrite time_limit node_limit alg ["alg", alg]
    (batch_attrs @ ["type", "instance"]);
  Notify.send_batch_completed_mail "Pancake_runs" "Basic" alg


let do_wted_batches ?(overwrite = false)
    ?(weights = Experiments.low_res_weights)
    ?(time_limit = Some (Experiments.default_time_limit))
    ?(node_limit = Some (Experiments.default_node_limit))
    ?(batch_attrs = ["ncakes", "10"])
    ?(heuristic = PDB)
    ?(max_mem=false)
    alg =
  let do_batch = match heuristic with 
      PDB -> do_batch 
    | Gap -> do_gap_batch in

    Notify.start_batchtime();
    List.iter
      (fun wt ->
	 do_batch max_mem overwrite time_limit node_limit
	   (Wrutils.str "%s %f" alg wt)
	   ["alg", alg; "wt", string_of_float wt]
	   (batch_attrs @ ["type", "instance"])) weights;
    Notify.send_batch_completed_mail "Pancake_runs" "Weighted" alg


let do_msc_k_wted_batches ?(overwrite = false)
    ?(time_limit = Some (Experiments.default_time_limit))
    ?(node_limit = Some (Experiments.default_node_limit))
    ?(c_size = Experiments.full_beams) ?(ks = Experiments.ks)
    ?(weights = Experiments.low_res_weights) ?(batch_attrs = ["ncakes", "10"])
    ?(heuristic = PDB)
    ?(max_mem=false)
    alg =
  let do_batch = match heuristic with 
      PDB -> do_batch 
    | Gap -> do_gap_batch in
  Notify.start_batchtime();
  List.iter
    (fun c ->
       List.iter
	 (fun k ->
	    List.iter
	      (fun wt ->
		 do_batch max_mem overwrite time_limit node_limit
		   (Wrutils.str "%s %f %i %i" alg wt k c)
		   ["alg", alg;
		    "commit", string_of_int c;
		    "k", string_of_int k;
		    "wt", string_of_float wt;]
		   (batch_attrs @ ["type", "instance"])) weights) ks) c_size;
  Notify.send_batch_completed_mail "Pancake_runs" "MSC_K_WTED" alg


let do_beam_batches ?(overwrite = false)
    ?(beams = Experiments.full_beams)
    ?(time_limit = Some (Experiments.default_time_limit))
    ?(node_limit = Some (Experiments.default_node_limit))
    ?(batch_attrs = ["ncakes", "10"])
    ?(heuristic = PDB)
    ?(max_mem=false)
    alg =
  let do_batch = match heuristic with 
      PDB -> do_batch 
    | Gap -> do_gap_batch in
  Notify.start_batchtime();
  List.iter
    (fun beam ->
       do_batch max_mem overwrite time_limit node_limit
	 (Wrutils.str "%s %i" alg beam)
	 ["alg", alg; "beam_width", string_of_int beam]
	 (batch_attrs @ ["type", "instance"])) beams;
  Notify.send_batch_completed_mail "Pancake_runs" "Beam" alg


let do_new_beam_batches ?(overwrite = false)
    ?(beams = Experiments.full_beams)
    ?(time_limit = Some (Experiments.default_time_limit))
    ?(node_limit = Some (Experiments.default_node_limit))
    ?(batch_attrs = ["ncakes", "10"])
    ?(sort_predicate = "f")
    ?(heuristic = PDB)
    ?(max_mem=false)
    alg =
  let do_batch = match heuristic with 
      PDB -> do_batch 
    | Gap -> do_gap_batch in
  Notify.start_batchtime();
  List.iter
    (fun beam ->
       do_batch max_mem overwrite time_limit node_limit
	 (Wrutils.str "%s %i %s" alg beam sort_predicate)
	 ["alg", alg; "beam_width", string_of_int beam;
	 "sort_predicate",sort_predicate]
	 (batch_attrs @ ["type", "instance"])) beams;
  Notify.send_batch_completed_mail "Pancake_runs" "new_beam" alg


let do_bucketed_stratified_new_beam_batches ?(overwrite = false)
    ?(beams = Experiments.full_beams)
    ?(time_limit = Some (Experiments.default_time_limit))
    ?(node_limit = Some (Experiments.default_node_limit))
    ?(batch_attrs = ["ncakes", "10"])
    ?(sort_predicate = "f")
    ?(bucket_size = 1.0)
    ?(heuristic = PDB)
    ?(max_mem=false)
    alg =
  let do_batch = match heuristic with 
      PDB -> do_batch 
    | Gap -> do_gap_batch in
  Notify.start_batchtime();
  List.iter
    (fun beam ->
       do_batch max_mem overwrite time_limit node_limit
	 (Wrutils.str "%s %i %s %f" alg beam sort_predicate bucket_size)
	 ["alg", alg; 
	  "beam_width", string_of_int beam;
	  "sort_predicate",sort_predicate;
	  "bucket_size", string_of_float bucket_size;
	 ]
	 (batch_attrs @ ["type", "instance"])) beams;
  Notify.send_batch_completed_mail "Pancake_runs" "new_beam" alg



let do_optimistic_batches ?(overwrite = false)
    ?(weights = Experiments.low_res_weights)
    ?(opt = Experiments.optimisms)
    ?(time_limit = Some (Experiments.default_time_limit))
    ?(node_limit = Some (Experiments.default_node_limit))
    ?(batch_attrs = ["ncakes", "10"])
    ?(heuristic = PDB)
    ?(max_mem=false)
    alg =
  let do_batch = match heuristic with 
      PDB -> do_batch 
    | Gap -> do_gap_batch in
  Notify.start_batchtime();
  List.iter
    (fun o ->
       List.iter
	 (fun wt ->
	    do_batch max_mem overwrite time_limit node_limit
	      (Wrutils.str "%s %f %f" alg wt o)
	      ["alg", alg;
	       "wt", string_of_float wt;
	       "optimism", string_of_float o;]
	      (batch_attrs @ ["type", "instance"])) weights)
    opt;
  Notify.send_batch_completed_mail "Pancake_runs" "Optimistic" alg



let do_restarting_beam_batches ?(overwrite = false)
    ?(beams = Experiments.full_beams)
    ?(time_limit = Some (Experiments.default_time_limit))
    ?(node_limit = Some (Experiments.default_node_limit))
    ?(batch_attrs = ["ncakes", "10"])
    ?(sort_predicate = "f")
    ?(p_reserve = 0.95)
    ?(reserve_sort_predicate = "f")
    ?(depth_bound = Dynamic 1.5)
    ?(heuristic = PDB)
    ?(max_mem=false)
    alg =
  assert (p_reserve < 1.0);
  let (ncakes) = List.assoc "ncakes" batch_attrs in
  let do_batch = match heuristic with 
      PDB -> do_batch 
    | Gap -> do_gap_batch in

    Notify.start_batchtime();
    List.iter
      (fun beam ->
	 
	 let depth_bound = match depth_bound with
	     Static db -> db 
	   | Dynamic dp -> int_of_float (dp *. (float_of_string ncakes)) in
	 let total_beam_nodes = float_of_int (depth_bound * beam) in
	 let all_nodes = int_of_float (total_beam_nodes /. p_reserve) in

	   do_batch max_mem overwrite time_limit node_limit
	     (Wrutils.str "%s %i %s %s %i %i" alg beam sort_predicate
		reserve_sort_predicate depth_bound all_nodes)
	     ["alg", alg; 
	      "beam_width", string_of_int beam;
	      "sort_predicate",sort_predicate;
	      "reserve_sort_predicate",reserve_sort_predicate;
	      "depth_bound",(string_of_int depth_bound);
	      "node_capacity",(string_of_int all_nodes);
	     ]
	     (batch_attrs @ ["type", "instance"])) beams;
    Notify.send_batch_completed_mail "Pancake_runs" "new_beam" alg



let do_bulb_batches 
    ?(overwrite = false)
    ?(beams = Experiments.full_beams)
    ?(time_limit = Some (Experiments.default_time_limit))
    ?(node_limit = Some (Experiments.default_node_limit))
    ?(batch_attrs = ["ncakes", "10"])
    ?(depth_bound = Dynamic 1.5)
    ?(heuristic = PDB)
    ?(max_mem=false)
    alg =
  let (ncakes) = List.assoc "ncakes" batch_attrs in

  let do_batch = match heuristic with 
      PDB -> do_batch 
    | Gap -> do_gap_batch in
    
  let depth_bound = match depth_bound with
      Static db -> db 
    | Dynamic dp -> int_of_float (dp *. (float_of_string ncakes)) in
    

  Notify.start_batchtime();
  List.iter
    (fun beam ->
       let node_capacity = depth_bound * beam in
	 do_batch max_mem overwrite time_limit node_limit
	 (Wrutils.str "%s %i %i" alg beam node_capacity)
	 ["alg", alg; 
	  "beam_width", string_of_int beam;
	  "node_capacity",string_of_int node_capacity;
	 ]
	 (batch_attrs @ ["type", "instance"])) beams;
  Notify.send_batch_completed_mail "Pancake_runs" "Beam" alg


let do_wted_beam_batches ?(overwrite = false)
    ?(beams = Experiments.full_beams)
    ?(wts = Experiments.low_res_weights)
    ?(time_limit = Some (Experiments.default_time_limit))
    ?(node_limit = Some (Experiments.default_node_limit))
    ?(batch_attrs = ["ncakes", "10"])
    ?(sort_predicate = "f")
    ?(heuristic = PDB)
    ?(max_mem=false)
    alg =
  let do_batch = match heuristic with 
      PDB -> do_batch 
    | Gap -> do_gap_batch in
    Notify.start_batchtime();
    List.iter
      (fun beam ->
	 List.iter (fun wt -> 
		      do_batch max_mem overwrite time_limit node_limit
			(Wrutils.str "%s %i %s %f" alg beam
			   sort_predicate wt)
			["alg", alg; 
			 "beam_width", string_of_int beam;
			 "sort_predicate",sort_predicate;
			 "wt",  string_of_float wt;
			]
			(batch_attrs @ ["type", "instance"])) wts) beams;
    Notify.send_batch_completed_mail "Pancake_runs" "new_beam" alg


let do_wted_bf_beam_batches ?(overwrite = false)
    ?(beams = Experiments.full_beams)
    ?(wts = Experiments.low_res_weights)
    ?(time_limit = Some (Experiments.default_time_limit))
    ?(node_limit = Some (Experiments.default_node_limit))
    ?(batch_attrs = ["ncakes", "10"])
    ?(heuristic = PDB)
    ?(max_mem=false)
    alg =
  let do_batch = match heuristic with 
      PDB -> do_batch 
    | Gap -> do_gap_batch in
    Notify.start_batchtime();
    List.iter
      (fun beam ->
	 List.iter (fun wt -> 
		      do_batch max_mem overwrite time_limit node_limit
			(Wrutils.str "%s %i %f" alg beam wt)
			["alg", alg; 
			 "beam_width", string_of_int beam;
			 "wt",  string_of_float wt;
			]
			(batch_attrs @ ["type", "instance"])) wts) beams;
    Notify.send_batch_completed_mail "Pancake_runs" "new_beam" alg
