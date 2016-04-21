(**

   For running towers of hanoi experiments

*)


type pdb_type = 
    Wilt
  | Felner
  | Felner_packed

let instance_root =
  Filename.concat Experiments.instance_root "hanoi"

let solver_binary =
  ref (Filename.concat Experiments.bin_root "hanoi_solver")


type db = 
    Dynamic of float
  | Static of int
  | StaticNodes of int



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
      Notify.try_and_email_on_fail (run cmd) "Hanoi_runs"


let do_run overwrite limits args attrs prob_path =
  (** [do_run overwrite limits macro args attrs prob_path] performs a
      single run on the given problem.  The results are stored in the
      RDB specified by data_root. *)
  let data_root = Filename.concat Experiments.data_root "hanoi" in
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


let do_batch ?(send_mail=true) 
    ?(pdb_type= Felner)
    ?(overwrite=false) ?(cost="Unit") ?(limits=[])
    ~npegs ~ndisks ~pdb_up ~pdb_down args alg_attrs =
  (** does a hanoi run with the specified parameters. *)
  let batch_attrs = [
      "type","instance";
    "npegs", string_of_int npegs;
    "ndisks", string_of_int ndisks; 
  ] in

  let pdb_string = match 
    pdb_type with
	Wilt -> "--old_pdb"
      | Felner -> "" 
      | Felner_packed -> "--bp_pdb" in

  let pdb_type = match 
    pdb_type with
	Wilt -> "Wilt"
      | Felner -> "Felner" 
      | Felner_packed -> "Felner_packed" in

  let pdb_data = [
    "pdb_type",pdb_type;
    "pdb_up", string_of_int pdb_up;
    "pdb_down", string_of_int pdb_down;
  ] in

  let batch_str = Printf.sprintf "npegs=%d, ndisks=%d" npegs ndisks in
  let attr_sets = Rdb.matching_attrs instance_root 
    (batch_attrs
    ) in
    if(send_mail) then
      Notify.start_batchtime ();

    List.iter (fun (inst_attrs:(string*string) list) ->
		 let instance_name_only = [Wrlist.last inst_attrs] in
		 let other_instance_attrs = 
		   List.tl (Wrlist.butlast inst_attrs) in
		 let cost_attr = [ "cost", cost ] in
		 let attrs = alg_attrs @ cost_attr @ 
		   other_instance_attrs @ 
		   pdb_data @ instance_name_only in
		 let args = Printf.sprintf 
		   "--cost %s --pdb-up %d --pdb-down %d %s %s" 
		   cost pdb_up pdb_down pdb_string args in
		 let prob_path = Rdb.path_for instance_root inst_attrs in
		   do_run overwrite limits args attrs prob_path)
      attr_sets;
    if(send_mail) then
      Notify.send_batch_completed_mail "Hanoi_runs" batch_str args




let do_basic_batch ?(overwrite = false)
    ?(pdb_type= Felner)
    ?(cost = "Unit")
    ?(limits=[
	(Limit.Time Experiments.default_time_limit);
	(Limit.Generated Experiments.default_node_limit);
      ])
    ~npegs ~ndisks ~pdb_up ~pdb_down
    alg =
  (* greedy, speedy, a_star *)
  let send_mail = false in
    Notify.start_batchtime();
    do_batch ~pdb_type:pdb_type ~send_mail ~overwrite ~cost ~limits 
      ~npegs ~ndisks ~pdb_up ~pdb_down
      alg ["alg", alg];
    Notify.send_batch_completed_mail "Hanoi_runs" "Basic" alg



let do_wted_batch ?(overwrite = false)
    ?(pdb_type= Felner)
    ?(cost = "Unit")
    ?(limits=[
	(Limit.Time Experiments.default_time_limit);
	(Limit.Generated Experiments.default_node_limit);
      ])
    ?(weights = Experiments.low_res_weights)
    ~npegs ~ndisks ~pdb_up ~pdb_down
    alg =
  (* algorithms with one float parameter *)
  let send_mail = false in
    Notify.start_batchtime();
    List.iter
      (fun wt ->
	 do_batch ~pdb_type:pdb_type ~send_mail ~overwrite ~cost ~limits 
	   ~npegs ~ndisks ~pdb_up ~pdb_down
	   (Wrutils.str "%s %f" alg wt)
	   ["alg", alg; "wt",string_of_float wt])
      weights;
    Notify.send_batch_completed_mail "Hanoi_runs" "Weighted" alg


let do_beam_batch ?(overwrite = false)
    ?(pdb_type= Felner)
    ?(cost = "Unit")
    ?(limits=[
	(Limit.Time Experiments.default_time_limit);
	(Limit.Generated Experiments.default_node_limit);
      ])
    ?(beam_widths = Experiments.full_beams)
    ~npegs ~ndisks ~pdb_up ~pdb_down
    alg =
  (* algoritjms with one integer parameter *)
  let send_mail = false in
    Notify.start_batchtime();
    List.iter
      (fun bw ->
	 do_batch ~pdb_type:pdb_type ~send_mail ~overwrite ~cost ~limits 
	   ~npegs ~ndisks ~pdb_up ~pdb_down
	   (Wrutils.str "%s %d" alg bw)
	   ["alg", alg; "beam_width",string_of_int bw])
      beam_widths;
    Notify.send_batch_completed_mail "Hanoi_runs" "Old Beam" alg



let do_new_beam_batch ?(overwrite = false)
    ?(pdb_type= Felner)
    ?(cost = "Unit")
    ?(limits=[
	(Limit.Time Experiments.default_time_limit);
	(Limit.Generated Experiments.default_node_limit);
      ])
    ?(beam_widths = Experiments.full_beams)
    ?(sort_predicate = "f")
    ~npegs ~ndisks ~pdb_up ~pdb_down
    alg =
  (* algoritjms with one integer parameter and one string parameter
     where the string parameter comes second.*)
  let send_mail = false in
    Notify.start_batchtime();
    List.iter
      (fun bw ->
	 do_batch ~pdb_type:pdb_type ~send_mail ~overwrite ~cost ~limits 
	   ~npegs ~ndisks ~pdb_up ~pdb_down
	   (Wrutils.str "%s %d %s" alg bw sort_predicate)
	   ["alg", alg; 
	    "beam_width",string_of_int bw;
	    "sort_predicate",sort_predicate;])
      beam_widths;
    Notify.send_batch_completed_mail "Hanoi_runs" "New Beam" alg


let do_hbss_batch ?(overwrite = false)
    ?(pdb_type= Felner)
    ?(cost = "Unit")
    ?(limits=[
	(Limit.Time Experiments.default_time_limit);
	(Limit.Generated Experiments.default_node_limit);
      ])
    ?(beam_widths=Experiments.full_beams)
    ~npegs ~ndisks ~pdb_up ~pdb_down
    alg node_capacity =
  (* hbss *)
  let send_mail = false in
  let biases = ["exp";"log";"lin";"p_2";"p_3";] in

  let beam_widths = List.map (fun a -> node_capacity / a) beam_widths in

    Notify.start_batchtime();
    List.iter (fun bias_function ->
      List.iter
	(fun depth_bound ->
	   do_batch ~pdb_type:pdb_type ~send_mail ~overwrite ~cost ~limits 
	     ~npegs ~ndisks ~pdb_up ~pdb_down
	     (Wrutils.str "%s %s %d" alg bias_function depth_bound)
	     ["alg", alg; 
	      "depth_bound",string_of_int depth_bound;
	      "bias_function",bias_function;])
	beam_widths;) biases;
    Notify.send_batch_completed_mail "Hanoi_runs" "New Beam" alg


let do_restarting_beam_batch ?(overwrite = false)
    ?(pdb_type= Felner)
    ?(cost = "Unit")
    ?(limits=[
	(Limit.Time Experiments.default_time_limit);
	(Limit.Generated Experiments.default_node_limit);
      ])
    ?(beam_widths = Experiments.full_beams)
    ?(sort_predicate = "f")
    ?(reserve_sort_predicate = "f")

    ?(p_reserve = 0.95)
    ?(depth_bound = Dynamic 10.0)

    ~npegs ~ndisks ~pdb_up ~pdb_down
    alg =
  (* algoritjms with one integer parameter and one string parameter
     where the string parameter comes second.*)
  let send_mail = false in
    Notify.start_batchtime();
    List.iter
      (fun bw ->

	 let depth_bound = match depth_bound with 
	     Static db -> db 
	   | Dynamic dp -> int_of_float (dp *. (float_of_int ndisks)) 
	   | StaticNodes nc -> (nc / bw) in
	 let total_beam_nodes = float_of_int (depth_bound * bw) in
	 let all_nodes = int_of_float (total_beam_nodes /. p_reserve) in

	   do_batch ~pdb_type:pdb_type ~send_mail ~overwrite ~cost ~limits 
	     ~npegs ~ndisks ~pdb_up ~pdb_down
	     (Wrutils.str "%s %d %s %s %d %d" 
		alg 
		bw 
		sort_predicate 
		reserve_sort_predicate
		depth_bound
		all_nodes
	     )
	     ["alg", alg; 
	      "beam_width",string_of_int bw;
	      "sort_predicate",sort_predicate;
	      "reserve_sort_predicate",reserve_sort_predicate;
	      "depth_bound",string_of_int depth_bound;
	      "node_capacity",string_of_int all_nodes;
	     ])
      beam_widths;
    Notify.send_batch_completed_mail "Hanoi_runs" "Restarting IB Beam" alg



let do_bulb_batch ?(overwrite = false)
    ?(pdb_type= Felner)
    ?(cost = "Unit")
    ?(limits=[
	(Limit.Time Experiments.default_time_limit);
	(Limit.Generated Experiments.default_node_limit);
      ])
    ?(beam_widths = Experiments.full_beams)

    ?(depth_bound = Dynamic 10.0)

    ~npegs ~ndisks ~pdb_up ~pdb_down
    alg =


  let send_mail = false in
    Notify.start_batchtime();
    List.iter
      (fun bw ->
	 let depth_bound_i = match depth_bound with
	     Static db -> db 
	   | Dynamic dp -> int_of_float (dp *. (float_of_int ndisks)) 
	   | StaticNodes nc -> nc / bw in


	 let node_capacity = 
	   match depth_bound with 
	       StaticNodes n -> n 
	     | _ -> depth_bound_i * bw in

	   do_batch ~pdb_type:pdb_type ~send_mail ~overwrite ~cost ~limits 
	     ~npegs ~ndisks ~pdb_up ~pdb_down
	     (Wrutils.str "%s %d %d" alg bw node_capacity)
	     ["alg", alg; 
	      "beam_width",string_of_int bw;
	      "node_capacity",string_of_int node_capacity;])
      beam_widths;
    Notify.send_batch_completed_mail "Hanoi_runs" "BULB" alg


let do_ml_wted_batch ?(overwrite = false)
    ?(pdb_type= Felner)
    ?(cost = "Unit")
    ?(limits=[
	(Limit.Time Experiments.default_time_limit);
	(Limit.Generated Experiments.default_node_limit);
      ])
    ?(weights = Experiments.low_res_weights)
    ?(node_capacity = 1000000)
    ~npegs ~ndisks ~pdb_up ~pdb_down
    alg =
  (* algorithms with one float parameter *)
  let send_mail = false in
    Notify.start_batchtime();
    List.iter
      (fun wt ->
	 do_batch ~pdb_type:pdb_type ~send_mail ~overwrite ~cost ~limits 
	   ~npegs ~ndisks ~pdb_up ~pdb_down
	   (Wrutils.str "%s %f %d" alg wt node_capacity)
	   ["alg", alg; 
	    "wt",string_of_float wt;
	    "node_capacity",string_of_int node_capacity;  
	   ])
      weights;
    Notify.send_batch_completed_mail "Hanoi_runs" "ml_weighted" alg
