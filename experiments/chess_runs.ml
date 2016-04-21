(**

   For running solitaire chess experiments

*)

type db = 
    Dynamic of float
  | Static of int
  | StaticNodes of int



let instance_root =
  Filename.concat Experiments.instance_root "solitaire_chess"

let solver_binary =
  ref (Filename.concat Experiments.bin_root "solitaire_chess_solver")



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
      Notify.try_and_email_on_fail (run cmd) "Chess_runs"


let do_run overwrite limits args attrs prob_path =
  (** [do_run overwrite limits macro args attrs prob_path] performs a
      single run on the given problem.  The results are stored in the
      RDB specified by data_root. *)
  let data_root = Filename.concat Experiments.data_root "solitaire_chess" in
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
    ?(overwrite=false) ?(cost="Unit") ?(limits=[])
    ~rows ~cols ~n_pieces ~piece_distribution
    args alg_attrs =
  (** does a chess run with the specified parameters. *)
  let batch_attrs = [
    "piece_distribution",piece_distribution;
    "rows",string_of_int rows;
    "cols",string_of_int cols;
    "n_pieces",string_of_int n_pieces;
  ] in

  let batch_str = Printf.sprintf "rows=%d, cols=%d, n_pieces=%d" 
    rows cols n_pieces in
  let attr_sets = Rdb.matching_attrs instance_root (batch_attrs) in
    if(send_mail) then
      Notify.start_batchtime ();

    List.iter (fun (inst_attrs:(string*string) list) ->
		 let cost_attr = [ "cost", cost ] in
		 let attrs = alg_attrs @ cost_attr @ 
		   inst_attrs in
		 let args = Printf.sprintf "--cost %s %s" cost args in
		 let prob_path = Rdb.path_for instance_root inst_attrs in
		   do_run overwrite limits args attrs prob_path)
      attr_sets;
    if(send_mail) then
      Notify.send_batch_completed_mail "Chess_runs" batch_str args


let do_basic_batch ?(overwrite = false)
    ?(cost = "Unit")
    ?(limits=[
	(Limit.Time Experiments.default_time_limit);
	(Limit.Generated Experiments.default_node_limit);
      ])
    ~rows ~cols ~n_pieces ~piece_distribution
    alg =
  (* greedy, speedy, a_star *)
  let send_mail = false in
    Notify.start_batchtime();
    do_batch ~send_mail ~overwrite ~cost ~limits 
      ~rows ~cols ~n_pieces ~piece_distribution
      alg ["alg", alg];
    Notify.send_batch_completed_mail "Chess_runs" "Basic" alg


let do_wted_batch ?(overwrite = false)
    ?(cost = "Unit")
    ?(limits=[
	(Limit.Time Experiments.default_time_limit);
	(Limit.Generated Experiments.default_node_limit);
      ])
    ?(weights = Experiments.low_res_weights)
    ~rows ~cols ~n_pieces ~piece_distribution
    alg =
  (* algorithms with one float parameter *)
  let send_mail = false in
    Notify.start_batchtime();
    List.iter
      (fun wt ->
	 do_batch ~send_mail ~overwrite ~cost ~limits 
	   ~rows ~cols ~n_pieces ~piece_distribution
	   (Wrutils.str "%s %f" alg wt)
	   ["alg", alg; "wt",string_of_float wt])
      weights;
    Notify.send_batch_completed_mail "Chess_runs" "Weighted" alg


let do_beam_batch ?(overwrite = false)
    ?(cost = "Unit")
    ?(limits=[
	(Limit.Time Experiments.default_time_limit);
	(Limit.Generated Experiments.default_node_limit);
      ])
    ?(beam_widths = Experiments.full_beams)
    ~rows ~cols ~n_pieces ~piece_distribution
    alg =
  (* algoritjms with one integer parameter *)
  let send_mail = false in
    Notify.start_batchtime();
    List.iter
      (fun bw ->
	 do_batch ~send_mail ~overwrite ~cost ~limits 
	   ~rows ~cols ~n_pieces ~piece_distribution
	   (Wrutils.str "%s %d" alg bw)
	   ["alg", alg; "beam_width",string_of_int bw])
      beam_widths;
    Notify.send_batch_completed_mail "Chess_runs" "Old Beam" alg


let do_new_beam_batch ?(overwrite = false)
    ?(cost = "Unit")
    ?(limits=[
	(Limit.Time Experiments.default_time_limit);
	(Limit.Generated Experiments.default_node_limit);
      ])
    ?(beam_widths = Experiments.full_beams)
    ?(sort_predicate = "f")
    ~rows ~cols ~n_pieces ~piece_distribution
    alg =
  (* algoritjms with one integer parameter and one string parameter
     where the string parameter comes second.*)
  let send_mail = false in
    Notify.start_batchtime();
    List.iter
      (fun bw ->
	 do_batch ~send_mail ~overwrite ~cost ~limits 
	   ~rows ~cols ~n_pieces ~piece_distribution
	   (Wrutils.str "%s %d %s" alg bw sort_predicate)
	   ["alg", alg; 
	    "beam_width",string_of_int bw;
	    "sort_predicate",sort_predicate;])
      beam_widths;
    Notify.send_batch_completed_mail "Chess_runs" "New Beam" alg


let do_restarting_beam_batch ?(overwrite = false)
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
    ~rows ~cols ~n_pieces ~piece_distribution
    alg =
  (* algoritjms with one integer parameter and one string parameter
     where the string parameter comes second.*)
  let send_mail = false in
    Notify.start_batchtime();
    List.iter
      (fun bw ->

	 let depth_bound = match depth_bound with 
	     Static db -> db 
	   | Dynamic dp -> int_of_float (dp *. (float_of_int n_pieces)) 
	   | StaticNodes nc -> (nc / bw) in
	 let total_beam_nodes = float_of_int (depth_bound * bw) in
	 let all_nodes = int_of_float (total_beam_nodes /. p_reserve) in

	   do_batch ~send_mail ~overwrite ~cost ~limits 
	     ~rows ~cols ~n_pieces ~piece_distribution
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
    Notify.send_batch_completed_mail "Chess_runs" "Restarting IB Beam" alg



let do_bulb_batch ?(overwrite = false)
    ?(cost = "Unit")
    ?(limits=[
	(Limit.Time Experiments.default_time_limit);
	(Limit.Generated Experiments.default_node_limit);
      ])
    ?(beam_widths = Experiments.full_beams)

    ?(depth_bound = Dynamic 10.0)
    ~rows ~cols ~n_pieces ~piece_distribution
    alg =


  let send_mail = false in
    Notify.start_batchtime();
    List.iter
      (fun bw ->
	 let depth_bound_i = match depth_bound with
	     Static db -> db 
	   | Dynamic dp -> int_of_float (dp *. (float_of_int n_pieces)) 
	   | StaticNodes nc -> nc / bw in


	 let node_capacity = 
	   match depth_bound with 
	       StaticNodes n -> n 
	     | _ -> depth_bound_i * bw in

	   do_batch ~send_mail ~overwrite ~cost ~limits 
	     ~rows ~cols ~n_pieces ~piece_distribution
	     (Wrutils.str "%s %d %d" alg bw node_capacity)
	     ["alg", alg; 
	      "beam_width",string_of_int bw;
	      "node_capacity",string_of_int node_capacity;])
      beam_widths;
    Notify.send_batch_completed_mail "Chess_runs" "BULB" alg


let do_ml_wted_batch ?(overwrite = false)
    ?(cost = "Unit")
    ?(limits=[
	(Limit.Time Experiments.default_time_limit);
	(Limit.Generated Experiments.default_node_limit);
      ])
    ?(weights = Experiments.low_res_weights)
    ?(node_capacity = 1000000)

    ~rows ~cols ~n_pieces ~piece_distribution

    alg =
  (* algorithms with one float parameter *)
  let send_mail = false in
    Notify.start_batchtime();
    List.iter
      (fun wt ->
	 do_batch ~send_mail ~overwrite ~cost ~limits 

	   ~rows ~cols ~n_pieces ~piece_distribution
	   (Wrutils.str "%s %f %d" alg wt node_capacity)
	   ["alg", alg; 
	    "wt",string_of_float wt;
	    "node_capacity",string_of_int node_capacity;  
	   ])
      weights;
    Notify.send_batch_completed_mail "Chess_runs" "ml_weighted" alg
