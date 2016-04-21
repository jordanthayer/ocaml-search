(* $Id: runs.ml,v 1.1 2006/06/25 00:02:50 ruml Exp ruml $
   doing runs for tiles domain*)

open Printf

let data_root = Experiments.data_root ^ "tiles"
and instance_root = Experiments.instance_root ^ "tiles_instances"
and solver_binary = ref (Experiments.bin_root ^ "tiles_solver")


let feasible_p =
  (** Determines if new randomly generated instances are solvable, as
      not all tiles boards are solvable *)
  (fun i ->
     true(*
     let size = Tiles_instance.num_tiles i in
     let orig_start = Tiles_instance.get_start i in
     let start = Array.make (size+1) 0 in
     let revord = ref 0 in
       Array.iteri (fun i a -> start.(a) <- i) orig_start;
       for i = 0 to (size) do
	 for j = i+1 to (size) do
	   if start.(i) > start.(j) && start.(j) <> 0 then
	     revord := !revord + 1
	 done
       done;
       (!revord mod 2) = 0*))
  (*Experiments.feasible_p_dups
     (fun (a,b,c,d,e,f) -> match a with
	| None -> false
	| _ -> true)
     (Tiles.default_interface Tiles.unit_cost)
    (* if it is solvable for un-weighted then it is solveable when
       weighted. *) *)

(*
let if_not_exists attrs i f =
  (** Generates a new problem if one of the given configuration does not exist.
      [attrs] the problem configuration
      [i] the instance number
      [f] a function for generating new problems *)
  let _, time =
    Wrsys.with_time (fun () ->
		       let path = Rdb.path_for data_root attrs in
			 if Sys.file_exists path then
			   Wrutils.pr "%s exists, skipping...\n%!" path
			 else
			   let p = f () in
			     Wrio.with_outfile path
			       (fun ch -> Tiles_instance.write p ch))
  in Wrutils.pr "Wrote problem %d, time=%f.\n%!" i time
*)

(*
let feasible_instance rows cols =
  (** Generates a single feasible instance of size [cols] by [rows] *)
  Wrutils.eval_until
    (fun () ->
       Verb.pe Verb.toplvl "Trying....\n";
       flush stdout;
       Tiles_instance.random rows cols)
    (fun i -> (feasible_p i))
*)
(*
let make_instances rows cols num =
  (** Makes a fixed number of instances of a given size.
      [rows] the y dimension of the board
      [cols] the x dimension of the board
      [num] the number of instances to generate *)
  Random.self_init ();
  for i = 1 to num do
    let attrs = [ "type", "instance";
		  "model", "random";
		  "rows", string_of_int rows;
		  "cols", string_of_int cols;
		  "num", string_of_int i; ] in
      if_not_exists attrs i
	(fun () -> feasible_instance rows cols)
  done
*)

(*
let make_8_puzzle_instances () =
  (** Make all instances of the 8-puzzle (except the goal).
      [rows] the y dimension of the board
      [cols] the x dimension of the board
      [i] the number of instances to generate *)
  let next =
    let next_node = Tiles_kre.Eight_puzzle.gkre_instances false false in
      (fun () -> Tiles_kre.node_to_problem 3 (next_node ()))
  in
  let i = ref 0 in
    try
      ignore (next ());			(* skip the goal instance. *)
      while true do
	let attrs = [ "type", "instance";
		      "model", "eight_puzzle";
		      "rows", "3";
		      "cols", "3";

		      (* The directories get large because there are
			 181440 states we need to generate... so we
			 bucket them by 500. *)
		      "bucket", string_of_int (!i mod 500);

		      "num", string_of_int !i; ] in
	  if_not_exists attrs !i next;
	  incr i
      done;
      failwith "Shouldn't reach here"
    with End_of_file -> ()
*)

let instance_specs =
  (** this list lets you specify which tile puzzles to run on.  The
      first number says how many there are (if there are more, it
      skips the remaining ones, if there are less, not sure what
      happens).  The next 2 numbers are the size of the puzzles you
      want to actually run on.  So if you don't like 7x7 puzzles
      don't have that line in there.   *)
  ref   [

(*    100,3,3;*)
    100, 4, 4;
    100, 5, 5;
    100, 6, 6;
    100, 7, 7;
(*
  [ 20, 4, 4;]			  (* look at the first 20 instances *)
    100, 7, 7;
*)

  ]

(*
let make_all_instances () =
  (** Generates all of the predefined instances *)
  List.iter (fun (num, rows, cols) ->
	       make_instances rows cols num)
    !instance_specs
*)

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
       	Some(t) -> Wrutils.str " --exp %d " t
      | _ -> "" in
    (* this prints out the thing that is sent to the command line. *)
  let go () =
(*
    Printf.fprintf stderr "\n%s%!\n" (Wrutils.str "%s %s%s%s < %s"
	 !solver_binary limit_time limit_nodes args prob_path);
*)
    let pstring = (Wrutils.str "%s --memory max --add %s %s %s < %s"
	 !solver_binary limit_time limit_nodes args prob_path) in
      Verb.pe Verb.debug "\npstring: %s\n\n" pstring;
      Wrsys.with_subprocess_pipes
	~sig_on_exception:Sys.sigkill pstring
      (fun to_solver from_solver ->
	 Datafile.write_header_pairs outch;
	 Datafile.write_pairs outch attrs;
	 Datafile.write_pairs outch ["attrs", (Wrstr.encode_pairs attrs)];
	 let res = Datafile.pipe_data from_solver outch in
	   Datafile.write_trailer_pairs outch;
	   res) in
    Notify.try_and_email_on_fail go "Tiles_runs"


let do_run overwrite time_limit node_limit args prob_path attrs =
  (** sets up run file and calls solver on problem *)
  let limits = [(match time_limit with None -> Limit.Never
		   | Some t -> Limit.Time t);
		(match node_limit with None -> Limit.Never
		   | Some n -> Limit.Generated n)] in
  let run_file = Rdb.path_for data_root
    (Rdb.filter_attrs ["type"] attrs) in
  let attrs = Rdb.filter_attrs ["time limit"; "node limit"] attrs in

    if (Limit.has_new_limits run_file limits) then
      (Wrutils.pr "less strict limits!\n!")
    else
      (Wrutils.pr "more strict limits!\n!");
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
	     (Wrfname.make_relative instance_root prob_path);
	   (* the number after the some allows people to specify a
	      limit on the number of nodes to be generated. *)

	   let c,t =
	     call_solver
	       time_limit node_limit args prob_path attrs outch in
	     Wrutils.pr "%.1f in %.3f.\n%!" c t)

let eightbuckets = Wrlist.range ~min:0 499


let do_eightpuzzles overwrite ?(cost="unit") ?(buckets=eightbuckets)
    ?(moves = "standard") ?(heuristic = "manhattan")
    time_limit node_limit alg_attrs args =
  List.iter
    (fun (_,_,_) ->
       List.iter
	 (fun bsize ->
	    let batch_attrs = ["model", "eight_puzzle";
			       "rows", "3";
			       "cols", "3";
			       "bucket", string_of_int bsize; ] in
	      List.iter
		(fun prob_attrs ->
		   let prob_path = Rdb.path_for instance_root prob_attrs
		   and attrs = alg_attrs @
		     ([("cost", cost); "moves", moves] @ prob_attrs) in
		     do_run overwrite time_limit node_limit
		       (Wrutils.str " --cost %s --moves %s --heuristic %s %s"
			  cost moves heuristic args)
			  prob_path attrs)
		       (Rdb.matching_attrs instance_root batch_attrs))
		eightbuckets)
	 !instance_specs


let do_batches
    overwrite
    ?(use_opt_cost = false)
    ?(cost="unit")
    ?(moves = "standard")
    ?(buckets = eightbuckets)
    ?(heuristic = "manhattan")
    time_limit node_limit alg_attrs args model =
  if model = "eight_puzzle"
  then (do_eightpuzzles overwrite ~cost ~moves ~buckets
	  time_limit node_limit alg_attrs
	  args)
  else
    (** runs an alg on all problems *)
    List.iter
      (fun (_,rows,cols) ->
	 let batch_attrs = ["model", model;
			    "rows", string_of_int rows;
			    "cols", string_of_int cols;] in
	   List.iter
	     (fun prob_attrs ->
		let opt_cost = if not use_opt_cost then infinity
		else
		  let idastar_run = ["alg", "idastar";
				     "heuristic",heuristic;
				     "cost", cost;
				     "moves", moves;]@ prob_attrs in
		  let opt_path = Rdb.path_for data_root idastar_run in
		  let opt_df = Datafile.load opt_path in
		  let opt_cost = float_of_string (Datafile.get_val
						    opt_df "final sol cost") in
		    opt_cost in
		let prob_path = Rdb.path_for instance_root prob_attrs
		and attrs = alg_attrs @
		  (["cost", cost;
		    "moves", moves;
		    "heuristic", heuristic;] @ prob_attrs) in
		let args = (if use_opt_cost
			    then Wrutils.str "%s %f" args opt_cost
			    else args) in

		  do_run overwrite time_limit node_limit
		    (Wrutils.str " --cost %s --moves %s --heuristic %s %s"
		       cost moves heuristic args)
		    prob_path attrs)
	     (Rdb.matching_attrs instance_root batch_attrs))
      !instance_specs


let do_basic_batch ?(moves = "standard") ?(models = ["korf"; (*"random"*)])
    ?(time_limit = (Some Experiments.default_time_limit))
    ?(node_limit = (Some Experiments.default_node_limit)) ?(overwrite = false)
    ?(cost = "unit") ?(heuristic = "manhattan") ?(append = "")
    alg =
  (** Runs algorithms which take no arguments (astar, greedy) against the
      tiles problems *)
  Notify.start_batchtime();
  List.iter (fun model ->
	       do_batches ~moves ~cost ~heuristic
		 overwrite time_limit node_limit
		 ["alg", alg;] (Printf.sprintf "%s%s" alg append) model) models;
  Notify.send_batch_completed_mail "Tiles_runs" "Basic" alg


let do_beam_batch ?(moves = "standard") ?(models = ["korf"; (*"random"*)])
    ?(cost = "unit")
    ?(time_limit = (Some Experiments.default_time_limit))
    ?(node_limit = (Some Experiments.default_node_limit))
    ?(beam_widths = Experiments.full_beams) ?(overwrite = false)
    ?(heuristic = "manhattan")  ?(append = "") alg =
  (** Does a set of beam width runs on the full set of beam widths by default.
      [alg] is the algorithm to be run *)
  Notify.start_batchtime();
  List.iter (fun model ->
	       List.iter (fun beam_width ->
			    do_batches ~moves:moves ~cost:cost
			      ~heuristic
			      overwrite time_limit node_limit
			      ["alg", alg;
			       "beam_width", string_of_int beam_width;]
			      (Wrutils.str "%s %d%s" alg beam_width append)
			      model)
		 beam_widths)  models;
  Notify.send_batch_completed_mail "Tiles_runs" "Beam" alg


let do_bulb_batch ?(moves = "standard") ?(models = ["korf"; "random"])
    ?(time_limit = (Some Experiments.default_time_limit))
    ?(node_limit = (Some Experiments.default_node_limit))
    ?(bulb_ht_size = None)
    ?(beam_widths = Experiments.full_beams) ?(overwrite = false)
    ?(heuristic = "manhattan") alg =
  (** Does a set of beam width runs on the full set of beam widths by default.
      [alg] is the algorithm to be run *)
  let node_limit_number = match bulb_ht_size with
      None -> 100000000
    | Some n -> n in
    Notify.start_batchtime();
    List.iter (fun model ->
		 List.iter (fun beam_width ->
			      do_batches ~moves:moves
				~heuristic
				overwrite time_limit node_limit
				["alg", alg;
				 "beam_width", string_of_int beam_width;
				 "node_capacity", string_of_int node_limit_number]
				(Wrutils.str "%s %d %d" alg beam_width node_limit_number) model)
		   beam_widths)  models;
    Notify.send_batch_completed_mail "Tiles_runs" "Beam" alg


let do_wted_batch ?(moves = "standard") ?(cost="unit")
    ?(models = ["korf"; "random"])
    ?(time_limit = (Some Experiments.default_time_limit))
    ?(node_limit = (Some Experiments.default_node_limit))
    ?(weights = Experiments.low_res_weights) ?(overwrite = false)
    ?(heuristic = "manhattan") alg =
  (** Performs search on all problems for algorithms which take a single
      float, the suboptimality bound, as their argument. [alg] is the alg to
      be run. *)
  Notify.start_batchtime();
  List.iter (fun model ->
	       List.iter
		 (fun wt ->
		    do_batches ~moves ~cost ~heuristic overwrite
		      time_limit node_limit
		      ["alg", alg;
		       "wt", string_of_float wt;
		      ]
		      (Wrutils.str "%s %f" alg wt) model)
		 (List.filter (fun n -> n >= 1.) weights)) models;
  Notify.send_batch_completed_mail "Tiles_runs" "Weighted" alg



let do_wted_beam_batch heuristic overwrite time_limit node_limit alg wt beam =
  do_batches ~moves:"standard" ~cost:"unit"
    ~heuristic overwrite time_limit node_limit
    ["alg", alg;
     "wt", string_of_float wt;        (* order really matters here*)
     "beam", string_of_int beam] (* wt (bound) must come first*)
    (Wrutils.str "%s %f %i" alg wt beam) "korf"


let do_wted_beam_batches ?(heuristic = ["manhattan";])
    ?(overwrite = false) ?(time_limit = Some (Experiments.default_time_limit))
    ?(node_limit = Some (Experiments.default_node_limit))
    ?(weights = Experiments.low_res_weights)
    ?(beams = [1; 5; 10; 100; 1000]) alg =
  Notify.start_batchtime();
    List.iter
      (fun h ->
	 List.iter
	   (fun beam ->
	      List.iter
		(fun w -> do_wted_beam_batch h overwrite time_limit
		   node_limit alg w beam)
		weights) beams) heuristic;
    Notify.send_batch_completed_mail "Grid_runs" "Optimistic" alg




let do_msc_k_wted_batches ?(moves = "standard") ?(cost = "unit")
    ?(models = ["korf"; "random"]) ?(heuristic = "manhattan")
    ?(overwrite = false) ?(time_limit = Some (Experiments.default_time_limit))
    ?(node_limit = Some (Experiments.default_node_limit))
    ?(c_size = Experiments.full_beams) ?(ks = Experiments.ks)
    ?(weights = Experiments.low_res_weights) alg =
  Notify.start_batchtime();
  List.iter
    (fun model ->
       List.iter
	 (fun c ->
	    List.iter
	      (fun k ->
		 List.iter
		   (fun wt ->
		      do_batches ~moves ~cost ~heuristic overwrite
			time_limit node_limit
			["alg", alg;
			 "commit", string_of_int c;
			 "k", string_of_int k;
			 "wt", string_of_float wt;]
			(Wrutils.str "%s %f %i %i" alg wt k c) model) weights)
	      ks) c_size) models;
  Notify.send_batch_completed_mail "Tiles_runs" "MSC_K_WTED" alg


let contract_astar_batch ?(moves = "standard") ?(cost = "unit")
    ?(models = ["korf"; "random"]) ?(heuristic = "manhattan")
    ?(overwrite = false) alg_name =
  Notify.start_batchtime();
  let l1 = Wrlist.range ~min:1 ~step:(fun i -> i - 5000) 2_500_000 in
  let w_res = List.map (fun a -> a,1000) l1 in
  List.iter
    (fun (deadline,res) ->
       do_batches ~moves ~cost ~heuristic overwrite None (Some deadline)
	 ["alg", alg_name;
	  "contract", string_of_int deadline;
	  "res", string_of_int res;]
	 (Printf.sprintf "%s %i %i" alg_name deadline res)
	 "korf")
    (List.rev w_res)


let deadline_contract_batch ?(moves = "standard") ?(cost = "unit")
    ?(models = ["korf";]) ?(heuristic = "manhattan")
    ?(overwrite = false) alg_name =
  Notify.start_batchtime();
  let deadlines =
    [60.; 30.; 15.; 7.5; 3.75; 1.875; 0.9375; 0.46875; 0.234375; 0.1171875;
     0.05859375; 0.029296875; 0.0146484375; 0.00732421875;] in
  let w_res = List.map (fun a -> a,1000) deadlines in
    List.iter
      (fun (deadline,res) ->
	 do_batches ~moves ~cost ~heuristic overwrite (Some deadline) None
	   ["alg", alg_name;
	    "deadline", string_of_float deadline;
	    "res", string_of_int res;]
	   (Printf.sprintf "%s %f %i" alg_name deadline res)
	   "korf")
      (List.rev w_res)



let do_new_beam_batches ?(moves = "standard") ?(cost = "unit")
    ?(models = ["korf"; "random"])
    ?(overwrite = false) ?(time_limit = Some (Experiments.default_time_limit))
    ?(node_limit = Some (Experiments.default_node_limit))
    ?(beam_widths = Experiments.full_beams)
    ?(sort_predicate = "f") alg =
  Notify.start_batchtime();
  List.iter
    (fun model ->
       List.iter
	 (fun bw ->
	    do_batches ~moves:moves ~cost:cost overwrite time_limit node_limit
	      ["alg", alg;
	       "beam_width", string_of_int bw;
	       "sort_predicate", sort_predicate]
	      (Wrutils.str "%s %d %s" alg bw sort_predicate) model) beam_widths) models;
  Notify.send_batch_completed_mail "Tiles_runs" "new beam" alg


let do_optimistic_batch ?(moves = "standard") ?(models = ["korf";])
    ?(cost="unit") ?(heuristic = "manhattan")
    overwrite time_limit node_limit alg wt opt =
  (** Performs search on all problems for algorithms which take two
      floats, the suboptimality bound and the desired level of optimism  as
      their argument. [alg] is the alg to be run. *)
  assert (opt >= 1.);
  List.iter
    (fun model ->
       do_batches ~moves ~cost ~heuristic overwrite time_limit node_limit
	 ["alg", alg;
	  "wt", string_of_float wt;
	  "optimism", string_of_float opt]
	 (Wrutils.str "%s %f %f" alg wt opt) model) models


let do_optimistic_batches
    ?(moves = "standard")
    ?(time_limit = (Some Experiments.default_time_limit))
    ?(node_limit = (Some Experiments.default_node_limit))
    ?(weights = Experiments.low_res_weights)
    ?(cost="unit") ?(heuristic = "manhattan")
    ?(opt = Experiments.optimisms) ?(overwrite = false) alg =
  (** calls [do_optimistic_search] on all weights and optimism *)
  Notify.start_batchtime();
  List.iter
    (fun opt ->
       List.iter
	 (fun w -> do_optimistic_batch ~moves ~cost ~heuristic
	    overwrite time_limit node_limit alg w opt)
	 (List.filter (fun n -> n >= 1.1) weights))
    opt;
  Notify.send_batch_completed_mail "Tiles_runs" "Optimistic" alg


let do_8_puzzle overwrite time_limit node_limit moves cost alg_attrs args =
  (** runs an alg on all problems *)
  let batch_attrs = ["model", "eight_puzzle";
		     "rows", "3";
		     "cols", "3"; ] in
    List.iter
      (fun bucket ->
	 let batch_attrs = batch_attrs @ ["bucket", string_of_int bucket]
	 in
	   List.iter
	     (fun prob_attrs ->
		let prob_path = Rdb.path_for instance_root prob_attrs
		and attrs =
		  alg_attrs @ (("cost", cost)
			       :: ("moves", moves) :: prob_attrs)
		in
		  do_run overwrite time_limit node_limit
		    (args ^ " --cost " ^ cost ^ " --moves " ^ moves)
		    prob_path attrs)
	     (Rdb.matching_attrs instance_root batch_attrs))
      (*
	(Wrlist.range ~min:0 499)
      *)
      [0; 1]		     (* just the first two buckets for now. *)


let do_realtime_batch ?(time_limit = (Some Experiments.default_time_limit))
    ?(node_limit = (Some Experiments.default_node_limit))
    ?(bounds = [1;3;5;10]) ?(grains = [1; 3; 5; 10; 20; 50])
    ?(r_vals = [0.001;0.01;0.1;0.3;0.5;0.9])
    ?(branch_factors = [1.0;1.2;1.5;2.;3.]) ?(overwrite = false)
    ?(moves = "standard") ?(cost = "unit") () =
  (** runs real-time algorithms on korf puzzles, eight puzzles, and
      snlemons_easy *)
  let learn_batch_attrs = ["alg", "dtastar_learn";
			   "model", "random";
			   "cost", cost;
			   "moves", moves;] in
  let learn_prob_attrs = (List.hd
			    (Rdb.matching_attrs
			       data_root learn_batch_attrs)) in
    (* for korf_25_easy *)
    ((let alg = "rtastar" in
	List.iter
	  (fun bound ->
	     (do_batches ~moves:moves overwrite time_limit
		node_limit ["alg", alg;
			    "bound",
			    (string_of_int bound)]
		(Wrutils.str "%s %d" alg bound) "korf_25_easy"))
	  bounds);
     (let alg = "srtastar" in
	List.iter
	  (fun bound ->
	     (do_batches ~moves:moves overwrite time_limit
		node_limit ["alg", alg;
			    "bound",
			    (string_of_int bound)]
		(Wrutils.str "%s %d" alg bound) "korf_25_easy"))
	  bounds);
     (let alg = "dtastar" in
      let learn_path = Wrutils.str "%s.learn"
	(Rdb.path_for data_root
	   ((Rdb.filter_attrs ["type"; "num"] learn_prob_attrs)@
	      [("num","condensed")])) in
	List.iter
	  (fun grain -> List.iter (fun r -> do_batches ~moves:moves overwrite time_limit
				     node_limit ["alg", alg;
						 "r", (string_of_float r);
						 "grain_size", (string_of_int grain)]
				     (Wrutils.str "%s %d %d %f %s" alg grain 10 r learn_path)
				     "korf_25_easy") r_vals) grains);
     (let alg = "astar" in
	do_batches ~moves:moves overwrite time_limit
	  node_limit ["alg", alg] alg
	  "korf_25_easy");

     (* for snlemons easy puzzles *)
     (*(let alg = "rtastar" in
	List.iter
	  (fun bound -> (do_batches ~moves:moves
			   overwrite time_limit
			   node_limit ["alg", alg; "bound",
				       (string_of_int bound)]
			   (Wrutils.str "%s %d" alg bound)
			   "snlemons_easy")) bounds);
     (let alg = "srtastar" in
	List.iter
	  (fun bound -> (do_batches ~moves:moves
			   overwrite time_limit
			   node_limit ["alg", alg; "bound",
				       (string_of_int bound)]
			   (Wrutils.str "%s %d" alg bound)
			   "snlemons_easy")) bounds);
     (let alg = "dtastar" in
      let learn_path = Wrutils.str "%s.learn"
	(Rdb.path_for data_root
	   ((Rdb.filter_attrs ["type"; "num"] learn_prob_attrs)@
	      [("num","condensed")])) in
	List.iter
	  (fun grain -> List.iter
	     (fun r -> do_batches ~moves:moves overwrite time_limit
		node_limit ["alg", alg;
			    "r", (string_of_float r);
			    "grain_size", (string_of_int grain)]
		(Wrutils.str "%s %d %d %f %s" alg grain 10 r learn_path)
		"snlemons_easy") r_vals) grains);
     (let alg = "astar" in do_batches ~moves:moves overwrite
			     time_limit
			     node_limit ["alg", alg] alg "snlemons_easy");*)

     (* for all korf puzzles *)
     (let alg = "rtastar" in
	List.iter
	  (fun bound ->
	     (do_batches ~moves:moves overwrite time_limit node_limit ["alg", alg;
							  "bound",
							  (string_of_int bound)]
		(Wrutils.str "%s %d" alg bound) "korf"))
	  bounds);
     (let alg = "srtastar" in
	List.iter
	  (fun bound ->
	     (do_batches ~moves:moves overwrite time_limit node_limit ["alg", alg;
							  "bound",
							  (string_of_int bound)]
		(Wrutils.str "%s %d" alg bound) "korf"))
	  bounds);
     (let alg = "dtastar" in
      let learn_path = Wrutils.str "%s.learn"
	(Rdb.path_for data_root
	   ((Rdb.filter_attrs ["type"; "num"] learn_prob_attrs)@
	      [("num","condensed")])) in
	List.iter
	  (fun grain -> List.iter
	     (fun r -> do_batches ~moves:moves overwrite time_limit
		node_limit ["alg", alg;
			    "r", (string_of_float r);
			    "grain_size", (string_of_int grain)]
		(Wrutils.str "%s %d %d %f %s" alg grain 10 r learn_path)
		"korf") r_vals) grains));
    Notify.send_batch_completed_mail "realtime" "basic" "all"

let do_dta_batch
    ?(time_limit = (Some Experiments.default_time_limit))
    ?(node_limit = (Some Experiments.default_node_limit))
    ?(bounds = [1;3;5;10]) ?(grains = [1; 3; 5; 10; 20; 50])
    ?(r_vals = [0.001;0.01;0.1;0.3;0.5;0.9])
    ?(branch_factors = [1.0;1.2;1.5;2.;3.]) ?(overwrite = false)
    ?(moves = "macro") ?(cost = "unit") () =
  (** runs real-time algorithms on eight puzzles *)
  (*let idta_learn_batch_attrs = ["alg", "dtastar_learn";
				"model", "eight_puzzle";
				"cost", cost;
				"moves", moves;] in*)
  let dta_learn_batch_attrs = ["alg", "dtastar_learn";
			       "model", "eight_puzzle";
			       "cost", cost;
			       "moves", moves;] in
    ((let alg = "dtastar" in
      let learn_prob_attrs = (List.hd
				(Rdb.matching_attrs
				   data_root dta_learn_batch_attrs)) in
      let learn_path = Wrutils.str "%s.learn"
	(Rdb.path_for data_root
	   ((Rdb.filter_attrs ["type"; "num"] learn_prob_attrs)@
	      [("num","condensed")])) in
	List.iter
	  (fun grain -> List.iter
	     (* let do_8_puzzle overwrite time_limit node_limit moves
		cost alg_attrs args *)
	     (fun r -> do_batches ~moves:moves
		~buckets:(Wrlist.range ~min:0 0) overwrite time_limit
		node_limit ["alg", alg;
			    "r", (string_of_float r);
			    "grain_size", (string_of_int grain)]
		(Wrutils.str "%s %d %d %f %s" alg grain 10 r learn_path)
		"eight_puzzle") r_vals) grains);
     (*(let alg = "idtastar" in
      let learn_prob_attrs = (List.hd
				(Rdb.matching_attrs
				   data_root idta_learn_batch_attrs)) in
      let learn_path = Wrutils.str "%s.learn"
	(Rdb.path_for data_root
	   ((Rdb.filter_attrs ["type"; "num"] learn_prob_attrs)@
	      [("num","condensed")])) in
	List.iter
	  (fun grain -> List.iter
	     (fun r -> do_batches ~moves:moves
		~buckets:(Wrlist.range ~min:0 0)overwrite time_limit
		node_limit ["alg", alg;
			    "r", (string_of_float r);
			    "grain_size", (string_of_int grain)]
		(Wrutils.str "%s %d %d %f %s" alg grain 10 r learn_path)
		"eight_puzzle") r_vals) grains)*));
    Notify.send_batch_completed_mail "dtastar" moves "all"

(*
let make_dta_samples () =
  (** creates 50 random 4*4 instances for DTA* to learn from *)
  make_instances 4 4 50
*)

let do_im_15 bins control_factor overwrite moves cost model =
  Notify.start_batchtime();
  do_batches overwrite None None ~cost:cost ~moves:moves
    ["alg", "idastar_im";
     "max-bins", string_of_int bins;
     "control-factor", string_of_float control_factor;
    ]
    (Wrutils.str "idastar_im %d %f" bins control_factor)
    model;
  Notify.send_batch_completed_mail "Tiles_runs" "IDA*_IM" "idastar_im"


let do_cr_15 overwrite moves cost model =
  let control_factor = 2. in
  let bins = [ 100; ] in
  Notify.start_batchtime();
    List.iter
      (fun bins ->
	 do_batches overwrite None None ~cost:cost ~moves:moves
	   ["alg", "idastar_cr";
	    "max-bins", string_of_int bins;
	    "control-factor", string_of_float control_factor;
	   ]
	   (Wrutils.str "idastar_cr %d %f" bins control_factor)
	   model)
      bins;
  Notify.send_batch_completed_mail "Tiles_runs" "IDA*_CR" "idastar_cr"

let do_im_8 overwrite moves cost =
  let control_factor = 2. in
  let bins = [ 100; ] in
  Notify.start_batchtime();
    List.iter
      (fun bins ->
	 do_8_puzzle overwrite None None moves cost
	   ["alg", "idastar_im";
	    "max-bins", string_of_int bins;
	    "control-factor", string_of_float control_factor;
	   ]
	   (Wrutils.str "idastar_im %d %f" bins control_factor))
      bins;
  Notify.send_batch_completed_mail "Tiles_runs" "IDA*_IM" "idastar_im"


let do_cr_8 overwrite moves cost =
  let control_factor = 2. in
  let bins = [ 100; ] in
  Notify.start_batchtime();
    List.iter
      (fun bins ->
	 do_8_puzzle overwrite None None moves cost
	   ["alg", "idastar_cr";
	    "max-bins", string_of_int bins;
	    "control-factor", string_of_float control_factor;
	   ]
	   (Wrutils.str "idastar_cr %d %f" bins control_factor))
      bins;
  Notify.send_batch_completed_mail "Tiles_runs" "IDA*_CR" "idastar_cr"


let do_batched_8 overwrite moves cost alg =
  Notify.start_batchtime();
  do_8_puzzle overwrite None None moves cost ["alg", alg; ] alg;
  Notify.send_batch_completed_mail "Tiles_runs" alg alg


let do_continuing_par_astar_15 overwrite p0 p_rate moves cost model frac_time =
  Notify.start_batchtime();
  (** runs an alg on all problems *)
  let alg_attrs =
    ["alg", "continuing_par_astar";
     "fraction of astar time", string_of_float frac_time;
     "initial p", string_of_float p0;
     "p decrease rate", string_of_float p_rate;
    ] in
    List.iter
      (fun (_,rows,cols) ->
	 let batch_attrs = ["model", model;
			    "rows", string_of_int rows;
			    "cols", string_of_int cols; ] in
	   List.iter
	     (fun prob_attrs ->
		let prob_path =
		  Rdb.path_for instance_root prob_attrs in
		let prob_attrs =
		  ([("cost", cost); "moves", moves] @ prob_attrs) in
		let attrs = alg_attrs @ prob_attrs in
		let num = List.assoc "num" prob_attrs in
		let opt_data_path =
		  Rdb.path_for (User_paths.data_root ^ "/hierarchical/korf100")
		    ["alg", "astar"; "num", num ]
		in
		let opt_df = Datafile.load opt_data_path in
		let opt_cost = Datafile.get_val opt_df "final sol cost" in
		let opt_time = Datafile.get_val opt_df "total raw cpu time" in
		let time_limit =
		  Some (frac_time *. (float_of_string opt_time))
		in
		let args =
		  Wrutils.str "continuing_par_astar %f %f %s"
		    p0 p_rate opt_cost
		in
		  do_run overwrite time_limit None
		    (args ^ " --cost " ^ cost ^ " --moves " ^ moves)
		    prob_path attrs)
	     (Rdb.matching_attrs instance_root batch_attrs))
      !instance_specs;
    Notify.send_batch_completed_mail
      "Tiles_runs" "Continuing-Partial-A*" "continuing_par_astar"


let do_continuingf_par_astar_15
    overwrite p0 p_rate moves cost model frac_time =
  Notify.start_batchtime();
  (** runs an alg on all problems *)
  let alg_attrs =
    ["alg", "continuingf_par_astar";
     "fraction of astar time", string_of_float frac_time;
     "initial p", string_of_float p0;
     "p decrease rate", string_of_float p_rate;
    ] in
    List.iter
      (fun (_,rows,cols) ->
	 let batch_attrs = ["model", model;
			    "rows", string_of_int rows;
			    "cols", string_of_int cols; ] in
	   List.iter
	     (fun prob_attrs ->
		let prob_path =
		  Rdb.path_for instance_root prob_attrs in
		let prob_attrs =
		  ([("cost", cost); "moves", moves] @ prob_attrs) in
		let attrs = alg_attrs @ prob_attrs in
		let num = List.assoc "num" prob_attrs in
		let opt_data_path =
		  Rdb.path_for (User_paths.data_root ^ "/hierarchical/korf100")
		    ["alg", "astar"; "num", num ]
		in
		let opt_df = Datafile.load opt_data_path in
		let opt_cost = Datafile.get_val opt_df "final sol cost" in
		let opt_time = Datafile.get_val opt_df "total raw cpu time" in
		let time_limit =
		  Some (frac_time *. (float_of_string opt_time))
		in
		let args =
		  Wrutils.str "continuingf_par_astar %f %f %s"
		    p0 p_rate opt_cost
		in
		  do_run overwrite time_limit None
		    (args ^ " --cost " ^ cost ^ " --moves " ^ moves)
		    prob_path attrs)
	     (Rdb.matching_attrs instance_root batch_attrs))
      !instance_specs;
    Notify.send_batch_completed_mail
      "Tiles_runs" "Continuingf-Partial-A*" "continuingf_par_astar"

let do_continuing_bestf_par_astar_15
    overwrite p0 p_rate moves cost model frac_time =
  Notify.start_batchtime();
  (** runs an alg on all problems *)
  let alg_attrs =
    ["alg", "continuing_bestf_par_astar";
     "fraction of astar time", string_of_float frac_time;
     "initial p", string_of_float p0;
     "p decrease rate", string_of_float p_rate;
    ] in
    List.iter
      (fun (_,rows,cols) ->
	 let batch_attrs = ["model", model;
			    "rows", string_of_int rows;
			    "cols", string_of_int cols; ] in
	   List.iter
	     (fun prob_attrs ->
		let prob_path =
		  Rdb.path_for instance_root prob_attrs in
		let prob_attrs =
		  ([("cost", cost); "moves", moves] @ prob_attrs) in
		let attrs = alg_attrs @ prob_attrs in
		let num = List.assoc "num" prob_attrs in
		let opt_data_path =
		  Rdb.path_for (User_paths.data_root ^ "/hierarchical/korf100")
		    ["alg", "astar"; "num", num ]
		in
		let opt_df = Datafile.load opt_data_path in
		let opt_cost = Datafile.get_val opt_df "final sol cost" in
		let opt_time = Datafile.get_val opt_df "total raw cpu time" in
		let time_limit =
		  Some (frac_time *. (float_of_string opt_time))
		in
		let args =
		  Wrutils.str "continuing_bestf_par_astar %f %f %s"
		    p0 p_rate opt_cost
		in
		  do_run overwrite time_limit None
		    (args ^ " --cost " ^ cost ^ " --moves " ^ moves)
		    prob_path attrs)
	     (Rdb.matching_attrs instance_root batch_attrs))
      !instance_specs;
    Notify.send_batch_completed_mail
      "Tiles_runs" "Continuing-Bestf-Partial-A*" "continuing_bestf_par_astar"

let do_restarting_par_astar_15 overwrite p0 p_rate moves cost model frac_time =
  Notify.start_batchtime();
  (** runs an alg on all problems *)
  let alg_attrs =
    ["alg", "restarting_par_astar";
     "fraction of astar time", string_of_float frac_time;
     "initial p", string_of_float p0;
     "p decrease rate", string_of_float p_rate;
    ] in
    List.iter
      (fun (_,rows,cols) ->
	 let batch_attrs = ["model", model;
			    "rows", string_of_int rows;
			    "cols", string_of_int cols; ] in
	   List.iter
	     (fun prob_attrs ->
		let prob_path =
		  Rdb.path_for instance_root prob_attrs in
		let prob_attrs =
		  ([("cost", cost); "moves", moves] @ prob_attrs) in
		let attrs = alg_attrs @ prob_attrs in
		let num = List.assoc "num" prob_attrs in
		let opt_data_path =
		  Rdb.path_for (User_paths.data_root ^ "/hierarchical/korf100")
		    ["alg", "astar"; "num", num ]
		in
		let opt_df = Datafile.load opt_data_path in
		let opt_cost = Datafile.get_val opt_df "final sol cost" in
		let opt_time = Datafile.get_val opt_df "total raw cpu time" in
		let time_limit =
		  Some (frac_time *. (float_of_string opt_time))
		in
		let args =
		  Wrutils.str "restarting_par_astar %f %f %s"
		    p0 p_rate opt_cost
		in
		  do_run overwrite time_limit None
		    (args ^ " --cost " ^ cost ^ " --moves " ^ moves)
		    prob_path attrs)
	     (Rdb.matching_attrs instance_root batch_attrs))
      !instance_specs;
    Notify.send_batch_completed_mail
      "Tiles_runs" "Restarting-Partial-A*" "restarting_par_astar"


let do_no_param_estimate_15 overwrite moves cost model alg frac_time =
  Notify.start_batchtime();
  let alg_attrs =
    ["alg", alg; "fraction of astar time", string_of_float frac_time; ]
  in
    List.iter
      (fun (_,rows,cols) ->
	 let batch_attrs = ["model", model;
			    "rows", string_of_int rows;
			    "cols", string_of_int cols; ] in
	   List.iter
	     (fun prob_attrs ->
		let prob_path =
		  Rdb.path_for instance_root prob_attrs in
		let prob_attrs =
		  ([("cost", cost); "moves", moves] @ prob_attrs) in
		let attrs = alg_attrs @ prob_attrs in
		let num = List.assoc "num" prob_attrs in
		let opt_data_path =
		  Rdb.path_for (User_paths.data_root ^ "/hierarchical/korf100")
		    ["alg", "astar"; "num", num ]
		in
		let opt_df = Datafile.load opt_data_path in
		let opt_cost = Datafile.get_val opt_df "final sol cost" in
		let opt_time = Datafile.get_val opt_df "total raw cpu time" in
		let time_limit =
		  Some (frac_time *. (float_of_string opt_time))
		in
		let args = Wrutils.str "%s %s" alg opt_cost in
		  do_run overwrite time_limit None
		    (args ^ " --cost " ^ cost ^ " --moves " ^ moves)
		    prob_path attrs)
	     (Rdb.matching_attrs instance_root batch_attrs))
      !instance_specs;
    Notify.send_batch_completed_mail "Tiles_runs" alg alg


let do_count_subtrees_15 overwrite moves cost model =
  Notify.start_batchtime();
  let alg_attrs =
    ["alg", "count_subtrees";
    ] in
    List.iter
      (fun (_,rows,cols) ->
	 let batch_attrs = ["model", model;
			    "rows", string_of_int rows;
			    "cols", string_of_int cols; ] in
	   List.iter
	     (fun prob_attrs ->
		let prob_path =
		  Rdb.path_for instance_root prob_attrs in
		let prob_attrs =
		  ([("cost", cost); "moves", moves] @ prob_attrs) in
		let attrs = alg_attrs @ prob_attrs in
		let num = List.assoc "num" prob_attrs in
		let opt_data_path =
		  Rdb.path_for (User_paths.data_root ^ "/hierarchical/korf100")
		    ["alg", "astar"; "num", num ]
		in
		let opt_df = Datafile.load opt_data_path in
		let opt_cost = Datafile.get_val opt_df "final sol cost" in
		let args = Wrutils.str "count_subtrees %s" opt_cost in
		  do_run overwrite None None
		    (args ^ " --cost " ^ cost ^ " --moves " ^ moves)
		    prob_path attrs)
	     (Rdb.matching_attrs instance_root batch_attrs))
      !instance_specs;
    Notify.send_batch_completed_mail
      "Tiles_runs" "Subtree Counting" "count_subtrees"




let model_on_instance alg =
  let base_instance_attrs = ["rows", "4";
			     "cols", "4";
			     "costs", "unit";
			     "moves", "standard";
			     "model", "korf";
			     "alg", "greedier_adapt";] in
    for i = 1 to 100 do
      (let iat = ("num", string_of_int i)::base_instance_attrs in
       let old_run = (Dataset.load_from_rdb_with_domain ~domain:"tiles" iat
			~name:"oldrun") in
       let herr = (Dataset.get_values float_of_string
		     ~sort:Dataset.Last "h_step" old_run).(0)
       and derr = (Dataset.get_values float_of_string
		     ~sort:Dataset.Last "d_step" old_run).(0) in
	 do_run false (Some 300.) None
	   (Wrutils.str " -- %s %f %f" alg herr derr)
	   (Wrutils.str
	      "./group/data/tiles_instances/korf/4/4/%i" i)
	   ["rows", "4";
	    "cols", "4";
	    "costs", "unit";
	    "moves", "standard";
	    "model", "korf";
	    "num", string_of_int i;
	    "alg", (Wrutils.str "%s_%s" alg "this_instance")])
    done


let model_on_next_instance alg =
  let base_instance_attrs = ["rows", "4";
			     "cols", "4";
			     "costs", "unit";
			     "moves", "standard";
			     "model", "korf";
			     "alg", "greedier_adapt";] in
    for i = 1 to 100 do
      (let iat = ("num", string_of_int((i mod 100) + 1))::base_instance_attrs in
       let old_run = (Dataset.load_from_rdb_with_domain ~domain:"tiles" iat
			~name:"oldrun") in
       let herr = (Dataset.get_values float_of_string
		     ~sort:Dataset.Last "h_step" old_run).(0)
       and derr = (Dataset.get_values float_of_string
		     ~sort:Dataset.Last "d_step" old_run).(0) in
	 do_run false (Some 300.) None
	   (Wrutils.str " -- %s %f %f" alg herr derr)
	   (Wrutils.str
	      "./group/data/tiles_instances/korf/4/4/%i" i)
	   ["rows", "4";
	    "cols", "4";
	    "costs", "unit";
	    "moves", "standard";
	    "model", "korf";
	    "num", string_of_int i;
	    "alg", (Wrutils.str "%s_%s" alg "next_instance")])
    done



(* EOF *)
