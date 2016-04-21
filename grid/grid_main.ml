(* $Id: main.ml,v 1.1 2004/12/22 20:17:58 ruml Exp ruml $
   main file for grid problem solving
*)


let run_alg alg b limit =
  (** writes results to stdout. assumes that header stuff like problem
      attrs are already written.  This is the main function for the stand-alone
      solver. *)
  let do_run = alg b limit in
  let (s, e, g, p, m, d), t = Wrsys.with_time do_run in
  let cost, len =
    (match s with
       | None -> (Datafile.write_pairs stdout ["found solution", "no"];
		  infinity, -1)
       | Some (p, f) -> (Datafile.write_pairs stdout ["found solution", "yes"];
			 (Grid.check_path b p f), (List.length p))) in
  let trail_pairs = ["final sol cost", string_of_float cost;
		     "final sol length", string_of_int len;
		     "total raw cpu time", string_of_float t;
		     "total nodes expanded", string_of_int e;
		     "total nodes generated", string_of_int g] @
    (Limit.to_trailpairs limit) in
    Datafile.write_pairs stdout trail_pairs


and instance_root = "./group/data/grid_instances"

let select_interface heuristic corruption path alg_name =
  match alg_name with
    | "truth" -> Grid_interfaces.record_interface
    | "record_astar" -> Grid_interfaces.exhaust_interface
    | "aseps_near" -> Grid_interfaces.nearest_interface
    | "dwa_redux_near" -> Grid_interfaces.nearest_interface
    | "tqs_corrupted_truth" ->
	Truth_heuristics.corrupted_hhat ~iroot:instance_root ~p:path corruption
    | "record_tqs_h_exp" ->
	Truth_heuristics.corrupted_hhat ~iroot:instance_root ~p:path corruption
    | "record_tqs_h_clean" ->
	Truth_heuristics.corrupted_hhat ~iroot:instance_root ~p:path corruption
    | "record_tqs_h_geq" ->
	Truth_heuristics.corrupted_hhat ~iroot:instance_root ~p:path corruption
    | "record_tqs_h_focal" ->
	Truth_heuristics.corrupted_hhat ~iroot:instance_root ~p:path corruption
    | "lsslrtastar" -> Grid_interfaces.wp_interface
    | "srtastar" -> Grid_interfaces.wp_interface
    | "rtastar" -> Grid_interfaces.wp_interface
    | "dtastar" -> Grid_interfaces.wp_interface
    | "dtastar_learn" -> Grid_interfaces.wp_interface
    | "hillclimbing" -> Grid_interfaces.wp_interface
    | "enforced_hillclimbing" -> Grid_interfaces.wp_interface
    | _ ->
	(if Str.string_match (Str.regexp_case_fold ".*vector.*") alg_name 0
	 then Grid_interfaces.avg_vect_interface
	   (Offline_vectors.string_to_model alg_name)
	 else
	   (match heuristic with
	      | "bandwidth" -> (Truth_heuristics.online_bandwidth_interface
				  corruption)
	      |	"manhattan" -> Grid_interfaces.default_interface
	      | "constantly_one" -> Grid_interfaces.constantly_one
	      | "nearest" -> Grid_interfaces.nearest_interface
	      | "corrupted_truth" -> Truth_heuristics.truth_interface
		  ~iroot:instance_root ~p:path corruption
	      | "online_truth" -> Truth_heuristics.online_truth_interface
	      | "online_corrupted" ->
		  (Truth_heuristics.online_corrupted_interface corruption)
	      | "canonical" ->
		  (Canonical_heuristic.canonical_interface ~g_path:path 50)
	      | "canonical_max" ->
		  (Canonical_heuristic.canonical_maxed_with_admiss_interface
		     ~g_path:path 50)
	      | "countblocked" ->
		  Grid_inadmiss.inadmissible_heuristic_interface_countblocked
	      | "scaled" -> Grid_inadmiss.inadmissible_heuristic_interface_scaled
	      | _ -> failwith
		  (Wrutils.str "%s: Heuristic not recognized" heuristic)))


let get_alg heuristic corruption path name =
  let sh = Grid_algs.with_path6
  and ib = select_interface heuristic corruption path name
  and arg_count,alg_call =
    match name with
      | "ted" ->
	  (let p = Grid_instance.load path in
	   let rec gs = { Grid.pos = List.hd p.Grid.goal;
			  Grid.parent = gs} in
	     (1, Single_queue_bidirectional.call_search_from_iface gs
		Grid.get_parent))
      | _ -> (let tables = Alg_table_dups.table @ Alg_table_dd.table in
	      let acount, alg = Wrlist.get_entry tables "alg" name in
		acount, alg) in
    arg_count, (Alg_initializers.string_array_init alg_call sh ib)



let set_up_alg heuristic corruption path args =
  (** looks at command line.  returns alg_func (which is world -> node
      option * int * int) and list of string pairs for logging parameters. *)
  let n = Array.length args in
    if n < 1 then
      (Wrutils.pr "expects a board on stdin, writes to stdout.\n";
       Wrutils.pr "First arg must be an algorithm name (such as \"a_star\"),\n";
       Wrutils.pr "second arg must be either \"4-way\" or \"8-way\",\n";
       Wrutils.pr "other args depend on alg (eg, \"max_dim\").\n";
       failwith "not enough command line arguments")
    else
      let alg_name = args.(1) in
      let count, initer = get_alg heuristic corruption path alg_name in
	if n <> (1 + count) then
	  failwith (Wrutils.str "%s takes %d arguments after alg name" alg_name (count - 1));
	let args = Array.sub args 1 count in
	  initer args


let parse_non_alg_args () =
  let v = ref 3
  and limit = ref [Limit.Never]
  and heuristic = ref "manhattan"
  and corruption = ref 1.
  and others = ref ["pad"] in
    Arg.parse
      [ "-v", Arg.Set_int v,
	"verbosity setting (between 1=nothing and 5=everything, default 3)";

	"--wall", Arg.Float
	  (fun l -> limit := (Limit.WallTime l)::!limit),
	"wall time limit (in seconds, default no limit)";

	"--time", Arg.Float
	  (fun l -> limit := (Limit.Time l)::!limit),
	"search time limit (in seconds, default no limit)";

	"--gen", Arg.Int (fun l -> limit := (Limit.Generated l)::!limit),
	"search generation limit";

	"--exp", Arg.Int (fun l -> limit := (Limit.Expanded l)::!limit),
	"search expansion limit";

	"--heuristic", Arg.Set_string heuristic,
	"selects the heuristic to be used during the search";

	"--corruption", Arg.Set_float corruption,
	"Scales herustic down";

	"--", Arg.Rest (fun str ->
			  let vals = Str.split (Str.regexp " ") str in
			    List.iter (fun s -> Wrutils.push s others) vals),
	"Everything after this is interpreted as an argument to the solver";
      ]
      (fun s -> Wrutils.push s others) "";
    !v, !limit, !heuristic, !corruption, Array.of_list (List.rev !others)


let main () =
  (** alg from command line, board from stdin, results to stdout *)
  let v,limit,heuristic, corruption, args = parse_non_alg_args () in
    (* Eventually I'll want to replace this so that the board is an argument
       and not a pipe. *)
  let file_name = (Unix.readlink (Unix.readlink "/dev/stdin")) in
  let a = set_up_alg heuristic corruption file_name args
  and b = Grid_instance.read
    (Str.string_match (Str.regexp ".*seedinst.*") file_name 0)
    (Grid_instance.prob_path_to_instance file_name) stdin in
    Verb.with_level v
      (fun () ->
	 run_alg a b limit)


let _ = main ()


(* EOF *)
