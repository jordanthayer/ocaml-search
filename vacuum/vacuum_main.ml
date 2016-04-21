(** Vacuum Main, made for running vacuums. *)

module VI = Vacuum_interfaces
module VIA = Vacuum_inadmiss
module RV = Reverse_vacuum

let with_path6 (s, e, g, p, m, d) =
  (match s with
    None -> None
  | Some (n, f) -> Some (n, f)), e, g, p, m, d

let run_alg alg b limit =
  (** writes results to stdout. assumes that header stuff like problem
    attrs are already written.  This is the main function for the stand-alone
    solver. *)
  let do_run = alg b limit in
  let (s, e, g, p, m, d), t = Wrsys.with_time do_run in
    (match s with
	 None -> Datafile.write_pairs stdout ["found solution", "no"]
       | Some (p,f) -> Datafile.write_pairs stdout ["found solution", "yes"]);
  let cost, len = (match s with
		       None -> infinity, 0
		     | Some (p, f) -> f, Vacuum.sol_length p) in
    let trail_pairs = ["final sol cost", string_of_float cost;
		       "final sol length", string_of_int len;
		       "total raw cpu time", string_of_float t;
		       "total nodes expanded", string_of_int e;
		       "total nodes generated", string_of_int g] @
      (Limit.to_trailpairs limit) in
      Datafile.write_pairs stdout trail_pairs


let wp_iface heuristic =
  match heuristic with
    | "spanning tree" -> VI.symmetric_wp_interface
    | "improved d" -> VI.improved_d_wp_interface
    | "greedy" -> Vacuum_inadmiss.greedy_wp_interface
    | "displacement" -> VI.displacement_wp_interface
    | "chris" -> VI.chris_wp_interface
    | "chris_id" -> VI.chris_improved_d_wp_interface
    | _ -> failwith "No other with parent interfaces"


let select_interface heuristic memo cost iface name =
  match iface with
    | "" ->
	(match name with
	   | "idastar_im"
	   | "idastar_im2"
	   | "idastar_im3"
	   | "idastar_im_no_depth"
	   | "idastar_cr" -> VI.pathmax_interface
	   | "hillclimbing" -> wp_iface heuristic
	   | "enforced_hillclimbing" -> wp_iface heuristic
	   | "lsslrtastar" -> wp_iface heuristic
	   | _ ->
	       (match cost with
		  | "unit" ->
		      (match heuristic with
			 | "spanning tree" -> VI.symmetric_interface
			 | "improved d" -> VI.improved_d_interface
			 | "displacement" -> VI.displacement_interface
			 | "scaled_spanning" ->
			     (VIA.scaled_symmetric_interface 0.2)
			 | "greedy" -> (VIA.greedy_interface)
			 | "chris" -> VI.chris_interface
			 | "chris_id" -> VI.chris_improved_d_interface
			 | "greedy_chris" ->
			     Vacuum_inadmiss.greedy_chris_interface
			 | _ -> failwith
			     (Wrutils.str "%s not recognized as a heuristic"
				heuristic))
		  | "heavy" ->
		      (match heuristic with
			 | "spanning tree" -> VI.heavy_interface ~memo
			 | "improved d" -> VI.heavy_greedy_interface ~memo
			 | _ -> failwith
			     (Wrutils.str "%s not recognized as a heuristic"
				heuristic))
		  | _ -> failwith "Didn't recognize cost"))

    | "reverse_spanning" -> RV.reverse_interface_spanning
    | "reverse_displacement" -> RV.reverse_interface_displacement
    | "exhaust_spanning" -> VI.exhaust_symmetric
    | "heavy" -> VI.heavy_interface ~memo
    | "heavy_greedy" -> VI.heavy_greedy_interface ~memo
    | "heavy_inadmiss" -> Vacuum_inadmiss.heavy_inadmiss_interface
    | "record" -> RV.reverse_heavy_greedy_interface ~memo
    | _ -> failwith "Iface functionality not yet ready"


let get_alg heuristic memo cost iface name =
  let sh = with_path6
  and ib = select_interface heuristic memo cost iface name
  and arg_count, alg_call = (Wrlist.get_entry
			       (Alg_table_dups.table @ Alg_table_dd.table)
			       "alg" name) in
    arg_count, (Alg_initializers.string_array_init alg_call sh ib)


let set_up_alg heuristic memo cost iface args =
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
      let count, initer = get_alg heuristic memo cost iface alg_name in
	if n <> (1 + count) then
	  failwith (Wrutils.str "%s takes %d arguments after alg name"
		      alg_name (count - 1));
	let args = Array.sub args 1 count in
	  initer args


let parse_non_alg_args () =
  let v = ref 2
  and limit = ref [Limit.Never]
  and heuristic = ref "improved d"
  and cost = ref "standard"
  and memo = ref false
  and iface = ref ""
  and others = ref ["pad"] in
    Arg.parse
      ((Limit.arg_specs limit) @
	 [ "-v", Arg.Set_int v,
	   "verbosity setting (between 1=nothing and 5=everything, default 2)";

	   "--heuristic", Arg.Set_string heuristic,
	   "selects the heuristic to be used during the search";

	   "--cost", Arg.Set_string cost,
	   "Selects the cost model (standard, heavy)";

	   "--iface", Arg.Set_string iface,
	   "specifies an interface for use in search";

	   "--memo", Arg.Set memo, "Toggles heuristic memoization";

	   "--", Arg.Rest (fun str ->
			     let vals = Str.split (Str.regexp " ") str in
			       List.iter (fun s -> Wrutils.push s others) vals),
	   "Everything after this is interpreted as an argument to the solver";
	 ])
      (fun s -> Wrutils.push s others) "";
    !v, !limit, !heuristic, !cost, !iface, !memo,
  Array.of_list (List.rev !others)


let main () =
  (** alg from command line, board from stdin, results to stdout *)
  let v, limit, heuristic, cost, iface, memo, args = parse_non_alg_args () in
  let file_name = (Unix.readlink (Unix.readlink "/dev/stdin")) in
  let a = set_up_alg heuristic memo cost iface args
  and inst =
    Vacuum_instance.read (Vacuum_instance.prob_path_to_instance file_name) stdin
  in Verb.with_level v (fun () -> run_alg a inst limit)

let _ = main ()
(* EOF *)
