(* $Id: runs.ml,v 1.1 2004/12/22 20:18:00 ruml Exp ruml $
   stand-alone solver
*)

let run_alg alg prob limit =
  (** writes results to stdout. assumes that header stuff like problem
      attrs are already written.  This is the main function for the
      stand-alone solver. *)
  let do_run = alg prob limit in
  let cost = ref infinity in
  let (sol, e, g,_,_,_), t = Wrsys.with_time do_run in
    flush_all();
      (match sol with
	   None -> Datafile.write_pairs stdout ["found solution", "no"]
	 |Some (s,c) ->
	    (Datafile.write_pairs stdout ["found solution", "yes"];
	     cost := c;
	     ignore (Tsp.check_tour prob.Tsp_instances.dists s c)));
      let trail_pairs = ["final sol cost", string_of_float !cost;
			 "total raw cpu time", string_of_float t;
			 "total nodes expanded", string_of_int e;
			 "total nodes generated", string_of_int g] @
	(Limit.to_trailpairs limit) in
	Datafile.write_pairs stdout trail_pairs


(***********************)

let select_interface heuristic name =
  match name with
    | "idastar_im" -> Tsp_instances.pathmax_interface
    | "idastar_cr" -> Tsp_instances.pathmax_interface
    | _ -> (match heuristic with
	      | "greedy" -> Tsp_inadmiss.greedy_interface
	      | "mixed" -> Tsp_inadmiss.mixed_interface
	      | _ -> Tsp_instances.default_interface)

let get_alg heuristic name =
  let sh = Tsp.unwrap_results
  and ib = select_interface heuristic name
  and arg_count, alg_call = (Wrlist.get_entry
			       (Alg_table_dups.table @ Alg_table_dd.table)
			       "alg" name) in
    arg_count, (Alg_initializers.string_array_init alg_call sh ib)


let set_up_alg heuristic args =
  (** looks at command line.  returns alg_func (which is world -> node
      option * int * int) and list of string pairs for logging parameters. *)
  let n = Array.length args in
    if n < 1
    then
      (Wrutils.pr "expects a problem on stdin, writes log info to stdout.\n";
       Wrutils.pr "First arg must be an algorithm name (such as \"a_star\"),\n";
       Wrutils.pr "other args depend on alg (eg, weight for wted_a_star).\n";
       failwith "not enough command line arguments")
    else
      (let alg_name = args.(1) in
       let count, initer = get_alg heuristic alg_name in
	 if n <> (1 + count) then
	   failwith (Wrutils.str "%s takes %d arguments after alg name; got %d"
		       alg_name (count) n);
	 let args = Array.sub args 1 count in
	   initer args)

let parse_non_alg_args () =
  let v = ref 2
  and limit = ref [Limit.Never]
  and heuristic = ref ""
  and others = ref ["pad"] in
    Arg.parse
      ((Limit.arg_specs limit) @
	 [ "-v", Arg.Set_int v,
	   "verbosity setting (between 1=nothing and 5=everything, default 2)";


	   "--heuristic", Arg.Set_string heuristic,
	   "selects the heuristic to be used during the search";

	   "--", Arg.Rest (fun str ->
			     let vals = Str.split (Str.regexp " ") str in
			       List.iter (fun s -> Wrutils.push s others)
				 vals),
	   "Everything after this is interpreted as an argument to the solver";
	 ])
      (fun s -> Wrutils.push s others) "";
    !v, !limit, !heuristic, Array.of_list (List.rev !others)

let main () =
  (** alg from command line, instance from stdin, results to stdout *)
  let v,limit, heuristic, args = parse_non_alg_args () in
  let a = set_up_alg heuristic args
  and p = Tsp_instances.read_tsplib stdin in
    Verb.with_level v
      (fun () -> run_alg a p limit)


let _ = main ()


(* EOF *)
