(* Solver for the rucksack problem *)


let run_alg limit alg prob =
  (** writes results to stdout. assumes that header stuff like problem
      attrs are already written.  This is the main function for the
      stand-alone solver. *)
  let do_run = alg prob limit in
  let (s,e,g,p,m),t = Wrsys.with_time do_run in
  let cost,len = (match s with
		      None ->
			(Datafile.write_pairs stdout ["found solution", "no"];
			 infinity,0)
		    | Some (p,f) ->
			(Datafile.write_pairs stdout ["found solution", "yes"];
			 f, Zero_one_tree.sol_length p)) in
    let trail_pairs = ["final sol cost", string_of_float cost;
		       "final sol length", string_of_int len;
		       "total raw cpu time", string_of_float t;
		       "total nodes expanded", string_of_int e;
		       "total nodes generated", string_of_int g] @
      (Limit.to_trailpairs limit) in
      Datafile.write_pairs stdout trail_pairs


let get_alg name =
  let sh = (fun id -> id)
  and ib = Zero_one_tree.default_interface
  and arg_count, alg_call = Wrlist.get_entry Alg_table_nodups.table "alg" name
  in arg_count, (Alg_initializers.string_array_init alg_call sh ib)


let set_up_alg args =
  (** looks at command line.  returns alg_func (which is world -> node
      option * int * int) and list of string pairs for logging parameters. *)
  let n = Array.length args in
    if n < 2 then
      (Wrutils.pr "expects a problem on stdin, writes log info to stdout.\n";
       Wrutils.pr "First arg must be an algorithm name (such as \"a_star\"),\n";
       Wrutils.pr "other args depend on alg (eg, weight for wted_a_star).\n";
       failwith "not enough command line arguments")
    else
      let alg_name = args.(1) in
      let count, initer = get_alg alg_name in
	if n <> (1 + count)
	then failwith (Wrutils.str "%s takes %d arguments after alg name"
			 alg_name (count - 2));
	initer (Array.sub args 1 count)

let parse_non_alg_args () =
  let v = ref Verb.always
  and limit = ref [Limit.Never]
  and others = ref ["pad"] in
    Arg.parse [ "-v", Arg.Set_int v, "verbosity setting (between 1=nothing and 5=everything, default 1)";
		"--time", Arg.Float (fun l -> limit := (Limit.Time l)::!limit), "search time limit (in seconds, default no limit)";
		"--gen", Arg.Int (fun l -> limit := (Limit.Generated l)::!limit), "search generation limit";
		"--exp", Arg.Int (fun l -> limit := (Limit.Expanded l)::!limit), "search expansion limit";
		]
      (fun s -> Wrutils.push s others) "";
    !v, !limit, Array.of_list (List.rev !others)


let main () =
  (** alg from command line, board from stdin, results to stdout *)
  let v,limit,args = parse_non_alg_args () in
  let alg = set_up_alg args
  and problem = Zero_one_tree.read_instance "from_std_in" stdin in
    Verb.with_level v
      (fun () -> run_alg limit alg problem)


let _ = main ()



(* EOF *)
