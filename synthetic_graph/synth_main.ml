(** Main file for running solvers against synthetic domains *)

let run_alg alg b limit =
  (** writes results to stdout. assumes that header stuff like problem
    attrs are already written.  This is the main function for the stand-alone
    solver. *)
  let do_run = alg b limit in
  let (s, e, g, p, m), t = Wrsys.with_time do_run in
    (match s with
	 None -> Datafile.write_pairs stdout ["found solution", "no"]
       | Some (p,f) -> Datafile.write_pairs stdout ["found solution", "yes"]);
  let cost, len = (match s with
		       None -> infinity, 0
		     | Some (p, f) -> f, (p.Random_tree.data.Stu_tree.depth)) in
    let trail_pairs = [
      "final sol cost", string_of_float cost;
		       "final sol length", string_of_int len;
		       "total raw cpu time", string_of_float t;
		       "total nodes expanded", string_of_int e;
		       "total nodes generated", string_of_int g] @
      (Limit.to_trailpairs limit) in
      Datafile.write_pairs stdout trail_pairs

and instance_root = User_paths.instance_root ^ "synthetic_instances"


let select_interface cache heuristic error alg_name =
  match alg_name with
(*    | "idastar_online_model" ->
	Stu_tree.online_model_interface ~cache:cache
	  ~h:(Stu_tree.string_to_heuristic ~opt_arg:error heuristic)
	  instance_root*)
    | _ -> Stu_tree.make_interface ~cache:cache
	~h:(Stu_tree.string_to_heuristic ~opt_arg:error heuristic)
	  instance_root


let get_alg cache heuristic error name =
  let sh = (fun (a,b,c,d,e) -> (a,b,c,d,e))
  and ib = select_interface cache heuristic error name
  and arg_count, alg_call = Wrlist.get_entry Alg_table_nodups.table "alg" name
  in arg_count, (Alg_initializers.string_array_init alg_call sh ib)


let set_up_alg cache args =
  (** looks at command line.  returns alg_func (which is world -> node
      option * int * int) and list of string pairs for logging parameters. *)
  let n = Array.length args in
    if n < 1 then
      (Wrutils.pr "expects a tree on stdin, writes to stdout.\n";
       Wrutils.pr "First arg must be a heurisic\n";
       Wrutils.pr "Second arg must be an algorithm name (such as \"a_star\"),\n";
       Wrutils.pr "other args depend on alg (eg, \"max_dim\").\n";
       failwith "not enough command line arguments")
    else
      let heuristic = args.(1)
      and error = float_of_string args.(2)
      and alg_name = args.(3) in
      let count, initer = get_alg cache heuristic error alg_name in
	if n <> (3 + count) then
	  failwith (Wrutils.str "%s takes %d arguments after alg name" alg_name (count - 3));
	let args = Array.sub args 3 count in
	  initer args


let parse_non_alg_args () =
  let v = ref 3
  and limit = ref [Limit.Never]
  and cache = ref false
  and others = ref ["pad"] in
    Arg.parse
      [ "-v", Arg.Set_int v,
	"verbosity setting (between 1=nothing and 5=everything, default 3)";

	"-cache", Arg.Set cache, "turns caching of the heuristic on";

	"--wall", Arg.Float
	  (fun l -> limit := (Limit.WallTime l)::!limit),
	"wall time limit (in seconds, default no limit)";

	"--time", Arg.Float (fun l -> limit := (Limit.Time l)::!limit),
	"search time limit (in seconds, default no limit)";

	"--gen", Arg.Int (fun l -> limit := (Limit.Generated l)::!limit),
	"search generation limit";

	"--exp", Arg.Int (fun l -> limit := (Limit.Expanded l)::!limit),
	"search expansion limit";
      ]
      (fun s -> Wrutils.push s others) "";
    !v, !limit,!cache, Array.of_list (List.rev !others)


let main () =
  (** alg from command line, board from stdin, results to stdout *)
  let v,limit,cache,args = parse_non_alg_args () in
    (* Eventually I'll want to replace this so that the board is an argument
       and not a pipe. *)
  let file_name = (Unix.readlink (Unix.readlink "/dev/stdin")) in
  let a = set_up_alg cache args
  and b = Stu_tree.read_instance_ch file_name stdin in
    Verb.with_level v
      (fun () ->
	 run_alg a b limit)


let _ = main ()
(* EOF *)
