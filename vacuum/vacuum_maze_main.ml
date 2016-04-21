(** For running condensed vacuum mazes.  Copied off of vacuum_main.ml.

    @author eaburns
    @since 2009-11-30
*)

let run_alg alg b limit =
  (** writes results to stdout. assumes that header stuff like problem
      attrs are already written.  This is the main function for the stand-alone
      solver. *)
  let do_run = alg b limit in
  let (s, e, g, p, m, d), t = Wrsys.with_time do_run in
    (match s with
	 None -> (Datafile.write_pairs stdout ["found solution", "no"];
		  Verb.pr Verb.always "%d\t%d\t%d\t%d\t%f\n" 0 0 e g t;)
       | Some (p,f) -> Datafile.write_pairs stdout ["found solution", "yes"]);
    let cost, len = (match s with
			 None -> infinity, 0
		       | Some (p, f) -> f, Vacuum_maze.sol_length p) in
    let s = Wrsys.get_proc_status [| "VmPeak:" |] (Unix.getpid ()) in
    let trail_pairs = ["final sol cost", string_of_float cost;
		       "final sol length", string_of_int len;
		       "peak virtual mem usage kb", s.(0);
		       "total raw cpu time", string_of_float t;
		       "total nodes expanded", string_of_int e;
		       "total nodes generated", string_of_int g] @
      (Limit.to_trailpairs limit) in
      Datafile.write_pairs stdout trail_pairs


let select_interface = function
  | _ -> Vacuum_maze.default_interface


let with_path6 (s, e, g, p, m, d) =
  (match s with
    None -> None
  | Some (n, f) -> Some (n, f)), e, g, p, m, d

let get_alg name =
  let sh = with_path6
  and ib = select_interface name
  and arg_count, alg_call =
    (Wrlist.get_entry
       (Alg_table_dups.table @ Alg_table_dd.table)
       "alg"
       name) in
    arg_count, (Alg_initializers.string_array_init alg_call sh ib)

let set_up_alg args =
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
      let count, initer = get_alg alg_name in
	if n <> (1 + count) then
	  failwith (Wrutils.str "%s takes %d arguments after alg name" alg_name (count - 1));
	let args = Array.sub args 1 count in
	  initer args


let parse_non_alg_args () =
  let v = ref Verb.often
  and limit = ref [Limit.Never]
  and others = ref ["pad"] in
    Arg.parse
      ([ "-v", Arg.Set_int v,
	 "verbosity setting (between 1=nothing and 5=everything, default 2)";
       ] @ (Limit.arg_specs limit))
      (fun s -> Wrutils.push s others) "";
    !v, !limit, Array.of_list (List.rev !others)


let main () =
  (** alg from command line, board from stdin, results to stdout *)
  let v,limit,args = parse_non_alg_args () in
    Verb.with_level v
      (fun () ->
	 let file_name = (Unix.readlink (Unix.readlink "/dev/stdin")) in
	 let a = set_up_alg args
	 and inst =
	   Vacuum_instance.read (Vacuum_instance.prob_path_to_instance file_name) stdin
	 in
	 let maze = Vacuum_maze.of_instance inst
	 in run_alg a maze limit)


let _ = main ()
(* EOF *)

