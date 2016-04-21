(**

   Builds executables for solitaire chess.

*)


let run_alg get_len alg b limit =
  (** writes results to stdout. assumes that header stuff like problem
    attrs are already written.  This is the main function for the stand-alone
    solver. *)
  let do_run = alg b limit in
  let (s, e, g, p, m, d), t = Wrsys.with_time do_run in
    (match s with
	 None -> (Datafile.write_pairs stdout ["found solution", "no"];)
       | Some (p,f) -> Datafile.write_pairs stdout ["found solution", "yes"]);
  let cost, len = (match s with
		       None -> infinity, 0
		     | Some (p, f) -> f, get_len p) in
    let trail_pairs = ["final sol cost", string_of_float cost;
		       "final sol length", string_of_int len;
		       "total raw cpu time", string_of_float t;
		       "total nodes expanded", string_of_int e;
		       "total nodes generated", string_of_int g] @
      (Limit.to_trailpairs limit) in
      Datafile.write_pairs stdout trail_pairs


let with_path6 (s, e, g, p, m, d) =
  (match s with None -> None | Some (n, f) -> Some (n, f)), e, g, p, m, d


let get_alg interface name =
  let sh = with_path6 in
  let arg_count, alg_call = (Wrlist.get_entry
			       (Alg_table_dups.table @ Alg_table_dd.table)
			       "alg" name) in
    arg_count, (Alg_initializers.string_array_init alg_call sh interface)


let parse_non_alg_args () =
  let v = ref 2
  and limit = ref [Limit.Never]
  and cost = ref "Unit"
  and others = ref []
  in 

  let parse_memory_limit mls = 
    match mls with 
	"max" -> limit := (Limit.MachineMemory)::!limit
      | bad_string -> failwith 
	  (Printf.sprintf "\"%s\" is an invalid memory limit" bad_string) in

    Arg.parse
      [ "-v", Arg.Set_int v,
	"Verbosity setting (between 1=nothing and 5=everything, default 2)";

	"--wall", Arg.Float
	  (fun l -> limit := (Limit.WallTime l)::!limit),
	"wall time limit (in seconds, default no limit)";

	"--time", Arg.Float (fun l -> limit := (Limit.Time l)::!limit),
	"search time limit (in seconds, default no limit)";

	"--gen", Arg.Int (fun l -> limit := (Limit.Generated l)::!limit),
	"search generation limit";

	"--exp", Arg.Int (fun l -> limit := (Limit.Expanded l)::!limit),
	"search expansion limit";

	"--memory", Arg.String parse_memory_limit,
	"specifies how much memory to use";

	"--max_mem", Arg.Unit (fun () -> limit := (Limit.MachineMemory)::!limit),
	"quit when main memory is exhausted";

	"--cost", Arg.Set_string cost,
	"Specify what cost function to use";

	"--", Arg.Rest (fun str ->
			  let vals = Str.split (Str.regexp " ") str in
			    List.iter (fun s -> Wrutils.push s others)
			      vals),
	"Everything after this is interpreted as an argument to the solver";
      ]
      (fun s -> Wrutils.push s others) "";
    !v, !limit, !cost, 
  Array.of_list (List.rev !others)



let set_up_alg 
    iface
    inst args =
  (** Looks at command line.  returns alg_func (which is world -> node
      option * int * int) and list of string pairs for logging
      parameters. *)


  let n = Array.length args in
    if n < 1 then
      (Wrutils.pr "Expects a board on stdin, writes to stdout.\n";
       Wrutils.pr "First arg must be an algorithm name (such as \"a_star\")\n";
       failwith "not enough command line arguments")
    else
      let alg_name = args.(0) in

      let count, initer = get_alg iface alg_name in
	if n <> (count) then
	  (
	    (Printf.fprintf stderr "n is %d count is %d\n" n count);
	    failwith
	      (Wrutils.str "%s takes %d arguments after alg name"
		 alg_name (count - 1)));

	initer args


let main () =
  (** alg from command line, board from stdin, results to stdout *)


  let v, limit, cost_name, args = 
    parse_non_alg_args () in
    Verb.with_level v
      (fun () ->
	 let inst = Solitaire_chess_io.read_instance stdin in
	   (
	     let iface = Solitaire_chess_interfaces.solitaire_chess_interface
	       cost_name in
	     let alg = set_up_alg iface inst args in
	       run_alg Solitaire_chess.sol_length alg inst limit
	   )
      )


let _ = main ()
