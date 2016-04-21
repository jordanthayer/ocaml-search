(* $Id: runs.ml,v 1.1 2004/12/22 20:18:00 ruml Exp ruml $

   stand-alone solver
*)
(************** stand-along solver **************)


let run_alg alg prob lim =
  (** writes results to stdout. assumes that header stuff like problem
      attrs are already written.  This is the main function for the
      stand-alone solver. *)
  Datafile.write_colnames stdout
    ["sol cost"; "nodes expanded"; "nodes generated"; "raw cpu time"];
  let (s, e, g, p, q, d),t = Wrsys.with_time (alg prob lim) in
    (match s with
	 None -> Datafile.write_pairs stdout ["found solution", "no"]
       | Some (sol, cost) ->
	   Wrutils.pr "%f\t%d\t%d\t%f\n" cost e g t;
	   Datafile.write_pairs stdout ["found solution", "yes";
					"final sol cost", string_of_float cost]);
    let trail_pairs = [ "total raw cpu time", string_of_float t;
			"total nodes expanded", string_of_int e;
			"total nodes generated", string_of_int g] in
      Datafile.write_pairs stdout trail_pairs


let get_alg name =
  let sh = (fun (s,e,g,p,q,d) -> (s,e,g,p,q,d))
  and ib = Msa.default_interface
  and arg_count,alg_call = (Wrlist.get_entry
			  (Alg_table_dups.table  @ Alg_table_dd.table)
			  "alg" name) in
    arg_count, (Alg_initializers.string_array_init alg_call sh ib)


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
	if n <> (count + 1) then
	  failwith (Wrutils.str "%s takes %d arguments after alg name, got %d"
		      alg_name (count - 2) n);
	let args = Array.sub args 0 count in
	  initer args

let parse_non_alg_args () =
  let v = ref 3
  and limit = ref [Limit.Never]
  and others = ref ["pad"]
  and heuristic = ref "" in
    Arg.parse
      [ "-v", Arg.Set_int v,
	"verbosity setting (between 1=nothing and 5=everything, default 3)";

	"--wall", Arg.Float
	  (fun l -> limit := (Limit.WallTime l)::!limit),
	"wall time limit (in seconds, default no limit)";

	"--time", Arg.Float (fun l -> limit := (Limit.Time l)::!limit),
	"search time limit (in seconds, default no limit)";

	"--gen", Arg.Int (fun l -> limit := (Limit.Generated l)::!limit),
	"search generation limit";

	"--exp", Arg.Int (fun l -> limit := (Limit.Expanded l)::!limit),
	"search expansion limit";
	"--heuristic", Arg.Set_string heuristic,
	"selects the heuristic to be used during the search"; ]
      (fun s -> Wrutils.push s others) "";
    !v, !limit, Array.of_list (List.rev !others)


let main () =
  (** alg from command line, instance from stdin, results to stdout *)
  let v, limit, args = parse_non_alg_args () in
  let a = set_up_alg args
  and v = 3
  and b = Msa.read stdin in
    Verb.with_level v
      (fun () -> run_alg a b limit)


let _ = main ()


(* EOF *)
