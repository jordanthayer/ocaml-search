(* $Id: main.ml,v 1.1 2005/04/06 00:38:57 ruml Exp ruml $
   main for stand-alone regression tplan executable
*)

let run_alg alg domain problem limit =
  let do_run = alg (domain,problem) limit in
  let (sol, e, g, p, q),t = Wrsys.with_time do_run in
    flush_all ();
    (match sol with
	 None ->
	   Verb.pr Verb.always "infinity\t-1\t%d\t%d\t%f\n" e g t;
	   Datafile.write_pairs stdout ["found solution", "no";
					"final sol cost", "infinity"]
       | Some (s, ms) ->
	   Verb.force 2 (lazy (Tpl_regression.print_plan stderr s.Tpl_regression.s));
	   Datafile.write_pairs stdout ["found solution", "yes";
					"final sol cost", string_of_float ms]);

    let trail_pairs = ["total raw cpu time", string_of_float t;
		       "total nodes expanded", string_of_int e;
		       "total nodes generated", string_of_int g;
		       "total nodes pruned", string_of_int p;
		       "max search queue", string_of_int q;
		       "time per node", string_of_float (t /. (float) g)] @
      (Limit.to_trailpairs limit) in
      Datafile.write_pairs stdout trail_pairs


let read ch =
  let buf = Lexing.from_channel ch in
  let domain = Wrlexing.parse_verb 5 stderr "planning domain"
		 (Parse_pddl.domain Lex.lexer) buf in
  let problem = Wrlexing.parse_verb 5 stderr "problem instance"
		  (Parse_pddl.problem Lex.lexer) buf in
  let problem = Tpl_domain.correct_problem domain problem in
    domain, problem


let run_alg_ch alg limit ch =
  let domain, problem = read ch in
    run_alg alg domain problem limit


let get_alg name =
  let sh = (fun id -> id)
  and ib = Tpl_regression.default_interface
  and arg_count, alg_call =  Wrlist.get_entry Alg_table_nodups.table "alg" name
  in arg_count, (Alg_initializers.string_array_init alg_call sh ib)


let set_up_alg args =
  (** looks at command line.  returns alg_func (which is world -> node
    option * int * int) and list of string pairs for logging parameters. *)
  let n = Array.length args in
    if n < 1 then
      (Wrutils.pr "expects a problem on stdin, writes log info to stdout.\n";
       Wrutils.pr "First arg must be an algorithm name (such as \"a_star\"),\n";
       Wrutils.pr "other args depend on alg (eg, weight for wted_a_star).\n";
       failwith "not enough command line arguments");
    let alg_name = args.(0) in
    let count, initer = get_alg alg_name in
      if n <> count then
	failwith (Wrutils.str "%s takes %d arguments after alg name (got %d)"
		    alg_name count (n - 1));
      initer args


let parse_non_alg_args () =
  let v = ref 3
  and limit = ref [Limit.Never]
  and others = ref [] in
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
	"search expansion limit";]

      (fun s -> Wrutils.push s others) "";
    !v, !limit, Array.of_list (List.rev !others)


let main () =
  (** alg from command line, instance from stdin, results to stdout *)
  let v, limit, args = parse_non_alg_args () in
  let a = set_up_alg args in
    Verb.with_level v
      (fun () -> run_alg_ch a limit stdin)


let _ = main ()


(* EOF *)
