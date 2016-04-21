(* $Id: main.ml,v 1.1 2005/01/30 17:33:39 ruml Exp ruml $

   stand-alone solver
*)


let run_alg alg prob limit =
  (** writes results to stdout. assumes that header stuff like problem
      attrs are already written.  This is the main function for the
      stand-alone solver. *)
  let do_run = alg prob limit in
  let (s,e,g,p,m,d),t = Wrsys.with_time do_run in
  let cost,len = (match s with
		      None ->
			(Datafile.write_pairs stdout ["found solution", "no"];
			 infinity,0)
		    | Some (p,f) ->
			(Datafile.write_pairs stdout ["found solution", "yes"];
			 let pth, t = Dynamic.extract_path s in
			   f, (List.length pth))) in
    let trail_pairs = ["final sol cost", string_of_float cost;
		       "final sol length", string_of_int len;
		       "total raw cpu time", string_of_float t;
		       "total nodes expanded", string_of_int e;
		       "total nodes generated", string_of_int g] @
      (Limit.to_trailpairs limit) in
      Datafile.write_pairs stdout trail_pairs


let get_alg heuristic name =
  let re = Str.regexp ".*lazy.*" in
  let sh = (fun id -> id)
  and ib = (if(Str.string_match re name 0) then
	      Dynamic.lazy_interface
	    else (match heuristic with
		    | "slowdown" -> Drn_inadmiss.slowdown_interface
		    | "slowdownp" ->
			Drn_inadmiss.slowdown_percent_interface 0.25
		    | "round" -> Drn_inadmiss.round_interface
		    | "mirror" -> Drn_inadmiss.mirror_interface
		    | _ -> Dynamic.default_interface)) in
  let arg_count, alg_call = (Wrlist.get_entry
			       (Alg_table_dups.table @ Alg_table_dd.table)
			       "alg" name) in
    arg_count, (Alg_initializers.string_array_init alg_call sh ib)


let set_up_alg heuristic args =
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
      let count, initer = get_alg heuristic alg_name in
	if n <> (1 + count)
	then failwith (Wrutils.str "%s takes %d arguments after alg name"
			 alg_name (count - 2));
	initer (Array.sub args 1 count)

let parse_non_alg_args () =
  let v = ref 3
  and limit = ref [Limit.Never]
  and heuristic = ref ""
  and others = ref ["pad"] in
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
	"selects the heuristic to be used during the search";

      "--", Arg.Rest (fun str ->
			let vals = Str.split (Str.regexp " ") str in
			  List.iter (fun s -> Wrutils.push s others)
			    vals),
      "Everything after this is interpreted as an argument to the solver";
      ]
      (fun s -> Wrutils.push s others) "";
    !v, !limit, !heuristic, Array.of_list (List.rev !others)


let main () =
  (** alg from command line, board from stdin, results to stdout *)
  let v,limit,heuristic,args = parse_non_alg_args () in
  let alg = set_up_alg heuristic args
  and board = Drn_instance.read stdin in
    Verb.with_level v
      (fun () -> run_alg alg board limit)


let _ = main ()



(* EOF *)
