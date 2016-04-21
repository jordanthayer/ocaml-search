(** For simple testing.

    @author eaburns
    @since 2010-07-25
*)

let astar_h_max in_file =
  let module Graph = Planning_graph.No_mutex in
    Wrio.with_infile in_file
      (fun inch ->
	 let domain, initial, goal = Sas_parser.parse inch in
	 let is_goal = Sas.make_is_goal goal
	 and expand = Sas.make_expand domain initial
	 and build_graph = Graph.make_create domain initial goal in
	 let h state =
	   let pg = build_graph state in
	     (Graph.number_of_layers pg) -. 1.
	 in Search.astar is_goal expand h initial)


let astar_h1 in_file =
  Wrio.with_infile in_file
    (fun inch ->
       let domain, initial, goal = Sas_parser.parse inch in
       let is_goal = Sas.make_is_goal goal in
       let expand = Sas.make_expand domain initial in
       let h = Hm_heuristics.H1.make_progression domain goal
       in Search.astar is_goal expand h initial)


let astar_h2 in_file =
  Wrio.with_infile in_file
    (fun inch ->
       let domain, initial, goal = Sas_parser.parse inch in
       let is_goal = Sas.make_is_goal goal in
       let expand = Sas.make_expand domain initial in
       let h = Hm_heuristics.H2.make_progression domain goal
       in Search.astar is_goal expand h initial)


let main () =
  let verb = ref Verb.always in
  let heuristic = ref "h1" and problem = ref "output.sas" in
    Arg.parse [
      "-v", Arg.Set_int verb, "verbosity";
      "--heuristic", Arg.Set_string heuristic, "default: h1";
    ]
      (fun prb -> problem := prb)
      "plan [--heuristic <heuristic>] [<problem>]";
    Verb.set_default !verb;
    Printf.printf "Running with heuristic %s on %s\n" !heuristic !problem;
    let (res, exp, gens), time =
      Wrsys.with_time (fun () -> match !heuristic with
			 | "h1" -> astar_h1 !problem
			 | "h2" -> astar_h2 !problem
			 | "h_max" -> astar_h_max !problem
			 | _ -> failwith "Unknown heuristic")
    in
      Printf.printf "time: %f\n" time;
      Printf.printf "%d expansions\n" exp;
      Printf.printf "%d generations\n" gens;
      begin match res with
	| None -> Printf.printf "No solution\n";
	| Some ops ->
	    List.iter (fun op -> Printf.printf "%s\n" op.Sas.o_name;)
	      (List.rev ops);
	    Printf.printf "%d actions\n" (List.length ops);
      end


let _ = main ()
