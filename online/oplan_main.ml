(*
  Online Planner. Will be first implemented by wrapping the
  progression planner with online capability: (1) receiving message; (2)
  ask the simulator for the future committements; (3) new branching
  rules that avoid mutex with future committements.

  try with a single search algorithm -- refer to main_pro/reg.ml for how
  to interface with multiple search algorithms.

  Note: regression would have to be armed with a STN. *)

let parse_args () =
  let verb = ref 3
  and p_alg = ref "unspeficied"
  and s_alg = ref "unspecified"
  and obj = ref "unspecified"
  and s_heu = ref "rp"
  and ept = ref 1.0
  and pp = ref false
  and weight = ref 3.0
  and runtime = ref 100.0
  and basetime = ref infinity
   in
    Arg.parse [ "-debug", Arg.Int (fun i -> verb := i),
		"\tverbosity of debugging output (1-5)";
	      "-p", Arg.String (fun s -> p_alg := s),
	      "\tSelect planner type";
	      "-s", Arg.String (fun s -> s_alg := s),
	      "\tSelect the search algorithm";
	      "-o", Arg.String (fun s -> obj := s),
	      "\tSelect the objective function";
	      "-ept", Arg.Float (fun f -> ept := f),
	      "\tSet the estimated planning time value";
	      "-step_heu", Arg.String (fun s -> s_heu := s),
	      "\tSelect the heuristic for optimizing steps";
	      "-pp", Arg.Set pp,
	      "\tPost-process the fixed-time plan to reduce mpt effect";
	      "-w", Arg.Float (fun f -> weight := f),
	      "\tSet weight for the weighted A* search algorithm";
	      "-t", Arg.Float (fun f -> runtime := f),
	      "\tSet the runtime limit for search algorithm";
	      "-bt", Arg.Float (fun f -> basetime := f),
	      "\tSet the basetime for synchronization"]
      (fun s -> raise (Arg.Bad s))
      "Online Temporal Progression Planner, by Minh Do (minhdo@parc.com).";
    Args.set_args !p_alg !s_alg !obj !s_heu !pp !weight !runtime;
    Interactive.est_plan_time := !ept;
    if !basetime <> infinity then
      Time.set_basetime !basetime;
    !verb
      

let main () =
  let v = parse_args () in
    Verb.with_level v
      (fun () ->
	 Verb.pe 3 "Online forward planning! Send bug to Minh Do (minhdo@parc.com)\n";
	 flush_all ();
	 let buff = Lexing.from_channel stdin in
	 let domain = Wrlexing.parse_verb 5 stderr "planning domain"
			(Parse_pddl_o.domain Lex_pddl_o.lexer) buff in
	 let init_prob = Wrlexing.parse_verb 5 stderr "problem instance"
			   (Parse_pddl_o.problem Lex_pddl_o.lexer) buff in
	   Verb.pe 4 "%s\n%s\n" (Domain.domain_str domain)
	     (Domain.problem_str init_prob);
	   Apsp.build_scores init_prob;
	   Interactive.sim_interact domain init_prob buff;
	   Verb.pe 2 "Done planning!\n")


let _ = main ()


(* EOF *)
