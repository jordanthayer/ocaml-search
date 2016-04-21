

type plan_alg = PROGRESSION | POP

type search_alg = ASTAR | WTED_ASTAR | BUGSY

type obj = MAKESPAN | STEPS

type step_heu = RP | APSP
    
let p_alg = ref PROGRESSION
and s_alg = ref ASTAR
and obj = ref MAKESPAN
and s_heu = ref RP
and pp = ref false (* post-processing the fixed-time plan *)
and weight = ref 3.0 (* for weighted A* *)
and runtime = ref 100. (* for limitting the algorithm runtime *)


(*** setup search strategy based on input arguments *)
	       
let set_plan_alg p =
  (** parse in the type of plan algorithm selected *)
  match p with
      "progression" | "prog" ->
	p_alg := PROGRESSION;
	Verb.pe 3 "Planning algorithm: PROGRESSION ";
	if !pp then
	  Verb.pe 3 "(with temporal post-processing)\n"
	else
	  Verb.pe 3 "(no temporal post-processing - NOT RECOMMENDED)\n"
    | "pop" ->
	p_alg := POP;
	Verb.pe 3 "Planning algorithm: Partial Order Planning\n"
    | _ ->failwith "Search_o.set_plan_alg: Wrong planning algorithm, use: prog or pop"

	
let set_search_alg a =
  (** parse in the type of search algorithm selected *)
  match a with
      "astar" ->
	s_alg := ASTAR;
	Verb.pe 3 "Search algorithm: A*\n"
    | "wastar" ->
	s_alg := WTED_ASTAR;
	Verb.pe 3 "Search algorithm: Weighted A* (w = %.2f)\n" !weight
    | "bugsy" ->
	s_alg := BUGSY;
	failwith "Search algorithm: BUGSY is not ready at the moment";
    | _ -> failwith "Search_o.set_search_alg: Wrong algorithm name, use: astar, wastar, or bugsy"


let set_objective o =
  (** parse in the objective function used *)
  match o with
      "makespan" ->
	obj := MAKESPAN;
	Verb.pe 3 "Objective function: minimizing makespan\n"
    | "steps" ->
	obj := STEPS;
	Verb.pe 3 "Objective function: minimizing number of steps\n"
    | _ -> failwith "Search_o.set_objective: Wrong objective function name, use: makespan or steps"

	
let set_step_heu h =
  (** set the heuristic used for optimizing steps *)
  if !p_alg = POP then
    (s_heu := APSP;
     if !obj = STEPS then
       Verb.pe 3 "Heuristic for optimizing steps: APSP\n")
  else
    match h with
	"rp" -> s_heu := RP;
	  if !obj = STEPS then
	    Verb.pe 3 "Heuristic for optimizing steps: RP\n"
      | "apsp" -> s_heu := APSP;
	  if !obj = STEPS then
	    Verb.pe 3 "Heuristic for optimizing steps: APSP\n"
      | _ -> failwith "Search_o.set_step_heu: unknown step-heuristic name, use: rp or apsp"

  
let set_args p s o h pp_flag w t =
  pp := pp_flag;
  set_plan_alg p;
  set_search_alg s;
  set_objective o;
  set_step_heu h;
  weight := w;
  runtime := t
  
(* EOF *)
