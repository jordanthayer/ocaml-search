(** Some shortcuts.

    @author eaburns
    @since 2009-09-24
*)
#load "str.cma";;
#load "nums.cma";;
#load "unix.cma";;
#use "use.ml";;

let breadth_first in_file =
  Wrio.with_infile in_file
    (fun inch ->
       let domain, initial, goal = Sas_parser.parse inch in
       let is_goal = Sas.make_is_goal goal
       and expand = Sas.make_expand domain initial
       in Search.breadth_first is_goal expand initial)


let astar in_file =
  Wrio.with_infile in_file
    (fun inch ->
       let domain, initial, goal = Sas_parser.parse inch in
       let is_goal = Sas.make_is_goal goal
       and expand = Sas.make_expand domain initial
       and h = Sas.make_goal_lits_heuristic goal
       in Search.astar is_goal expand h initial)


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


let test_load in_file =
  let module Graph = Planning_graph.No_mutex in
    Wrio.with_infile in_file
      (fun inch ->
	 let domain, initial, goal = Sas_parser.parse inch in
(*
	 let is_goal = Sas.make_is_goal goal in
	 let expand = Sas.make_expand domain initial in
*)
	 let build_pg = Graph.make_create domain initial goal in
(*
	 let conf_graph = Conflict_graph.build domain.Sas.operators in
	   Conflict_graph.output stdout conf_graph;
*)
	   Printf.printf "h_{init}=%f\n"
	     (Graph.number_of_layers (build_pg initial))
      )
