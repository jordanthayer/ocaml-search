(* $Id: search.ml,v 1.1 2005/04/06 00:39:13 ruml Exp ruml $

   search algorithms for tplan - only a_star is used right now;
   but can add other algorithms as well....
*)


let runtime = 100.

(**** interface to different planning/search algorithms ****)

let a_star_pro prob p t =
  let iface = Progression.default_interface prob p t in
  let s, e, g, _,_ = Astar.no_dups iface [||] in
    s, e, g


let a_star_pop prob p t =
  let iface = Pop.default_interface prob p t in
  let s, e, g, _,_ = Astar.no_dups iface [||] in
    s, e, g


let wted_a_star_pro wt prob p t =
  let iface = Progression.default_interface prob p t in
  let s, e, g, _,_ = Wted_astar.no_dups iface [|string_of_float wt|] in
    s, e, g


let wted_a_star_pop wt prob p t =
  let iface = Pop.default_interface prob p t in
  let s, e, g, _,_ = Wted_astar.no_dups iface [|string_of_float wt|] in
    s, e, g



(**** interface to interactive.ml ****)

let run_alg_pro problem plan time =
  (** fully grounded [problem]; global [plan]; and plan start [time] *)
  Datafile.write_colnames stderr
    ["sol cost"; "nodes expanded"; "nodes generated"; "raw cpu time"];
  let (sol, e, g),t =
    match !Args.s_alg with
	Args.ASTAR ->
	  Wrsys.with_time (fun () -> a_star_pro problem plan time)
      | Args.WTED_ASTAR ->
	  Wrsys.with_time (fun () -> wted_a_star_pro !Args.weight problem plan time)
      | Args.BUGSY ->
	  failwith "Do not support BUGSY for progression planner at the moment"
  in
    match sol with
	None -> Wrutils.pf stderr "no plan found: return empty plan\n";
	  None
      | Some (s, ms) ->
	(* Verb.force 2 (lazy (Progression.print_plan_state stdout s)); *)
	  Wrutils.pr "#  %f\t%d\t%d\t%f\n" ms e g t;
	  let trail_pairs = ["final sol cost", string_of_float ms;
			   "total raw cpu time", string_of_float t;
			   "total nodes expanded", string_of_int e;
			   "total nodes generated", string_of_int g] in
	  Datafile.write_pairs stdout trail_pairs;
	  Some s.Progression.plan


let run_alg_pop problem plan time =
  (** fully grounded [problem]; global [plan]; and plan start [time] *)
  Datafile.write_colnames stdout
    ["sol cost"; "nodes expanded"; "nodes generated"; "raw cpu time"];
  let (sol, e, g),t =
    match !Args.s_alg with
	Args.ASTAR -> Wrsys.with_time (fun () -> a_star_pop problem plan time)
      | Args.WTED_ASTAR ->
	  Wrsys.with_time (fun () -> wted_a_star_pop !Args.weight problem plan time)
      | Args.BUGSY ->
	  failwith "Do not support BUGSY for progression planner at the moment"
  in
    flush_all ();
    match sol with
      None -> Wrutils.pf stderr  "no plan found: return empty plan\n";
	  None
      | Some (s, ms) ->
	(* Verb.force 2 (lazy (Progression.print_plan_state stdout s)); *)
	  Wrutils.pr "#  %f\t%d\t%d\t%f\n" ms e g t;
	let trail_pairs = ["final sol cost", string_of_float ms;
			   "total raw cpu time", string_of_float t;
			   "total nodes expanded", string_of_int e;
			   "total nodes generated", string_of_int g] in
	  Datafile.write_pairs stderr trail_pairs;
	  flush_all ();
	  Some (O_utils.fix_plan_time s)



(* EOF *)
