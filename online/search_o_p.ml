(* $Id: search.ml,v 1.1 2005/04/06 00:39:13 ruml Exp ruml $

   search algorithms for tplan - only a_star is used right now;
   but can add other algorithms as well....
*)



let a_star prob p t =
  let root, expand, goal_p, h = Progression.init prob p t in
  let s, e, g, _ = Astar.a_star_dups root goal_p expand
			   Wrutils.identity h in
    s, e, g

(*
let wted_a_star wt prob p t =
  let root, expand, goal_p, h = Progression.init prob p t in
  let s, e, g, _ = Wted_astar.wted_a_star_dups root goal_p expand
			   Wrutils.identity h wt in
    s, e, g
*)


let run_alg problem plan time =
  (** fully grounded [problem]; global [plan]; and plan start [time] *)
  Datafile.write_colnames stdout
    ["sol cost"; "nodes expanded"; "nodes generated"; "raw cpu time"];
  let (sol, e, g),t = Wrsys.with_time (fun () -> a_star problem plan time) in
    flush_all ();
    match sol with
      None -> Wrutils.pr "no plan found: return empty plan\n";
	  []
      | Some (s, ms) ->
	(* Verb.force 2 (lazy (Progression.print_plan_state stdout s)); *)
	  Wrutils.pr "%f\t%d\t%d\t%f\n" ms e g t;
	let trail_pairs = ["final sol cost", string_of_float ms;
			   "total raw cpu time", string_of_float t;
			   "total nodes expanded", string_of_int e;
			   "total nodes generated", string_of_int g] in
	  Datafile.write_pairs stdout trail_pairs;
	  s.Progression.plan



(* EOF *)
