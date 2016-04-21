(* $Id: search.ml,v 1.1 2005/04/06 00:39:13 ruml Exp ruml $

   interface to search algorithms for regression planner
*)

(** no args **)
(** domain, problem, limit **)

let greedy d p l =
  let root, expand, goal_p, h = Tpl_regression.init d p in
    Greedy.greedy ~limit:l root goal_p expand h


(** this is very ugly and needs to be fixed. **)
let speedy d p l =
  (* h = h_cheap; d = d_cheap *)
  let root, expand, goal_p, hd = Tpl_regression.init_speedy d p in
    Greedy.speedy ~limit:l root goal_p expand hd


let uniform_cost d p l =
  let root, expand, goal_p, h = Tpl_regression.init d p in
    Limit.unwrap_sol5 Astar.unwrap_uniform
      (Astar.uniform_cost_search ~limit:l root goal_p expand)

let a_star d p l =
  let root, expand, goal_p, h = Tpl_regression.init d p in
    Astar.a_star_search ~limit:l root goal_p expand h


let make_logger () =
  (** prevent anytime A* from writing thousands of lines - only write if
    improved significantly *)
  let start = Sys.time ()
  and threshold = ref infinity in
    (fun n f i ->
       (** cost, length, expanded, gen, time *)
       if f <= !threshold then
	 let t = (Sys.time ()) -. start in
	   (* cost, exp generated time *)
	   Wrutils.pr "%f\t%d\t%d\t%f\n%!"
	     f i.Limit.expanded i.Limit.generated t;
	   threshold := f *. 0.999)


let log3 sol cost i =
  ()

(**********************************************************)

(** one arg **)

let wted_a_star wt d p l =
  let root, expand, goal_p, h = Tpl_regression.init d p in
    Wted_astar.wted_a_star ~limit:l root goal_p expand h wt


let dwa wt d p l =
  let root, expand, goal_p, hd = Tpl_regression.init_aseps d p in
    Dwa_redux.wa_no_dups ~limit:l root goal_p expand hd wt

let dyn_wted_a_star weight d p lim =
  let root, expand, goal_p, hd = Tpl_regression.init_aseps d p in
  let depth = truncate (ceil (
			  match (hd root) with
			      _,d -> d)) in
    Dwastar.dyn_wted_a_star ~limit:lim root goal_p expand
      (fun n -> fst (hd n)) weight depth

let dwa_redux weight d p lim =
  let root,expand, goal_p, hd = Tpl_regression.init_aseps d p in
    Dwa_redux.no_dups ~limit:lim
      root goal_p expand hd weight


let dwa_redux_otb weight d p lim =
  let root, expand, goal_p, hd = Tpl_regression.init_aseps d p in
    Dwa_redux.no_dups_otb ~limit:lim
      root goal_p expand hd weight

(****** bugsy ******)

let log_estimates secs_per cheap_h_under cheap_d_under
    close_h_under close_d_under =
  ()


let bugsy cost time d p l =
  (** domain [d], problem [p] and two coefficiences [cost], [time] for
      bugsy *)
  let root, expand, goal_p, h_cheap, d_cheap, hd_close =
    Tpl_regression.init_op d p in
  let cheap_hd = (fun s -> h_cheap s, d_cheap s)
  and close_hd = hd_close
  and expands_per = 10000 in
  let s, e, g, p, q, d = (Bugsy.search ~limit:l ~dups:false
			    root goal_p expand Wrutils.identity
			    cheap_hd close_hd cost time
 			    ~notify_estimates:log_estimates
			    expands_per) in
    s, e, g, p, q

(*
  Local Variables:
  compile-command: "ocm use"
  End: *)
(* EOF *)
