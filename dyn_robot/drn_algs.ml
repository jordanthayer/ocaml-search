(* Seperated out the algorithms from some of the other dynamic stuff *)
open Drn_instance
open Dynamic


(* Unbounded *)
(*************************************************************************)
let greedy problem lim =
  let problem = expand_obstacles problem in
    Greedy.greedy_dups
      ~limit:lim
      (make_root problem)
      goal_p expand key
      (make_approx_time_h problem)


let greedier problem lim =
  let problem = expand_obstacles problem in
    Greedier.greedier
      ~limit:lim
      (make_root problem)
      goal_p expand key
      (make_approx_time_h problem)


let speedy problem lim =
  let problem = expand_obstacles problem in
    Greedy.speedy_dups
      ~limit:lim
      (make_root problem)
      goal_p
      expand
      key
      (hd problem)


let speedier problem lim =
  let problem = expand_obstacles problem in
  let d = (make_approx_d problem)
  and h = (make_approx_time_h problem) in
    Greedier.speedier
      ~limit:lim
      (make_root problem)
      goal_p
      expand
      key
      d
      h


(*************************************************************************)
(* Bounded Quality *)
(*************************************************************************)


let uniform_cost problem lim =
  let problem = expand_obstacles problem in
    Astar.uniform_cost_dups
      ~limit:lim
      (make_root problem)
      goal_p
      expand
      key


let a_star problem lim =
  let problem = expand_obstacles problem in
    Astar.a_star_dups
      ~limit:lim
      (make_root problem)
      goal_p
      expand
      key
      (make_approx_time_h problem)


let astar_2 problem lim =
  let problem = expand_obstacles problem in
    Wted_astar.wted_a_star_dups
      ~limit:lim
      (make_root problem)
      goal_p
      expand
      key
      (make_approx_time_h problem)
      1.


let wted_a_star wt problem lim =
  let problem = expand_obstacles problem in
    Wted_astar.wted_a_star_dups
      ~limit:lim
      (make_root problem)
      goal_p
      expand
      key
      (make_approx_time_h problem)
      wt


let wted_a_star_dd wt problem lim =
  let problem = expand_obstacles problem in
    Admiss_astar.w_a_star_dups_admiss
      ~weight:wt
      ~limit:lim
      (make_root problem)
      goal_p
      expand
      key
      (make_approx_time_h problem)


let dyn_wted_astar wt problem lim =
  let problem = expand_obstacles problem in
  let d = (make_approx_d problem)
  and h = (make_approx_time_h problem)
  and r = make_root problem in
  let depth = d r in
    Dwastar.dyn_wted_a_star_dups
      ~limit:lim
      r
      goal_p
      expand
      key
      h
      wt
      (int_of_float depth)


let dwa wt problem lim =
  let problem = expand_obstacles problem in
    Dwa_redux.wa_dups
      ~limit:lim
      (make_root problem)
      goal_p
      expand
      key
      (hd problem)
      wt

let dwad wt problem lim =
  let problem = expand_obstacles problem in
    Dwa_redux.wa_drop_dups
      ~limit:lim
      (make_root problem)
      goal_p
      expand
      key
      (hd problem)
      wt

let dwa_redux wt problem lim =
  let problem = expand_obstacles problem in
    Dwa_redux.dups
      ~limit:lim
      (make_root problem)
      goal_p
      expand
      key
      (hd problem)
      wt

let dwa_redux_dd wt problem lim =
  let problem = expand_obstacles problem in
    Dwa_redux.drop_dups
      ~limit:lim
      (make_root problem)
      goal_p
      expand
      key
      (hd problem)
      wt



let dwa_redux_otb wt problem lim =
  let problem = expand_obstacles problem in
    Dwa_redux.dups_otb
      ~limit:lim
      (make_root problem)
      goal_p
      expand
      key
      (hd problem)
      wt


let d_astar w lim = dwa_redux 1. w lim


(* EOF *)
