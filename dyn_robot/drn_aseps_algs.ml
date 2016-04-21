(* Aseps Algs *)
open Drn_instance
open Dynamic

(* Tools *)
let hd problem =
  let d = (make_approx_d problem)
  and h = (make_approx_time_h problem) in
    (fun n -> h n, d n)


(* algs*)

let aseps wt problem lim =
  let problem = expand_obstacles problem in
    Aseps.a_star_eps_dups
      ~limit:lim
      (make_root problem)
      goal_p
      expand
      key
      (hd problem)
      wt



let aseps wt problem lim =
  let problem = expand_obstacles problem in
    Aseps_fixed_focal.scheduled_dups
      ~limit:lim
      ~dur:10
      (make_root problem)
      goal_p
      expand
      key
      (hd problem)
      wt



let tqs wt problem lim =
  let problem = expand_obstacles problem in
    Three_queue_search.tqs_dups
      ~limit:lim
      (make_root problem)
      goal_p
      expand
      key
      (hd problem)
      wt



let tqs_dd wt problem lim =
  let problem = expand_obstacles problem in
    Three_queue_search_dd.tqs_dd
      ~limit:lim
      (make_root problem)
      goal_p
      expand
      key
      (hd problem)
      wt


let tqs_pd wt problem lim =
  let problem = expand_obstacles problem in
    Three_queue_search_dd.tqs_pd
      ~limit:lim
      (make_root problem)
      goal_p
      expand
      key
      (hd problem)
      wt



let tqs_m1_dd wt problem lim =
  let problem = expand_obstacles problem in
    Three_queue_search_dd.m1_dd
      ~limit:lim
      (make_root problem)
      goal_p
      expand
      key
      (hd problem)
      wt
(* EOF *)
