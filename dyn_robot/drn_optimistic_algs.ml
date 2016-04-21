(* Optimistic Algs *)
open Drn_instance
open Dynamic

(* Tools *)
let hd problem =
  let d = (make_approx_d problem)
  and h = (make_approx_time_h problem) in
    (fun n -> h n, d n)

let make_wt c agg =
  (c -. 1.) *. agg +. 1.

(* Algs *)


let aggressive wt w lim =
  let problem = expand_obstacles w in
    Aggressive.aggressive_search_dups 
      ~limit:lim 
      (make_root problem) 
      goal_p
      (make_approx_time_h problem) 
      expand 
      (make_wt wt 2.) 
      wt 
      key


let aggressive_dd wt w lim =
  let problem = expand_obstacles w in
    Aggressive.aggressive_delay_dups 
      ~limit:lim 
      (make_root problem) 
      goal_p
      (make_approx_time_h problem) 
      expand 
      (make_wt wt 2.) 
      wt 
      key


let aggressive_dip wt w lim =
  let problem = expand_obstacles w in
    Aggressive.aggressive_delay_dups_ip
      ~limit:lim 
      (make_root problem) 
      goal_p
      (make_approx_time_h problem) 
      expand 
      (make_wt wt 2.) 
      wt 
      key



let dw_cheap wt w lim =
  let problem = expand_obstacles w in
    Aggressive.aggressive_search_dwa_dups
      ~limit:lim 
      (make_root problem) 
      goal_p
      (hd problem)
      expand 
      (make_wt wt 2.) 
      wt 
      key



let dw_cheap_dd wt w lim =
  let problem = expand_obstacles w in
    Aggressive.aggressive_dwa_delay_dups
      ~limit:lim 
      (make_root problem) 
      goal_p
      (hd problem)
      expand 
      (make_wt wt 2.) 
      wt 
      key



let dw_cheap_dip wt w lim =
  let problem = expand_obstacles w in
    Aggressive.aggressive_dwa_delay_dups_ip
      ~limit:lim 
      (make_root problem) 
      goal_p
      (hd problem)
      expand 
      (make_wt wt 2.) 
      wt 
      key
