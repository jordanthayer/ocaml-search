(* George (Learning) Based algorithms *)
open Drn_instance
open Dynamic

let hd problem =
  let d = (make_approx_d problem)
  and h = (make_approx_time_h problem) in
    (fun n -> h n, d n)


(* Unbounded *)
(*************************************************************************)
let george problem lim =
  let problem = expand_obstacles problem in
    Simple_estimatedf.george_dups_global_err 
      ~limit:lim
      (make_root problem)
      goal_p
      expand
      key
      (hd problem)

(*************************************************************************)
(* Bounded Quality *)
(*************************************************************************)

let george_c_reckless wt problem lim =
  let problem = expand_obstacles problem in
    Simple_estimatedf.george_dups_reckless_err 
      ~limit:lim
      ~wt:(Some wt)
      (make_root problem)
      goal_p
      expand
      key
      (hd problem)


let george_c_reckless_dd wt problem lim =
  let problem = expand_obstacles problem in
    Simple_estimatedf.george_drops_dups_reckless_err 
      ~limit:lim
      ~wt:(Some wt)
      (make_root problem)
      goal_p
      expand
      key
      (hd problem)
