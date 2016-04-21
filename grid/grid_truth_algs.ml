(* Algs using true heuristic values
 *)

open Grid
open Grid_algs

let t_astar w lim =
  with_path6 (Astar.dups (truth_interface w lim))

let t_wted_astar wt w lim =
  with_path6 (Wted_astar.dups (truth_interface w lim) wt)

let t_aseps wt w lim =
  with_path6 (Aseps.dups (truth_interface w lim) wt)

let t_aseps_badd wt w lim =
  with_path6 (Aseps.dups (true_h_degraded_d w lim) wt)

let t_aseps_badh wt w lim =
  with_path6 (Aseps.dups (true_d_degraded_h w lim) wt)

(* EOF *)
