open Tsp_instances
open Tsp
open Tsp_algs

(** A* eps **)
let aseps wt p lim =
  unwrap_results (Aseps.a_star_eps ~limit:lim 
		    (Tsp.make_initial p.dists p.symmetric)
		    Tsp.goal_p (Tsp.make_expand p.dists p.symmetric)
		    (Tsp.make_hd p.dists p.symmetric) wt)

let asepslow wt p lim =
  unwrap_results (Aseps.a_star_eps_old ~limit:lim 
		    (Tsp.make_initial p.dists p.symmetric)
		    Tsp.goal_p (Tsp.make_expand p.dists p.symmetric)
		    (Tsp.make_hd p.dists p.symmetric) wt)

let asepsched wt p lim =
  unwrap_results (Aseps_fixed_focal.scheduled
		    ~limit:lim 
		    ~dur:10
		    (Tsp.make_initial p.dists p.symmetric)
		    Tsp.goal_p (Tsp.make_expand p.dists p.symmetric)
		    (Tsp.make_hd p.dists p.symmetric) wt)

let aseps_global_err wt p lim =
  unwrap_results (F_fhat.george_global_err ~limit:lim 
		    (Tsp.make_initial p.dists p.symmetric)
		    Tsp.goal_p
		    (Tsp.make_expand p.dists p.symmetric)
		    (Tsp.make_hd p.dists p.symmetric)
		    wt)

let aseps_level_err wt p lim =
  unwrap_results (F_fhat.george_level_err ~limit:lim 
		    (Tsp.make_initial p.dists p.symmetric)
		    Tsp.goal_p
		    (Tsp.make_expand p.dists p.symmetric)
		    (Tsp.make_hd p.dists p.symmetric)
		    wt)

let aseps_level_smooth_err wt p lim =
  unwrap_results (F_fhat.george_level_smooth_err ~limit:lim 
		    (Tsp.make_initial p.dists p.symmetric)
		    Tsp.goal_p
		    (Tsp.make_expand p.dists p.symmetric)
		    (Tsp.make_hd p.dists p.symmetric)
		    wt)

let aseps_path_err wt p lim =
  unwrap_results (F_fhat.george_path_err ~limit:lim 
		    (Tsp.make_initial p.dists p.symmetric)
		    Tsp.goal_p
		    (Tsp.make_expand p.dists p.symmetric)
		    (Tsp.make_hd p.dists p.symmetric)
		    wt)    

let aseps_window_err wt p lim =
  unwrap_results (F_fhat.george_window_err ~limit:lim 
		    (Tsp.make_initial p.dists p.symmetric)
		    Tsp.goal_p
		    (Tsp.make_expand p.dists p.symmetric)
		    (Tsp.make_hd p.dists p.symmetric)
		    wt)

let aseps_classic_w wt p lim =
  unwrap_results (F_fhat.george_classic_w ~limit:lim 
		    (Tsp.make_initial p.dists p.symmetric)
		    Tsp.goal_p
		    (Tsp.make_expand p.dists p.symmetric)
		    (Tsp.make_hd p.dists p.symmetric)
		    wt)

(** fhat then d **)
let global_then_d wt p lim =
  unwrap_results (F_fhat.george_global_err ~limit:lim 
		    (Tsp.make_initial p.dists p.symmetric)
		    Tsp.goal_p
		    (Tsp.make_expand p.dists p.symmetric)
		    (Tsp.make_hd p.dists p.symmetric)
		    wt)

let level_then_d wt p lim =
  unwrap_results (F_fhat.george_level_err ~limit:lim 
		    (Tsp.make_initial p.dists p.symmetric)
		    Tsp.goal_p
		    (Tsp.make_expand p.dists p.symmetric)
		    (Tsp.make_hd p.dists p.symmetric)
		    wt)

let level_smooth_then_d wt p lim =
  unwrap_results (F_fhat.george_level_smooth_err ~limit:lim 
		    (Tsp.make_initial p.dists p.symmetric)
		    Tsp.goal_p
		    (Tsp.make_expand p.dists p.symmetric)
		    (Tsp.make_hd p.dists p.symmetric)
		    wt)

let path_then_d wt p lim =
  unwrap_results (F_fhat.george_path_err ~limit:lim 
		    (Tsp.make_initial p.dists p.symmetric)
		    Tsp.goal_p
		    (Tsp.make_expand p.dists p.symmetric)
		    (Tsp.make_hd p.dists p.symmetric)
		    wt)    

let window_then_d wt p lim =
  unwrap_results (F_fhat.george_window_err ~limit:lim 
		    (Tsp.make_initial p.dists p.symmetric)
		    Tsp.goal_p
		    (Tsp.make_expand p.dists p.symmetric)
		    (Tsp.make_hd p.dists p.symmetric)
		    wt)

let classic_w_then_d wt p lim =
  unwrap_results (F_fhat.george_classic_w ~limit:lim 
		    (Tsp.make_initial p.dists p.symmetric)
		    Tsp.goal_p
		    (Tsp.make_expand p.dists p.symmetric)
		    (Tsp.make_hd p.dists p.symmetric)
		    wt)

let aseps_george_clean wt p lim =
  unwrap_results
    (F_fhat.george_global_err_cleanup ~limit:lim 
       (Tsp.make_initial p.dists p.symmetric)
       Tsp.goal_p
       (Tsp.make_expand p.dists p.symmetric)
       (Tsp.make_hd p.dists p.symmetric)
       wt)


let tqs wt p lim =
  unwrap_results
    (Tqs_nodups.tqs ~limit:lim
       (Tsp.make_initial p.dists p.symmetric)
       Tsp.goal_p
       (Tsp.make_expand p.dists p.symmetric)
       (Tsp.make_hd p.dists p.symmetric)
       wt)


let fhat_eps wt p lim =
  unwrap_results
    (F_fhat.george_lms_cleanup ~limit:lim 
       (Tsp.make_initial p.dists p.symmetric)
       Tsp.goal_p
       (Tsp.make_expand p.dists p.symmetric)
       (Tsp.make_hd p.dists p.symmetric)
       wt)
