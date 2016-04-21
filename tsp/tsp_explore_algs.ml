open Tsp_instances
open Tsp
open Tsp_algs

(** stand alone cleanups **)
let cleanup wt p lim =
  unwrap_results (Tracking_anytime_astar.standalone_cleanup ~limit:lim 
		    ~cutoff:wt
		    (Tsp.make_initial p.dists p.symmetric) Tsp.goal_p
		    (Tsp.make_expand p.dists p.symmetric)
		    (Tsp.make_h_mst p.dists p.symmetric)
		    (make_logger ()))

let global_err_cleanup wt p lim =
  unwrap_results (Anytime_estimatedf.global_err_standalone_cleanup ~limit:lim 
		    ~cutoff:wt
		    (Tsp.make_initial p.dists p.symmetric)
		    Tsp.goal_p
		    (Tsp.make_expand p.dists p.symmetric)
		    (Tsp.make_hd p.dists p.symmetric)
		    (make_logger ()))

let level_err_cleanup wt p lim =
  unwrap_results (Anytime_estimatedf.level_err_standalone_cleanup ~limit:lim 
		    ~cutoff:wt
		    (Tsp.make_initial p.dists p.symmetric)
		    Tsp.goal_p
		    (Tsp.make_expand p.dists p.symmetric)
		    (Tsp.make_hd p.dists p.symmetric)
		    (make_logger ()))

(*
let level_smooth_err_cleanup wt p lim =
  unwrap_results (Anytime_estimatedf.level_smooth_err_standalone_cleanup
  ~limit:lim 
		    ~cutoff:wt
		    (Tsp.make_initial p.dists p.symmetric)
		    Tsp.goal_p
		    (Tsp.make_expand p.dists p.symmetric)
		    (Tsp.make_hd p.dists p.symmetric)
		    (make_logger ()))
*)
  
let path_err_cleanup wt p lim =
  unwrap_results (Anytime_estimatedf.path_err_standalone_cleanup ~limit:lim 
		    ~cutoff:wt
		    (Tsp.make_initial p.dists p.symmetric)
		    Tsp.goal_p
		    (Tsp.make_expand p.dists p.symmetric)
		    (Tsp.make_hd p.dists p.symmetric)
		    (make_logger ()))    

let window_err_cleanup wt p lim = 
  unwrap_results (Anytime_estimatedf.window_err_standalone_cleanup ~limit:lim 
		    ~cutoff:wt
		    (Tsp.make_initial p.dists p.symmetric)
		    Tsp.goal_p
		    (Tsp.make_expand p.dists p.symmetric)
		    (Tsp.make_hd p.dists p.symmetric)
		    (make_logger ()))

let classic_w_cleanup wt p lim =
  unwrap_results (Anytime_estimatedf.classic_w_standalone_cleanup ~limit:lim 
		    ~cutoff:wt
		    (Tsp.make_initial p.dists p.symmetric)
		    Tsp.goal_p
		    (Tsp.make_expand p.dists p.symmetric)
		    (Tsp.make_hd p.dists p.symmetric)
		    (make_logger ()))
