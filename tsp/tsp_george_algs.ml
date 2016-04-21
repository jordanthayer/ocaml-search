
open Tsp_instances
open Tsp
open Tsp_algs

(** george variants **)
let george_global_err wt p lim =
  unwrap_results (Simple_estimatedf.george_global_err ~limit:lim 
		    ~wt:(Some wt)
		    (Tsp.make_initial p.dists p.symmetric)
		    Tsp.goal_p
		    (Tsp.make_expand p.dists p.symmetric)
		    (Tsp.make_hd p.dists p.symmetric))

let george_reckless_err wt p lim =
  unwrap_results (Simple_estimatedf.george_reckless_err ~limit:lim 
		    ~wt:(Some wt)
		    (Tsp.make_initial p.dists p.symmetric)
		    Tsp.goal_p
		    (Tsp.make_expand p.dists p.symmetric)
		    (Tsp.make_hd p.dists p.symmetric))

let george_split_err wt p lim =
  unwrap_results (Simple_estimatedf.george_reckless_split_err ~limit:lim 
		    ~wt:(Some wt)
		    (Tsp.make_initial p.dists p.symmetric)
		    Tsp.goal_p
		    (Tsp.make_expand p.dists p.symmetric)
		    (Tsp.make_hd p.dists p.symmetric))


let george_level_err wt p lim =
  unwrap_results (Simple_estimatedf.george_level_err ~limit:lim 
		    ~wt:(Some wt)
		    (Tsp.make_initial p.dists p.symmetric)
		    Tsp.goal_p
		    (Tsp.make_expand p.dists p.symmetric)
		    (Tsp.make_hd p.dists p.symmetric))

let george_level_smooth_err wt p lim =
  unwrap_results (Simple_estimatedf.george_level_smooth_err ~limit:lim 
		    ~wt:(Some wt)
		    (Tsp.make_initial p.dists p.symmetric)
		    Tsp.goal_p
		    (Tsp.make_expand p.dists p.symmetric)
		    (Tsp.make_hd p.dists p.symmetric))

let george_path_err wt p lim =
  unwrap_results (Simple_estimatedf.george_path_err ~limit:lim 
		    ~wt:(Some wt)
		    (Tsp.make_initial p.dists p.symmetric)
		    Tsp.goal_p
		    (Tsp.make_expand p.dists p.symmetric)
		    (Tsp.make_hd p.dists p.symmetric))    

let george_window_err wt p lim =
  unwrap_results (Simple_estimatedf.george_window_err ~limit:lim 
		    ~wt:(Some wt)
		    (Tsp.make_initial p.dists p.symmetric)
		    Tsp.goal_p
		    (Tsp.make_expand p.dists p.symmetric)
		    (Tsp.make_hd p.dists p.symmetric))

let george_classic_w wt p lim =
  unwrap_results (Simple_estimatedf.george_classic_w ~limit:lim 
		    ~wt:(Some wt)
		    (Tsp.make_initial p.dists p.symmetric)
		    Tsp.goal_p
		    (Tsp.make_expand p.dists p.symmetric)
		    (Tsp.make_hd p.dists p.symmetric))

let max_w_r wt p lim =
  unwrap_results (Maxw.max_w_reckless ~limit:lim 
		    (Tsp.make_initial p.dists p.symmetric)
		    Tsp.goal_p
		    (Tsp.make_expand p.dists p.symmetric)
		    (Tsp.make_h_mst p.dists p.symmetric) wt
		    (make_logger()))

let max_w wt p lim =
  (** [wt] of 1 is A*. [wt] > 1 is inadmissible. *)
  unwrap_results (Maxw.max_w ~limit:lim 
		    (Tsp.make_initial p.dists p.symmetric)
		    Tsp.goal_p
		    (Tsp.make_expand p.dists p.symmetric)
		    (Tsp.make_h_mst p.dists p.symmetric) wt
		    (make_logger()))

let george_learner_m1 wt p lim =
  unwrap_results (Simple_estimatedf.george_globalp_err ~limit:lim 
		    ~wt:(Some wt)
		    (Tsp.make_initial p.dists p.symmetric)
		    Tsp.goal_p
		    (Tsp.make_expand p.dists p.symmetric)
		    (Tsp.make_hd p.dists p.symmetric))

let george_learner_m2 wt p lim =
  unwrap_results (Simple_estimatedf.george_globalp_err ~limit:lim 
		    ~wt:(Some wt)
		    (Tsp.make_initial p.dists p.symmetric)
		    Tsp.goal_p
		    (Tsp.make_expand p.dists p.symmetric)
		    (Tsp.make_hd p.dists p.symmetric))
