open Grid
open Grid_algs

let cleanup_log wt w lim =
  with_path6 (Tracking_anytime_astar.standalone_cleanup_dups ~limit:lim
		~cutoff:wt (make_root w) (make_goal_p w) (make_expand w)
		key (get_cheapest_h w) (make_logger()))

let global_cleanup_log wt w lim =
  with_path6 (Anytime_estimatedf.dups_global_err_standalone_cleanup ~limit:lim
		~cutoff:wt (make_root w) (make_goal_p w) (make_expand w)
		key (get_cheapest_hd w) (make_logger()))    

let window_cleanup_log wt w lim =
  with_path6 (Anytime_estimatedf.dups_window_err_standalone_cleanup ~limit:lim
		~cutoff:wt (make_root w) (make_goal_p w) (make_expand w)
		key (get_cheapest_hd w) (make_logger()))    

let level_cleanup_log wt w lim =
  with_path6 (Anytime_estimatedf.dups_level_err_standalone_cleanup ~limit:lim
		~cutoff:wt (make_root w) (make_goal_p w) (make_expand w)
		key (get_cheapest_hd w) (make_logger()))
    
let path_cleanup_log wt w lim =
  with_path6 (Anytime_estimatedf.dups_path_err_standalone_cleanup ~limit:lim
		~cutoff:wt (make_root w) (make_goal_p w) (make_expand w)
		key (get_cheapest_hd w) (make_logger()))    

let classic_w_cleanup_log wt w lim =
  with_path6 (Anytime_estimatedf.dups_classic_w_standalone_cleanup ~limit:lim
		~cutoff:wt (make_root w) (make_goal_p w) (make_expand w)
		key (get_cheapest_hd w) (make_logger()))


(* EOF *)
