open Tpl_search_reg

(** georges **)      
let george_global_c wt d p l =
  let root, expand, goal_p, hd = Tpl_regression.init_aseps d p in
    Simple_estimatedf.george_global_err ~limit:l ~wt:(Some wt) root goal_p expand hd


let george_wreckless_c wt d p l =
  let root, expand, goal_p, hd = Tpl_regression.init_aseps d p in
    Simple_estimatedf.george_reckless_err ~limit:l ~wt:(Some wt) root goal_p expand hd

let george_split_c wt d p l =
  let root, expand, goal_p, hd = Tpl_regression.init_aseps d p in
    Simple_estimatedf.george_reckless_split_err ~limit:l ~wt:(Some wt) 
      root goal_p expand hd

let george_level_c wt d p l =
  let root, expand, goal_p, hd = Tpl_regression.init_aseps d p in
    Simple_estimatedf.george_level_err ~limit:l ~wt:(Some wt) root goal_p expand hd

let george_level_smooth_c wt d p l =
  let root, expand, goal_p, hd = Tpl_regression.init_aseps d p in
    Simple_estimatedf.george_level_smooth_err ~limit:l ~wt:(Some wt) root goal_p expand hd

let george_path_c wt d p l =
  let root, expand, goal_p, hd = Tpl_regression.init_aseps d p in
    Simple_estimatedf.george_path_err ~limit:l ~wt:(Some wt) root goal_p expand hd

let george_window_c wt d p l =
  let root, expand, goal_p, hd = Tpl_regression.init_aseps d p in
    Simple_estimatedf.george_window_err ~limit:l ~wt:(Some wt) root goal_p expand hd

let george_classic_w_c wt d p l =
  let root, expand, goal_p, hd = Tpl_regression.init_aseps d p in
    Simple_estimatedf.george_classic_w ~limit:l ~wt:(Some wt) root goal_p expand hd

let george_legion_c wt d p l =
  let root, expand, goal_p, hd = Tpl_regression.init_aseps d p in
    Simple_estimatedf.george_legion ~limit:l ~wt:(Some wt) root goal_p expand hd

(** max w **)
let max_w wt d p l =
  let root, expand, goal_p, h = Tpl_regression.init d p in
    Maxw.max_w ~limit:l root goal_p expand h wt log3

let max_w_r wt d p l =
  let root, expand, goal_p, h = Tpl_regression.init d p in
    Maxw.max_w_reckless ~limit:l root goal_p expand h wt log3

let max_w_nth wt d p l =
  let root, expand, goal_p, h = Tpl_regression.init d p in
    Maxw.max_w_nth ~limit:l root goal_p expand h wt log3


let george_learner_m1 wt d p l =
  let root, expand, goal_p, hd = Tpl_regression.init_aseps d p in
    Simple_estimatedf.george_globalp_err ~limit:l ~wt:(Some wt) root goal_p expand hd

let george_learner_m2 wt d p l =
  let root, expand, goal_p, hd = Tpl_regression.init_aseps d p in
    Simple_estimatedf.george_globalp_err ~limit:l ~wt:(Some wt) root goal_p expand hd

(* EOF *)
