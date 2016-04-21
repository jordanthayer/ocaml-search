open Tpl_search_reg

(******** A*-epsilon ****)
      
let aseps wt d p l =
  let root, expand, goal_p, hd_close = Tpl_regression.init_aseps d p in
    Aseps.a_star_eps ~limit:l root goal_p expand hd_close wt


let aseps_old wt d p l =
  let root, expand, goal_p, hd_close = Tpl_regression.init_aseps d p in
    Aseps.a_star_eps_old ~limit:l root goal_p expand hd_close wt

(** f, then f^ *)
      
let f_then_global wt d p l =
  let root, expand, goal_p, hd = Tpl_regression.init_aseps d p in
    F_fhat.george_global_err ~limit:l root goal_p expand hd wt


let f_then_level wt d p l =
  let root, expand, goal_p, hd = Tpl_regression.init_aseps d p in
    F_fhat.george_level_err ~limit:l root goal_p expand hd wt


let f_then_level_smooth wt d p l =
  let root, expand, goal_p, hd = Tpl_regression.init_aseps d p in
    F_fhat.george_level_smooth_err ~limit:l root goal_p expand hd wt


let f_then_path wt d p l =
  let root, expand, goal_p, hd = Tpl_regression.init_aseps d p in
    F_fhat.george_path_err ~limit:l root goal_p expand hd wt


let f_then_window wt d p l =
  let root, expand, goal_p, hd = Tpl_regression.init_aseps d p in
    F_fhat.george_window_err ~limit:l root goal_p expand hd wt


let f_then_classic_w wt d p l =
  let root, expand, goal_p, hd = Tpl_regression.init_aseps d p in
    F_fhat.george_classic_w ~limit:l root goal_p expand hd wt


let aseps_george_clean weight d p l =
  let root, expand, goal_p, hd = Tpl_regression.init_aseps d p in
    F_fhat.george_global_err_cleanup ~limit:l
      root
      goal_p
      expand
      hd
      weight


let fhat_eps weight d p l =
  let root, expand, goal_p, hd = Tpl_regression.init_aseps d p in
    F_fhat.george_lms_cleanup ~limit:l
      root
      goal_p
      expand
      hd
      weight


let strip_dups (sfop, a, b, c, d, e) =
  sfop,a,b,c,d


let tqs weight d p l = 
  let root, expand, goal_p, hd = Tpl_regression.init_aseps d p in
    Tqs_nodups.tqs ~limit:l
	 root
	 goal_p
	 expand
	 hd
	 weight

let aseps_george_clean2 weight d p l =
  strip_dups
  (let root, expand, goal_p, hd = Tpl_regression.init_aseps d p in
     F_fhat.george_global_err_dups_cleanup ~limit:l
       root
       goal_p
       expand
       Tpl_regression.key
       hd
       weight)

(** f^ then f *)
