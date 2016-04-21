open Tpl_search_reg

(** anytime george, no clean up **)

let make_wt c agg =
  (c -. 1.) *. agg +. 1.

let aggressivee_global wt d p l =
  let root, expand, goal_p, hd = Tpl_regression.init_aseps d p in
    Clamped_aggressive.aggressive_global ~limit:l 
      root goal_p hd expand (make_wt wt 2.) wt

let aggressivee_level wt d p l =
  let root, expand, goal_p, hd = Tpl_regression.init_aseps d p in
    Clamped_aggressive.aggressive_level ~limit:l 
      root goal_p hd expand (make_wt wt 2.) wt

let aggressivee_level_smooth wt d p l =
  let root, expand, goal_p, hd = Tpl_regression.init_aseps d p in
    Clamped_aggressive.aggressive_level_smooth ~limit:l 
      root goal_p hd expand (make_wt wt 2.) wt

let aggressivee_path wt d p l =
  let root, expand, goal_p, hd = Tpl_regression.init_aseps d p in
    Clamped_aggressive.aggressive_path ~limit:l 
      root goal_p hd expand (make_wt wt 2.) wt

let aggressivee_window wt d p l =
  let root, expand, goal_p, hd = Tpl_regression.init_aseps d p in
    Clamped_aggressive.aggressive_window ~limit:l 
      root goal_p hd expand (make_wt wt 2.) wt

let aggressivee_classic_w wt d p l =
  let root, expand, goal_p, hd = Tpl_regression.init_aseps d p in
    Clamped_aggressive.aggressive_classic_w ~limit:l 
      root goal_p hd expand (make_wt wt 2.) wt
      
(****** anytime algs need sol logger ******)

let aggressive wt d p l =
  let root, expand, goal_p, h = Tpl_regression.init d p in
    Aggressive.aggressive_search ~limit:l root goal_p h expand (make_wt wt 2.) wt

let aggressive_10x wt d p l =
  let root, expand, goal_p, h = Tpl_regression.init d p in
    Aggressive.aggressive_search root goal_p h expand (make_wt wt 10.) wt

let aggressive_20x wt d p l =
  let root, expand, goal_p, h = Tpl_regression.init d p in
    Aggressive.aggressive_search root goal_p h expand (make_wt wt 20.) wt

let aggressive_50x wt d p l =
  let root, expand, goal_p, h = Tpl_regression.init d p in
    Aggressive.aggressive_search root goal_p h expand (make_wt wt 50.) wt

let aggressive_100x wt d p l =
  let root, expand, goal_p, h = Tpl_regression.init d p in
    Aggressive.aggressive_search root goal_p h expand (make_wt wt 100.) wt


let anytime_a_star_cutoff_2x wt d p l =
  let root, expand, goal_p, h = Tpl_regression.init d p 
  and weight = (make_wt wt 2.) in
    Tracking_anytime_astar.anytime_wted_a_star ~limit:l ~weight:weight
      ~cutoff:wt root goal_p expand h log3


let fg_m1 wt d p lim =
  let root, expand, goal_p, hd = Tpl_regression.init_aseps d p in
  Furious_george.model_1 
		~limit:lim
		root
		goal_p
		hd
		expand
		wt (* Optimistic weight unused atm *)
		

let fg_m2 wt d p lim =
  let root, expand, goal_p, hd = Tpl_regression.init_aseps d p in
    Furious_george.model_2
		~limit:lim
		root
		goal_p
		hd
		expand
		wt (* Optimistic weight unused atm *)

		
let dw_aggressive wt d p lim =
  let root, expand, goal_p, hd = Tpl_regression.init_aseps d p in
    Aggressive.aggressive_search_dwa ~limit:lim 
      root goal_p
      hd expand (make_wt wt 2.) wt
