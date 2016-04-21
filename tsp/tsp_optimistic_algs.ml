open Tsp_instances
open Tsp
open Tsp_algs

let make_wt c agg =
  (c -. 1.) *. agg +. 1.

let aggressive_global c p lim =
  unwrap_results (Clamped_aggressive.aggressive_global ~limit:lim
		(Tsp.make_initial p.dists p.symmetric)
		Tsp.goal_p
		(Tsp.make_hd p.dists p.symmetric)
		(Tsp.make_expand p.dists p.symmetric)
		(make_wt c 2.)
		c)   

let aggressive_path c p lim =
  unwrap_results (Clamped_aggressive.aggressive_path ~limit:lim
		(Tsp.make_initial p.dists p.symmetric)
		Tsp.goal_p
		(Tsp.make_hd p.dists p.symmetric)
		(Tsp.make_expand p.dists p.symmetric)
		(make_wt c 2.)
		c)   
   
let aggressive_level c p lim =
  unwrap_results (Clamped_aggressive.aggressive_level ~limit:lim
		(Tsp.make_initial p.dists p.symmetric)
		Tsp.goal_p
		(Tsp.make_hd p.dists p.symmetric)
		(Tsp.make_expand p.dists p.symmetric)
		(make_wt c 2.)
		c)   

let aggressive_level_smooth c p lim =
  unwrap_results (Clamped_aggressive.aggressive_level_smooth ~limit:lim
		(Tsp.make_initial p.dists p.symmetric)
		Tsp.goal_p
		(Tsp.make_hd p.dists p.symmetric)
		(Tsp.make_expand p.dists p.symmetric)
		(make_wt c 2.)
		c)   

let aggressive_window c p lim =
  unwrap_results (Clamped_aggressive.aggressive_window ~limit:lim
		(Tsp.make_initial p.dists p.symmetric)
		Tsp.goal_p
		(Tsp.make_hd p.dists p.symmetric)
		(Tsp.make_expand p.dists p.symmetric)
		(make_wt c 2.)
		c)   

let aggressive_classic_w c p lim =
  unwrap_results (Clamped_aggressive.aggressive_classic_w ~limit:lim
		(Tsp.make_initial p.dists p.symmetric)
		Tsp.goal_p
		(Tsp.make_hd p.dists p.symmetric)
		(Tsp.make_expand p.dists p.symmetric)
		(make_wt c 2.)
		c)   

(** anytime A* **)
let anytime_a_star wt p lim =
  unwrap_results (Anytime_astar.anytime_wted_a_star ~limit:lim 
		    ~weight:wt
		    (Tsp.make_initial p.dists p.symmetric) Tsp.goal_p
		    (Tsp.make_expand p.dists p.symmetric)
		    (Tsp.make_h_mst p.dists p.symmetric)
		    (make_logger ()))


let ara_star_init wt p lim log =
  unwrap_results2
       (Arastar2.ara_star 
	  ~wts:(Arastar2.mk_wtlist wt 0.2)
	  ~limit:lim
	  (Tsp.make_initial p.dists p.symmetric)
	  Tsp.goal_p
	  (Tsp.make_expand p.dists p.symmetric)
	  (Tsp.make_h_mst p.dists p.symmetric)
	  key
	  log)


let ara_star wt p lim =
  ara_star_init wt p lim
    (make_ara_logger())


let wted_a_star wt p lim =
  unwrap_results (Wted_astar.wted_a_star ~limit:lim 
		    (Tsp.make_initial p.dists p.symmetric)
		    Tsp.goal_p (Tsp.make_expand p.dists p.symmetric)
		    (Tsp.make_h_mst p.dists p.symmetric) wt)


let aggressive wt p lim = 
  let initial = Tsp.make_initial p.dists p.symmetric
  and expand = Tsp.make_expand p.dists p.symmetric in
    unwrap_results (Aggressive.aggressive_search ~limit:lim 
		      initial
		      Tsp.goal_p 
		      (Tsp.make_h_mst p.dists p.symmetric)
		      expand
		      (make_wt wt 2.) wt)


let fg_m1 wt p lim = 
  let initial = Tsp.make_initial p.dists p.symmetric
  and expand = Tsp.make_expand p.dists p.symmetric in
    unwrap_results (Furious_george.model_1 ~limit:lim 
		      initial
		      Tsp.goal_p 
		      (Tsp.make_hd p.dists p.symmetric)
		      expand
		      wt)


let fg_m2 wt p lim = 
  let initial = Tsp.make_initial p.dists p.symmetric
  and expand = Tsp.make_expand p.dists p.symmetric in
    unwrap_results (Furious_george.model_2 ~limit:lim 
		      initial
		      Tsp.goal_p 
		      (Tsp.make_hd p.dists p.symmetric)
		      expand
		      wt)


let awastar_l1 p lim =
  let initial = Tsp.make_initial p.dists p.symmetric
  and expand = Tsp.make_expand p.dists p.symmetric in
    unwrap_results
      (Awa_weightlist.do_search_ndups
	 ~limit:lim
	 ~wl:Richter.richterl1
	 initial
	 Tsp.goal_p 
	 expand
	 (Tsp.make_h_mst p.dists p.symmetric)
	 (make_logger()))


let awastar_l2 p lim =
  let initial = Tsp.make_initial p.dists p.symmetric
  and expand = Tsp.make_expand p.dists p.symmetric in
    unwrap_results
      (Awa_weightlist.do_search_ndups
	 ~limit:lim
	 ~wl:Richter.richterl2
	 initial
	 Tsp.goal_p 
	 expand
	 (Tsp.make_h_mst p.dists p.symmetric)
	 (make_logger()))

let awastar_aralist p lim =
  let initial = Tsp.make_initial p.dists p.symmetric
  and expand = Tsp.make_expand p.dists p.symmetric in
    unwrap_results
      (Awa_weightlist.do_search_ndups
	 ~limit:lim
	 ~wl:Awa_weightlist.ara_list
	 initial
	 Tsp.goal_p 
	 expand
	 (Tsp.make_h_mst p.dists p.symmetric)
	 (make_logger()))


let ara_l1 p lim =
  let initial = Tsp.make_initial p.dists p.symmetric
  and expand = Tsp.make_expand p.dists p.symmetric in
    unwrap_results(
      Limit.unwrap_sol5 Arastar.unwrap
	(Arastar_list.ara_star_list
	   ~limit:lim
	   Richter.richterl1
	   initial
	   Tsp.goal_p
	   expand
	   (Tsp.make_h_mst p.dists p.symmetric)
	   (make_ara_logger())))


let ara_l2 p lim =
  let initial = Tsp.make_initial p.dists p.symmetric
  and expand = Tsp.make_expand p.dists p.symmetric in
    unwrap_results(
      Limit.unwrap_sol5 Arastar.unwrap
	(Arastar_list.ara_star_list
	   ~limit:lim
	   Richter.richterl2
	   initial
	   Tsp.goal_p
	   expand
	   (Tsp.make_h_mst p.dists p.symmetric)
	   (make_ara_logger())))

let aggressive_dwa wt p lim = 
  let initial = Tsp.make_initial p.dists p.symmetric
  and expand = Tsp.make_expand p.dists p.symmetric in
    unwrap_results (Aggressive.aggressive_search_dwa ~limit:lim 
		      ~dbound:(Some (float_of_int (Tsp.prob_size p.dists)))
		      initial
		      Tsp.goal_p 
		      (Tsp.make_hd p.dists p.symmetric)
		      expand
		      (make_wt wt 2.) wt)
