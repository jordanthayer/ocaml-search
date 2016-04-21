(* Recording algs for grid world *)
open Grid
open Grid_rev_heuristics
open Grid_algs


(* Truth *)
let truth w lim =
  let new_w = {
    blocked = w.blocked;
    costs = w.costs;
    moves = w.moves;
    goal = [w.start];
    start = (List.hd w.goal);
    instance = not_known;} in
    with_path6 (Offline_gather.record_dups
		  ~print_path:print_path
		  (make_root new_w)
		  (no_goal_p new_w)
		  (make_expand w)
		  key
		  print_key
		  (get_cheapest_hd w)
		  (get_cheapest_rhd w) (* hd *))       (* rev hd *)


(* Astar *)
let astar_exp w lim =
  with_path6 
    (Astar.a_star_dups 
       ~limit:lim 
       ~node_recorder:(Astar.record_node print_key key)
       (make_root w) 
       (make_goal_p w)	
       (make_expand w) 
       key 
       (get_cheapest_h w))

let astar_struct w lim =
  with_path6 
    (Astar.a_star_dups 
       ~limit:lim 
       ~dpq_recorder:(Astar.dpq_recorder print_key key)
       (make_root w) 
       (make_goal_p w)	
       (make_expand w) 
       key 
       (get_cheapest_h w))

(* wted Astar *)

let wastar_dd_exp wt w lim =
  (** [wt] of 1 is A*. [wt] > 1 is inadmissible. *)
  with_path6 (Admiss_astar.w_a_star_dups_admiss 
		~weight:wt 
		~limit:lim
		~node_recorder:(Admiss_astar.record_node print_key key)
		(make_root w)
		(make_goal_p w) 
		(make_expand w) 
		key 
		(get_cheapest_h w))


let wastar_dd_str wt w lim =
  (** [wt] of 1 is A*. [wt] > 1 is inadmissible. *)
  with_path6 (Admiss_astar.w_a_star_dups_admiss 
		~weight:wt 
		~limit:lim
		~dpq_recorder:(Admiss_astar.dpq_recorder print_key key)
		(make_root w)
		(make_goal_p w) 
		(make_expand w) 
		key 
		(get_cheapest_h w))

(* Clamped adaptive *)
let ca_exp wt w lim =
  with_path6 (Simple_estimatedf.george_dups_reckless_err 
		~limit:lim
		~wt:(Some wt)
		~node_recorder:(Estimatedf.record_node print_key key)
		(make_root w)
		(make_goal_p w)
		(make_expand w)
		key
		(get_cheapest_hd w))


let ca_str wt w lim =
  with_path6 (Simple_estimatedf.george_dups_reckless_err 
		~limit:lim
		~wt:(Some wt)
		~dpq_recorder:(Estimatedf.dpq_recorder print_key key)
		(make_root w)
		(make_goal_p w)
		(make_expand w)
		key
		(get_cheapest_hd w))

(* Revised Dynamically Weighted Astar *)

let rdwa_exp weight w lim =
 with_path6 (Dwa_redux.dups 
	       ~limit:lim
	       ~node_recorder:(Dwa_redux.record_node print_key key)
	       (make_root w) 
	       (make_goal_p w) 
	       (make_expand w)
	       key 
	       (get_cheapest_hd w) 
	       weight)

let rdwa_str weight w lim =
 with_path6 (Dwa_redux.dups 
	       ~limit:lim
	       ~dpq_recorder:(Dwa_redux.dpq_recorder print_key key)
	       (make_root w) 
	       (make_goal_p w) 
	       (make_expand w)
	       key 
	       (get_cheapest_hd w) 
	       weight)

(* Multiple queues beyond here.  Still not sure how to handle that *)
(* A* eps *)

let aseps_exp weight w lim =
  (** within a factor of [weight] of optimal.  1 gives A*. [d] is some
      measure of effort, eg search distance to goal. *)
  with_path6 (Aseps.a_star_eps_dups 
		~limit:lim 
		~node_recorder:(Aseps.record_node print_key key)
		(make_root w) 
		(make_goal_p w)
		(make_expand w) key 
		(get_cheapest_hd w) 
		weight)

let aseps_open weight w lim =
  (** within a factor of [weight] of optimal.  1 gives A*. [d] is some
      measure of effort, eg search distance to goal. *)
  with_path6 (Aseps.a_star_eps_dups 
		~limit:lim 
		~geq_fun:(Aseps.record_open print_key key)
		(make_root w) 
		(make_goal_p w)
		(make_expand w) key 
		(get_cheapest_hd w) 
		weight)

let aseps_focal weight w lim =
  (** within a factor of [weight] of optimal.  1 gives A*. [d] is some
      measure of effort, eg search distance to goal. *)
  with_path6 (Aseps.a_star_eps_dups 
		~limit:lim 
		~geq_fun:(Aseps.record_focal print_key key)
		(make_root w) 
		(make_goal_p w)
		(make_expand w) key 
		(get_cheapest_hd w) 
		weight)

(* fheps *)
let fhat_eps_exp weight w lim =
  with_path6 (F_fhat.george_lms_dups_cleanup ~limit:lim
		~node_recorder:(Furious_george.record_node print_key key)
		(make_root w)
		(make_goal_p w)
		(make_expand w)
		key
		(get_cheapest_hd w)
		weight)


let fhat_eps_flist weight w lim =
  with_path6 (F_fhat.george_lms_dups_cleanup ~limit:lim
		~f_recorder:(Furious_george.dpq_recorder print_key key)
		(make_root w)
		(make_goal_p w)
		(make_expand w)
		key
		(get_cheapest_hd w)
		weight)


let fhat_eps_open weight w lim =
  with_path6 (F_fhat.george_lms_dups_cleanup ~limit:lim
		~open_recorder:(Furious_george.record_open print_key key)
		(make_root w)
		(make_goal_p w)
		(make_expand w)
		key
		(get_cheapest_hd w)
		weight)


let fhat_eps_focal weight w lim =
  with_path6 (F_fhat.george_lms_dups_cleanup ~limit:lim
		~focal_recorder:(Furious_george.record_focal print_key key)
		(make_root w)
		(make_goal_p w)
		(make_expand w)
		key
		(get_cheapest_hd w)
		weight)

(* Optimistic Search *)
let make_wt c agg =
  (c -. 1.) *. agg +. 1.

let aggressive_exp wt w lim =
  with_path6 (Aggressive.aggressive_search_dups ~limit:lim
		~node_recorder:(Aggressive.record_node print_key key)
		(make_root w) (make_goal_p w)
		(get_cheapest_h w) (make_expand w) (make_wt wt 2.) wt key)


let aggressive_agg wt w lim =
  with_path6 (Aggressive.aggressive_search_dups ~limit:lim
		~agg_recorder:(Aggressive.dpq_recorder print_key key)
		(make_root w) (make_goal_p w)
		(get_cheapest_h w) (make_expand w) (make_wt wt 2.) wt key)


let aggressive_clean wt w lim =
  with_path6 (Aggressive.aggressive_search_dups ~limit:lim
		~clean_recorder:(Aggressive.dpq_recorder print_key key)
		(make_root w) (make_goal_p w)
		(get_cheapest_h w) (make_expand w) (make_wt wt 2.) wt key)


(*** Fheps recorder for debugging ***)

let record_lfheps_exp wt w lim f_vect d_vect =
 with_path6 (LFheps.dups_search ~limit:lim
	       ~record_exp:(LFheps.record_node print_key key)
	       (make_root w) (make_goal_p w) (make_expand w) key
	       (get_cheapest_hd w) wt f_vect d_vect)


let record_lfheps_open wt w lim f_vect d_vect =
 with_path6 (LFheps.dups_search ~limit:lim
	       ~geq_record:(LFheps.record_open print_key key)
	       (make_root w) (make_goal_p w) (make_expand w) key
	       (get_cheapest_hd w) wt f_vect d_vect)


let record_lfheps_focal wt w lim f_vect d_vect =
 with_path6 (LFheps.dups_search ~limit:lim
	       ~geq_record:(LFheps.record_focal print_key key)
	       (make_root w) (make_goal_p w) (make_expand w) key
	       (get_cheapest_hd w) wt f_vect d_vect)

let record_lfheps_clean wt w lim f_vect d_vect =
 with_path6 (LFheps.dups_search ~limit:lim
	       ~clean_record:(LFheps.dpq_recorder print_key key)
	       (make_root w) (make_goal_p w) (make_expand w) key
	       (get_cheapest_hd w) wt f_vect d_vect)

(*** Fhat then d recorder ***)

let record_laseps_exp wt w lim vect =
 with_path6 (LAseps.dups_search ~limit:lim
	       ~exp_record:(LAseps.record_node print_key key)
	       (make_root w) (make_goal_p w) (make_expand w) key
	       (get_cheapest_hd w) wt vect)
	       

let record_laseps_open wt w lim vect =
 with_path6 (LAseps.dups_search ~limit:lim
	       ~geq_record:(LAseps.record_open print_key key)
	       (make_root w) (make_goal_p w) (make_expand w) key
	       (get_cheapest_hd w) wt vect)


let record_laseps_focal wt w lim vect =
 with_path6 (LAseps.dups_search ~limit:lim
	       ~geq_record:(LAseps.record_focal print_key key)
	       (make_root w) (make_goal_p w) (make_expand w) key
	       (get_cheapest_hd w) wt vect)

(* EOF *)
