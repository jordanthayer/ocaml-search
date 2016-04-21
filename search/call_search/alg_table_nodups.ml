(** Table of algorithms which don't handle duplicates in any particular
    way *)

open Anytime_searches

(* alg name, (arg count, function to call) *)

let table =
  [

    "richter_l1", (1, restarts_rl1);
    "richter_l2", (1, restarts_rl2);
    "richter_synth", (1, restarts_synth);
    "awastar_l1", (1, awastar_rl1);
    "awastar_l2", (1, awastar_rl2);
    "awastar_synth", (1, awastar_synth);
    "arastar_l1", (1, arastar_rl1);
    "arastar_l2", (1, arastar_rl2);
    "arastar_synth", (1, arastar_synth);
    "awastar_aralist", (2, awastar_aralist);
    "arastar", (2, arastar_default);
    "rbfs", (1, Recursive_best_first.no_dups);

    "uniform", (1, Uniform_cost_search.no_dups);
    "fixed_duration_rc", (2, Reverse_ss_searches.fd);
    "scaling_duration_rc", (2, Reverse_ss_searches.scaling_duration);

    "hillclimbing", (1, Astar_hc.no_dups);
    "enforced_hillclimbing", (1, Astar_hc.no_dups_enforced);
    "enforced_hillclimbing_nop", (1, Astar_hc.no_dups_enforced);

    "astar", (1, Astar.no_dups);
    "dfs", (1, Depth_first_search.no_dups);
    "b", (1, Astar.b_no_dups);
    "bprime", (1, Bprime.no_dups);
    "dastar", (1, Astar_distance.no_dups);
    "greedy", (1, Greedy.no_dups);
    "greedy_adapt", (1, Greedy_adaptive.no_dups);
    "greedy_lms_adapt", (1, Greedy_adaptive.lms_no_dups);
    "greedy_fixed", (5, Greedy_adaptive.lms_no_dups_no_learn);
    "greedy_path_adapt", (1, Path_based_adaptive.greedy_path);
    "greedy_rr", (1, Greedy_round_robin.no_dups);
    "speedy", (1, Speedy.no_dups);
    "speedy_adapt", (1, Greedy_adaptive.speedy_no_dups);
    "dfsbdd", (1, Depth_first_search.no_dups);
    "alpha_g_astar", (2, Alpha_astar.alpha_g_nodups);
    "alpha_gp_astar", (2, Alpha_astar.alpha_primeg_nodups);
    "alpha_h_astar", (2, Alpha_astar.alpha_h_nodups);
    "alpha_hp_astar", (2, Alpha_astar.alpha_primeh_nodups);
    "wted_astar", (2, Wted_astar.no_dups);
    "ml_wted_astar", (3, Wted_astar.memory_limited_no_dups);
    "wted_astar_on_gen", (2, Wted_astar.no_dups_on_gen);
    "dyn_wted_astar", (2, Dynamically_wted_astar.no_dups);
    "dwa_redux", (2, Revised_dwastar.no_dups);
    "dwa_redux_otb", (2, Dwastar_tiebreaking.g);
    "d_wastar", (2, Wastar_tiebreaking.d);
    "aseps", (2, Aseps.no_dups);
    "aeps_np", (2, Aepsilon.never_nodups);
    "aeps_ap", (2, Aepsilon.always_nodups);
    "aeps_dthresh", (2, Aepsilon.dtb_nodups);
    "aeps_mingrow", (2, Aepsilon.mwg_nodups);
    "aeps_deps", (2, Aepsilon.eps_nodups);
    "bugsy", (3, Bugsy.no_dups);
    "bugsy-simple", (3, Bugsy_simple.no_dups);

    (********************* Iterative searches ************************)

    "idastar", (1, Iterative_deepening_astar.no_dups);
    "idastar_im", (3, Iterative_searches.idastar_im_no_dups);
    "idastar_im_lazy_vec", (3, Iterative_searches.idastar_im_lazy_vec_no_dups);
    "idastar_im_garray", (3, Iterative_searches.idastar_im_garray_no_dups);
    "idastar_cr", (3, Iterative_searches.idastar_cr_no_dups);

    (********************* Real-time searches ************************)

    "dtastar", (5, Dtastar_old.no_dups);
    "dtastar_learn", (3, Dtastar_old.dta_learn);
    "rtastar", (2, Rtastar.no_dups);
    "srtastar", (2, Srtastar.no_dups);


    "anytime_baseline", (1, Baseline_anytime.two_phase);
    "anytime_greedy", (1, Anytime_greedy.no_dups);
    "anytime_speedy", (1, Anytime_speedy.no_dups);
    "anytime_window_astar", (1, Awastar.no_dups);
    "anytime_window_astar_scale", (1, Awastar.no_dups_scaling);
    "anytime_window_astar_h", (1, Awastar_h.no_dups);
    "anytime_window_astar_d", (1, Awastar_d.no_dups);
    "anytime_window_astar_d_scale", (1, Awastar_d.no_dups_scaling);
    "anytime_window_astar_g", (1, Awastar_d.no_dups);
    "anytime_astar", (2, Anytime_astar.no_dups);
    "anytime_dyn_astar", (2, Anytime_dwastar.no_dups);
    "anytime_dwaredux", (2, Anytime_rdwastar.no_dups);
    "anytime_aseps", (2, Anytime_aseps.no_dups);
    "anytime_aeps_dthresh", (2, Aepsilon.anytime_dtb_nodups);
    "anytime_hd_reckless", (1, Anytime_adaptive.hd_ss_reckless_no_dups);
    "anytime_hdp_reckless", (2, Anytime_adaptive.hd_ss_fhp_reckless_no_dups);
    "anytime_chdp_reckless", (2, Anytime_adaptive.hd_ss_cfhp_reckless_no_dups);
    "anytime_tqs_hd_reckless", (2, Tqs_fast_v2.anytime_hd_ss_reckless_no_dups);
    "anytime_tqs_spec", (2, Tqs_specified_hhat.anytime_hd_ss_reckless_no_dups);
    "beam_stack_search", (2, Beam_stack_search.no_dups);
    "itsastar_greedy", (1, Itsastar.search);

    "raseps", (2, Anytime_aseps.restarting_nodups);
    "raeps_dthresh", (2, Aepsilon.restarting_dtb_nodups);
    "restarts_dwastar", (2, Restarts_dwastar.no_dups);
    "restarts_rdwastar", (2, Restarts_rdwastar.no_dups);
    "restarts_alpha_h_astar", (2, Restarting_alpha_astar.alpha_h_nodups);
    "restarts_hdp_reckless", (2, Restarting_adaptive.hd_ss_fhp_reckless_no_dups);
    "restarts_chdp_reckless", (2, Restarting_adaptive.hd_ss_cfhp_reckless_no_dups);
    "restarts_tqs_hd_reckless", (2, Restarting_tqs_calls.hd_ss_reckless_no_dups);
    "restarts_tqs_spec", (2, Restarting_tqs_calls.supplied_hd_ss_reckless_no_dups);

    "araeps_dthresh", (2, Aepsilon.repairing_dtb_nodups);
    "araseps", (2, Anytime_aseps.repairing_nodups);
    "ardwastar", (2, Ardwastar.no_dups);
    "arrdwastar", (2, Arrdwastar.no_dups);
    "aralpha_h_astar", (2, Repairing_alpha_astar.alpha_h_nodups);
    "repair_hdp_reckless", (2, Repairing_adaptive.hd_ss_fhp_reckless_no_dups);
    "repair_chdp_reckless", (2, Repairing_adaptive.hd_ss_cfhp_reckless_no_dups);
    "repair_tqs_hd_reckless", (2, Restarting_tqs_calls.repair_hd_ss_reckless_no_dups);
    "repair_tqs_spec", (2, Restarting_tqs_calls.repair_supplied_hd_ss_reckless_no_dups);

    "aggressive", (3, Optimistic_search.no_dups);
    "aggressive_dwacheap", (3, Dyn_wted_optimistic.no_dups);
    "aggressive_dwastar", (3, Dwa_optimistic.no_dups);
    "limited_enthusiasm", (2, Clamped_searches.mult_no_dups);
    "max_w", (2, Max_w.no_dups);
    "beam", (2, Beam.call_beam_search);
    "bf_beam", (2, Beam.call_bf_beam_search);
    "f_bf_beam", (2, Beam.call_f_bf_beam_search);


    (********************* Learning Study  *****************************)
    (* Model using only h *)
    "h_cleanup_reckless", (2, Cleanup_searches.cleanup_no_dups);
    "aggressive_fhp", (2, Cleanup_searches.fd_cleanup_no_dups);
    "aggressive_fhp_path", (2, Path_cleanup.austin_uc);
    "aggressive_fhp_lms", (2, Cleanup_searches.lms_fd_cleanup_no_dups);
    "aggressive_fhp_fixed",
    (6, Cleanup_searches.lms_fd_cleanup_no_dups_no_learn);
    "aggressive_fhp_pm", (2, Cleanup_searches.fd_cleanup_pm_no_dups);
    "tqs_h_reckless", (2, Tqs.h_ss_reckless_no_dups);
    "tqs_h_gupdate", (2, Tqs_gupdate.h_ss_reckless_no_dups);
    "tqs_h_conservative", (2, Tqs.h_ss_conservative_no_dups);
    "tqs_h_reckless_fast", (2, Tqs_fast.h_ss_reckless_no_dups);
    "tqs_h_geo_fast", (2, Tqs_fast.h_ss_geo_no_dups);
    "tqs_h_conservative_fast", (2, Tqs_fast.h_ss_conservative_no_dups);
    "tqs_hd_reckless", (2, Tqs_fast_v2.hd_ss_reckless_no_dups);
    "tqs_hd_pm_reckless", (2, Tqs_fast_v2.hd_pm_ss_reckless_no_dups);
    "tqs_hd_reckless_no_fh", (2, Tqs_fast_v2.hd_ss_reckless_no_fh_no_dups);
    "tqs_hd_geo", (2, Tqs_fast_v2.hd_ss_geo_no_dups);
    "tqs_hd_con", (2, Tqs_fast_v2.hd_ss_conservative_no_dups);
    "tqs_rev_hd_reckless", (2, Tqs_rev_fast.hd_globalavg_reckless_no_dups);
    "tqs_hd_lms", (2, Tqs_fast_v2.hd_lms_reckless_no_dups);
    "tqs_hd_lms_geo", (2, Tqs_fast_v2.hd_lms_geo_no_dups);
    "tqs_hd_lms_pm", (2, Tqs_fast_v2.hd_lms_pm_reckless_no_dups);
    "tqs_hd_lms_pm_geo", (2, Tqs_fast_v2.hd_lms_pm_geo_no_dups);
    "tqs_hdh_lms", (2, Tqs_fast_v2.hdh_lms_reckless_no_dups);
    "clamped_h_reckless", (2, Clamped_searches.h_ss_reckless_no_dups);
    "clamped_h_conservative", (2, Clamped_searches.h_ss_conservative_no_dups);
    "clamped_h_geo", (2, Clamped_searches.h_ss_geo_no_dups);
    "clamped_h_fhp_reckless", (2, Clamped_searches.h_ss_fhp_reckless_no_dups);
    "clamped_h_fhp_conservative", (2, Clamped_searches.h_ss_fhp_conservative_no_dups);
    "clamped_h_fhp_geo", (2, Clamped_searches.h_ss_fhp_geo_no_dups);
    "clamped_hd_reckless", (2, Clamped_searches.hd_ss_reckless_no_dups);
    "fhat_switch_h_ss", (2, Reverse_ss_searches.fq_only_seems_bounded);
    "rev_cleanup_simple", (2, Reverse_ss_searches.simple_nodups);
    "tqs_h_reckless_no_d",(2, Tqs.h_ss_reckless_no_d);
    "tqs_h_reckless_no_fh",(2, Tqs.h_ss_reckless_no_fh);
    "tqs_vector_m1", (2, Tqs_rev.vector);
    "aseps_hhat", (2, F_then_hhat.no_dups);
    "aseps_fhat", (2, F_then_hhat.no_dups);
    "clamped_fhat", (2, Clamped_specified_hhat.no_dups);
    "optimistic_fhat", (2, Cleanup_specified_hhat.cleanup_no_dups);
    "optimistic_fhatprime", (2, Cleanup_specified_hhat.cleanup_fp_no_dups);

    (********************* Recording  *****************************)
    "record_astar", (1, Recording_astar.expand_recorder_nodups);
    "record_astar_queue", (1, Recording_astar.queue_recorder_nodups);
    "truth", (1, Recording_astar.truth_record_nodups);
    "record_uniform", (1, Recording_uniform.expand_recorder_nodups);
    "record_uniform_queue", (1, Recording_uniform.queue_recorder_nodups);
    "record_greedy", (1, Recording_greedy.expand_recorder_nodups);
    "record_greedy_queue", (1, Recording_greedy.queue_recorder_nodups);
    "record_speedy", (1, Recording_speedy.expand_recorder_nodups);
    "record_speedy_queue", (1, Recording_speedy.queue_recorder_nodups);
    "record_wted_astar", (2, Recording_wastar.expand_recorder_nodups);
    "record_wted_astar_queue", (2, Recording_wastar.queue_recorder_nodups);
  ]

(* EOF *)
