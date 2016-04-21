(** Alg table for algorithms which handle duplicates in some special manner
    for example by dropping them outright *)

open Anytime_searches

let table  =
  [
    "richter_l1_dd", (1, restarts_rl1_dd);
    "richter_l2_dd", (1, restarts_rl2_dd);

    "sach_dd", (1, Sach.dd);
    "hd_hybrid", (1, Hd_hybrid.dups);
    "whd_hybrid", (2, Hd_hybrid.wted_dups);
    "greedier", (1, Greedy.drop_dups);
    "greedier_adapt", (1, Greedy_adaptive.drop);
    "greedier_adapt_supplied", (3, Greedy_adaptive.drop_with_args);
    "greedier_adapt_seeded", (3, Greedy_adaptive.seeded_drop);
    "greedier_path_seeded", (4, Path_based_adaptive.seeded_greedy_path_dd);
    "greedier_lms_adapt", (1, Greedy_adaptive.lms_drop);
    "greedier_lms_adapt_m2", (1, Greedy_adaptive.lms_dd_2);
    "greedier_dlms_adapt", (1, Greedy_adaptive.dlms_drop);
    "greedier_hlms_adapt", (1, Greedy_adaptive.hlms_drop);
    "greedier_path_adapt", (1, Path_based_adaptive.greedy_path_dd);
    "speedier_path_adapt", (1, Path_based_adaptive.speedy_path_dd);
    "speediest_path_adapt_dd", (1, Path_based_adaptive.speediest_path_dd);
    "speediest_dd", (1, Speediest.drop_dups);
    "wted_speediest_dd", (2, Wted_speediest.drop_dups);
    "speediest_dd_m2", (1, Speediest.drop_dups_m2);
    "speediest_dd_m3", (1, Speediest.drop_dups_m3);
    "greedier_hpath_adapt", (1, Path_based_adaptive.recur_path_dd);
    "greedier_h_adapt", (1, Clamped_searches.greedy_recurh_dd);
    "justh_path_adapt_dd", (1, Path_based_adaptive.justh_path_dd);
    "justh_adapt_dd", (1, Clamped_searches.greedy_h_dd);
    "greedier_lms_fixed", (5, Greedy_adaptive.lms_drop_no_learn);
    "greedier_revh_adapt", (1, Greedy_adaptive.revh_drop);
    "greedier_ann_fixed", (2, Greedy_adaptive.ann_drop_no_learn);
    "greedier_ann_adapt", (1, Greedy_adaptive.ann_drop);
    "greedier_dann_adapt", (1, Greedy_adaptive.dann_drop);
    "greedier_hann_adapt", (1, Greedy_adaptive.hann_drop);
    "greedier_revh_lms_adapt", (1, Greedy_adaptive.lms_revh_drop);
    "greedier_revh_ann_adapt", (1, Greedy_adaptive.rev_ann_drop);
    "greedier_rr", (1, Greedy_round_robin.drop);
    "greedier_fixed", (5, Greedy_adaptive.lms_drop_no_learn);
    "speedier", (1, Speedy.drop_dups);
    "speedier_bc", (2, Speedy_baseline.dd);
    "speedier_adapt", (1, Greedy_adaptive.speedy_drop);
    "wted_astar_dd", (2, Wted_astar.drop_dups);
    "len_astar_dd", (1, Benton_cushing.dd);
    "len_wted_astar_dd", (2, Benton_cushing.wted_dd);
    "double_wted_astar_dd", (2, Double_weighted_astar.drop);
    "ml_wted_astar_dd", (3, Wted_astar.memory_limited_drop_dups);

    "aseps_dd", (2, Aseps.dd);
    "wted_astar_dd_on_dups", (2, Wted_astar.drop_dups_on_gen);
    "wted_astar_dd_htie", (2, Wastar_tiebreaking.h_dd);
    "wted_astar_dd_gtie", (2, Wastar_tiebreaking.g_dd);
    "wted_astar_dd_rtie", (2, Wastar_tiebreaking.random_dd);
    "dyn_wted_astar_dd", (2, Dynamically_wted_astar.drop_dups);
    "dwa_redux_dd", (2, Revised_dwastar.drop_dups);
    "dwa_redux_otb_dd", (2, Dwastar_tiebreaking.g_dd);
    "d_wastar_dd", (2, Wastar_tiebreaking.d_dd);
    "aggressive_dd", (3, Optimistic_search.delay_dups);
    "aggressive_fhp_path_dd", (2, Path_cleanup.austin_uc_dd);
    "aggressive_fhp_path_test_dd", (2, Path_cleanup.skept_uc_dd);
    "aggressive_dwacheap_dd", (3, Dyn_wted_optimistic.delay_dups);
    "aggressive_dwastar_dd", (3, Dwa_optimistic.delay_dups);

    "sol_printer_astar", (2, Node_recorder.dups);


    "bulb", (3, Bulb.call_bulb);
    "bulb_printing", (3, Bulb.call_printing_bulb);

    "hbss", (3, Hbss_graph.hbss);

    "beam_dups", (2, Beam.call_beam_search_dups);
    "beam", (2, Beam.call_beam_search_no_closed);
    "f_beam", (2, Beam.call_f_beam_search_no_closed);
    "f_bf_beam", (2, Beam.call_f_bf_beam_search_no_closed);
    "bf_beam", (2, Beam.call_bf_beam_search_no_closed);
    "f_beam_ghost_closed", (2, Beam.call_f_beam_search_ghost_closed);

    "wted_beam", (4, Newbeam.wted_beam);

    "hybrid_beam", (2, Hybrid_beam.hbs);
    "hybrid_beam_strat", (3, Hybrid_beam.hbs_stratification);
    "leniant_hybrid_beam_strat", (3, Hybrid_beam.leniant_hbs_stratification);
    "record_hybrid_beam_strat_node", (3, Hybrid_beam.hbs_record);

    "bucketed_hybrid_beam_strat", (4, Hybrid_beam.bucketed_hbs_stratification);
    "bucketed_leniant_hybrid_beam_strat", (4, Hybrid_beam.bucketed_leniant_hbs_stratification);
    "bucketed_record_hybrid_beam_strat_node", (4, Hybrid_beam.bucketed_hbs_record);

    "bucketed_wted_hybrid_beam_strat",
    (5, Hybrid_beam.bucketed_wted_hbs_stratification);
    "bucketed_wted_leniant_hybrid_beam_strat",
    (5, Hybrid_beam.bucketed_wted_leniant_hbs_stratification);


    "new_beam", (3, Newbeam.basic_beam);
    "new_beam_restarting", (4, Newbeam.restarting_beam);
    "new_beam_noderec", (3, Newbeam.node_record_beam);
    "new_beam_queuerec", (3, Newbeam.queue_record_beam);
    "local_closed_beam", (3, Newbeam.purge_beam);
    "no_closed_beam", (3, Newbeam.no_close_beam);

    "new_beam_restarting_bd", (6, Newbeam.ib_beam_bd);
    "new_beam_restarting_bdd", (6, Newbeam.ib_beam_bdd);
    "new_beam_restarting_bd_one", (6, Newbeam.ib_beam_bd_one);
    "new_beam_restarting_bd_one_prune", (6, Newbeam.ib_beam_bd_one_prune);


    "adaptive_beam_hd_ss", (2, Adaptive_beam.global_hd_ss);
    "adaptive_beam_fd_ss", (2, Adaptive_beam.global_fd_ss);
    "adaptive_beam_h_ss", (2, Adaptive_beam.global_h_ss);
    "adaptive_beam_lms_h", (2, Adaptive_beam.lms_h);
    "adaptive_beam_lms_hd", (2, Adaptive_beam.lms_hd);

    "adaptive_bf_beam_hd_ss", (2, Adaptive_bf_beam.global_hd_ss);
    "adaptive_bf_beam_fd_ss", (2, Adaptive_bf_beam.global_fd_ss);
    "adaptive_bf_beam_h_ss", (2, Adaptive_bf_beam.global_h_ss);
    "adaptive_bf_beam_lms_h", (2, Adaptive_bf_beam.lms_h);
    "adaptive_bf_beam_lms_hd", (2, Adaptive_bf_beam.lms_hd);

    "stochastic_beam", (2, Stochastic_beam_search.call_stochastic_beam_search);
    "stochastic_beam_no_closed",
    (2, Stochastic_beam_search.call_stochastic_beam_search_no_closed);

    "bounded_stochastic_beam",
    (3, Stochastic_beam_search.call_bounded_stochastic_beam_search);
    "bounded_stochastic_beam_local_closed",
    (3, Stochastic_beam_search.call_bounded_stochastic_beam_search);
    "bounded_stochastic_beam_no_closed",
    (3, Stochastic_beam_search.call_bounded_stochastic_beam_search_no_closed);


    "complete_anytime_beam", (2, Complete_anytime_beam.cab);
    "double_beam", (2, Double_beam.run_double_beam_search);

    "militant_f_beam_dups", (2, Beam.call_militant_f_beam_search_dups);
    "f_beam_dups", (2, Beam.call_f_beam_search_dups);
    "fhp_beam_dups", (2, Fhp_beam.call_fhp_beam_search_dups);
    "bf_beam_dups", (2, Beam.call_bf_beam_search_dups);
    "f_bf_beam_dups", (2, Beam.call_f_bf_beam_search_dups);
    "wted_f_bf_beam_dups", (3, Beam.call_wted_f_bf_beam_search_dups);

    "heuristic_analysis", (2, Ha.heuristic_analysis);

    "ib_beam_search", (3, Beam.call_restarting_beam_search_dups);


    "degraded_h_beam_dups", (2, Beam.call_beam_search_dups);
    "degraded_h_f_beam_dups", (2, Beam.call_f_beam_search_dups);
    "degraded_h_bf_beam_dups", (2, Beam.call_bf_beam_search_dups);
    "degraded_h_f_bf_beam_dups", (2, Beam.call_f_bf_beam_search_dups);
    "degraded_h_beam_dups2", (2, Beam.call_beam_search_dups);
    "degraded_h_f_beam_dups2", (2, Beam.call_f_beam_search_dups);
    "degraded_h_bf_beam_dups2", (2, Beam.call_bf_beam_search_dups);
    "degraded_h_f_bf_beam_dups2", (2, Beam.call_f_bf_beam_search_dups);
    "lazy_f_beam_dups2", (2, Beam.call_f_beam_search_dups);
    "lazy_bf_beam_dups2", (2, Beam.call_bf_beam_search_dups);
    "lazy_f_bf_beam_dups2", (2, Beam.call_f_bf_beam_search_dups);
    "f_beam_queue_dups", (2, Beam.call_f_beam_search_queue_dups);

    "beam_h_check", (3, Newbeam.queue_check_beam_h_star);
    "beam_f_check", (3, Newbeam.queue_check_beam_f_star);
    "beam_check_best_f", (3, Newbeam.queue_check_beam_print_best_f);
    "beam_check_best_h", (3, Newbeam.queue_check_beam_print_best_h);


    "clamped_h_reckless_dd", (2, Clamped_searches.h_ss_reckless_dd);
    "clamped_h_conservative_dd", (2, Clamped_searches.h_ss_conservative_dd);
    "clamped_h_fhp_reckless_dd", (2, Clamped_searches.h_ss_fhp_reckless_dd);
    "h_cleanup_reckless_dd", (2, Cleanup_searches.cleanup_dd);
    "aggressive_fhp_dd",(2, Cleanup_searches.fd_cleanup_dd);
    "aggressive_fhp_lms_dd",(2, Cleanup_searches.lms_fd_cleanup_dd);
    "aggressive_fhp_ann_dd",(2, Cleanup_searches.ann_fd_cleanup_dd);
    "tqs_h_reckless_dd", (2, Tqs.h_ss_reckless_dd);
    "tqs_h_reckless_hard_d", (2, Tqs.h_ss_reckless_hard_delay);
    "tqs_h_conservative_dd", (2, Tqs.h_ss_conservative_dd);
    "tqs_h_reckless_no_d_dd",(2, Tqs.h_ss_reckless_no_d_dd);
    "tqs_h_reckless_no_fh_dd",(2, Tqs.h_ss_reckless_no_fh_dd);
    "tqs_h_reckless_dd_fast", (2, Tqs_fast.h_ss_reckless_dd);
    "tqs_h_reckless_hard_d_fast", (2, Tqs_fast.h_ss_reckless_hard_delay);
    "tqs_h_geo_dd_fast", (2, Tqs_fast.h_ss_geo_dd);
    "tqs_h_geo_hard_d_fast", (2, Tqs_fast.h_ss_geo_hard_delay);
    "tqs_h_conservative_dd_fast", (2, Tqs_fast.h_ss_conservative_dd);
    "tqs_hd_reckless_dd", (2, Tqs_fast_v2.hd_ss_reckless_dd);
    "tqs_rev_hd_reckless_dd", (2, Tqs_rev_fast.hd_globalavg_reckless_delay);
    "tqs_hd_lms_dd", (2, Tqs_fast_v2.hd_lms_reckless_dd);
    "limited_enthusiasm_dd", (2, Clamped_searches.mult_dd);
    "tqs_truth_hhat_dd", (2, Tqs_specified_hhat.h_dd);
    "tqs_truth_hdhat_dd", (2, Tqs_specified_hhat.hd_dd);
    "anytime_window_astar_dd", (1, Awastar.dd);
    "anytime_window_astar_scale_dd", (1, Awastar.dd_scaling);
    "anytime_window_astar_d_dd", (1, Awastar_d.dd);
    "anytime_window_astar_d_scale_dd", (1, Awastar_d.dd_scaling);

    (******* Recording *******)
    "record_greedier", (1, Recording_greedy.expand_recorder_dd);
    "record_greedier_queue", (1, Recording_greedy.queue_recorder_dd);
    "record_speedier", (1, Recording_speedy.expand_recorder_dd);
    "record_speedier_queue", (1, Recording_speedy.queue_recorder_dd);
    "record_wted_astar_dd", (2, Recording_wastar.expand_recorder_dd);
    "record_wted_astar_dd_queue", (2, Recording_wastar.queue_recorder_dd);
    "record_ca_h_reckless_dd_exp", (2, Recording_clamped.h_ss_reckless_dd_exp);
    "record_ca_h_reckless_dd_queue", (2, Recording_clamped.h_ss_reckless_dd_queue);
    "record_h_reckless_dd_exp", (2, Recording_unbounded_ss.h_ss_reckless_dd_exp);
    "record_h_reckless_dd_queue", (2, Recording_unbounded_ss.h_ss_reckless_dd_queue);
    "record_f_beam_dups_node", (2, Recording_beam.call_f_beam_search_dups_noderec);
    "record_f_beam_dups_queue", (2, Recording_beam.call_f_beam_search_dups_queuerec);
    "record_beam_dups_node", (2, Recording_beam.call_beam_search_dups_noderec);
    "record_beam_dups_queue", (2, Recording_beam.call_beam_search_dups_queuerec);
    "record_f_beam_dups_prune", (2, Recording_beam.call_f_beam_search_dups_prunerec);
    "record_f_beam_dups_count", (2,
				 Recording_beam.call_f_beam_search_dups_prunecount);
    "record_f_bf_beam_dups_node", (2,
				   Recording_beam.call_f_bf_beam_search_dups_noderec);
    "record_f_bf_beam_dups_prune", (2,
				   Recording_beam.call_f_bf_beam_search_dups_prunerec);

  ]


(* EOF *)
