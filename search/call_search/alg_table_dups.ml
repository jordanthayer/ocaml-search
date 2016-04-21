(** The algorithm table for algorithms which take duplicates into account
    through some sort of key function, but then just re-expand them as
    normal *)

open Anytime_searches
open Iterative_searches

(* alg name, (arg count, function to call) *)
let table =
  [
    "lama11_m2", (1, Lama11.dups_rl1);
    "lama11_pss_uc", (1, Lama11_pss.dups_rl1_uc);
    "lama11_pss", (1, Lama11_pss.dups_rl1);
    "aees_pss", (1, Aees_pss.dups);
    "aees_pss_pm", (1, Aees_pss.dups_pm);
    "aees", (1, Aees.dups);
    "aees_pm", (1, Aees.dups_pm);
    "richter_l1", (1, restarts_rl1_dups);
    "richter_l2", (1, restarts_rl2_dups);
    "richter_synth", (1, restarts_synth_dups);
    "awastar_l1", (1, awastar_rl1_dups);
    "awastar_l2", (1, awastar_rl2_dups);
    "awastar_synth", (1, awastar_synth_dups);
    "arastar_l1", (1, arastar_rl1_dups);
    "arastar_l2", (1, arastar_rl2_dups);
    "arastar_synth", (1, arastar_synth_dups);
    "arastar", (3, arastar_default_dups);
    "awastar_aralist", (2, awastar_aralist_dups);
    "aras", (3, Aras.dups);
    "aras_mon", (6, Aras.mon_dups);

    "uniform", (1, Uniform_cost_search.dups);
    "avg_bfactor", (1, Uniform_cost_search.dups_abf);
    "fixed_duration_rc", (4, Reverse_ss_searches.fd_dups);
    "scaling_duration_rc", (3, Reverse_ss_searches.scaling_duration_dups);
    "bugsy-old", (4, Bugsy_old.dups);
    "bugsy", (6, Bugsy.dups);
    "bugsy-simple", (3, Bugsy_simple.dups);
    "wastar", (3, Wastar.dups);
    "breadth_first", (1, Breadth_first_search_graph.dups);

    (********************* Memory saving search ************************)

    "breadth_first_heuristic", (2, Breadth_first_heuristic.search);
    "frontier_astar", (1, Frontier_search.astar_dups);

    (********************* Real-time searches ************************)

    "dtastar-old", (5, Dtastar_old.dups);
    "dtastar", (6, Dtas.dups);
    "dtastar_learn", (3, Dtastar_old.dta_learn_dups);

    "idastar2", (2, Idastar.dups);
    "idastar_path", (2, Idastar_sspath.dups);
    (* this needs to be tweaked later to deal with other domains *)
    "idastar_path_cr", (2, Idastar_sspath_cr.dups_with_cr_tiles);
(*    "idastar_path_m2", (1, Idastar_sspath_m2.dups);*)

    "idtastar", (5, Idtastar.dups);
    "idtastar_learn", (3, Idtastar.idta_learn_dups);
    "idtastar_ideal", (5, Idtastar_ideal.dups);
    "idtastar_ideal_learn", (5, Idtastar_ideal.idta_ideal_learn_dups);
    "rtastar", (2, Rtastar.dups);
    "srtastar", (2, Srtastar.dups);
    "dtaserr", (3, Srtastar.dtaserr_dups);
    "lsslrtastar", (2, Astar_lsslrta.dups);

    "hillclimbing", (1, Astar_hc.dups);
    "enforced_hillclimbing", (1, Astar_hc.dups_enforced);
    "enforced_hillclimbing_nop", (1, Astar_hc.dups_enforced);


    (********************* size estimation ************************)

    "full_astar", (1, Full_astar.dups);
    "restarting_par_astar", (4, Partial_astar.restarting_dups);
    "continuing_par_astar", (4, Partial_astar.continuing_dups);
    "continuingf_par_astar", (4, Partial_astar.continuing_with_f_dups);
    "continuing_bestf_par_astar",
    (4, Partial_astar.continuing_with_bestf_dups);
    "random_trees", (2, Random_trees.dups);
    "knuth_sampling", (2, Knuth_sampling.dups);
    "kilby_counting", (2, Duplicate_counting.kilby_dups);
    "knuth_counting", (2, Duplicate_counting.knuth_dups);
    "count_subtrees", (2, Partial_astar.dups_count_subtrees);

    "dfmin_vs_growth", (3, Dfmin_vs_growth.dups);

    (********************* IDA* bound estimation ************************)


    (*
      "idastar_record_estimates", (1, idastar_domain_model_total_dups);
      "idastar_offline_model", (1, idastar_domain_model_dups);
      "idastar_online_model", (1, idastar_domain_model_dups);
      "idastar_double_bound", (1, (idastar_double_bound_dups));
      "idastar_gkre_model", (2, (idastar_gkre_model_dups));
    *)

    "idastar_im", (3, idastar_im_dups);
    "idastar_trans", (1, Idastar_transposition.do_search);
    "idastar_im_lazy_vec", (3, idastar_im_lazy_vec_dups);
    "idastar_im_garray", (3, idastar_im_garray_dups);
    "idastar_cr", (3, idastar_cr_dups);
    "widastar", (2, Weighted_idastar.dups);
    (*
      beam searches and I do believe it belongs here since it tracks duplicates.
      "beam", (2, single_int_init beam);
    *)

    "generic_beam", (2, Generic_beam.breadth_beam_dups);
    "gen_f_beam", (2, Generic_beam.breadth_beam_dups);
    "random_beam", (2, Random_beam.random_dups);
    "greedy_path_beam", (2, Austin_beam.greedy_path_dups);
    "seeded_greedy_path_beam", (5, Austin_beam.seeded_greedy_path_dups);
    "seeded_fhat_path_beam", (5, Austin_beam.seeded_austin_dups);
    "fhat_path_beam", (2, Austin_beam.austin_dups);
    "bf_greedy_path_beam", (2, Austin_beam.bfs_greedy_path_dups);
    "bf_fhat_path_beam", (2, Austin_beam.bfs_austin_dups);
    "laso_br", (2, Laso_br.do_search);
    "laso_bst", (2, Laso_bst.do_search);
    "greedy_br", (1, Br_greedy.dups);
    "greedy_bst", (1, Bst_greedy.dups);
    "greedier_br", (1, Br_greedy.dd);
    "greedier_bst", (1, Bst_greedy.dd);
    "skeptical_br", (2, Br_skeptical.do_search);

    "wa_hack", (2, Wa_hack.dups);

    "smastar", (3, Smastar.smastar);

    "wted_bf_beam", (4, Bf_beam.wted_bf_dups);

    "astar", (1, Astar.dups); (* this is how we should do it! *)
    "astar_path", (1, Path_based_adaptive.austin_uc_dups);
    "len_astar", (1, Benton_cushing.dups);
    "mixed_length", (1, Lmix_search.dups);
    "fml", (1, Lmix_mark2.dups);
    "fringe", (1, Fringe.dups);
    "kbfs", (2, Astar.k_dups);
    "dfs", (1, Depth_first_search.dups_cycle);
    "dfsbdd", (1, Depth_first_search.dups_hash);
    "b", (1, Astar.b_dups);
    "bprime", (1, Bprime.dups);
    "dastar", (1, Astar_distance.dups);
    "idastar", (1, Iterative_deepening_astar.dups);
    "idastar_with_bound", (2, Iterative_deepening_astar.dups_with_bound);

    "idastar_full", (1, Iterative_deepening_astar.dups_total);
    (* idastar_full is IDA* but it finishes searching its final
       iteration. *)

    "rbfs", (1, Recursive_best_first.dups);
    "greedy", (1, Greedy.dups);
    "greedy_lms_fixed", (5, Greedy_adaptive.lms_dups_no_learn);
    "greedy_ann_fixed", (2, Greedy_adaptive.ann_dups_no_learn);
    "greedy_adapt", (1, Greedy_adaptive.dups);
    "greedy_adapt_supplied", (3, Greedy_adaptive.dups_with_args);
    "greedy_adapt_seeded", (3, Greedy_adaptive.seeded_dups);
    "greedy_revh_adapt", (1, Greedy_adaptive.revh_dups);
    "greedy_dlms_adapt", (1, Greedy_adaptive.dlms_dups);
    "greedy_hlms_adapt", (1, Greedy_adaptive.hlms_dups);
    "greedy_lms_adapt", (1, Greedy_adaptive.lms_dups);
    "greedy_lms_adapt_m2", (1, Greedy_adaptive.lms_dups_2);
    "greedy_ann_adapt", (1, Greedy_adaptive.ann_dups);
    "greedy_dann_adapt", (1, Greedy_adaptive.dann_dups);
    "greedy_hann_adapt", (1, Greedy_adaptive.hann_dups);
    "greedy_revh_lms_adapt", (1, Greedy_adaptive.lms_revh_dups);
    "greedy_revh_ann_adapt", (1, Greedy_adaptive.rev_ann_dups);
    "greedy_path_adapt", (1, Path_based_adaptive.greedy_path_dups);
    "sach", (1, Sach.dups);
    "speedy_path_adapt", (1, Path_based_adaptive.speedy_path_dups);
    "speediest_path_adapt", (1, Path_based_adaptive.speediest_path_dups);
    "greedy_hpath_adapt", (1, Path_based_adaptive.recur_path_dups);
    "greedy_h_adapt", (1, Clamped_searches.greedy_recurh_dups);
    "justh_path_adapt", (1, Path_based_adaptive.justh_path_dups);
    "justh_adapt", (1, Clamped_searches.greedy_h_dups);
    "greedy_rr", (1, Greedy_round_robin.dups);
    "speedy", (1, Speedy.dups);
    "speediest", (1, Speediest.dups);
    "wted_speediest", (2, Wted_speediest.dups);
    "speediest_m2", (1, Speediest.dups_m2);
    "speedy_bc", (2, Speedy_baseline.dups);
    "greedy_bc", (2, Greedy_baseline.dups);
    "astar_bc", (2, Astar_baseline.dups);
    "potential", (2, Potential_search.dups);
    "potential_d", (2, Potential_d.dups);
    "potential_hhat", (2, Potential_hhat.dups);
    "dpot", (2, Dpot.dups);
    "bees", (2, Bees.austin_dups);
    "cbees", (1, Cbees.austin_dups);
    "cbees_pm", (1, Cbees.austin_dups_pm);
    "cbeeps", (1, Beeps.anytime_dups);
    "cbeeps_pm", (1, Beeps.anytime_dups_pm);
    "effort", (2, Effort.austin_dups);
    "beevs", (2, Beevs.austin_dups);
    "beeps", (2, Beeps.dups);
    "beeps_pm", (2, Beeps.dups_pm);
    "abeeps", (2, Beeps.anytime_dups);
    "abeeps_pm", (2, Beeps.anytime_dups_pm);
    "beeps_velocity", (2, Beeps_velocity.dups);
    "beeps_velocity_pm", (2, Beeps_velocity.dups_pm);
    "beeps_dhat", (2, Beeps_dhat.dups);
    "beeps_hhat", (2, Beeps_hhat.dups);
    "beeps_hhat_pm", (2, Beeps_hhat.dups_pm);
    "speedy_adapt", (1, Greedy_adaptive.speedy_dups);
    "alpha_g_astar", (2, Alpha_astar.alpha_g_dups);
    "alpha_gp_astar", (2, Alpha_astar.alpha_primeg_dups);
    "alpha_h_astar", (2, Alpha_astar.alpha_h_dups);
    "alpha_hp_astar", (2, Alpha_astar.alpha_primeh_dups);

    "beam_stack_search", (2, Beam_stack_search.dups);
    "beam_stack_search_noseed", (2, Beam_stack_search.no_seed);
    "bounded_beam_stack_search", (3, Beam_stack_search.exact_seed);

    "wted_astar", (2, Wted_astar.dups);
    "len_wted_astar", (2, Benton_cushing.wted_dups);
    "double_wted_astar", (2, Double_weighted_astar.dups);
    "ml_wted_astar", (3, Wted_astar.memory_limited_dups);
    "msc_kwastar_exp", (4, Msc_k_wted_astar.dups_exp);
    "msc_kwastar_gen", (4, Msc_k_wted_astar.dups_gen);

    "degraded_h_wted_astar", (2, Wted_astar.dups);
    "degraded_h_wted_astar2", (2, Wted_astar.dups);
    "lazy_wted_astar2", (2, Wted_astar.dups);
    "wted_astar_on_gen", (2,  Wted_astar.dups_on_gen);
    "dyn_wted_astar", (2, Dynamically_wted_astar.dups);
    "dwa_redux", (2, Revised_dwastar.dups);
    "dwa_redux_otb", (2, Dwastar_tiebreaking.g_dups);
    "d_wastar", (2, Wastar_tiebreaking.d_dups);
    "aseps", (2, Aseps.dups);
    "aeps_m2_close", (2, Aeps_m2.dups_close);
    "aeps_m2_finc", (2, Aeps_m2.dups_finc);
    "aeps_np", (2, Aepsilon.never_dups);
    "aeps_ap", (2, Aepsilon.always_dups);
    "aeps_dthresh", (2, Aepsilon.dtb_dups);
    "aeps_mingrow", (2, Aepsilon.mwg_dups);
    "aeps_deps", (2, Aepsilon.eps_dups);
    "aseps_dae", (2, Aseps.dups_on_expand);
    "aseps_alt", (2, Aseps_sched.alt);

    "nonparametric_astar", (1, Nonparm_astar.dups);
    "anytime_window_astar", (1, Awastar.dups);
    "anytime_baseline", (1, Baseline_anytime.two_phase_dups);
    "anytime_greedy", (1, Anytime_greedy.dups);
    "anytime_speedy", (1, Anytime_speedy.dups);
    "anytime_window_astar_scale", (1, Awastar.dups_scaling);
    "anytime_window_astar_g", (1, Awastar_g.dups);
    "anytime_window_astar_h", (1, Awastar_h.dups);
    "anytime_window_astar_d", (1, Awastar_d.dups);
    "anytime_window_astar_d_scale", (1, Awastar_d.dups_scaling);
    "anytime_window_weighted_astar", (2, Awwastar.dups);
    "anytime_astar", (2, Anytime_astar.dups);
    "anytime_skeptical", (2, Anytime_skeptical.austin_dups);
    "anytime_dyn_astar", (2, Anytime_dwastar.dups);
    "anytime_dwaredux", (2, Anytime_rdwastar.dups);
    "anytime_aseps", (2, Anytime_aseps.dups);
    "anytime_aeps_dthresh", (2, Aepsilon.anytime_dtb_dups);
    "anytime_hd_reckless", (1, Anytime_adaptive.hd_ss_reckless_dups);
    "anytime_hdp_reckless", (2, Anytime_adaptive.hd_ss_fhp_reckless_dups);
    "anytime_chdp_reckless", (2, Anytime_adaptive.hd_ss_cfhp_reckless_dups);
    "anytime_alpha_h_astar", (2, Alpha_astar.anytime_alpha_h_dups);
    "anytime_tqs_hd_reckless", (2, Tqs_fast_v2.anytime_hd_ss_reckless_dups);
    "anytime_tqs_spec", (2, Tqs_specified_hhat.anytime_hd_ss_reckless_dups);
    "anytime_ees_l3", (2, Call_ees_l3.hd_ss_continued_dups);

    "itsastar_greedy", (1, Itsastar.search_dups);

    "raseps", (2, Anytime_aseps.restarting_dups);
    "raeps_dthresh", (2, Aepsilon.restarting_dtb_dups);
    "restarts_dwastar", (2, Restarts_dwastar.dups);
    "restarts_rdwastar", (2, Restarts_rdwastar.dups);
    "restarts_alpha_h_astar", (2, Restarting_alpha_astar.alpha_h_dups);
    "restarts_hdp_reckless", (2, Restarting_adaptive.hd_ss_fhp_reckless_dups);
    "restarts_chdp_reckless", (2, Restarting_adaptive.hd_ss_cfhp_reckless_dups);
    "restarts_tqs_hd_reckless", (2, Restarting_tqs_calls.hd_ss_reckless_dups);
    "restarts_tqs_spec", (2, Restarting_tqs_calls.supplied_hd_ss_reckless_dups);
    "restarts_ees_l3", (2, Call_ees_l3.hd_ss_restarting_dups);

    "araeps_dthresh", (2, Aepsilon.repairing_dtb_dups);
    "araseps", (2, Anytime_aseps.repairing_dups);
    "ardwastar", (2, Ardwastar.dups);
    "arrdwastar", (2, Arrdwastar.dups);
    "aralpha_h_astar", (2, Repairing_alpha_astar.alpha_h_dups);
    "repair_hdp_reckless", (2, Repairing_adaptive.hd_ss_fhp_reckless_dups);
    "repair_chdp_reckless", (2, Repairing_adaptive.hd_ss_cfhp_reckless_dups);
    "repair_tqs_hd_reckless", (2, Restarting_tqs_calls.repair_hd_ss_reckless_dups);
    "repair_tqs_spec", (2, Restarting_tqs_calls.repair_supplied_hd_ss_reckless_dups);
    "repair_ees_l3", (2, Call_ees_l3.hd_ss_repairing_dups);

    "aggressive", (3, Optimistic_search.dups);
    "aggressive_dwastar", (3, Dyn_wted_optimistic.dups);
    "aggressive_dwacheap", (3, Dwa_optimistic.dups);
    "limited_enthusiasm", (2, Clamped_searches.mult_dups);
    "max_w", (2, Max_w.dups);


    (********************* Learning Algs  *****************************)
    "h_cleanup_reckless", (2, Cleanup_searches.cleanup_dups);
    "aggressive_fhp", (2, Cleanup_searches.fd_cleanup_dups);
    "aggressive_fhp_path", (2, Path_cleanup.austin_uc_dups);
    "aggressive_fhp_path_pm", (2, Path_cleanup.austin_uc_dups_pm);
    "aggressive_fhp_path_test", (2, Path_cleanup.skept_uc_dups);
    "aggressive_fhp_lms", (2, Cleanup_searches.lms_fd_cleanup_dups);
    "aggressive_fhp_fixed_lms",
    (6, Cleanup_searches.lms_fd_cleanup_dups_no_learn);
    "aggressive_fhp_fixed", (2, Cleanup_searches.fd_fixed_cleanup_dups);
    "aggressive_fhp_ann", (2, Cleanup_searches.ann_fd_cleanup_dups);
    "aggressive_fhp_pm", (2, Cleanup_searches.fd_cleanup_pm_dups);
    "clamped_h_reckless", (2, Clamped_searches.h_ss_reckless_dups);
    "clamped_lms_reckless", (2, Clamped_searches.h_lms_fhp_reckless_dups);
    "clamped_fixed", (6, Clamped_searches.h_lms_fhp_reckless_dups_no_learn);
    "clamped_ann_reckless", (2, Clamped_searches.h_ann_fhp_reckless_dups);
    "clamped_h_conservative", (2, Clamped_searches.h_ss_conservative_dups);
    "clamped_h_geo", (2, Clamped_searches.h_ss_geo_dups);
    "clamped_h_fhp_reckless", (2, Clamped_searches.h_ss_fhp_reckless_dups);
    "clamped_h_fhp_conservative", (2, Clamped_searches.h_ss_fhp_conservative_dups);
    "clamped_h_fhp_geo", (2, Clamped_searches.h_ss_fhp_geo_dups);
    "fhat_switch_h_ss", (2, Reverse_ss_searches.fq_only_seems_bounded_dups);
    "rev_cleanup_simple", (2, Reverse_ss_searches.simple_dups);
    "tqs_h_reckless", (2, Tqs.h_ss_reckless_dups);
    "tqs_h_gupdate", (2, Tqs_gupdate.h_ss_reckless_dups);
    "tqs_h_conservative", (2, Tqs.h_ss_conservative_dups);
    "tqs_h_reckless_no_d",(2, Tqs.h_ss_reckless_no_d_dups);
    "tqs_h_reckless_no_fh",(2, Tqs.h_ss_reckless_no_fh_dups);
    "tqs_h_reckless_fast", (2, Tqs_fast.h_ss_reckless_dups);
    "tqs_h_lms_reckless_fast", (2, Tqs_fast.h_lms_reckless_dups);
    "tqs_h_geo_fast", (2, Tqs_fast.h_ss_geo_dups);
    "tqs_h_conservative_fast", (2, Tqs_fast.h_ss_conservative_dups);
    "tqs_vector_m1", (2, Tqs_rev.vector_dups);
    "tqs_hd_reckless", (2, Tqs_fast_v2.hd_ss_reckless_dups);
    "tqs_hd_path", (2, Tqs_fast_v2.hd_ss_path_dups);

    "tqs_rewrite", (2, Tqs_rewrite.dups);
    "tqs_rewrite_pm", (2, Tqs_rewrite.dups_pm);
    "continued_tqs_rewrite", (2, Continued_tqs_rewrite.dups);
    "continued_tqs_rewrite_pm", (2, Continued_tqs_rewrite.dups_pm);
    "restarting_tqs_rewrite", (1, Restarting_tqs_rewrite.dups);
    "restarting_tqs_rewrite_pm", (1, Restarting_tqs_rewrite.dups_pm);
    "repairing_tqs_rewrite", (1, Repairing_tqs_rewrite.dups);
    "repairing_tqs_rewrite_pm", (1, Repairing_tqs_rewrite.dups_pm);

    "tqs_l1", (2, Tqs_lesion.dups_l1);
    "tqs_l2", (2, Tqs_lesion.dups_l2);
    "tqs_bc", (3, Tqs_as_bc.dups);
    "tqs_coo", (2, Tqs_coo.dups);
    "fdn", (2, Fdn.dups);
    "aseps_coo", (2, Aseps_coo.dups);
    "tqs_probe_m2", (2, Three_queue_probes.dups);
    "tqs_hd_path_agg", (2, Tqs_fast_v2.hd_ss_path_aggressive);
    "tqs_hd_path_agg_m2", (2, Tqs_fast_v2.hd_ss_path_aggressive_m2);
    "tqs_hd_path_agg_m2_pm", (2, Tqs_fast_v2.hd_ss_path_aggressive_m2_pm);
    "tqs_hd_path_pm", (2, Tqs_fast_v2.hd_ss_path_pm_dups);
    "tqs_hd_path_oldcorrect", (2, Tqs_fast_v2.hd_ss_path_oldschool_dups);
    "tqs_hd_reckless_oldcorrect",
    (2, Tqs_fast_v2.hd_ss_oldstyle_reckless_dups);
    "tqs_hd_reckless_ia", (2, Tqs_fast_inadmiss.hd_ss_reckless_dups);
    "tqs_hd_pm_reckless", (2, Tqs_fast_v2.hd_pm_ss_reckless_dups);
    "tqs_hd_reckless_no_fh", (2, Tqs_fast_v2.hd_ss_reckless_no_fh_dups);
    "tqs_hd_geo", (2, Tqs_fast_v2.hd_ss_geo_dups);
    "tqs_hd_pm_geo", (2, Tqs_fast_v2.hd_pm_ss_geo_dups);
    "tqs_hd_con", (2, Tqs_fast_v2.hd_ss_conservative_dups);
    "tqs_rev_hd_reckless", (2, Tqs_rev_fast.hd_globalavg_reckless_dups);
    "tqs_rev_hd_nomem", (2, Tqs_rev_fast.hd_reckless_dups);
    "tqs_hd_lms", (2, Tqs_fast_v2.hd_lms_reckless_dups);
    "tqs_hd_lms_geo", (2, Tqs_fast_v2.hd_lms_geo_dups);
    "tqs_hd_lms_pm", (2, Tqs_fast_v2.hd_lms_pm_reckless_dups);
    "tqs_hd_lms_pm_geo", (2, Tqs_fast_v2.hd_lms_pm_geo_dups);
    "tqs_hdh_lms", (2, Tqs_fast_v2.hdh_lms_reckless_dups);
(*    "tqs_hd_batch", (2, Do_batched_lms.hd_batched_reckless_dups);
    "tqs_hd_batch_dd", (2, Do_batched_lms.hd_batched_reckless_dd);*)
    "clamped_hd_reckless", (2, Clamped_searches.hd_ss_reckless_dups);
    "clamped_hd_path", (2, Path_based_adaptive.austin_clamped_dups);
    "clamped_hdp_path", (2, Path_based_adaptive.austin_clamped_dups_prime);
    "aseps_hhat", (2, F_then_hhat.dups);

    (*** Supplied with inadmissle heuristic info *)
    "aseps_fhat", (2, F_then_hhat.dups);
    "clamped_fhat", (2, Clamped_specified_hhat.dups);
    "optimistic_fhat", (2, Cleanup_specified_hhat.cleanup_dups);
    "optimistic_fhatprime", (2, Cleanup_specified_hhat.cleanup_fp_dups);
    "tqs_corrupted_truth", (2, Tqs_fast.h_ss_reckless_dups);
    "tqs_truth_hhat", (2, Tqs_specified_hhat.h_dups);
    "tqs_specified_hdhat", (2, Tqs_specified_hhat.hd_dups);

    (********************* Recording  *****************************)

    "truth", (1, Recording_astar.tab_record_dups);
    "record_allnodes", (1, Recording_uniform.allnodes_dups);
    "record_uniform", (1, Recording_uniform.expand_recorder_dups);
    "record_uniform_queue", (1, Recording_uniform.queue_recorder_dups);
    "record_greedy", (1, Recording_greedy.expand_recorder_dups);
    "record_greedy_queue", (1, Recording_greedy.queue_recorder_dups);
    "record_speedy", (1, Recording_speedy.expand_recorder_dups);
    "record_speedy_queue", (1, Recording_speedy.queue_recorder_dups);
    "record_astar", (1, Recording_astar.expand_recorder_dups);
    "record_astar_queue", (1, Recording_astar.queue_recorder_dups);
    "record_wted_astar", (2, Recording_wastar.expand_recorder_dups);
    "record_wted_astar_queue", (2, Recording_wastar.queue_recorder_dups);
    "record_ca_h_reckless_exp", (2, Recording_clamped.h_ss_reckless_dups_exp);
    "record_ca_h_reckless_queue", (2, Recording_clamped.h_ss_reckless_dups_queue);
    "record_h_reckless_exp", (2, Recording_unbounded_ss.h_ss_reckless_dups_exp);
    "record_h_reckless_queue", (2, Recording_unbounded_ss.h_ss_reckless_dups_queue);
    "record_tqs_exp", (2, Recording_tqs_v2.dups);
    "record_tqs_focal", (2, Recording_tqs_v2.dups_focal);
    "record_tqs_geq", (2, Recording_tqs_v2.dups_geq);
    "record_tqs_clean", (2, Recording_tqs_v2.dups_clean);
    "record_aseps_exp", (2, Recording_aseps.record_exp_dups);
    "record_aseps_geq", (2, Recording_aseps.record_geq_dups);
    "record_aseps_focal", (2, Recording_aseps.record_focal_dups);
    "record_tqs_h_exp", (2, Recording_tqs.h_ss_rec_exp_dups);
    "record_tqs_h_clean", (2, Recording_tqs.h_ss_rec_clean_dups);
    "record_tqs_h_geq", (2, Recording_tqs.h_ss_rec_geq_dups);
    "record_tqs_h_focal", (2, Recording_tqs.h_ss_rec_focal_dups);
    "record_aggressive_exp", (3, Recording_optimistic.expand_recorder_dups);
    "record_aggressive_clean", (3, Recording_optimistic.clean_recorder_dups);
    "record_aggressive_prime", (3, Recording_optimistic.prime_recorder_dups);

    "record_dfs_exp", (1, Recording_dfs.expand_recorder_dups);
    "record_dfs_q", (1, Recording_dfs.stack_recorder_dups);

    (******************************** Deadline *****************************)
    "contract_astar", (2, Contract_astar.do_dups);
    "das_rewrite", (2, Das_rewrite.dups);
    "approx_contract_astar", (3, Contract_astar.approx_dups);
    "sf_aca", (3, Speedy_first_acastar.sfa_dups);
    "sf_aca_time", (3, Speedy_first_acastar.sfa_dups_time);
    "sf_aca_time_austin", (3, Speedy_first_anytime.speedy_first_acastar);

    (******************************** Progress estimate *********************)

    (* Deprecated A* calls *)
    "astar_est_rb", (2, Aest_ringbuff.dups_est);
    "astar_vest_rb", (2, Aest_ringbuff.dups_vest);
    "astar_vest_rb_prime", (2, Aest_ringbuff.dups_vest');
    "astar_est_histo", (2, Aest_histo.dups_est);
    "astar_est_histo_all", (2, Aest_histo.dups_est_all);


    (* velocity *)
    "wastar_velocity", (2, West_velocity.dups);
    "astar_velocity", (1, West_ringbuff.dups_astar);
    "greedy_velocity", (1, Gest_velocity.dups_est);
    "speedy_velocity", (1, Gest_velocity.speedy_dups_est);

    "greedy_velocity_hat", (1, Ghest_velocity.dups);
    "speedy_velocity_hat", (1, Ghest_velocity.speedy_dups);
    "wastar_velocity_hat", (2, West_velocity.dups_hat);

    "astar_gpercent", (1, Aest_ringbuff.dups_pest);
    "astar_fpercent", (2,  Aest_fvelocity.dups_pest);
    "astar_fvelocity", (2, Aest_fvelocity.dups);

    (* Percent estimates *)
    "greedy_hpercent", (1, Gest_ringbuff.dups_pest);
    "greedy_percent", (1, Gest_ringbuff.dups_pest);
    "greedy_percent_velocity", (1, Gest_percent_velocity.dups_pest);
    "greedy_percent_prime", (1, Gest_ringbuff.dups_pest_prime);
    "greedy_percent_f", (1, Gest_ringbuff.dups_fpest);
    "speedy_percent", (1, Gest_ringbuff.speedy_dups_pest);
    "wastar_percent", (2, West_pest.dups);
    "wastar_percent_hack", (2, West_pest.dups_hack);
    "wastar_hpercent", (2, West_pest.dups_hest);
    "wastar_hpercent_prime", (2, West_pest.dups_hest_prime);
    "wastar_hpercent_prime2", (2, West_pest.dups_hest_prime2);
    "wastar_percent_velocity", (2, West_percent_velocity.dups);
    "wastar_percent_prime", (2, West_pest.dups_prime);
    "wastar_percent_velocity_prime", (2, West_percent_velocity.dups_prime);

    "greedy_percent_hat", (1, Ghest_ringbuff.dups_pest);
    "speedy_percent_hat", (1, Ghest_ringbuff.speedy_dups_pest);

    "wastar_percent_hat", (2, West_pest.dups_pest_hat);
    "wastar_percent_hat_prime", (2, West_pest.dups_pest_hat_prime);

    "greedy_phisto", (2, Gest_phisto.dups_est);
    "speedy_phisto", (2, Gest_phisto.speedy_dups_est);
    "wastar_phisto", (3, Wastar_phisto.dups);

    "astar_percent_optf", (2, Aest_percentage.dups);

    (* Vacilation *)
    "greedy_vacillation", (2, Gest_vacilation.dups_est);
    "greedy_vacillation_hat", (2, Ghest_vacilation.dups_est);
    "wastar_vacillation", (3, Wastar_vacilation.dups_est);
    "wastar_vacillation_hat", (3, Wastar_vacilation.dups_est_hat);


    (* histograms *)
    "greedy_histo", (2, Gest_histo.dups_est);
    "speedy_histo", (2, Gest_histo.speedy_dups_est);
    "wastar_histo", (3, Wastar_histo.dups);

    "greedy_histo_all", (2, Gest_histo.dups_est_all);
    "speedy_histo_all", (2, Gest_histo.speedy_dups_est_all);
    "wastar_histo_all_h", (3, Wastar_histo.dups_all);

  ]


(* EOF *)
