(** A epsilon, as Ghallab intended, implemented in our heuristic search
    framework *)

open Aseps (* has the exact same node structure. *)

(* Persevere strategies *)

let always_persevere _ _ = true
and never_persevere _ _ = false

and make_rand_persever p =
  assert (p <= 1.);
  Random.self_init();
  (fun _ _ -> (Random.float 1.) < p)

and d_threshold_on_best dv =
  (* Wheeler suggested a d value of somewhere around 5~10 *)
  assert (dv > 0.);
  (fun _ best -> best.d <= dv)

and d_threshold_on_min dv =
  assert (dv >= 0.);
  (fun minf _ -> minf.d >= dv)

and min_will_grow t =
  assert (t <= 1.);
  assert (t >= 0.);
  (fun minf _ -> ((minf.f -. minf.g) /. minf.g) > t)

and eps_diff eps =
  assert (eps >= 1.);
  (fun minf best -> (best.f /. minf.f) <= eps)


(* Base callers *)

let no_dups p_strat sface args =
  let bound = Search_args.get_float "Aepsilon.no_dups" args 0 in
  Limit.unwrap_sol5 unwrap
    (Focused_search.no_dups
       p_strat
       (make_iface sface)
       just_f
       d_then_f_then_g
       (make_close_enough_p bound)
       just_f
       set_q_pos
       get_q_pos)


let dups p_strat sface args =
  let bound = Search_args.get_float "Aepsilon.dups" args 0 in
  Limit.unwrap_sol6 unwrap
    (Focused_search.dups
       p_strat
       (make_iface sface)
       just_f
       d_then_f_then_g
       (make_close_enough_p bound)
       just_f
       set_q_pos
       get_q_pos)



let anytime_no_dups p_strat sface args =
  let bound = Search_args.get_float "Aepsilon.no_dups" args 0 in
  Limit.unwrap_sol5 unwrap
    (Focused_search.no_dups ~anytime:true
       p_strat
       (make_iface sface)
       just_f
       d_then_f_then_g
       (make_close_enough_p bound)
       just_f
       set_q_pos
       get_q_pos)


let anytime_dups p_strat sface args =
  let bound = Search_args.get_float "Aepsilon.dups" args 0 in
  Limit.unwrap_sol6 unwrap
    (Focused_search.dups ~anytime:true
       p_strat
       (make_iface sface)
       just_f
       d_then_f_then_g
       (make_close_enough_p bound)
       just_f
       set_q_pos
       get_q_pos)


let repairing_no_dups p_strat sface args =
  let bound = Search_args.get_float "Aepsilon.no_dups" args 0 in
  let wtlist = Arastar.mk_wtlist bound 0.2 in
  Limit.unwrap_sol5 unwrap
    (Repairing_focused_search.no_dups wtlist
       p_strat
       (make_iface sface)
       just_f
       d_then_f_then_g
       make_close_enough_p
       just_f
       set_q_pos
       get_q_pos)


let repairing_dups p_strat sface args =
  let bound = Search_args.get_float "Aepsilon.dups" args 0 in
  let wtlist = Arastar.mk_wtlist bound 0.2 in
  Limit.unwrap_sol6 unwrap
    (Repairing_focused_search.dups
       wtlist
       p_strat
       (make_iface sface)
       just_f
       d_then_f_then_g
       make_close_enough_p
       just_f
       set_q_pos
       get_q_pos)


let restarting_no_dups p_strat sface args =
  let bound = Search_args.get_float "Aepsilon.no_dups" args 0 in
  let wtlist = Arastar.mk_wtlist bound 0.2 in
  Limit.unwrap_sol5 unwrap
    (Restarting_focused_search.no_dups wtlist
       p_strat
       (make_iface sface)
       just_f
       d_then_f_then_g
       make_close_enough_p
       just_f
       set_q_pos
       get_q_pos)


let restarting_dups p_strat sface args =
  let bound = Search_args.get_float "Aepsilon.dups" args 0 in
  let wtlist = Arastar.mk_wtlist bound 0.2 in
  Limit.unwrap_sol6 unwrap
    (Restarting_focused_search.dups
       wtlist
       p_strat
       (make_iface sface)
       just_f
       d_then_f_then_g
       make_close_enough_p
       just_f
       set_q_pos
       get_q_pos)

(* Callers integrated with persevere strategies *)

let never_nodups s args = no_dups never_persevere s args
and never_dups s args = dups never_persevere s args
and always_nodups s args = no_dups always_persevere s args
and always_dups s args = dups always_persevere s args
and dtb_nodups s args = no_dups (d_threshold_on_best 10.) s args
and dtb_dups s args = dups (d_threshold_on_best 10.) s args

and mwg_nodups s args =
  let bound = Search_args.get_float "Aepsilon.mwg_nodups" args 0 in
    no_dups (min_will_grow (1./.bound)) s args
and mwg_dups s args =
  let bound = Search_args.get_float "Aepsilon.mwg_dups" args 0 in
    dups (min_will_grow (1./.bound)) s args
and eps_nodups s args =
  let bound = Search_args.get_float "Aepsilon.eps_nodups" args 0 in
    no_dups (eps_diff (1.5 *. bound)) s args
and eps_dups s args =
  let bound = Search_args.get_float "Aepsilon.eps_dups" args 0 in
    dups (eps_diff (1.5 *. bound)) s args






let anytime_never_nodups s args = anytime_no_dups never_persevere s args
and anytime_never_dups s args = anytime_dups never_persevere s args
and anytime_always_nodups s args = anytime_no_dups always_persevere s args
and anytime_always_dups s args = anytime_dups always_persevere s args
and anytime_dtb_nodups s args = anytime_no_dups (d_threshold_on_best 10.) s args
and anytime_dtb_dups s args = anytime_dups (d_threshold_on_best 10.) s args

and anytime_mwg_nodups s args =
  let bound = Search_args.get_float "Aepsilon.mwg_nodups" args 0 in
    anytime_no_dups (min_will_grow (1./.bound)) s args
and anytime_mwg_dups s args =
  let bound = Search_args.get_float "Aepsilon.mwg_dups" args 0 in
    anytime_dups (min_will_grow (1./.bound)) s args
and anytime_eps_nodups s args =
  let bound = Search_args.get_float "Aepsilon.eps_nodups" args 0 in
    anytime_no_dups (eps_diff (1.5 *. bound)) s args
and anytime_eps_dups s args =
  let bound = Search_args.get_float "Aepsilon.eps_dups" args 0 in
    anytime_dups (eps_diff (1.5 *. bound)) s args





let repairing_never_nodups s args = repairing_no_dups never_persevere s args
and repairing_never_dups s args = repairing_dups never_persevere s args
and repairing_always_nodups s args = repairing_no_dups always_persevere s args
and repairing_always_dups s args = repairing_dups always_persevere s args
and repairing_dtb_nodups s args = repairing_no_dups (d_threshold_on_best 10.) s args
and repairing_dtb_dups s args = repairing_dups (d_threshold_on_best 10.) s args

and repairing_mwg_nodups s args =
  let bound = Search_args.get_float "Aepsilon.mwg_nodups" args 0 in
    repairing_no_dups (min_will_grow (1./.bound)) s args
and repairing_mwg_dups s args =
  let bound = Search_args.get_float "Aepsilon.mwg_dups" args 0 in
    repairing_dups (min_will_grow (1./.bound)) s args
and repairing_eps_nodups s args =
  let bound = Search_args.get_float "Aepsilon.eps_nodups" args 0 in
    repairing_no_dups (eps_diff (1.5 *. bound)) s args
and repairing_eps_dups s args =
  let bound = Search_args.get_float "Aepsilon.eps_dups" args 0 in
    repairing_dups (eps_diff (1.5 *. bound)) s args



let restarting_never_nodups s args = restarting_no_dups never_persevere s args
and restarting_never_dups s args = restarting_dups never_persevere s args
and restarting_always_nodups s args = restarting_no_dups always_persevere s args
and restarting_always_dups s args = restarting_dups always_persevere s args
and restarting_dtb_nodups s args = restarting_no_dups (d_threshold_on_best 10.) s args
and restarting_dtb_dups s args = restarting_dups (d_threshold_on_best 10.) s args

and restarting_mwg_nodups s args =
  let bound = Search_args.get_float "Aepsilon.mwg_nodups" args 0 in
    restarting_no_dups (min_will_grow (1./.bound)) s args
and restarting_mwg_dups s args =
  let bound = Search_args.get_float "Aepsilon.mwg_dups" args 0 in
    restarting_dups (min_will_grow (1./.bound)) s args
and restarting_eps_nodups s args =
  let bound = Search_args.get_float "Aepsilon.eps_nodups" args 0 in
    restarting_no_dups (eps_diff (1.5 *. bound)) s args
and restarting_eps_dups s args =
  let bound = Search_args.get_float "Aepsilon.eps_dups" args 0 in
    restarting_dups (eps_diff (1.5 *. bound)) s args

(* EOF *)
