(** Search interfaces for the scanalyzer domain.

    @author eaburns
    @since 2011-02-28
*)

open Scanalyzer
open Scanalyzer_inst

let default_interface inst limit =
  let goal = canonical_goal inst in
  let init = initial_state inst in
  let cycle_cost = canonical_norm_cycle_cost in
  let scan_cost = canonical_scan_cycle_cost in
  let mult_costs = true in
  let ops = operators_with_cost inst cycle_cost scan_cost in
(*
  let h = make_all_triples_heuristic inst mult_costs ops goal in
*)
  let h = make_all_pairs_heuristic inst mult_costs ops goal in
  let d = h in
  let hd n = h n, d n in
  let t _ = 0 in
  let domain_expand = make_expand inst mult_costs ops in
  let key s = s in
  let nbatches = nbatches inst in
  let hash = Hashtbl.hash_param nbatches nbatches in
  let equals = int_array_equal in
  let goal_p = int_array_equal goal in
  let p_update _ _ = () in
    Search_interface.make ~h ~d ~t ~hd ~domain_expand ~key ~hash ~equals
      ~goal_p ~p_update ~halt_on:limit Search_interface.Scanalyzer
      init (fun _ _ -> false) (fun _ -> ())
