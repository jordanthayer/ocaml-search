(* $Id: csp_tree_algs.ml,v 1.1 2004/01/13 23:45:48 ruml Exp ruml $

   tree search algorithms for CSPs
*)


let make_root csp =
  Csp_tree.initial_state
    Csp_tree.most_constrained_variable
    Csp_tree.log_promise_order
    csp


let dfs csp halt_spec logger =
  let root = make_root csp in
    Dfs.depth_first_search
      ~optimal_p:Csp_tree.satisfied
      (* no pruning function or previous best *)
      ~halt:halt_spec
      ~log:logger
      Wrutils.identity
      Csp_tree.better_p
      Csp_tree.leaf_p
      Csp_tree.get_opt_child
      root


let ilds_top csp halt_spec logger =
  let root = make_root csp in
    Lds.ilds_top
      ~optimal_p:Csp_tree.satisfied
      ~halt:halt_spec
      ~log:logger
      Wrutils.identity
      (Csp_tree.num_vars root)
      Csp_tree.better_p
      Csp_tree.leaf_p
      Csp_tree.get_opt_child
      root


let ilds_bottom csp halt_spec logger =
  let root = make_root csp in
    Lds.ilds_bottom
      ~optimal_p:Csp_tree.satisfied
      ~halt:halt_spec
      ~log:logger
      Wrutils.identity
      (Csp_tree.num_vars root)
      Csp_tree.better_p
      Csp_tree.leaf_p
      Csp_tree.get_opt_child
      root


let dds csp halt_spec logger =
  let root = make_root csp in
    Dds.search
      ~optimal_p:Csp_tree.satisfied
      ~halt:halt_spec
      ~log:logger
      Wrutils.identity
      Csp_tree.better_p
      Csp_tree.leaf_p
      Csp_tree.get_opt_child
      root


let indecision_sum csp halt_spec logger =
  let root = make_root csp in
    Indecision.indecision_search
      (Csp_tree.num_vars root)
      (Csp_tree.max_domain_size root)
      Csp_tree.child_costs
      ~optimal_p:Csp_tree.satisfied
      ~halt:halt_spec
      ~log:logger
      Wrutils.identity
      Csp_tree.better_p
      Csp_tree.leaf_p
      Csp_tree.get_opt_child
      root


let indecision_max csp halt_spec logger =
  let root = make_root csp in
    Indecision_max.max_indecision_search
      (Csp_tree.num_vars root)
      (Csp_tree.max_domain_size root)
      Csp_tree.child_costs
      ~optimal_p:Csp_tree.satisfied
      ~halt:halt_spec
      ~log:logger
      Wrutils.identity
      Csp_tree.better_p
      Csp_tree.leaf_p
      Csp_tree.get_opt_child
      root


let blfs_sep csp halt_spec logger =
  let root = make_root csp in
    Separate.blfs_separate
      (Csp_tree.num_vars root)
      (Csp_tree.max_domain_size root)
      Csp_tree.num_unsassigned
      ~num_probes:20
      ~optimal_p:Csp_tree.satisfied
      ~halt:halt_spec
      ~log:logger
      Wrutils.identity
      Csp_tree.better_p
      Csp_tree.leaf_p
      Csp_tree.get_opt_child
      root


let blfs_quad csp halt_spec logger =
  let root = make_root csp in
    Quad.blfs_quad
      (Csp_tree.num_vars root)
      (Csp_tree.max_domain_size root)
      Csp_tree.num_unsassigned
      ~num_probes:20
      ~optimal_p:Csp_tree.satisfied
      ~halt:halt_spec
      ~log:logger
      Wrutils.identity
      Csp_tree.better_p
      Csp_tree.leaf_p
      Csp_tree.get_opt_child
      root


(* idfs

   adaptive probing

   random probing

   iterative broadening
*)


(* EOF *)
