(** Where inadmissible heuristics for vacuum world problems go to die *)

open Vacuum

let scaled_manhattan obst_p l1 l2 =
  (1. +. obst_p) *.  (manhattan l1 l2)


let scaled_spanning_tree obst_p =
  spanning_tree ~d_calc:(scaled_manhattan obst_p)


let scaled_spanning_tree_heuristic obst_p n =
  let verts = n.pos :: n.remaining_dirt in
    (scaled_spanning_tree obst_p) verts


let make_heavy_greedy w =
  let max_dirt = List.length w.dirt in
    (fun n ->
       let weight = (float
		       (max_dirt + (List.length n.remaining_dirt))) /. 2. in
	 weight *. (greedy_heuristic n))

(* Interfaces *)

let scaled_symmetric_interface obst_p w lim =
  let hd = (fun n -> let v = spanning_tree_heuristic n in v,v)
  and rh = make_reverse_spanning_tree_heuristic w in
    (Search_interface.make
       ~h:(scaled_spanning_tree_heuristic obst_p)
       ~d:(scaled_spanning_tree_heuristic obst_p)
       ~hd:hd
       ~rev_h:rh
       ~rev_d:rh
       ~rev_hd:(fun n -> let v = rh n in v,v)
       ~t:(node_type w)
       ~domain_expand:(make_expand_func w)
       ~key:(key w)
       ~key_print:key_to_string
       ~goal_p:goal_p
       ~halt_on:lim
       ~get_sol_length:sol_length
       ~p_update:update_parent
       Search_interface.Vacuum
       (make_root w)
       (fun _ _ -> false)
       (fun _ -> ()))


let greedy_interface w lim =
  let hd = (fun n -> let v = spanning_tree_heuristic n in v,v)
  and rh = make_reverse_spanning_tree_heuristic w in
    (Search_interface.make
       ~h:(greedy_heuristic)
       ~d:(greedy_heuristic)
       ~hd:hd
       ~rev_h:rh
       ~rev_d:rh
       ~rev_hd:(fun n -> let v = rh n in v,v)
       ~t:(node_type w)
       ~domain_expand:(make_expand_func w)
       ~key:(key w)
       ~key_print:key_to_string
       ~goal_p:goal_p
       ~halt_on:lim
       ~get_sol_length:sol_length
       ~p_update:update_parent
       Search_interface.Vacuum
       (make_root w)
       (fun _ _ -> false)
       (fun _ -> ()))


let heavy_inadmiss_interface w lim =
  let hd = (fun n -> let v = spanning_tree_heuristic n in v,v)
  and rh = make_reverse_spanning_tree_heuristic w in
    (Search_interface.make
       ~h:(make_heavy_greedy w)
       ~d:(greedy_heuristic)
       ~hd:hd
       ~rev_h:rh
       ~rev_d:rh
       ~rev_hd:(fun n -> let v = rh n in v,v)
       ~t:(node_type w)
       ~domain_expand:(make_expand_func w)
       ~key:(key w)
       ~key_print:key_to_string
       ~goal_p:goal_p
       ~halt_on:lim
       ~get_sol_length:sol_length
       ~p_update:update_parent
       Search_interface.Vacuum
       (make_root w)
       (fun _ _ -> false)
       (fun _ -> ()))

let greedy_chris_interface w lim =
  let chris_h = make_chris_heuristic w.dirt in
  let hd = (fun n -> let v = chris_h n in v,v)
  and rh = make_reverse_spanning_tree_heuristic w in
    (Search_interface.make
       ~h:(greedy_heuristic)
       ~d:(greedy_heuristic)
       ~hd:hd
       ~rev_h:rh
       ~rev_d:rh
       ~rev_hd:(fun n -> let v = rh n in v,v)
       ~t:(node_type w)
       ~domain_expand:(make_expand_func w)
       ~key:(key w)
       ~key_print:key_to_string
       ~goal_p:goal_p
       ~halt_on:lim
       ~get_sol_length:sol_length
       ~p_update:update_parent
       Search_interface.Vacuum
       (make_root w)
       (fun _ _ -> false)
       (fun _ -> ()))



let greedy_wp_interface w lim =
  let hd = (fun n -> let v = spanning_tree_heuristic n in v,v)
  and rh = make_reverse_spanning_tree_heuristic w in
    (Search_interface.make
       ~h:(greedy_heuristic)
       ~d:(greedy_heuristic)
       ~hd:hd
       ~rev_h:rh
       ~rev_d:rh
       ~rev_hd:(fun n -> let v = rh n in v,v)
       ~t:(node_type w)
       ~domain_expand:(make_expand_func ~wparent:true w)
       ~predecessor:(make_expand_func ~wparent:true w)
       ~key:(key w)
       ~key_print:key_to_string
       ~goal_p:goal_p
       ~halt_on:lim
       ~get_sol_length:sol_length
       ~p_update:update_parent
       Search_interface.Vacuum
       (make_root w)
       (fun _ _ -> false)
       (fun _ -> ()))



(* EOF *)
