(**

    @author jtd7
    @since 2010-10-25
*)

open Vacuum

let symmetric_interface w lim =
  let hd = (fun n -> let v = spanning_tree_heuristic n in v,v)
  and rh = make_reverse_spanning_tree_heuristic w in
    (Search_interface.make
       ~h:spanning_tree_heuristic
       ~d:spanning_tree_heuristic
       ~hd:hd
       ~rev_h:rh
       ~rev_d:rh
       ~rev_hd:(fun n -> let v = rh n in v,v)
       ~t:(node_type w)
       ~domain_expand:(make_expand_func w)
       ~predecessor:(make_expand_func ~wparent:true w)
       ~key:(key w)
       ~hash:(Fn.identity)
       ~key_print:key_to_string
       ~equals:(=)
       ~goal_p:goal_p
       ~halt_on:lim
       ~get_sol_length:sol_length
       ~p_update:update_parent
       Search_interface.Vacuum
       (make_root w)
       (fun _ _ -> false)
       (fun _ -> ()))



let heavy_interface ~memo w lim =
  let hd = (make_heavy_spanning_tree_heuristic ~memo w)
  and d = spanning_tree_heuristic
  and rh = make_reverse_spanning_tree_heuristic w in
    (Search_interface.make
       ~h:(fun n -> fst (hd n))
       ~d
       ~hd
       ~rev_h:rh
       ~rev_d:rh
       ~rev_hd:(fun n -> let v = rh n in v,v)
       ~t:(node_type w)
       ~domain_expand:(make_expand_func ~costfn:(make_heavy_vac_costfn w) w)
       ~predecessor:(make_expand_func ~wparent:true w)
       ~key:(key w)
       ~hash:(Fn.identity)
       ~key_print:key_to_string
       ~equals:(=)
       ~goal_p:goal_p
       ~halt_on:lim
       ~get_sol_length:sol_length
       ~p_update:update_parent
       Search_interface.Heavy_vacuum
       (make_root w)
       (fun _ _ -> false)
       (fun _ -> ()))


let heavy_greedy_interface ~memo w lim =
  let hd = (make_heavy_spanning_tree_heuristic ~memo w)
  and d = greedy_heuristic
  and rh = make_reverse_spanning_tree_heuristic w in
    (Search_interface.make
       ~h:(fun n -> fst (hd n))
       ~d
       ~hd:(fun n -> (fst (hd n)), d n)
       ~rev_h:rh
       ~rev_d:rh
       ~rev_hd:(fun n -> let v = rh n in v,v)
       ~t:(node_type w)
       ~domain_expand:(make_expand_func ~costfn:(make_heavy_vac_costfn w) w)
       ~predecessor:(make_expand_func ~wparent:true w)
       ~key:(key w)
       ~hash:(Fn.identity)
       ~key_print:key_to_string
       ~goal_p:goal_p
       ~halt_on:lim
       ~get_sol_length:sol_length
       ~p_update:update_parent
       Search_interface.Heavy_vacuum
       (make_root w)
       (fun _ _ -> false)
       (fun _ -> ()))


let exhaust_symmetric w lim =
  { (symmetric_interface w lim) with
      Search_interface.goal_p = (fun _ -> false)}


let chris_interface w lim =
  let chris_h = make_chris_heuristic w.dirt in
    (Search_interface.make
       ~h:chris_h
       ~d:chris_h
       ~hd:(fun n -> let v = chris_h n in v,v)
       ~t:(node_type w)
       ~domain_expand:(make_expand_func w)
       ~predecessor:(make_expand_func ~wparent:true w)
       ~key:(key w)
       ~hash:(Fn.identity)
       ~key_print:key_to_string
       ~goal_p:goal_p
       ~halt_on:lim
       ~get_sol_length:sol_length
       ~p_update:update_parent
       Search_interface.Vacuum
       (make_root w)
       (fun _ _ -> false)
       (fun _ -> ()))


let chris_wp_interface w lim =
  let chris_h = make_chris_heuristic w.dirt in
    (Search_interface.make
       ~h:chris_h
       ~d:chris_h
       ~hd:(fun n -> let v = chris_h n in v,v)
       ~t:(node_type w)
       ~domain_expand:(make_expand_func ~wparent:true w)
       ~predecessor:(make_expand_func ~wparent:true w)
       ~key:(key w)
       ~hash:(Fn.identity)
       ~key_print:key_to_string
       ~goal_p:goal_p
       ~halt_on:lim
       ~get_sol_length:sol_length
       ~p_update:update_parent
       Search_interface.Vacuum
       (make_root w)
       (fun _ _ -> false)
       (fun _ -> ()))


let chris_improved_d_interface w lim =
  let chris_h = make_chris_heuristic w.dirt in
    (Search_interface.make
       ~h:chris_h
       ~d:greedy_heuristic
       ~hd:(fun n -> chris_h n, greedy_heuristic n)
       ~t:(node_type w)
       ~domain_expand:(make_expand_func w)
       ~predecessor:(make_expand_func ~wparent:true w)
       ~key:(key w)
       ~hash:(Fn.identity)
       ~key_print:key_to_string
       ~goal_p:goal_p
       ~halt_on:lim
       ~get_sol_length:sol_length
       ~p_update:update_parent
       Search_interface.Vacuum
       (make_root w)
       (fun _ _ -> false)
       (fun _ -> ()))


let chris_improved_d_wp_interface w lim =
  let chris_h = make_chris_heuristic w.dirt in
    (Search_interface.make
       ~h:chris_h
       ~d:greedy_heuristic
       ~hd:(fun n -> chris_h n, greedy_heuristic n)
       ~t:(node_type w)
       ~domain_expand:(make_expand_func ~wparent:true w)
       ~predecessor:(make_expand_func ~wparent:true w)
       ~key:(key w)
       ~hash:(Fn.identity)
       ~key_print:key_to_string
       ~goal_p:goal_p
       ~halt_on:lim
       ~get_sol_length:sol_length
       ~p_update:update_parent
       Search_interface.Vacuum
       (make_root w)
       (fun _ _ -> false)
       (fun _ -> ()))



let improved_d_interface w lim =
  let hd = (fun n -> spanning_tree_heuristic n, greedy_heuristic n)
  and rh = make_reverse_spanning_tree_heuristic w
  and rd = reverse_naive w in
    (Search_interface.make
       ~h:spanning_tree_heuristic
       ~d:greedy_heuristic
       ~hd:hd
       ~rev_h:rh
       ~rev_d:rd
       ~rev_hd:(fun n -> rh n, rd n)
       ~t:(node_type w)
       ~domain_expand:(make_expand_func w)
       ~predecessor:(make_expand_func ~wparent:true w)
       ~key:(key w)
       ~hash:(Fn.identity)
       ~key_print:key_to_string
       ~goal_p:goal_p
       ~halt_on:lim
       ~get_sol_length:sol_length
       ~p_update:update_parent
       Search_interface.Vacuum
       (make_root w)
       (fun _ _ -> false)
       (fun _ -> ()))


let from_path path =
  let w = Vacuum_instance.load path in
    improved_d_interface w []


let heavy_from_path path =
  let w = Vacuum_instance.load path in heavy_greedy_interface ~memo:true w []


let displacement_interface w lim =
  let h = displacement_heuristic in
    (Search_interface.make
       ~h:h
       ~d:h
       ~hd:(fun n -> let v = h n in v,v)
       ~t:(node_type w)
       ~domain_expand:(make_expand_func w)
       ~predecessor:(make_expand_func ~wparent:true w)
       ~key:(key w)
       ~hash:(Fn.identity)
       ~key_print:key_to_string
       ~goal_p:goal_p
       ~halt_on:lim
       ~get_sol_length:sol_length
       ~p_update:update_parent
       Search_interface.Vacuum
       (make_root w)
       (fun _ _ -> false)
       (fun _ -> ()))



let symmetric_wp_interface w lim =
  let hd = (fun n -> let v = spanning_tree_heuristic n in v,v)
  and rh = make_reverse_spanning_tree_heuristic w in
    (Search_interface.make
       ~h:spanning_tree_heuristic
       ~d:spanning_tree_heuristic
       ~hd:hd
       ~rev_h:rh
       ~rev_d:rh
       ~rev_hd:(fun n -> let v = rh n in v,v)
       ~t:(node_type w)
       ~domain_expand:(make_expand_func ~wparent:true w)
       ~predecessor:(make_expand_func ~wparent:true w)
       ~key:(key w)
       ~hash:(Fn.identity)
       ~key_print:key_to_string
       ~goal_p:goal_p
       ~halt_on:lim
       ~get_sol_length:sol_length
       ~p_update:update_parent
       Search_interface.Vacuum
       (make_root w)
       (fun _ _ -> false)
       (fun _ -> ()))


let improved_d_wp_interface w lim =
  let hd = (fun n -> spanning_tree_heuristic n, greedy_heuristic n)
  and rh = make_reverse_spanning_tree_heuristic w
  and rd = reverse_naive w in
    (Search_interface.make
       ~h:spanning_tree_heuristic
       ~d:greedy_heuristic
       ~hd:hd
       ~rev_h:rh
       ~rev_d:rd
       ~rev_hd:(fun n -> rh n, rd n)
       ~t:(node_type w)
       ~domain_expand:(make_expand_func ~wparent:true w)
       ~predecessor:(make_expand_func ~wparent:true w)
       ~key:(key w)
       ~hash:(Fn.identity)
       ~key_print:key_to_string
       ~goal_p:goal_p
       ~halt_on:lim
       ~get_sol_length:sol_length
       ~p_update:update_parent
       Search_interface.Vacuum
       (make_root w)
       (fun _ _ -> false)
       (fun _ -> ()))



let displacement_wp_interface w lim =
  let h = displacement_heuristic in
    (Search_interface.make
       ~h:h
       ~d:h
       ~hd:(fun n -> let v = h n in v,v)
       ~t:(node_type w)
       ~domain_expand:(make_expand_func ~wparent:true w)
       ~predecessor:(make_expand_func ~wparent:true w)
       ~key:(key w)
       ~hash:(Fn.identity)
       ~key_print:key_to_string
       ~goal_p:goal_p
       ~halt_on:lim
       ~get_sol_length:sol_length
       ~p_update:update_parent
       Search_interface.Vacuum
       (make_root w)
       (fun _ _ -> false)
       (fun _ -> ()))

let pathmax_interface w lim =
  let interface = improved_d_interface w lim in
    interface.Search_interface.initial.h <-
      spanning_tree_heuristic interface.Search_interface.initial;
    { interface with
	Search_interface.h = (fun n -> n.h);
	Search_interface.hd = (fun n -> n.h, displacement_heuristic n);
	Search_interface.domain_expand = (make_expand_func ~pathmax:true w);

	Search_interface.d = displacement_heuristic;
    }


let online_model_interface w lim =
  { (improved_d_interface w lim) with
      Search_interface.h = displacement_heuristic;
      Search_interface.d = displacement_heuristic;
  }

(* EOF *)
