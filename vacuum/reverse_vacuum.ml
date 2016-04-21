(** Solving vacuum problems in reverse *)

(* Suck now places dirt.*)

open Vacuum


let reverse_starts t =
  List.map
    (fun p -> { t with start = p;
		  dirt = t.dirt}) t.dirt


let invert_key init_dirt =
  (fun n ->
     let sum = ref 0 in
       Array.iteri (fun i a -> if not a then sum := !sum +
		      int_of_float (2. ** (float_of_int i)))
	 (Array.of_list
	    (List.map (fun a -> List.mem a n.remaining_dirt) init_dirt));
       !sum,n.pos)


(* All recorded costs will be off by a constant 2 *)
let make_rsearch_expand ?(costfn = (fun _ -> 0.)) w =
  let normal_expand = make_expand_func ~costfn w in
    (fun n g ->
       if g = 0.
       then (let starts = List.map make_root (reverse_starts w) in
	       List.map (fun n -> ({n with remaining_dirt =
			     List.filter (fun pos -> pos <> n.pos)
			       n.remaining_dirt}, 1. )) starts)
       else
	 (List.filter (fun (n,g) -> n.remaining_dirt <> w.dirt)
	    (normal_expand n g)))


let reverse_heuristic w hfun =
  (fun n ->
     let inverted_dirt =
       List.filter (fun d -> not (List.mem d n.remaining_dirt)) w.dirt in
       hfun { n with remaining_dirt = inverted_dirt})


let reverse_interface_displacement w lim =
  let h = reverse_heuristic w displacement_heuristic in
    (Search_interface.make
       ~h:h
       ~d:h
       ~hd:(fun n -> let v = h n in v,v)
       ~rev_h:h
       ~rev_d:h
       ~rev_hd:(fun n -> let v = h n in v,v)
       ~t:(node_type w)
       ~domain_expand:(make_rsearch_expand w)
       ~key:(key w)
       ~key_print:key_to_string
       ~goal_p:(fun _ -> false)
       ~halt_on:lim
       ~get_sol_length:sol_length
       ~p_update:update_parent
       Search_interface.Vacuum
       (make_root w)
       (fun _ _ -> false)
       (fun _ -> ()))

let reverse_interface_spanning w lim =
  let h = reverse_heuristic w spanning_tree_heuristic in
    (Search_interface.make
       ~h:spanning_tree_heuristic
       ~d:spanning_tree_heuristic
       ~hd:(fun n -> let v = h n in v,v)
       ~rev_h:h
       ~rev_d:h
       ~rev_hd:(fun n -> let v = h n in v,v)
       ~t:(node_type w)
       ~domain_expand:(make_rsearch_expand w)
       ~key:(key w)
       ~key_print:key_to_string
       ~goal_p:(fun _ -> false)
       ~halt_on:lim
       ~get_sol_length:sol_length
       ~p_update:update_parent
       Search_interface.Vacuum
       (make_root w)
       (fun _ _ -> false)
       (fun _ -> ()))



let reverse_heavy_greedy_interface ~memo w lim =
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
       ~domain_expand:(make_rsearch_expand
			 ~costfn:(make_heavy_vac_costfn w) w)
       ~predecessor:(make_expand_func ~wparent:true w)
       ~key:(key w)
       ~hash:(Fn.identity)
       ~key_print:key_to_string
       ~goal_p:(fun _ -> false)
       ~halt_on:lim
       ~get_sol_length:sol_length
       ~p_update:update_parent
       Search_interface.Heavy_vacuum
       (make_root w)
       (fun _ _ -> false)
       (fun _ -> ()))



(* EOF *)
