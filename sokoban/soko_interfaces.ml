(** Search interfaces for use with sokoban *)

let with_path6 (s, e, g, p, m, d) =
  (match s with
    None -> None
  | Some (n, f) -> Some (Sokoban_instance.get_solution n, f)), e, g, p, m, d


let simple_manhattan (root,world) lim =
  let h = Soko_heuristics.make_manhattan_nearest_target_overlap world 1. in
  (Search_interface.make
     ~h:h
     ~d:h
     ~hd:(fun n -> let v = h n in v,v)
     ~domain_expand:(Sokoban_instance.make_expand world 1. 1.)
     ~predecessor:(Sokoban_instance.make_expand world 1. 1.)
     ~key:Sokoban_instance.key
     ~equals:(=)
     ~goal_p:(Sokoban_instance.make_goal world)
     ~halt_on:lim
     ~get_sol_length:(fun n -> List.length (Sokoban_instance.find_path n))
     Search_interface.Sokoban
     root
     (fun _ _ -> false)
     (fun _ -> ()))


let true_target_distance_overlap (root,world) lim =
  let h = Soko_heuristics.min_true_target_distance_overlap world 1. in
  (Search_interface.make
     ~h:h
     ~d:h
     ~hd:(fun n -> let v = h n in v,v)
     ~domain_expand:(Sokoban_instance.make_expand world 1. 1.)
     ~predecessor:(Sokoban_instance.make_expand world 1. 1.)
     ~key:Sokoban_instance.key
     ~equals:(=)
     ~goal_p:(Sokoban_instance.make_goal world)
     ~halt_on:lim
     ~get_sol_length:(fun n -> List.length (Sokoban_instance.find_path n))
     Search_interface.Sokoban
     root
     (fun _ _ -> false)
     (fun _ -> ()))


let solve_single_box (root,world) lim =
  let h,_ = Soko_heuristics.heuristic_solve_simplified world 1. 1. in
  (Search_interface.make
     ~h:h
     ~d:h
     ~hd:(fun n -> let v = h n in v,v)
     ~domain_expand:(Sokoban_instance.make_expand world 1. 1.)
     ~predecessor:(Sokoban_instance.make_expand world 1. 1.)
     ~key:Sokoban_instance.key
     ~equals:(=)
     ~goal_p:(Sokoban_instance.make_goal world)
     ~halt_on:lim
     ~get_sol_length:(fun n -> List.length (Sokoban_instance.find_path n))
     Search_interface.Sokoban
     root
     (fun _ _ -> false)
     (fun _ -> ()))


let solve_single_box_bi (root,world) lim =
  let h = Soko_heuristics.bipart_heuristic_solve_per_target world 1. 1. in
  (Search_interface.make
     ~h:h
     ~d:h
     ~hd:(fun n -> let v = h n in v,v)
     ~domain_expand:(Sokoban_instance.make_expand world 1. 1.)
     ~predecessor:(Sokoban_instance.make_expand world 1. 1.)
     ~key:Sokoban_instance.key
     ~equals:(=)
     ~goal_p:(Sokoban_instance.make_goal world)
     ~halt_on:lim
     ~get_sol_length:(fun n -> List.length (Sokoban_instance.find_path n))
     Search_interface.Sokoban
     root
     (fun _ _ -> false)
     (fun _ -> ()))

(* EOF *)
