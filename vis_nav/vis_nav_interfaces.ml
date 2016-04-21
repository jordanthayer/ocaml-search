(** Search interfaces for use with visibility based navigation *)

let with_path6 (s, e, g, p, m, d) =
  (match s with
    None -> None
  | Some (n, f) -> Some (Vis_nav_instance.get_solution n, f)), e, g, p, m, d


let simple_euclidian (root,world) lim =
  let h = Vis_nav_heuristics.make_euclidian world in
  (Search_interface.make
     ~h:h
     ~d:h
     ~hd:(fun n -> let v = h n in v,v)
     ~domain_expand:(Vis_nav_instance.make_expand world)
     ~predecessor:(Vis_nav_instance.make_expand world)
     ~key:Vis_nav_instance.key
     ~equals:(=)
     ~goal_p:(Vis_nav_instance.make_goal world)
     ~halt_on:lim
     ~key_print:(fun _ -> "")
     ~get_sol_length:(fun n -> List.length (Vis_nav_instance.find_path n))
     Search_interface.Vis_nav
     root
     (fun _ _ -> false)
     (fun _ -> ()))

(* EOF *)
