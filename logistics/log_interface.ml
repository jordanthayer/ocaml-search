(** A collection of interfaces to the logistics domain *)


let default_interface (static,instance) lim =
  (Search_interface.make
     ~h:(Log_heuristics.h static)
     ~d:(Log_heuristics.d static)
     ~hd:(Log_heuristics.hd static)
     ~domain_expand:(Log_instance.make_expand static)
     ~key:Log_instance.key
     ~equals:Log_instance.equals
     ~goal_p:(Log_instance.make_package_only_goal_p static)
     ~halt_on:lim
     ~get_sol_length:Log_instance.sol_length
     Search_interface.Logistics
     instance
     (fun _ _ -> false)
     (fun _ -> ()))

(* EOF *)
