(**

   makse orc interfaces

*)

let orc_interface (inst:Orc.board) lim = 
  (Search_interface.make
     ~h:(Orc.h_fun inst)
     ~d:(Orc.d_fun inst)
     ~hd:(Orc.hd_fun inst)
     (*       ~t:node_type*)
     ~domain_expand:(Orc.expand inst)
     (*       ~predecessor:(make_expand ncakes cost h_fun)*)
     ~key:(Orc.hash_key)
     ~key_print:(Orc.to_string)
     ~equals:(Orc.hash_compare)
     ~goal_p:(Orc.is_goal inst)
     ~halt_on:lim
(* ~get_sol_length:Orc.sol_length
*)
     ~p_update:(fun _ _ -> ())
     Search_interface.Orc
     {
       Orc.xLoc = -1;
       Orc.yLoc = -1;
       Orc.last_move = Orc.Initial;
       Orc.total_moves = 0
     }
     (fun _ _ -> false)
     (fun _ -> ()))
