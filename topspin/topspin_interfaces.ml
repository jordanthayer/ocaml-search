(**

   Makes topspin interfaces

*)


let topspin_interface heuristic_name inst lim = 
  let new_hash = Array_hasher.hash_array_function inst.Topspin.n_disks
    inst.Topspin.n_disks in
  let new_expand = Topspin.make_expand inst in
  let new_h = match
    heuristic_name with 
	"h_a" -> Topspin_heuristics.basic_h_a
      | "h_b" -> Topspin_heuristics.basic_h_b
      | "h_ab" -> Topspin_heuristics.basic_h_ab
      | "h_0" -> Topspin_heuristics.bad_h
      | s -> failwith (Printf.sprintf "No such heuristic: %s" s) in

  let hd s = 
    let new_hd = new_h s in
      new_hd, new_hd in

    (Search_interface.make
       ~h:new_h
       ~d:new_h
       ~hd:hd
       ~domain_expand:new_expand
       ~key:Topspin.key
       ~hash:new_hash
       (*
	 ~hash:Array_hasher.rank
       *)
       ~key_print:Topspin.to_string
       ~equals:Topspin.hash_compare
       ~goal_p:Topspin.is_goal
       ~halt_on:lim
       ~get_sol_length:Topspin.sol_length
       ~p_update:(fun _ _ -> ())
       Search_interface.Topspin
       inst.Topspin.initial_configuration
       (fun _ _ -> false)
       (fun _ -> ()))
