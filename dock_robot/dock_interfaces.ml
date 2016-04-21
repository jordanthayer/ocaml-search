(**

    @author jtd7
    @since 2011-01-20

   Dock yard robot interfaces, which are awesome
   not duck faces, which suck
*)

let make_interface inst limit h d hd =
  let log _ = ()
  and is_better _ _ = false
  and init = Dock_robot.make_init inst in
    Search_interface.make Search_interface.Dock_robot init is_better log
      ~t:(fun _ -> 0)
      ~h
      ~d
      ~hd
      ~domain_expand:(Dock_robot.make_expand inst)
      ~goal_p:(Dock_robot.make_is_goal inst)
      ~halt_on:limit
      ~hash:(Dock_robot.make_hash inst)
      ~equals:(=)
      ~key:Dock_robot.key
      ~key_print:Dock_robot.string_of_key
      ~get_sol_length:Dock_robot.sol_length


let make_assert_interface inst limit h d hd =
  (** tacks some interesting asserts into h and d to check heuristics
      validity *)
  let log _ = ()
  and is_better _ _ = false
  and init = Dock_robot.make_init inst
  and goal_p = Dock_robot.make_is_goal inst in
    Search_interface.make Search_interface.Dock_robot init is_better log
      ~t:(fun _ -> 0)
      ~h:(fun n -> let v = h n in
	    assert (v > 0. || (goal_p n));
	    v)
      ~d:(fun n -> let v = d n in
	    if (v > 0. || (goal_p n)) then v
	    else (Dock_robot.display n;
		  failwith (Printf.sprintf "d was %f%!" v)))
      ~hd:(fun n -> let hv,dv = hd n in
	    assert (hv > 0. || (goal_p n));
	    assert (dv > 0. || (goal_p n));
	    hv,dv)
      ~domain_expand:(Dock_robot.make_expand inst)
      ~goal_p
      ~halt_on:limit
      ~hash:(Dock_robot.make_hash inst)
      ~equals:(=)
      ~key:Dock_robot.key
      ~key_print:Dock_robot.string_of_key
      ~get_sol_length:Dock_robot.sol_length


let default_interface inst limit =
  let h = Dock_heuristics.make_h inst
  and d = Dock_heuristics.containers_out_of_place inst
  and hd = Dock_heuristics.make_hd inst in
    make_assert_interface inst limit h (fun n -> float (d n)) hd


let deepest_interface inst limit =
  let h = Dock_heuristics.make_h inst
  and d = Dock_heuristics.deepest_out_of_place inst in
  let hd = (fun n -> h n, let v = float (d n) in
	      Verb.pe Verb.debug "%f\n%!" v; v) in
    make_assert_interface inst limit h (fun n -> float (d n)) hd


let sum_deepest_interface inst limit =
  let hd_sup = Dock_heuristics.make_hd_sup inst in
  let hd = Dock_heuristics.make_hd inst in
  let hd = (fun n -> let h_base, _ = hd n in
	    let h_sup, d_sup = hd_sup n in
	      (* d_base counts packages out of place, already handled in hd_sup
		 don't want to double count *)
	      h_base +. h_sup, (float d_sup)) in
    make_assert_interface inst limit (fun n -> fst (hd n))
      (fun n -> snd (hd n)) hd



let dump_interface inst limit =
  let hd_sup = Dock_heuristics.make_dump_pile inst in
  let hd = Dock_heuristics.make_hd inst in
  let hd = (fun n -> let h_base, _ = hd n in
	    let h_sup, d_sup = hd_sup n in
	      (* d_base counts packages out of place, already handled in hd_sup
		 don't want to double count *)
	      h_base +. h_sup, (float d_sup)) in
    make_assert_interface inst limit (fun n -> fst (hd n))
      (fun n -> snd (hd n)) hd


let random_set_interfaces nlocs piles_per_loc cranes_per_loc ncontainers
    walk_leng n_instances =
  (** Makes a set of random instances of leng walk_leng *)
  let make_instance = (fun _ -> Dock_robot.random_inst_random_walk
			 ~nlocs ~piles_per_loc ~cranes_per_loc
			 ~ncontainers ~walk_leng) in
  let irange = Wrlist.range n_instances in
  let instances = List.map make_instance irange in
    List.map (fun i -> sum_deepest_interface i []) instances


(* EOF *)
