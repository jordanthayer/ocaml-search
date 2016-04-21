(**

    @author jtd7
    @since 2010-10-25
*)
open Pancake

let pdb_interface pdb_h pdb_d cost_name inst lim =
  (** [pdb_interface pdb_h pdb_d cost_name inst] makes the standard
      interface for the pancake domain.  The [pdb] must be pre-loaded
      or created. *)
  let ncakes = npancakes inst in
  let abstd_h =
    PDB.abstracted_cakes pdb_h.Pdb_ints.pdb_size ncakes in
  let abstd_d = PDB.abstracted_cakes pdb_d.Pdb_ints.pdb_size ncakes in
  let cost = cost_function_by_name ncakes cost_name in
  let phi_h n = PDB.make_phi abstd_h n
  and phi_d n = PDB.make_phi abstd_d n in
  let h_fun n = PDB.lookup pdb_h phi_h n.cakes
  and d_fun n = PDB.lookup pdb_d phi_d n.cakes in
  let hd s = h_fun s, d_fun s in
    (Search_interface.make
       ~h:h_fun
       ~d:d_fun
       ~hd:hd
       ~t:node_type
       ~domain_expand:(make_expand ncakes cost h_fun)
       ~predecessor:(make_expand ncakes cost h_fun)
       ~key:key
       ~hash:(Pancake.make_hash ncakes)
       ~key_print:int_array_to_record_string
       ~equals:Pancake.eq
       ~goal_p:is_goal
       ~halt_on:lim
       ~get_sol_length:sol_length
       ~p_update:(fun _ _ -> ())
       Search_interface.Pancake
       inst
       (fun _ _ -> false)
       (fun _ -> ()))


let exhaust_pdb pdb_h pdb_d cost_name inst lim =
  { (pdb_interface pdb_h pdb_d cost_name inst lim) with
      Search_interface.goal_p = (fun _ -> false)}


let hgap_interface cost_name inst lim =
  (** [hgap_interface cost_name inst] makes the standard interface for
      the pancake domain.  The [pdb] must be pre-loaded or created. *)
  if cost_name <> "unit"
  then invalid_arg "h_gap only works with unit costs (currently)";
  let ncakes = npancakes inst in
  let cost = cost_function_by_name ncakes cost_name in
  let h_fun s = s.this_h in
  let d_fun = h_fun in
  let hd s = h_fun s, d_fun s in
    (Search_interface.make
       ~h:h_fun
       ~d:d_fun
       ~hd:hd
       ~t:node_type
       ~domain_expand:(make_expand ncakes cost h_fun)
       ~predecessor:(make_expand ncakes cost h_fun)
       ~key:key
       ~hash:(Pancake.make_hash ncakes)
       ~key_print:int_array_to_record_string
       ~equals:Pancake.eq
       ~goal_p:is_goal
       ~halt_on:lim
       ~get_sol_length:sol_length
       ~p_update:(fun _ _ -> ())
       Search_interface.Pancake
       inst
       (fun _ _ -> false)
       (fun _ -> ()))


let exhaust_hgap cost_name inst lim =
  { (hgap_interface cost_name inst lim) with
      Search_interface.goal_p = (fun _ -> false)}


let rev_pdb_interface pdb_h pdb_d cost_name inst lim =
  (** [pdb_interface pdb_h pdb_d cost_name inst] makes the standard
      interface for the pancake domain.  The [pdb] must be pre-loaded
      or created. *)
  let ncakes = npancakes inst in
  let abstd_h =
    PDB.abstracted_cakes pdb_h.Pdb_ints.pdb_size ncakes in
  let abstd_d = PDB.abstracted_cakes pdb_d.Pdb_ints.pdb_size ncakes in
  let cost = cost_function_by_name ncakes cost_name in
  let phi_h n = PDB.make_phi abstd_h n
  and phi_d n = PDB.make_phi abstd_d n in
  let h_fun n = PDB.lookup pdb_h phi_h n.cakes
  and d_fun n = PDB.lookup pdb_d phi_d n.cakes in
  let hd s = h_fun s, d_fun s in
    (Search_interface.make
       ~h:(fun _ -> nan)
       ~d:(fun _ -> nan)
       ~hd:(fun _ -> nan,nan)
       ~rev_h:h_fun
       ~rev_d:d_fun
       ~rev_hd:hd
       ~t:node_type
       ~domain_expand:(make_expand ncakes cost h_fun)
       ~predecessor:(make_expand ncakes cost h_fun)
       ~key:key
       ~hash:(Pancake.make_hash ncakes)
       ~key_print:int_array_to_record_string
       ~equals:Pancake.eq
       ~goal_p:(make_arb_goal inst)
       ~halt_on:lim
       ~get_sol_length:sol_length
       ~p_update:(fun _ _ -> ())
       Search_interface.Pancake
       { inst with cakes = Array.of_list (Wrlist.range ~min:0 (ncakes - 1)) }
       (fun _ _ -> false)
       (fun _ -> ()))

(* EOF *)
