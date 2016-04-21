(**

    @author jtd7
    @since 2011-03-16
*)

open Laso_br
(*
let ordered_p a b =
  a.cost < b.cost ||
    ((a.cost = b.cost) && a.g < b.g)
*)

let make_sface weights get_features sface =
  let def_log = (Limit.make_default_logger (fun n -> n.g)
		   (fun n -> truncate n.depth)) in
    Search_interface.make
      ~node_expand:(make_expand ~get_features ~weights
		  sface.Search_interface.domain_expand)
      ~goal_p:(wrap sface.Search_interface.goal_p)
      ~key:(wrap sface.Search_interface.key)
      ~hash:sface.Search_interface.hash
      ~equals:sface.Search_interface.equals
      ~halt_on:sface.Search_interface.halt_on
      sface.Search_interface.domain
      { data = sface.Search_interface.initial;
	g = 0.; cost = 0.; depth = 0.; qpos = Dpq.no_position;}

      better_p
      (fun i ->
	 sface.Search_interface.info.Limit.log
	   (Limit.unwrap_info (fun n -> n.data) i);
	 def_log i)


let call_search weights get_features sface =
  let sface = make_sface weights get_features sface in
    Limit.unwrap_sol6 unwrap_sol
      (Best_first.search_dups sface ordered_p better_p setpos getpos)


let call_search_dd weights get_features sface =
  let sface = make_sface weights get_features sface in
    Limit.unwrap_sol6 unwrap_sol
      (Best_first.search_drop_dups sface ordered_p better_p setpos getpos)


let dups sface args =
  let weights = Load_laso_weights.get_weight_vector sface 100 in
  let features = (match sface.Search_interface.domain with
		    | Search_interface.Dock_robot
		    | Search_interface.Heavy_vacuum
		    | Search_interface.Vacuum
		    | Search_interface.Inv_tiles
		    | Search_interface.LGrid -> make_get_standard_features sface
		    | _ -> make_get_unit_features sface) in
    call_search weights features sface


let dd sface args =
  let weights = Load_laso_weights.get_weight_vector sface 100 in
  let features = (match sface.Search_interface.domain with
		    | Search_interface.Dock_robot
		    | Search_interface.Heavy_vacuum
		    | Search_interface.Vacuum
		    | Search_interface.Inv_tiles
		    | Search_interface.LGrid -> make_get_standard_features sface
		    | _ -> make_get_unit_features sface) in
    call_search_dd weights features sface

(* EOF *)

