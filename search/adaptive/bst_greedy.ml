(**

    @author jordan
    @since 2011-10-31
*)

open Laso_br
open Br_greedy

let dups sface args =
  let weights = Load_laso_weights.get_weight_vector sface 5 in
  let features = (match sface.Search_interface.domain with
		    | Search_interface.Dock_robot
		    | Search_interface.Heavy_vacuum
		    | Search_interface.Vacuum
		    | Search_interface.Inv_tiles
		    | Search_interface.LGrid -> make_get_standard_features sface
		    | _ -> make_get_unit_features sface) in
    call_search weights features sface


let dd sface args =
  let weights = Load_laso_weights.get_weight_vector sface 5 in
  let features = (match sface.Search_interface.domain with
		    | Search_interface.Dock_robot
		    | Search_interface.Heavy_vacuum
		    | Search_interface.Vacuum
		    | Search_interface.Inv_tiles
		    | Search_interface.LGrid -> make_get_standard_features sface
		    | _ -> make_get_unit_features sface) in
    call_search_dd weights features sface

(* EOF *)
