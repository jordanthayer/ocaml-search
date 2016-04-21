(**

    @author jtd7
    @since 2011-03-08
*)

open Laso_br

let call_search weights get_features beam_width sface =
  let key = wrap sface.Search_interface.key
  and goal = wrap sface.Search_interface.goal_p
  and root = { data = sface.Search_interface.initial;
	       g = 0.; cost = 0.; depth = 0.; qpos = Dpq.no_position;}
  and expand = (make_expand ~get_features ~weights
		  sface.Search_interface.domain_expand)
  and info = (Limit.make Limit.Nothing sface.Search_interface.halt_on better_p
		(Limit.make_default_logger (fun n -> n.g)
		   (fun n -> truncate n.depth))) in
    Generic_beam.breadth_first_dups ~dd:true info beam_width root goal expand
      ordered_p better_p sface.Search_interface.hash
      sface.Search_interface.equals key setpos getpos;
    Limit.unwrap_sol6 unwrap_sol (Limit.results6 info)


let do_search sface args =
  let beam_width = Search_args.get_int "Laso_bst" args 0 in
  let weights = Load_laso_weights.get_weight_vector sface beam_width in
  let features = (match sface.Search_interface.domain with
		    | Search_interface.Dock_robot
		    | Search_interface.Heavy_vacuum
		    | Search_interface.Vacuum
		    | Search_interface.Inv_tiles
		    | Search_interface.LGrid -> make_get_standard_features sface
		    | _ -> make_get_unit_features sface) in
    call_search weights features beam_width sface


(* EOF *)
