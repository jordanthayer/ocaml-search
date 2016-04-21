(**

    @author jtd7
    @since 2010-10-21
*)

type maxes = float array
let h = 0
and d = 1
and g = 2
and c = 3
and depth = 3

let weighted_norm weight i_h i_d =
  [| weight *. i_h; weight *. i_d; weight *. i_h; 1.|]

let wd_weighted_norm weight i_h i_d =
  [| weight *. i_h; weight *. i_d; weight *. i_h; weight *. i_d; 1.|]

let weighted_norm_from_sface sface weight =
  let ih,id = sface.Search_interface.hd sface.Search_interface.initial in
    weighted_norm weight ih id

let wd_norm_from_sface sface weight =
  let ih,id = sface.Search_interface.hd sface.Search_interface.initial in
    wd_weighted_norm weight ih id


let get_maxes = function
    (** Currently this is just a hack for exactly those domains I'm running
	on in this paper.  In the future we should probably do this in a
	principled way *)
  | Search_interface.UGrid -> [| 3000.; 3000.; 10000.; 1.|]
  | Search_interface.LGrid -> [| 4002000.; 3000.; 3000000.; 1.|]
  | Search_interface.Tiles -> [| 48.; 48.; 60.; 1.|]
  | Search_interface.Sequence -> [| nan; nan; nan; 1.|]
  | Search_interface.Salesman -> [| 100.; 100.; 100.; 1.|]
  | Search_interface.TPlan -> [| nan; nan; nan; 1.|]
  | Search_interface.OPlan -> [| nan; nan; nan; 1.|]
  | Search_interface.Robot -> [| 1000. /. 300.; 750.; 500.; 1.|]
  | Search_interface.Anticipation -> [| nan; nan; nan; 1.|]
  | Search_interface.Vacuum -> [| 1000.; 1000.; 1000.; 1.|]
  | Search_interface.Vacuum_maze -> [| nan; nan; nan; 1.|]
  | Search_interface.Synthetic -> [| nan; nan; nan; 1.|]
  | Search_interface.Rucksack -> [| nan; nan; nan; 1.|]
  | Search_interface.Pancake -> [| nan; nan; nan; 1.|]
  | Search_interface.Logistics -> [| nan; nan; nan; 1.|]
  | Search_interface.Sokoban -> [| nan; nan; nan; 1.|]
  | Search_interface.Vis_nav -> [| nan; nan; nan; 1.|]
  | Search_interface.Dock_robot -> [| nan; nan; nan; 1.|]
  | Search_interface.Openstacks -> [| nan; nan; nan; 1.|]
  | _ -> failwith "I don't know how to normalize that domain"




(* EOF *)
