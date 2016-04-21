(** Algorithms which are useful for creating static eps visualizations,
    typically going to be run from the toplevel. *)

let grid_vis_interface_problem file =
  let prob = Grid_instance.load file in
    Grid_vis_tool.vis_interface prob [Limit.Never],prob

let grid_vis_count_interface_problem file =
  let prob = Grid_instance.load file in
    Grid_vis_tool.vis_count_interface prob [Limit.Never],prob


let grid_vis_def_interface_problem file =
  let prob = Grid_instance.load file in
    Grid_vis_tool.def_interface prob [Limit.Never],prob


(* EOF *)
