(**

    @author jtd7
    @since 2011-03-09

   Converts recorded run data to laso data
*)

module RR = Recorded_run
module LBR = Laso_br


let best_first_laso run =
  let optimal_nodes = RR.get_optimal_nodes run in
    List.map (fun n -> { LBR.data = n.RR.key;
			 g = n.RR.g;
			 cost = 0.;
			 depth = 0.;
			 qpos = Dpq.no_position; }) optimal_nodes


let breadth_first_laso run =
  let optimal_nodes = RR.get_optimal_nodes run in
  let nodes = Array.create (truncate run.RR.max_depth) [] in
    List.iter (fun n -> nodes.(truncate n.RR.depth) <-
		 {LBR.data = n.RR.key;
		  cost = 0.;
		  depth = 0.;
		  g = n.RR.g;
		  qpos = Dpq.no_position;}::nodes.(truncate n.RR.depth)) optimal_nodes;
    Array.to_list nodes

(* EOF *)
