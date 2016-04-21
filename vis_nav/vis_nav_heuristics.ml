(** Heiristics for visibility based navigation *)

open Vis_nav_instance


let h0 _ =
  (** standard h0 function *)
  0.


let make_euclidian problem =
  (** Calculates the cost of moving in a straight line from current
      location to goal, ignoring obstacles. *)
  (fun node ->
     let gx = problem.goal.x
     and gy = problem.goal.y
     and x = node.location.x
     and y = node.location.y in
       sqrt (((x-.gx)**2.) +. ((y-.gy)**2.)))

(* EOF *)
