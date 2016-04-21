(** A plot type for drawing a tree.

    @author eaburns
    @since 2010-06-23
*)

open Geometry
open Drawing
open Printf

type node = {
  color : color;
  succs : node array;
}

(** [max_depth ?depth n] gets the max depth of the tree. *)
let rec max_depth ?(depth=0) n =
  (Array.fold_left
     (fun m n' -> max (max_depth ~depth:(depth+1) n') m)
     0 n.succs) + 1


(** {1 Tree vis plot} *)

class type plot_type =
object
  val mutable height : Length.t
  val mutable width : Length.t
  method display : unit
  method draw : Drawing.context -> unit
  method height : Length.t
  method output : string -> unit
  method set_size : w:Length.t -> h:Length.t -> unit
  method width : Length.t
  method draw : Drawing.context -> unit
end

class plot ?title draw_style root =
object

  inherit Spt.plot title

  method draw ctx =
    let w = ctx.units width and h = ctx.units height in
      draw_style ctx ~width:w ~height:h root

end

(** [plot ?title draw_style tree] creates a new tree visualization
    plot with the given tree dataset. *)
let plot ?title draw_style root = new plot ?title draw_style root


(** {1 Drawing sunbursts} *)

module Sunburst = struct

  let line_style =
    { default_line_style with line_width = Length.Pt 0.5 }

  (** [draw_tree ctx outline center ?depth ~r ~dr ~t ~dt node] draws
      a subtree in the pi-wedge between angles [t] and [t + dt].
      The root node ends at radius [r] and subsequent nodes are [dr]
      wider than the root.  This draws "pie-slices" and uses the
      painter's algorithm to make each node appear as a sector of a
      circle. *)
  let rec draw_tree ctx outline center ?(depth=0) ~r ~dr ~t ~dt node =
    let nsuccs = float (Array.length node.succs) in
    let dt' = dt /. nsuccs and r' = r +. dr and depth' = depth + 1 in
      ignore (Array.fold_left
		(fun t s ->
		   draw_tree ctx outline center ~depth:depth'
		     ~r:r' ~dr ~t ~dt:dt' s;
		   t +. dt')
		t node.succs);
      let dt = dt +. (epsilon_float *. dt) in
      let dr = dr +. (epsilon_float *. dr) in
	(* extra padding attempts to get rid of space-artifacts
	   between sectors. *)
      if outline
      then draw_sector ctx ~style:line_style center ~r ~dr ~t ~dt black;
      fill_sector ctx center ~r ~dr ~t ~dt node.color


  let make_style outline ctx ~width ~height root =
    let cx = width /. 2. and cy = height /. 2. in
    let center = point cx cy in
    let max_radius = min cx cy in
    let dr = max_radius /. (float (max_depth root)) in
      draw_tree ctx outline center ~r:0. ~dr ~t:0. ~dt:two_pi root


  (** [default_style ctx ~width ~height root] the default sunburst
      tree style. *)
  let default_style ctx ~width ~height root =
    make_style false ctx ~width ~height root

  (** [outlined_style ctx ~width ~height root] a sunburst tree style
      where each node is outlined in black. *)
  let outlined_style ctx ~width ~height root =
    make_style true ctx ~width ~height root

end
