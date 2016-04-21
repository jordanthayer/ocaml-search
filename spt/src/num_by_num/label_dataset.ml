(** A label dataset allows arbitrary lables to be placed on the plot.

    @author eaburns
    @since 2010-04-29
*)

open Num_by_num_dataset
open Drawing
open Geometry

(** The default style for labels. *)
let default_style =
  {
    text_font = "sans-serif";
    text_size = Length.Pt 8.;
    text_slant = font_slant_normal;
    text_weight = font_weight_normal;
    text_color = black;
  }


type label_x_location =
  | Label_xat
  | Label_before
  | Label_after


type label_y_location =
  | Label_yat
  | Label_above
  | Label_below


(** Add labels where the points are given data-coordinates.  [xoff]
    and [yoff] are plot-coordinate offsets to apply to the label
    before drawing it at its given location.  This can be used to
    place the label outside of the radius of a scatter point. *)
class label_dataset
  ?(style=default_style)
  ?(xloc=Label_xat) ?(yloc=Label_yat)
  ?(xoff=(Length.Pt 0.)) ?(yoff=(Length.Pt 0.))
  ?name label_points =
object (self)

  inherit dataset ?name ()

  method dimensions =
    let pts = Array.map fst label_points in
      points_rectangle pts


  method mean_y_value _ = nan, 0


  (** [position ctx pt' txt] position the label in the plot
      coordinate system. *)
  method private position ctx pt' txt =
    let x = pt'.x +. (ctx.units xoff) and y = pt'.y +. (ctx.units yoff) in
    let w, h = text_dimensions ctx ~style txt in
    let x' = match xloc with
      | Label_xat -> x
      | Label_before -> x -. (w /. 2.)
      | Label_after -> x +. (w /. 2.)
    and y' = match yloc with
      | Label_yat -> y
      | Label_above -> y -. (h /. 2.)
      | Label_below -> y +. (h /. 2.)
    in point x' y'


  method residual ctx ~src ~dst =
    let tr = point_transform ~src ~dst in
      Array.fold_left
	(fun r (pt, txt) ->
	   if rectangle_contains src pt
	   then begin
	     let pt' = self#position ctx (tr pt) txt in
	     let dims = text_rectangle ctx ~style ~pt:pt' txt in
	     let residue = rectangle_residual dst dims
	     in { (rectangle_max r residue) with x_max = 0. }
	   end else r)
	zero_rectangle label_points


  method x_over ctx ~src ~dst =
    let tr = point_transform ~src ~dst in
      Array.fold_left
	(fun lst (pt, txt) ->
	   let pt' = tr pt in
	   let pos = self#position ctx pt' txt in
	   let dims = text_rectangle ctx ~style ~pt:pos txt in
	     if rectangle_contains src pt && dims.x_max > dst.x_max then begin
	       let w, _ = text_dimensions ctx ~style txt in
	       let x, tgt =
		 match xloc with
		   | Label_xat ->
		       pt.x, dst.x_max -. w /. 2.
		   | Label_before ->
		       pt.x, dst.x_max
		   | Label_after ->
		       pt.x, dst.x_max -. w
	       in
		 (x, tgt) :: lst
	     end else
	       lst)
	[] label_points


  method draw ctx ~src ~dst =
    let tr = point_transform ~src ~dst in
      Array.iter
	(fun (pt, txt) ->
	   if rectangle_contains src pt then
	     let pt' = self#position ctx (tr pt) txt in
	       draw_text ctx ~style ~x:pt'.x ~y:pt'.y txt)
	label_points


  method draw_legend _ ~x:_ ~y:_ = ()

  method legend_dimensions _ = 0., 0.

  method avg_slope = nan

end

(** [label_dataset ?text_style ?xloc ?yloc ?xoff ?yoff ?name
    lbl_points] makes a new label dataset. *)
let label_dataset ?text_style ?xloc ?yloc ?xoff ?yoff ?name lbl_points =
  new label_dataset ?style:text_style ?xloc ?yloc ?xoff ?yoff ?name lbl_points
