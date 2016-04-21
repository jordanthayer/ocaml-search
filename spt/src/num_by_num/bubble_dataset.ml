(** A bubble plot dataset.


    @author eaburns
    @since 2010-04-28
*)

open Num_by_num_dataset
open Geometry
open Drawing

(** For plotting data with three values: x, y and z.  The result plots
    points at their x, y location as a scatter plot would however
    the z values are shown by changing the radius of the point. *)
class bubble_dataset
  ?(glyph=Circle_glyph) ?(color=gray)
  ?(min_radius=(Length.Pt 10.)) ?(max_radius=(Length.Pt 60.)) ?name triples =
object (self)
  inherit dataset ?name ()

  val triples = (triples : triple array)


  method dimensions =
    let pts = Array.map (fun t -> point t.i t.j) triples in
      points_rectangle pts

  method mean_y_value _ =
    let s, n =
      Array.fold_left (fun (s, n) t -> s +. t.j, n + 1) (0., 0) triples
    in s /. (float n), n


  (** [z_range] is the minimum and maximum z value of all triples.
      This is used for determining the radius of a point. *)
  method private z_range =
    let min, max =
      Array.fold_left (fun (min, max) t ->
			 let z = t.k in
			 let min' = if z < min then z else min
			 and max' = if z > max then z else max
			 in min', max')
	(infinity, neg_infinity) triples
    in range ~min ~max


  (** [compute_radius ctx zrange vl] gets the radius of the point. *)
  method private radius ctx zrange vl =
    let rrange = range (Length.as_pt min_radius) (Length.as_pt max_radius) in
      Length.Pt (range_transform ~src:zrange ~dst:rrange vl)


  method residual ctx ~src ~dst =
    let tr = point_transform ~src ~dst in
    let zrange = self#z_range in
      Array.fold_left
	(fun r t ->
	   let pt' = tr (point t.i t.j) in
	     if rectangle_contains dst pt'
	     then begin
	       let radius = self#radius ctx zrange t.k
	       in rectangle_max r (point_residual dst pt' (ctx.units radius))
	     end else r)
	zero_rectangle triples


  method draw ctx ~src ~dst =
    let tr = point_transform ~src ~dst in
    let zrange = self#z_range in
      Array.iter (fun t ->
		    let radius = self#radius ctx zrange t.k in
		    let pt = point t.i t.j in
		    let pt' = tr pt in
		      if rectangle_contains src pt
		      then draw_point ctx ~color radius glyph pt')
	triples


  method draw_legend ctx ~x ~y =
    draw_point ctx ~color (Length.Pt ((Length.as_pt min_radius) /. 2.))
      glyph (point x y)


  method legend_dimensions ctx =
    let r2 = ctx.units min_radius in r2, r2

  method avg_slope = rectangle_to_slope self#dimensions

end

let bubble_dataset ?glyph ?color ?min_radius ?max_radius ?name triples =
  new bubble_dataset ?glyph ?color ?min_radius ?max_radius ?name triples


let bubble_datasets ?(color = false) ?min_radius ?max_radius
    name_by_triples_list =
  let next_glyph = Factories.default_glyph_factory () in
  let next_color = (if color
		    then Factories.default_color_factory ()
		    else (fun () -> black))
  in
    List.map (fun (name,triples) ->
		(bubble_dataset ~color:(next_color ()) ~glyph:(next_glyph())
		   ?min_radius ?max_radius ?name triples))
      name_by_triples_list

(* EOF *)
