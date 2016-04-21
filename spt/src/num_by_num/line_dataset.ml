(** Lines and lines with errorbars.

    @author eaburns
    @since 2010-04-28
*)

open Num_by_num_dataset
open Drawing
open Geometry

(** The length of the line drawn in the legend. *)
let line_legend_length = Length.Cm 0.75


(** A line plot dataset. *)
class line_dataset
  dashes ?(line_width=Length.Pt 0.5) ?(color=black) ?name points =

object (self)

  inherit points_dataset ?name points

  val style =
    {
      line_color = color;
      line_dashes = dashes;
      line_width = line_width;
    }

  method draw ctx ~src ~dst =
    let tr = point_transform ~src ~dst in
      (*
	let pts = ref [] in
	for i = (Array.length points) - 1 downto 0 do
	pts := (tr points.(i)) :: !pts
	done;
      *)
      draw_line ctx ~box:src ~tr ~style (Array.to_list points)


  method draw_legend ctx ~x ~y =
    let half_length = (ctx.units line_legend_length) /. 2. in
    let x0 = x -. half_length and x1 = x +. half_length in
      draw_line ctx ~style [ point x0 y; point x1 y]


  method legend_dimensions ctx =
    (ctx.units line_legend_length), (ctx.units line_width)

  method avg_slope =
    let pts = points in
      if Array.length pts < 2 then nan
      else
	(let accum = ref 0.
	 and count = (Array.length pts - 2) in
	   for i = 0 to count
	   do
	     (let pt1 = pts.(i)
	      and pt2 = pts.(i+1) in
	      let dy = pt2.y -. pt1.y
	      and dx = pt2.x -. pt1.x in
		accum := !accum +. (abs_float (dy /. dx)))
	   done;
	   !accum /. (float count))

end

let default_radius =
  Length.Pt ((Length.as_pt Scatter_dataset.default_radius) /. 2.)


(** [line_points_dataset dashes glyph ?point_radius ?line_width
    ?color ?name points] makes a dataset that is a line with glyphs
    at each point. *)
let line_points_dataset dashes glyph ?(point_radius=default_radius)
    ?line_width ?color ?name points =
  new composite_dataset ?name
    [new line_dataset dashes ?line_width ?color ?name points;
     new Scatter_dataset.scatter_dataset glyph ~point_radius
       ?color ?name points;]


let line_dataset dashes ?line_width ?color ?name points =
  new line_dataset dashes ?line_width ?color ?name points


let line_datasets ?(color=false) ?line_width name_by_points_list =
  let next_dash = Factories.default_dash_factory () in
  let next_color = (if color
		    then Factories.default_color_factory ()
		    else (fun () -> black))
  in
    List.map (fun (name, points) ->
		line_dataset (next_dash ()) ?line_width ~color:(next_color())
		  ?name points)
      name_by_points_list


let line_points_datasets ?(color=false) name_by_points_list =
  let next_dash = Factories.default_dash_factory () in
  let next_glyph = Factories.default_glyph_factory () in
  let next_color = (if color
		    then Factories.default_color_factory ()
		    else (fun () -> black))
  in
    List.map (fun (name, points) ->
		line_points_dataset ~color:(next_color ()) (next_dash ())
		  (next_glyph()) ?name points)
      name_by_points_list


(** {2 Y Error bars} *)

(* This is like a lines points plot except that all points with the
   same x value are binned, averaged and drawn with error bars. *)

let y_errbar_dataset dashes glyph ?color ?line_width ?point_radius ?name pts =
  let bins = Geometry.bin_points pts in
  let means, confs =
    List.fold_left (fun (ms, cs) (x, ys) ->
		      if (Array.length ys) = 1 then
			(point x (Statistics.mean ys)) :: ms, cs
		      else begin
			let m, c = Statistics.mean_and_interval ys in
			  (point x m) :: ms, (triple x m c) :: cs
		      end)
      ([],[]) bins
  in
  let means = Array.of_list means in
  let line =
    line_points_dataset dashes glyph ?color ?line_width ?point_radius ?name means in
  let confs = Array.of_list confs in
  let bars = new Errbar_dataset.vertical_errbar_dataset ?color confs in
    new composite_dataset ?name [ line; bars; ]


(* EOF *)
