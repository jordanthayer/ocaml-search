(**
   Like a histogram, but rather than drawing a total rectangle for each bin,
   it displays just the top. Is better than standard histograms for comparing
   multiple histograms in some situations.

    @author jtd7
    @since 2010-10-29

*)

open Histogram_dataset
open Num_by_num_dataset
open Drawing
open Geometry


class histogram_dataset
  dashes ?(normalize=false) ?(line_width=Length.Pt 1.) ?(bg_color=gray)
  ?(line_color = black)
  ?name max_weight bin_min bin_max bin_width bins =
object(self)

  inherit dataset ?name ()

  val style = { line_color = line_color;
		line_dashes = dashes;
		line_width = line_width; }


  method dimensions =
    rectangle ~x_min:bin_min ~x_max:bin_max ~y_min:0. ~y_max:max_weight


  method mean_y_value _ =
    let s, n =
      Array.fold_left (fun (s, n) y -> s +. y, n + 1) (0., 0) bins
    in s /. (float n), n


  method residual ctx ~src ~dst = zero_rectangle


  method draw ctx ~src ~dst =
    let tr_pt = point_transform ~src ~dst in
    let points = ref [] in
      for i = 0 to (Array.length bins) - 2
      do
	  (let y_max = bins.(i)
	   and this_min = bin_start ~bin_min ~bin_width i
	   and this_max = bin_end ~bin_min ~bin_width i
	   and next_min = bin_start ~bin_min ~bin_width (i+1) in
	     points := ((point next_min y_max)::
			  (point this_max y_max)::
			  (point this_min y_max)::!points))
      done;
      draw_line ctx ~box:src ~tr:tr_pt ~style !points


  method draw_legend ctx ~x ~y =
    let half_length = (ctx.units line_legend_length) /. 2.
    and quarter_length = (ctx.units line_legend_length) /. 4. in
    let x_min = x -. half_length
    and x_max = x +. half_length
    and y_min = y -. quarter_length
    and y_max = y +. quarter_length in
    let mid =  y_min +. ((y_max -. y_min) /. 2.) in
      draw_line ctx ~style [point x_min mid; point x_max mid]


  method legend_dimensions ctx =
    (ctx.units line_legend_length),
  (max ((ctx.units line_legend_length) /. 4.)
     (ctx.units line_width))


  method avg_slope = nan
end

(** [values_histogram_dataset dashes ?normalize ?line_width ?bg_color
    ?bin_width ?name values] makes a histogram. *)
let values_histogram_dataset
    dashes ?normalize ?line_width ?bg_color ?line_color
    ?bin_width ?name values =
  let max_weight, bin_min, bin_max, bin_width, bins =
    make_bins ?normalize bin_width values
  in
    new histogram_dataset dashes ?normalize ?line_width ?bg_color ?line_color
      ?name max_weight bin_min bin_max bin_width bins


let histogram_dataset = values_histogram_dataset


(** [points_histogram_dataset dashes ?normalize ?line_width
    ?bg_color ?bin_width ?name points] makes a histogram given an
    array of points. *)
let points_histogram_dataset
    dashes ?normalize ?line_width ?line_color ?bg_color ?bin_width
    ?name points =
  let max_weight, bin_min, bin_max, bin_width, bins =
    bins_of_points ?normalize bin_width points
  in
    new histogram_dataset dashes ?normalize ?line_width ?bg_color ?line_color
      ?name max_weight bin_min bin_max bin_width bins

(* EOF *)
