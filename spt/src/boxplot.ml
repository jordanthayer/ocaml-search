(** Creating and drawing boxplots.

    @author eaburns
    @since 2010-06-09
*)

open Drawing
open Geometry

type t = {
  stats : stats;
  outliers : float array;
  color : Drawing.color;
  glyph : Drawing.glyph;
  point_radius : Length.t;
}

and stats = {
  q1 : float;
  q2 : float;
  q3 : float;
  upper_extreme : float;
  lower_extreme : float;
  mean : float;
  conf_upper : float;
  conf_lower : float;
}

let box_line_style = { default_line_style with line_width = Length.Pt 0.5 }

let minf a b = if (a:float) < b then a else b
let maxf a b = if (a:float) > b then a else b

(** [separate_outliers ~lower ~upper vls] separates the outliers
    that are outside the upper and lower fence values. *)
let separate_outliers ~lower ~upper vls =
  let out_lst, lower_extreme, upper_extreme =
    Array.fold_left
      (fun (os, l, u) v ->
	 if v > upper || v < lower
	 then v :: os, l, u
	 else begin
	   let u' = if v > u then v else u
	   and l' = if v < l then v else l
	   in os, l', u'
	 end)
      ([], infinity, neg_infinity) vls
  in Array.of_list out_lst, lower_extreme, upper_extreme


(** [create ?outliers ?color ?glyph ?point_radius values] makes a
    boxplot for the given values.

    Computed according to Tukey, "Exploratory Data Analysis" with
    the addition of 95% confidence intervals. *)
let create ?(outliers=true) ?(color=black) ?(glyph=Ring_glyph)
    ?(point_radius=Length.Pt 2.) values =
  let mean, conf_interval = Statistics.mean_and_interval values in
  let q1 = Statistics.percentile 25. values in
  let q3 = Statistics.percentile 75. values in
  let outliers, lower, upper =
    if outliers then
      let step = 1.5 *. (q3 -. q1) in
	separate_outliers ~lower:(q1 -. step) ~upper:(q3 +. step) values
    else
      let min, max = Statistics.min_and_max (fun x -> x) values in
	[||], min, max
  in
    {
      stats = {
	q1 = q1;
	q2 = Statistics.percentile 50. values;
	q3 = q3;
	upper_extreme = upper;
	lower_extreme = lower;
	mean = mean;
	conf_upper = mean +. conf_interval;
	conf_lower = mean -. conf_interval;
      };
      outliers = outliers;
      color = color;
      glyph = glyph;
      point_radius = point_radius;
    }


(** [draw_median_line cxt style src tr ~x0 ~x1 ~median] draws the
    median line if it is not clipped.  *)
let draw_median_line ctx style src tr ~x0 ~x1 ~median =
  let median' = tr median in
    if median <= src.max && median >= src.min
    then draw_line ctx ~style [ point x0 median'; point x1 median' ]


(** [draw_box ctx style src ~x0 ~x1 ~q1 ~q3] draws the boxplot box
    with clipping. *)
let draw_box ctx style src tr ~x0 ~x1 ~q1 ~q3 =
  let box = rectangle neg_infinity infinity src.min src.max in
  let tr p = point p.x (tr p.y) in
    draw_line ctx ~box ~tr ~style
      [ point x0 q3; point x1 q3; point x1 q1; point x0 q1; point x0 q3; ]


(** [fill_ci_box ctx color src tr ~x0 ~x1 ~lower ~upper] fills in
    the confidence interval box if it is not clipped. *)
let fill_ci_box ctx color src tr ~x0 ~x1 ~lower ~upper =
  if lower <= src.max && upper > src.min
  then begin
    let y_min = if lower < src.min then tr src.min else tr lower in
    let y_max = if upper > src.max then tr src.max else tr upper in
    let r = rectangle ~x_min:x0 ~x_max:x1 ~y_min ~y_max in
      fill_rectangle ctx ~color r
  end


(** [dimensions box] computes the dimensions of the box. *)
let dimensions box =
  let vs =
    Array.append
      [| box.stats.upper_extreme; box.stats.lower_extreme; |]
      box.outliers
  in
    range
      ~min:(Array.fold_left minf infinity vs)
      ~max:(Array.fold_left maxf neg_infinity vs)


(** [residual ctx ~src ~dst ~width ~x box] computes the residual. *)
let residual ctx ~src ~dst ~width ~x box =
  let r = ctx.units box.point_radius in
  let tr = range_transform ~src ~dst in
    Array.fold_left
      (fun res v ->
	 if v >= src.min && v <= src.max
	 then
	   let v' = tr v in
	   let max = if v' +. r > dst.max then (v' +. r) -. dst.max else 0.
	   and min = if v' -. r < dst.min then dst.min -. (v' -. r) else 0.
	   in range_max (range min max) res
	 else res)
      (range 0. 0.) box.outliers


(** [draw ctx ?interval ~src ~dst ~width ~x box] draws the boxplot
	centered at [x] with the given [width].  [src] and [dst] are the
	source and destination ranges for the y-axis. *)
let draw ctx ?(interval=true) ~src ~dst ~width ~x box =
  let tr = range_transform ~src ~dst in
  let lwidth = ctx.units box_line_style.line_width in
  let x0 = x -. (width /. 2.) +. lwidth
  and x1 = x +. (width /. 2.) -. lwidth in
  let color = box.color and radius = box.point_radius and glyph = box.glyph in
  let outliers = box.outliers in
  let q1 = box.stats.q1 and q3 = box.stats.q3 in
  let lower = box.stats.lower_extreme and upper = box.stats.upper_extreme in
    for i = 0 to (Array.length box.outliers) - 1 do
      let y = outliers.(i) in
	if y <= src.max && y >= src.min
	then draw_point ctx ~color radius glyph (point x (tr y));
    done;
    if interval then begin
      fill_ci_box ctx Drawing.gray src tr ~x0:(x -. (width /. 16.))
        ~x1:(x +. (width /. 16.)) ~lower:box.stats.conf_lower
        ~upper:box.stats.conf_upper
    end;
    draw_median_line ctx box_line_style src tr ~x0 ~x1 ~median:box.stats.q2;
    draw_box ctx box_line_style src tr ~x0 ~x1
      ~q1:box.stats.q1 ~q3:box.stats.q3;
    Errbar.draw_up ctx ~style:box_line_style ?cap_size:None
      ~src ~dst ~x ~y:q3 ~mag:(upper -. q3);
    Errbar.draw_down ctx ~style:box_line_style ?cap_size:None
      ~src ~dst ~x ~y:q1 ~mag:(q1 -. lower)
