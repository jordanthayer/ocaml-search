(** Drawing of error bars.

    @author eaburns
    @since 2010-04-28
*)

open Geometry
open Drawing


(** The line style for an error bar. *)
let errbar_line_style =
  {
    line_color = black;
    line_width = Length.Pt 0.5;
    line_dashes = [| |];
  }

(** The size of the cap on an error bar. *)
let default_cap_size = Length.Pt 2.


(** [residual_vert ctx up ?cap_size ~src_y ~dst_x ~x ~y ~mag]
    computes the residual for a vertical error bar.  The [x]
    coordinate is in the destination coordinate system and the [y]
    coordinate is in the data coordinate system.  [src_y] and
    [dst_x] are the range of the source and destination y-axis and
    x-axis values respectively.  This assumes that the center point
    for the error bar is within the destination rectangle. *)
let residual_vert ctx up ?(cap_size=default_cap_size)
    ~src_y ~dst_x ~x ~y ~mag =
  let errbar_cap_size = ctx.units cap_size in
  let y1 = if up then y +. mag else y -. mag in
  let clip =
    if up then
      sloppy_float_less src_y.max y1
    else
      sloppy_float_greater src_y.min y1
  in
    if clip then
      zero_rectangle
    else begin
      let x0 = x -. errbar_cap_size and x1 = x +. errbar_cap_size in
      let res_min = if x0 < dst_x.min then dst_x.min -. x0 else 0.
      and res_max = if x1 > dst_x.max then x1 -. dst_x.max else 0. in
	{ zero_rectangle with x_min = res_min; x_max = res_max }
    end


(** [residual_horiz ctx left ?cap_size ~src_x ~dst_y ~x ~y ~mag]
    computes the residual for a horizontal error bar.  The [x]
    coordinate is in the destination coordinate system and the [y]
    coordinate is in the data coordinate system.  [src_x] and
    [dst_y] are the range of the source and destination x-axis and
    y-axis values respectively.  This assumes that the center point
    for the error bar is within the destination rectangle. *)
let residual_horiz ctx left ?(cap_size=default_cap_size)
    ~src_x ~dst_y ~x ~y ~mag =
  let errbar_cap_size = ctx.units cap_size in
  let x1 = if left then x -. mag else x +. mag in
  let clip =
    if left
    then sloppy_float_greater src_x.min x1
    else sloppy_float_less src_x.max x1
  in
    if clip
    then zero_rectangle
    else begin
      let y0 = y -. errbar_cap_size and y1 = y +. errbar_cap_size in
      let y_min = if y0 > dst_y.min then y0 -. dst_y.min else 0.
      and y_max = if y1 < dst_y.max then dst_y.max -. y1 else 0. in
	{ zero_rectangle with y_min = y_min; y_max = y_max }
    end


(** [draw_up ctx ?style ?cap_size ~src ~dst ~x ~y ~mag] draws the
    top half of a vertical error bar.  The [x] coordinate is in the
    destination coordinate system and the [y] coordinate is in the
    data coordinate system.  [src] and [dst] are the range of the
    source and destination y-axis values.  This assumes that the
    center point for the error bar is within the destination
    rectangle. *)
let draw_up ctx ?(style=errbar_line_style) ?(cap_size=default_cap_size)
    ~src ~dst ~x ~y ~mag =
  let errbar_cap_size = ctx.units cap_size in
  let tr = range_transform ~src ~dst in
  let y1 = y +. mag in
    if y <= src.max && y1 >= src.min
    then begin
      let y0' = if y < src.min then tr src.min else tr y in
      let y1' = if y1 > src.max then tr src.max else tr y1 in
	draw_line ctx ~style [point x y0'; point x y1'];
	if Geometry.sloppy_float_leq y1 src.max
	then draw_line ctx ~style [ point (x -. errbar_cap_size) y1';
				    point (x +. errbar_cap_size) y1' ]
    end


(** [draw_down ctx ?style ?cap_size ~src ~dst ~x ~y ~mag] draws the
    bottom half of a vertical error bar.  The [x] coordinate is in
    the destination coordinate system and the [y] coordinate is in
    the data coordinate system.  [src] and [dst] are the range of
    the source and destination y-axis values.  This assumes that the
    center point for the error bar is within the destination
    rectangle. *)
let draw_down ctx ?(style=errbar_line_style) ?(cap_size=default_cap_size)
    ~src ~dst ~x ~y ~mag =
  let errbar_cap_size = ctx.units cap_size in
  let tr = range_transform ~src ~dst in
  let y1 = y -. mag in
    if y >= src.min && y1 < src.max
    then begin
      let y0' = if y > src.max then tr src.max else tr y  in
      let y1' = if y1 < src.min then tr src.min else tr y1 in
	draw_line ctx ~style [point x y0'; point x y1'];
	if Geometry.sloppy_float_geq y1 src.min
	then draw_line ctx ~style [ point (x -. errbar_cap_size) y1';
				    point (x +. errbar_cap_size) y1' ]
    end


(** [draw_left ctx ?style ?cap_size ~src ~dst ~x ~y ~mag] draws the
    top half of a vertical error bar.  The [y] coordinate is in the
    destination coordinate system and the [x] coordinate is in the
    data coordinate system.  [src] and [dst] are the range of the
    source and destination x-axis values.  This assumes that the
    center point for the error bar is within the destination
    rectangle. *)
let draw_left ctx ?(style=errbar_line_style) ?(cap_size=default_cap_size)
    ~src ~dst ~x ~y ~mag =
  let errbar_cap_size = ctx.units cap_size in
  let tr = range_transform ~src ~dst in
  let x1 = x -. mag in
    if x >= src.min && x1 <= src.max
    then begin
      let x0' = if x > src.max then tr src.max else tr x in
      let x1' = if x1 < src.min then tr src.min else tr x1 in
	draw_line ctx ~style [point x0' y; point x1' y];
	if Geometry.sloppy_float_geq x1 src.min
	then draw_line ctx ~style [ point x1' (y -. errbar_cap_size);
				    point x1' (y +. errbar_cap_size) ]
    end


(** [draw_right ctx ?style ?cap_size ~src ~dst ~x ~y ~mag] draws the
    top half of a vertical error bar.  The [y] coordinate is in the
    destination coordinate system and the [x] coordinate is in the
    data coordinate system.  [src] and [dst] are the range of the
    source and destination x-axis values.  This assumes that the
    center point for the error bar is within the destination
    rectangle. *)
let draw_right ctx ?(style=errbar_line_style) ?(cap_size=default_cap_size)
    ~src ~dst ~x ~y ~mag =
  let errbar_cap_size = ctx.units cap_size in
  let tr = range_transform ~src ~dst in
  let x1 = x +. mag in
    if x <= src.max && x1 >= src.min
    then begin
      let x0' = if x < src.min then tr src.min else tr x in
      let x1' = if x1 > src.max then tr src.max else tr x1 in
	draw_line ctx ~style [point x0' y; point x1' y];
	if Geometry.sloppy_float_leq x1 src.max
	then draw_line ctx ~style [ point x1' (y -. errbar_cap_size);
				    point x1' (y +. errbar_cap_size) ]
    end
