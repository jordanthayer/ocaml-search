(** Numeric by nominal plots.

    @author eaburns
    @since 2010-04-26
*)

open Geometry
open Drawing
open Verbosity

(** The amount of room to separate the y-axis from the data. *)
let default_y_axis_padding = Length.Pt 10.

(** The amount of room to separate the x-axis from the data. *)
let default_x_axis_padding = Length.Pt 5.

(** [data_dimensions ~y_min ~y_max datasets] computes the dimension
    of the y-axis. *)
let data_dimensions ~y_min ~y_max datasets =
  let min, max =
    List.fold_left (fun (min, max) ds ->
		      let r = ds#dimensions in
		      let ds_min = r.min and ds_max = r.max in
		      let min' = if ds_min < min then ds_min else min
		      and max' = if ds_max > max then ds_max else max
		      in min', max')
      (infinity, neg_infinity) datasets
  in
  let pad = if min = max then range_padding ~max ~min 0.01 else 0. in
  let min = match y_min with None -> (min -. pad) | Some m -> m in
  let max = match y_max with None -> (max +. pad) | Some m -> m in
  let r = range ~min ~max in
    vprintf verb_optional "data dimensions: y=[%f, %f]\n" r.min r.max;
    r


(** {1 Numeric by nomeric datasets} *)

class type dataset_type =
  object
    val name : string
    method dimensions : Geometry.range
    method draw :
      Drawing.context ->
      src:Geometry.range ->
      dst:Geometry.range -> width:float -> x:float -> unit
    method draw_x_label :
      Drawing.context ->
      x:float -> y:float -> Drawing.text_style -> width:float -> unit
    method residual :
      Drawing.context ->
      src:Geometry.range ->
      dst:Geometry.range -> width:float -> x:float -> Geometry.range
    method x_label_height :
      Drawing.context -> Drawing.text_style -> float -> float
    method n_items : int
  end

include Num_by_nom_dataset
include Boxplot_dataset
include Barchart_dataset

(** [fold_datasets ctx init f datasets] folds [f] over each dataset
    with the before padding, number of items and the dataset. *)
let fold_datasets ctx between_padding init f datasets =
  let padding = ctx.units between_padding in
  let _, _, vl =
    (List.fold_left
       (fun (first, pgroup, vl) ds ->
	  let nitems = ds#n_items in
	  let group = nitems > 1 in
	  let pad' =
	    if first then 0.
	    else
	      if pgroup || ((not pgroup) && group)
	      then padding *. 3.
	      else padding
	  in false, group, f vl pad' nitems ds)
       (true, false, init) datasets)
  in vl


(** {1 Numeric by nomeric plot} *)

class type plot_type =
  object
    val mutable height : Length.t
    val src : Geometry.range
    val mutable width : Length.t
    method display : unit
    method draw : Drawing.context -> unit
    method height : Length.t
    method output : string -> unit
    method set_size : w:Length.t -> h:Length.t -> unit
    method width : Length.t
  end

(** The line style of a horizontal line. *)
let horiz_line_style =
  {
    line_color = gray;
    line_dashes = [| |];
    line_width = Length.Pt 1.;
  }

(** [plot ?label_style ?legend_style ?tick_style ?horiz_lines ?title
    ?ylabel ?y_min ?y_max datasets] a plot that has a nominal x axis
    and a numeric y axis. *)
class plot
  ?(x_axis_padding=default_x_axis_padding)
  ?(y_axis_padding=default_y_axis_padding)
  ?(between_padding=between_padding)
  ?(label_text_style=Spt.default_label_style)
  ?(legend_text_style=Spt.default_legend_style)
  ?(tick_text_style=Spt.default_tick_style)
  ?(horiz_lines=[]) ?(seps=false)
  ?title ?ylabel ?y_min ?y_max (datasets : dataset_type list) =
  let _ = vprintf verb_optional "creating a numeric by nominal plot\n" in
object (self)
  inherit Spt.plot title

  (** The dimensions of the y-axis in data coordinates. *)
  val src = data_dimensions ~y_min ~y_max datasets

  (** [yaxis] creates a y-axis. *)
  method private yaxis =
    let nticks = Numeric_axis.recommended_ticks height in
    let ticks = Numeric_axis.tick_locations ~suggested_number:nticks src in
    verb_eval verb_debug
      (fun () ->
	vprintf verb_debug "y-ticks:\n";
	List.iter (fun (vl, name) -> match name with
	  | None -> vprintf verb_debug "\tminor: %f\n" vl
	  | Some _ -> vprintf verb_debug "\tmajor: %f\n" vl)
	  ticks);
    Numeric_axis.create ~label_text_style ~tick_text_style
      ~src ticks ylabel


  (** [x_axis_dimensions ctx] computes the x_min, x_max and
      item_width for each dataset to display its name on the
      x-axis. *)
  method private x_axis_dimensions ctx yaxis =
    let x_max, _ = self#size ctx in
    let x_min =
      Numeric_axis.resize_for_y_axis ctx
	~pad:(ctx.units Spt.text_padding) ~x_min:(ctx.units y_axis_padding)
	yaxis
    in
    let n = float (List.fold_left (fun s ds -> s + ds#n_items) 0 datasets) in
    let total_padding =
      fold_datasets ctx between_padding 0. (fun s pad _ _ -> pad +. s) datasets
    in
    let item_width =
      if n > 1.
      then ((x_max -. x_min) -. total_padding) /. n
      else (x_max -. x_min)
    in
    range x_min x_max, item_width


  (** [dst_y_range ctx ~y_min ~y_max ~item_width] get the range on
      the y-axis.  [item_width] is the amount of width afforded to a
      single item for each dataset. *)
  method private dst_y_range ctx ~y_min ~y_max ~item_width =
    let title_height =
      match title with
	| None -> 0.
	| Some txt -> snd (text_dimensions ctx ~style:tick_text_style txt) in
    let data_label_height =
      fold_datasets ctx between_padding 0.
	(fun m _ nitems ds ->
	  let width = item_width *. (float nitems) in
	  let h = ds#x_label_height ctx legend_text_style width
	  in if h > m then h else m)
	datasets
    in
    range
      ~min:((snd (self#size ctx)) -. data_label_height
	    -. (ctx.units x_axis_padding))
      ~max:(title_height +. (2. *. (ctx.units Spt.text_padding)))



  (** [draw_y_axis ctx ~dst yaxis] draws the y-axis. *)
  method private draw_y_axis ctx ~dst yaxis =
    Numeric_axis.draw_y_axis ctx ~pad:(ctx.units Spt.text_padding) ~dst yaxis


  (** [draw_x_axis ctx ~y ~xrange ~item_width] draws the x-axis. *)
  method private draw_x_axis ctx ~y ~xrange ~item_width =
    ignore
      (fold_datasets ctx between_padding xrange.min
	 (fun x pad nitems ds ->
	   let x = x +. pad in
	   let width = item_width *. (float nitems) in
	   ds#draw_x_label ctx ~x ~y legend_text_style ~width;
	   x +. width)
	 datasets)


  method draw ctx =
    vprintf verb_debug "drawing numeric by nominal plot\n";
    self#fill_background ctx;
    let yaxis = self#yaxis in
    let xrange, item_width = self#x_axis_dimensions ctx yaxis in
    let dst = self#dst_y_range ctx ~y_min ~y_max ~item_width in
    let tr = range_transform ~src ~dst in
    vprintf verb_debug
      "plot dimensions: x=[%f, %f], y=[%f, %f]\ntext width=%f\n"
      xrange.min xrange.max dst.min dst.max item_width;
    begin match title with
      | None -> ()
      | Some t ->
	(* (* this centers with respect to the whole plot
	   including the y-axis label. *)

	   let x = (fst (self#size ctx)) /. 2. and y = 0. in
	*)
	let x = (xrange.max +. xrange.min) /. 2. and y = 0. in
	draw_text_centered_below ~style:label_text_style ctx x y t
    end;
    ignore (List.iter (fun v ->
      if v >= src.min && v <= src.max
      then (draw_line ctx ~style:horiz_line_style
	      [ point xrange.min (tr v);
		point xrange.max (tr v); ]))
	      horiz_lines);
    ignore (fold_datasets ctx between_padding xrange.min
	      (fun x pad nitems ds ->
		let width = item_width *. (float nitems) in
		let x = x +. pad in
		(*
		  draw_rectangle ctx (rectangle x (x +. width)
		  dst.min dst.max);
		*)
		ds#draw ctx ~src ~dst ~width ~x;
		let sep_x = x -. pad /. 2. in
		if seps && sep_x > xrange.min then
		  draw_line ctx ~style:horiz_line_style
		    [ point sep_x dst.min;
		      point sep_x dst.max ];
		x +. width;)
	      datasets);
    self#draw_y_axis ctx ~dst yaxis;
    self#draw_x_axis ctx ~y:(dst.min +. (ctx.units x_axis_padding))
      ~xrange ~item_width
end

let plot ?x_axis_padding ?y_axis_padding
    ?between_padding ?label_text_style
    ?legend_text_style ?tick_text_style ?horiz_lines ?seps ?title ?ylabel
    ?y_min ?y_max datasets =
  new plot ?x_axis_padding ?y_axis_padding 
    ?between_padding ?label_text_style
    ?legend_text_style ?tick_text_style ?horiz_lines ?seps ?title ?ylabel
    ?y_min ?y_max datasets
