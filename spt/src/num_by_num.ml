(** Numeric by numeric plots.

    @author eaburns
    @since 2010-04-25
*)

open Geometry
open Drawing
open Verbosity

(** The amount of room to separate the axes from the data. *)
let default_axis_padding = Length.Pt 5.


(** [data_dimensions ~x_min ~x_max ~y_min ~y_max datasets] computes
    the range of the x and y axes for the given datasets. *)
let data_dimensions ~x_min ~x_max ~y_min ~y_max datasets =
  let r = match datasets with
    | d :: [] -> d#dimensions
    | d :: ds ->
	List.fold_left (fun r d -> rectangle_extremes r d#dimensions)
	  d#dimensions ds
    | [] ->
	rectangle ~x_min:infinity ~x_max:neg_infinity
	  ~y_min:infinity ~y_max:neg_infinity
  in
  let x_pad =
    if r.x_max = r.x_min
    then range_padding ~max:r.x_max ~min:r.x_min 0.01
    else 0. in
  let y_pad =
    if r.y_max = r.y_min
    then range_padding ~max:r.y_max ~min:r.y_min 0.01
    else 0. in
  let x_min' = match x_min with None -> (r.x_min -. x_pad) | Some m -> m
  and x_max' = match x_max with None -> (r.x_max +. x_pad) | Some m -> m
  and y_min' = match y_min with None -> (r.y_min -. y_pad) | Some m -> m
  and y_max' = match y_max with None -> (r.y_max +. y_pad) | Some m -> m in
  let r = rectangle ~x_min:x_min' ~x_max:x_max' ~y_min:y_min' ~y_max:y_max' in
    vprintf verb_optional "data dimensions: x=[%f, %f], y=[%f, %f]\n"
      r.x_min r.x_max r.y_min r.y_max;
    r


let resize_x ctx ~src ~dst datasets =
  let tgts =
    List.flatten (List.map (fun ds -> ds#x_over ctx ~src ~dst) datasets)
  in
    match tgts with
      | hd :: tl ->
	  let vl, tgt =
	    List.fold_left (fun ((vl, tgt) as max) ((v, t) as cur) ->
			      if t > tgt then cur else max)
	      hd tl
	  in
	    Geometry.find_new_dmax ~src:(xrange src) ~dst:(xrange dst) vl tgt
      | [] -> dst.x_max


(** {1 Numeric by numeric datasets} *)

(** The type of a numeric by numeric dataset. *)
class type dataset_type =
object

  (** The name of the dataset.  If there is no name then the dataset
      doesn't appear in the legend. *)
  val name : string option

  (** [name] gets the (optional) name of the dataset. *)
  method name : string option

  (** [avg_slope} returns the average rate of change across an entire
      num by num dataset.  Used for setting default axis skew (avg
      45 slope for all elements of the plot) *)
  method avg_slope : float


  (** [dimensions] gets the dimensions of this dataset in
      data-coordinates. *)
  method dimensions : Geometry.rectangle

  (** [draw ctx ~src ~dst] draws the data to the plot. *)
  method draw :
    Drawing.context ->
    src:Geometry.rectangle -> dst:Geometry.rectangle -> unit

  (** [draw_legend ctx ~x ~y] draws the legend entry centered at the
      given location. *)
  method draw_legend :
    Drawing.context -> x:float -> y:float -> unit

  (** [legend_dimensions ctx] gets the dimensions of the legend icon
      in plot-coordinates. *)
  method legend_dimensions : Drawing.context -> float * float

  (** [mean_y_value src] gets the mean y-value and the number of
      values this mean is over in the source coordinate system.
      This is used for sorting the legend.  If a dataset will not
      contribute to the competition over the legend locations then
      this should result in (nan, 0) .*)
  method mean_y_value : Geometry.rectangle -> float * int


  (** [residual ctx ~src ~dst] get a rectangle containing the maximum
      amount the dataset will draw off of the destination rectangle
      in each direction in plot-coordinates. *)
  method residual :
    Drawing.context ->
    src:Geometry.rectangle -> dst:Geometry.rectangle -> Geometry.rectangle


  (** [x_over ctx ~src ~dst] gets a (vl * over) list where vl is the
      source value and over is the amount that the given item is
      drawn over the end of the destination rectangle. *)
  method x_over :
    Drawing.context
    -> src:Geometry.rectangle
    -> dst:Geometry.rectangle
    -> (float * float) list
end

include Num_by_num_dataset
include Label_dataset
include Errbar_dataset
include Scatter_dataset
include Line_dataset
include Bubble_dataset
include Line_errbar_dataset
include Function_dataset
include Histogram_dataset
include Cdf_dataset
include Num_heatmap_dataset


(** {1 Numeric by numeric plot} *)

class type plot_type =
  object
    val mutable height : Length.t
    val src : Geometry.rectangle
    val mutable width : Length.t
    method display : unit
    method draw : Drawing.context -> unit
    method height : Length.t
    method output : string -> unit
    method set_size : w:Length.t -> h:Length.t -> unit
    method suggest_aspect : float
    method use_suggested_aspect : unit
    method width : Length.t
  end

(** [plot ?label_style ?legend_style ?tick_style ?title ?xlabel
    ?ylabel ?sort_legend ?legend_loc ?x_min ?x_max ?y_min ?y_max
    datasets] a plot that has a numeric x and y axis. *)
class plot
  ?(text_padding=Spt.text_padding)
  ?(axis_padding=default_axis_padding)
  ?(label_text_style=Spt.default_label_style)
  ?(legend_text_style=Spt.default_legend_style)
  ?(tick_text_style=Spt.default_tick_style)
  ?title ?xlabel ?ylabel
  ?(sort_legend=true) ?(legend_loc=Legend.Upper_right)
  ?x_min ?x_max ?y_min ?y_max
  (datasets : dataset_type list) =
  let _ = vprintf verb_optional "creating a numeric by numeric plot\n" in
object (self)
  inherit Spt.plot title

  (** The dimensions of the axes in the data coordinate system. *)
  val src = data_dimensions ~x_min ~x_max ~y_min ~y_max datasets


  method suggest_aspect =
    let avg_slopes = List.map (fun ds -> ds#avg_slope) datasets in
    let avg_slope = ((List.fold_left (+.) 0. avg_slopes) /.
		       (float (List.length datasets))) in
      avg_slope

  method use_suggested_aspect =
    let skew len s =
      match len with
	| Length.In flt -> Length.In (flt *. s)
	| Length.Cm flt -> Length.Cm (flt *. s)
	| Length.Px i -> Length.Px (truncate ((float i) *. s))
	| Length.Pt flt -> Length.Pt (flt *. s) in
    let ratio = self#suggest_aspect in
      if ratio <= 1. (* plot is tall, use plot height to determine size*)
      then self#set_size ~w:(skew self#width ratio) ~h:self#height
      else self#set_size ~w:self#width ~h:(skew self#height ratio)


  method private dst_rectangle ctx ~xaxis ~yaxis =
    (** [dst_rectangle ctx ~xaxis ~yaxis] get the dimensions of
	the destination rectangle. *)
    let axis_padding = ctx.units axis_padding in
    let xsize, ysize = self#size ctx in
    let title_height =
      match title with
	| None -> 0.
	| Some txt -> snd (text_dimensions ctx ~style:label_text_style txt) in
    let x_min' =
      Numeric_axis.resize_for_y_axis ctx
	~pad:(ctx.units text_padding) ~x_min:axis_padding yaxis in
    let y_min', x_max' =
      Numeric_axis.resize_for_x_axis
	ctx ~pad:(ctx.units text_padding) ~y_min:(ysize -. axis_padding)
	~dst:(range x_min' xsize) xaxis in
    let dst =
      rectangle ~x_min:x_min' ~x_max:x_max' ~y_min:y_min'
	~y_max:(title_height +. (ctx.units text_padding))
    in
    let residual =
      (* Maximum distance over the edge of the [dst] rectangle that
	 any dataset may need to draw. *)
      List.fold_left
	(fun r ds -> rectangle_max r (ds#residual ctx ~src ~dst))
	zero_rectangle datasets
    in
      vprintf verb_debug "residual: x_min=%g, x_max=%g, y_min=%g, y_max=%g\n"
	residual.x_min residual.x_max residual.y_min residual.y_max;
      let dst' =
	rectangle ~x_min:(dst.x_min +. residual.x_min)
	  ~x_max:(dst.x_max -. residual.x_max)
	  ~y_min:(dst.y_min -. residual.y_min)
	  ~y_max:(dst.y_max +. residual.y_max)
      in
      let x_max' = resize_x ctx ~src ~dst:dst' datasets in
      let r = { dst' with x_max = x_max' } in
	vprintf verb_debug "plot dimensions: x=[%f, %f], y=[%f, %f]\n"
	  r.x_min r.x_max r.y_min r.y_max;
	r


  (** [xaxis] creates the x-axis for the plot. *)
  method private xaxis =
    let src = xrange src in
    let nticks = Numeric_axis.recommended_ticks width in
    let ticks = Numeric_axis.tick_locations ~suggested_number:nticks src in
      (*
	verb_eval verb_debug
	(fun () ->
	vprintf verb_debug "x-ticks:\n";
	List.iter (fun (vl, name) -> match name with
	| None -> vprintf verb_debug "\tminor: %f\n" vl
	| Some _ -> vprintf verb_debug "\tmajor: %f\n" vl)
	ticks);
      *)
      Numeric_axis.create ~label_text_style ~tick_text_style ~src ticks xlabel


  (** [yaxis] creates the y-axis for the plot. *)
  method private yaxis =
    let src = yrange src in
    let nticks = Numeric_axis.recommended_ticks height in
    let ticks = Numeric_axis.tick_locations ~suggested_number:nticks src in
      (*
	verb_eval verb_debug
	(fun () ->
	vprintf verb_debug "y-ticks:\n";
	List.iter (fun (vl, name) -> match name with
	| None -> vprintf verb_debug "\tminor: %f\n" vl
	| Some _ -> vprintf verb_debug "\tmajor: %f\n" vl)
	ticks);
      *)
      Numeric_axis.create ~label_text_style ~tick_text_style ~src ticks ylabel


  (** [draw_x_axis ctx ~dst xaxis] draws the x-axis. *)
  method private draw_x_axis ctx ~dst xaxis =
    let _, ysize = self#size ctx in
      Numeric_axis.draw_x_axis ctx ~pad:(ctx.units text_padding)
	~height:ysize ~dst:(xrange dst) xaxis


  (** [draw_y_axis ctx ~dst] draws the y-axis. *)
  method private draw_y_axis ctx ~dst yaxis =
    Numeric_axis.draw_y_axis ctx ~pad:(ctx.units text_padding)
      ~dst:(yrange dst) yaxis


  method draw ctx =
    vprintf verb_debug "drawing numeric by numeric plot\n";
    self#fill_background ctx;
    let xaxis = self#xaxis and yaxis = self#yaxis in
    let dst = self#dst_rectangle ctx ~xaxis ~yaxis in
    let legend_txt_loc, legend_x, legend_y =
      Legend.locate ctx legend_text_style dst datasets legend_loc
    in
      begin match title with
	| None -> ()
	| Some t ->
	    (* (* this centers with respect to the whole plot
	       including the y-axis label. *)

	       let x = (fst (self#size ctx)) /. 2. and y = 0. in
	    *)
	    let x = (dst.x_max +. dst.x_min) /. 2. and y = 0. in
	      draw_text_centered_below ~style:label_text_style ctx x y t
      end;
      self#draw_x_axis ctx ~dst xaxis;
      self#draw_y_axis ctx ~dst yaxis;
      List.iter (fun ds -> ds#draw ctx ~src ~dst) datasets;
      save_transforms ctx;
      translate ctx legend_x legend_y;
      Legend.draw ctx sort_legend src legend_txt_loc
	legend_text_style datasets;
      restore_transforms ctx

end

let plot ?text_padding ?axis_padding ?label_text_style ?legend_text_style
    ?tick_text_style ?title ?xlabel ?ylabel ?sort_legend ?legend_loc ?x_min
    ?x_max ?y_min ?y_max datasets =
  new plot ?text_padding ?axis_padding ?label_text_style ?legend_text_style
    ?tick_text_style ?title ?xlabel ?ylabel ?sort_legend ?legend_loc ?x_min
    ?x_max ?y_min ?y_max datasets

(************************************************************)
(** {1 Standalone legend} *)

class standalone_legend
  ?(text_style = Spt.default_legend_style)
  ?(text_before = true)
  ?(vertical = true)
  (dss : dataset_type list) =
  let text_loc =
    if text_before then
      Legend.Text_before
    else
      Legend.Text_after
  in
object (self)

  inherit Spt.plot None

  method draw ctx =
    self#fill_background ctx;
    if vertical then
      Legend.do_draw_vertical ctx text_loc text_style dss
    else
      Legend.do_draw_horizontal ctx text_loc text_style dss
end


let standalone_legend ?text_style ?text_before ?vertical dss =
  new standalone_legend ?text_style ?text_before ?vertical dss
