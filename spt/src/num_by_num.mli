(** Numeric by numeric plots.

    @author eaburns
    @since 2010-06-09
*)

type label_x_location =
  | Label_xat
  | Label_before
  | Label_after


type label_y_location =
  | Label_yat
  | Label_above
  | Label_below


type line_errbar_style = {
  (** The style of a line and error bar plot. *)

  dashes : Length.t array;
  (* The dash pattern for the line. *)
  number : int;
  (* The number of the current line_errbar_dataset. *)
  count : int ref;
  (* A reference that counts the total number of associated
     line_errbar_datasets. *)
}


class type dataset_type =
object
  (** The type of a numeric by numeric dataset. *)

  val name : string option
    (** The name of the dataset.  If there is no name then the dataset
	doesn't appear in the legend. *)

  method name : string option
    (** [name] gets the (optional) name of the dataset. *)

  method avg_slope : float
    (** [avg_slope} returns the average rate of change across an
	entire num by num dataset.  Used for setting default axis skew
	(avg 45 slope for all elements of the plot) *)


  method dimensions : Geometry.rectangle
    (** [dimensions] gets the dimensions of this dataset in
	data-coordinates. *)

  method draw :
    Drawing.context ->
    src:Geometry.rectangle -> dst:Geometry.rectangle -> unit
    (** [draw ctx ~src ~dst] draws the data to the plot. *)

  method draw_legend :
    Drawing.context -> x:float -> y:float -> unit
    (** [draw_legend ctx ~x ~y] draws the legend entry centered at the
	given location. *)

  method legend_dimensions : Drawing.context -> float * float
    (** [legend_dimensions ctx] gets the dimensions of the legend
	icon in plot-coordinates. *)

  method mean_y_value : Geometry.rectangle -> float * int
    (** [mean_y_value src] gets the mean y-value and the number of
	values this mean is over in the source coordinate system.
	This is used for sorting the legend.  If a dataset will not
	contribute to the competition over the legend locations then
	this should result in (nan, 0) .*)


  method residual :
    Drawing.context ->
    src:Geometry.rectangle -> dst:Geometry.rectangle -> Geometry.rectangle
    (** [residual ctx ~src ~dst] get a rectangle containing the
	maximum amount the dataset will draw off of the destination
	rectangle in each direction in plot-coordinates. *)


  method x_over :
    Drawing.context
    -> src:Geometry.rectangle
    -> dst:Geometry.rectangle
    -> (float * float) list
    (** [x_over ctx ~src ~dst] gets a (vl * over) list where vl is the
	source value and over is the amount that the given item is drawn
	over the end of the destination rectangle. *)
end

val composite_dataset : ?name:string -> dataset_type list -> dataset_type
  (** A numeric by numeric dataset that is a composite of another set
      of numeric by numeric datasets. *)

val scatter_dataset :
  Drawing.glyph -> ?color:Drawing.color -> ?point_radius:Length.t ->
  ?name:string -> Geometry.point array -> dataset_type
  (** [scatter_dataset glyph ?color ?point_radius ?name points]
      creates a new scatter plot from the array of points,
      [points]. *)

val scatter_datasets :
  ?color:bool ->
  ?glyph_factory:(unit -> Drawing.glyph) ->
  ?point_radius:Length.t ->
  (string * Geometry.point array) list ->
  dataset_type list
    (** [scatter_datasets ?color ?point_radius named_points] creates a
	set of scatter plot datasets.  [named_points] is a list of
	tuples (name : string, points : point array).  If [color=true]
	then color is used, the default is to not use color. *)

val scatter_errbar_dataset :
  Drawing.glyph ->
  ?color:Drawing.color ->
  ?point_radius:Length.t ->
  ?xloc:label_x_location ->
  ?yloc:label_y_location ->
  ?name:string ->
  (string option * Geometry.point array) array ->
  dataset_type
    (** [scatter_errbar_dataset glyph ?color ?point_radius ?name sets]
	creates a scatter plot dataset with error bars.  [sets] is a
	tuple (name : string option, points : points array).  Each
	glyph is the mean x and y value of the associated points with
	error bars representing 95% confidence intervals.  If the
	optional string [name] is given as non [None] then a label is
	added with the name. *)

val scatter_errbar_datasets :
  ?color:bool ->
  (string * (string option * Geometry.point array) array) list ->
  dataset_type list
    (** [scatter_errbar_datasets ?color set_list] creates a list of
	scatter bar plots with error bars.  The [set_list] parameter
	is a list of sets, each in the form specified in the
	[scatter_errbar_dataset] function. *)


val y_errbar_dataset :
  Length.t array
  -> Drawing.glyph
  -> ?color:Drawing.color
  -> ?line_width:Length.t
  -> ?point_radius:Length.t
  -> ?name:string
  -> Geometry.point array
  -> dataset_type
  (** [y_errbar_dataset dashes glyph ?color ?line_width ?point_radius ?name
      points] bins the points by x value.  Draws a line with error
      bars showing the mean and 95\% confidence for the y values for
      the given x value. *)


val vert_errbar_dataset :
  ?color:Drawing.color -> Geometry.triple array -> dataset_type
  (** [vert_errbar_dataset ?color triples] Draws vertical error
      bars given a triples (x, low, high). *)


val line_dataset :
  Length.t array ->
  ?line_width:Length.t ->
  ?color:Drawing.color ->
  ?name:string ->
  Geometry.point array ->
  dataset_type
    (** [line_dataset dashes ?line_width ?color ?name points] creates
	a line.  [points] is an array of points. *)

val line_datasets :
  ?color:bool ->
  ?line_width:Length.t ->
  (string option * Geometry.point array) list ->
  dataset_type list
    (** [line_datasets ?color lines] creates a list of lines.  [lines]
	is a list of (name : string option, points : point array)
	tuples. *)

val line_points_dataset :
  Length.t array ->
  Drawing.glyph ->
  ?point_radius:Length.t ->
  ?line_width:Length.t ->
  ?color:Drawing.color ->
  ?name:string ->
  Geometry.point array ->
  dataset_type
    (** [line_points_dataset dashes glyphs ?point_radius ?line_width
	?color ?name points] creates a line with a glyph at each
	point.  [points] is an array of points. *)

val line_points_datasets :
  ?color:bool -> (string option * Geometry.point array) list ->
  dataset_type list
    (** [line_points_datasets ?color lines] creates a list of line
	with a glyph at each point.  [lines] is the same format as the
	[lines] parameter to the [line_datasets] function. *)

val bubble_dataset :
  ?glyph:Drawing.glyph ->
  ?color:Drawing.color ->
  ?min_radius:Length.t ->
  ?max_radius:Length.t ->
  ?name:string ->
  Geometry.triple array ->
  dataset_type
    (** [bubble_dataset ?glyph ?color ?min_radius ?max_radius ?name
	triples] creates a bubble plot dataset.  This is similar to a
	scatter plot but the glyphs are given different sizes based on
	a 3rd value.  [triples] is an array of triples. *)

val bubble_datasets :
  ?color:bool ->
  ?min_radius:Length.t ->
  ?max_radius:Length.t ->
  (string option * Geometry.triple array) list ->
  dataset_type list
    (** [bubble_datasets ?color ?min_radius ?max_radius sets] creates
	a list of bubble plot datasets. *)

val line_errbar_factory :
  (unit -> Length.t array) -> unit -> unit -> line_errbar_style
  (** [line_errbar_factory next_dash () ()] makes a line and error bar
      style factory.  This is used to place error bars so that they
      don't overlap when lines have the same domains. *)

val line_errbar_dataset :
  line_errbar_style ->
  ?line_width:Length.t ->
  ?color:Drawing.color ->
  ?name:string ->
  Geometry.point array array ->
  dataset_type
    (** [line_errbar_dataset style ?color ?line_width ?name lines]
	creates a line with error bar dataset.  The line shows the
	mean value of the common domain of [lines].  The error bars
	show 95% confidence intervals on the mean. *)

val line_errbar_datasets :
  ?color:bool ->
  ?line_width:Length.t ->
  (string option * Geometry.point array array) list ->
  dataset_type list
    (** [line_errbar_datasets ?color line_set] creates a list of line
	and error bar datasets. *)

val scatter_errbar_lines_dataset :
  ?xloc:label_x_location ->
  ?yloc:label_y_location ->
  Drawing.glyph ->
  Length.t array ->
  ?color:Drawing.color ->
  ?point_radius:Length.t ->
  ?line_width:Length.t ->
  ?name:string ->
  (Geometry.point array * string option) array ->
  dataset_type
    (** [scatter_errbar_lines_dataset glyph dashes ?color
	?point_radius ?line_width ?name sets] creates a scatter plot
	with error bars and a line connecting the points. *)

val scatter_errbar_lines_datasets :
  ?color:bool ->
  (string * (Geometry.point array * string option) array) list ->
  dataset_type list
    (** [scatter_errbar_lines_datasets ?color set_list] creates a list
	of scatter plots with error bars and lines connecting the
	points. *)

val function_dataset :
  Length.t array ->
  ?samples:int ->
  ?line_width:Length.t ->
  ?color:Drawing.color ->
  ?name:string -> (float -> float) ->
  dataset_type
    (** [function_dataset dashes ?samples ?line_width ?color ?name f]
	creates a dataset that draws a line showing the function [f].
	[samples] is the number of points to place when drawing the
	line. *)

val bestfit_dataset :
  glyph:Drawing.glyph ->
  dashes:Length.t array ->
  ?color:Drawing.color ->
  ?line_width:Length.t ->
  ?point_radius:Length.t ->
  ?degree:int ->
  ?fit_in_name:bool ->
  ?name:string ->
  Geometry.point array ->
  dataset_type
    (** [bestfit_dataset glyph dashes ?color ?line_width ?point_radius
	?degree ?fit_in_name ?name points] creates a scatter plot with
	a best-fit polynomial of degree [degree] (default 1, a line).
	If [fit_in_name] is true than the polynomial fit is added to
	the end of the dataset name in the legend (default true). *)

val bestfit_datasets :
  ?color:bool ->
  ?point_radius:Length.t ->
  ?line_width:Length.t ->
  ?degree:int ->
  ?fit_in_name:bool ->
  (string option * Geometry.point array) list ->
  dataset_type list
    (** [bestfit_datasets ?color ?point_radius ?line_width ?degree
	?fit_in_name point_sets] creates a list of scatter plots with
	best fit polynomials. *)


val values_histogram_dataset :
  Length.t array ->
  ?normalize:bool ->
  ?line_width:Length.t ->
  ?bg_color:Drawing.color ->
  ?bin_width:float ->
  ?name:string ->
  float array ->
  dataset_type
    (** [values_histogram_dataset dashes ?normalize ?line_width
	?bg_color ?bin_width ?name values] creates a histogram dataset
	showing the counts or density (if normalized) of the given set
	of [values]. *)

val histogram_dataset :
  Length.t array ->
  ?normalize:bool ->
  ?line_width:Length.t ->
  ?bg_color:Drawing.color ->
  ?bin_width:float ->
  ?name:string ->
  float array ->
  dataset_type
    (** [histogram_dataset dashes ?normalize ?line_width ?bg_color
	?bin_width ?name values] creates a histogram dataset showing
	the counts or density (if normalized) of the given set of
	[values]. *)

val points_histogram_dataset :
  Length.t array ->
  ?normalize:bool ->
  ?line_width:Length.t ->
  ?bg_color:Drawing.color ->
  ?bin_width:float ->
  ?name:string ->
  Geometry.point array ->
  dataset_type
    (** [points_histogram_dataset dashes ?normalize ?line_width
	?bg_color ?bin_width ?name points] creates a histogram dataset
	showing the points or density (if normalized) of the given set
	array of points. *)

val cdf_dataset :
  Length.t array ->
  ?normalize:bool ->
  ?nsamples:int ->
  ?line_width:Length.t ->
  ?color:Drawing.color ->
  ?name:string ->
  float array ->
  dataset_type
    (** [cdf_dataset dashes ?normalize ?nsamples ?line_width ?color
	?name values] creates a cumulative density dataset for
	[values]. *)

val cdf_datasets :
  ?normalize:bool ->
  ?nsamples:int ->
  ?color:bool ->
  (string * float array) list ->
  dataset_type list
    (** [cdf_datasets ?normalize ?nsamples ?color value_list] creates
	a list of CDF datasets. *)

val points_cdf_dataset :
  Length.t array ->
  ?normalize:bool ->
  ?nsamples:int ->
  ?line_width:Length.t ->
  ?color:Drawing.color ->
  ?name:string ->
  Geometry.point array ->
  dataset_type
    (** [points_cdf_dataset dashes ?normalize ?nsamples ?line_width
	?color ?name points] creates a cumulative density dataset for
	[points]. *)

val points_cdf_datasets :
  ?normalize:bool ->
  ?nsamples:int ->
  ?color:bool ->
  (string * Geometry.point array) list ->
  dataset_type list
    (** [points_cdf_datasets ?normalize ?nsamples ?color point_list]
	creates a list of CDF datasets. *)

val countmap_dataset :
  ?line_width:Length.t ->
  ?bin_size:Geometry.point ->
  ?gradient:(float -> Drawing.color) ->
  Geometry.point array ->
  dataset_type
(** [countmap_dataset ?line_width ?bin_size ?gradient points]
    creates a 2d histogram which is displayed as a heat map.  The
    color of each cell is based on the number of x, y points that
    fall within that cell. *)

val valuemap_dataset :
  ?line_width:Length.t ->
  ?bin_size:Geometry.point ->
  ?gradient:(float -> Drawing.color) ->
  Geometry.triple array ->
  dataset_type
(** [value_dataset ?line_width ?bin_size ?gradient triples] creates a
    heatmap where the 3rd point of the triple desides the color.  If
    multiple points fall within a cell, the 3rd points are summed
    together. *)

val heatmap_dataset :
  ?line_width:Length.t ->
  ?bin_size:Geometry.point ->
  ?gradient:(float -> Drawing.color) ->
  Geometry.triple array ->
  dataset_type
(** Alias for valuemap. *)

val label_dataset :
  ?text_style:Drawing.text_style ->
  ?xloc:label_x_location ->
  ?yloc:label_y_location ->
  ?xoff:Length.t ->
  ?yoff:Length.t ->
  ?name:string ->
  (Geometry.point * string) array ->
  dataset_type
(** [label_dataset ?text_style ?xloc ?yloc ?xoff ?yoff ?name
    lbl_points] makes a new label dataset. *)

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

val plot :
  ?text_padding:Length.t ->
  ?axis_padding:Length.t ->
  ?label_text_style:Drawing.text_style ->
  ?legend_text_style:Drawing.text_style ->
  ?tick_text_style:Drawing.text_style ->
  ?title:string ->
  ?xlabel:string ->
  ?ylabel:string ->
  ?sort_legend:bool ->
  ?legend_loc:Legend.location ->
  ?x_min:float ->
  ?x_max:float ->
  ?y_min:float ->
  ?y_max:float ->
  dataset_type list ->
  plot_type
    (** [plot ?label_text_style ?legend_text_style ?tick_text_style ?title
	?xlabel ?ylabel ?sort_legend ?legend_loc ?x_min ?x_max ?y_min
	?y_max datasets] creates a numeric by numeric plot. *)

val standalone_legend :
  ?text_style:Drawing.text_style -> ?text_before:bool -> ?vertical:bool
  -> dataset_type list -> Spt.plot_type
