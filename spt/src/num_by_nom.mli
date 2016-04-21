class type dataset_type =
object
  val name : string
    (** The name of the dataset is what appears on the x-axis. *)

  method dimensions : Geometry.range
    (** [dimensions] gets the min and maximum value from the
	dataset. *)

  method draw :
    Drawing.context ->
    src:Geometry.range ->
    dst:Geometry.range -> width:float -> x:float -> unit
    (** [draw ctx ~src ~dst width x] draws the dataset to the
	plot.  [x] is the left-hand-side x value. *)

  method draw_x_label :
    Drawing.context ->
    x:float -> y:float -> Drawing.text_style -> width:float -> unit
    (** [draw_x_label context ~x ~y style ~width] draws the x-axis
	label to the proper location. *)

  method residual :
    Drawing.context ->
    src:Geometry.range ->
    dst:Geometry.range -> width:float -> x:float -> Geometry.range
    (** [residual ctx ~src ~dst width x] get a rectangle containing
	the maximum amount the dataset will draw off of the
	destination rectangle in each direction. *)

  method x_label_height :
    Drawing.context -> Drawing.text_style -> float -> float
    (** [x_label_height context style width] is the height of the
	label on thesrc/ x-axis. *)


  method n_items : int
    (** [n_items] gets the number of dataset items.  Each item is
	allocated a fixed width across the plot.  A dataset with more
	than one item is allocated a proportion of the space that is
	[n_items] times the size of the single item width. *)

end


val dataset_group :
	string -> ?between_padding:Length.t ->
	dataset_type list -> dataset_type
  (** [dataset_group group_name datasets] bundles a list of datasets
      into a group that is displayed as one dataset.  The names of
      each individual dataset is shown with the group name beneath
      them. *)

val boxplot_dataset :
  ?interval:bool -> ?outliers:bool -> ?point_radius:Length.t -> string
  -> float array -> dataset_type
  (** [boxplot_dataset ?interval ?outliers ?point_radius name values]
	creates a boxplot dataset of the given values.  If [interval] is
	true (the default) then the confidence interval is drawn. *)

val boxplot_datasets :
  ?interval:bool ->
  ?outliers:bool ->
  ?point_radius:Length.t ->
  (string * float array) list ->
  dataset_type list
    (** [boxplot_datasets ?interval ?outliers ?point_radius sets]
	creates a list of boxplot datasets given a list of name
	and value tuples. *)

val barchart_dataset :
  Drawing.fill_pattern ->
  ?line_width:Length.t -> string -> float -> dataset_type
  (** [barchart_dataset fill_pattern ?line_width name value] creates a
      single bar for the given value. *)

val barchart_datasets :
  ?use_color:bool ->
  ?line_width:Length.t ->
  ?group:string ->
  (string * float) list ->
  dataset_type list
    (** [barchart_datasets ?use_color ?line_width bars] creates a list
	of bar charts given a list of name and value tuples. *)

val barchart_errbar_dataset :
  Drawing.fill_pattern ->
  ?line_width:Length.t ->
  string ->
  float array ->
  dataset_type
  (** [barchart_errbar_dataset fill_pattern ?line_width name values]
      creates a single bar for the mean of the given values with an
      error bar showing the 95% confidence interval. *)

val barchart_errbar_datasets :
  ?use_color:bool ->
  ?line_width:Length.t ->
  ?group:string ->
  (string * float array) list ->
  dataset_type list
    (** [barchart_errbar_datasets ?use_color ?line_width bars] creates
	a list of bar charts with error bars given a list of name and
	value list tuples. *)

val stacked_barchart_dataset :
  ?line_width:Length.t ->
  ?name:string ->
  ?fill_factory:(unit -> Drawing.fill_pattern) ->
  (string * float) array ->
  dataset_type
    (** [stacked_barchart_dataset ?line_width ?name ?fill_factory
	bars] creates a bar that is a stacked set of values.  [name]
	is the group name and each bar has a minor name too. *)

val stacked_barchart_datasets :
  ?line_width:Length.t ->
  ?group:string ->
  ?fill_factory:(unit -> Drawing.fill_pattern) ->
  (string option * (string * float) array) list ->
  dataset_type list
    (** [stacked_barchart_datasets ?line_width ?group ?fill_factory
	bars] creatse a list of stacked barchart datasets. *)

val layered_barchart_dataset :
  ?line_width:Length.t ->
  ?name:string ->
  ?sort:bool ->
  ?fill_factory:(unit -> Drawing.fill_pattern) ->
  (string * float) array ->
  dataset_type
(** [layered_barchart_dataset ?line_width ?name ?sort
    ?fill_factory bars] creates a fan of bars.  [name] is the
    group name and each bar has a minor name too. *)

val layered_barchart_datasets :
  ?line_width:Length.t ->
  ?group:string ->
  ?sort:bool ->
  ?fill_factory:(unit -> Drawing.fill_pattern) ->
  (string option * (string * float) array) list ->
  dataset_type list
(** [layered_barchart_datasets ?line_width ?group ?sort ?fill_factory
    bars] creatse a list of layered barchart datasets. *)

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

val plot :
  ?x_axis_padding:Length.t ->
  ?y_axis_padding:Length.t ->
  ?between_padding:Length.t ->
  ?label_text_style:Drawing.text_style ->
  ?legend_text_style:Drawing.text_style ->
  ?tick_text_style:Drawing.text_style ->
  ?horiz_lines:float list ->
  ?seps:bool ->
  ?title:string ->
  ?ylabel:string ->
  ?y_min:float ->
  ?y_max:float ->
  dataset_type list ->
  plot_type
(** [plot ?label_text_style ?legend_text_style ?tick_text_style
    ?horiz_lines ?seps ?title ?ylabel ?y_min ?y_max datasets] creates
    an numeric by nominal plot.

    @param seps, if true draws vertical separator lines between the
    datasets.  The default is false. *)
