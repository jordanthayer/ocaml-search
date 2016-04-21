(** Virtual numeric by numeric datasets.

    @author eaburns
    @since 2010-04-28
*)

open Geometry
open Drawing
open Verbosity

(** [dataset name] is a dataset that is plottable on a numeric x and
    y axis. *)
class virtual dataset ?name () =
object

  (** The name of the dataset.  If there is no name then the dataset
      doesn't appear in the legend. *)
  val name = (name : string option)

  (** [name] gets the (optional) name of the dataset. *)
  method name = name


  (** [residual ctx ~src ~dst] get a rectangle containing the
      maximum amount the dataset will draw off of the destination
      rectangle in each direction in plot-coordinates. *)
  method residual
    (_:context) ~(src:rectangle) ~(dst:rectangle) = zero_rectangle

  method x_over (_:context) ~(src:rectangle) ~(dst:rectangle) =
    ([]:(float * float) list)

  (** [draw_legend ctx ~x ~y] draws the legend entry centered at the
      given location. *)
  method virtual draw_legend : context -> x:float -> y:float -> unit


  (** [legend_dimensions ctx] gets the dimensions of the legend icon
      in plot-coordinates. *)
  method virtual legend_dimensions : context -> float * float


  (** [draw ctx ~src ~dst] draws the data to the plot. *)
  method virtual draw :
    context -> src:rectangle -> dst:rectangle -> unit


  (** [dimensions] gets the dimensions of this dataset in
      data-coordinates. *)
  method virtual dimensions : rectangle


  (** [mean_y_value src] gets the mean y-value and the number of
      values this mean is over in the source coordinate system.
      This is used for sorting the legend.  If a dataset will not
      contribute to the competition over the legend locations then
      this should result in (nan, 0) .*)
  method virtual mean_y_value : rectangle -> float * int


  (** [avg_slope} returns the average rate of change across an
      entire num by num dataset.  Used for setting default axis skew
      (avg 45 slope for all elements of the plot) *)
  method virtual avg_slope : float

end


(** {1 Points datasets} *)


(** A dataset composed of a set of points. *)
class virtual points_dataset ?name points =
object
  inherit dataset ?name ()

  (** The list of points. *)
  val points = (points : point array)

  (** [dimensions] gets the rectangle around the points. *)
  method dimensions = points_rectangle points

  method mean_y_value _ =
    let s, n =
      Array.fold_left (fun (s, n) p -> s +. p.y, n + 1) (0., 0) points
    in s /. (float n), n

  initializer
    Array.iter (fun p ->
		  begin match classify_float p.x with
		    | FP_nan -> vprintf verb_normal "warning: x value is NAN\n"
		    | _ -> ()
		  end;
		  begin match classify_float p.y with
		    | FP_nan -> vprintf verb_normal "warning: y value is NAN\n"
		    | _ -> ()
		  end)
      points
end

(** {1 Composite datasets} *)


(** A dataset composed of a set of datasets. *)
class composite_dataset ?name datasets =
object

  inherit dataset ?name ()

  val datasets = (datasets : dataset list)

  method dimensions =
    List.fold_left (fun r ds -> rectangle_extremes r ds#dimensions)
      (rectangle ~x_min:infinity ~x_max:neg_infinity
	 ~y_min:infinity ~y_max:neg_infinity) datasets


  method mean_y_value src =
    let s, n =
      List.fold_left (fun (s, n) ds ->
			let mean, number = ds#mean_y_value src in
			  match classify_float mean with
			    | FP_nan | _ when number <= 0 -> s, n
			    | _ ->
				assert (number > 0);
				s +. (mean *. (float number)), n + number)
	(0., 0) datasets
    in s /. (float n), n


  method residual ctx ~src ~dst =
    List.fold_left
      (fun r ds -> rectangle_max r (ds#residual ctx ~src ~dst))
      zero_rectangle datasets

  method x_over ctx ~src ~dst =
    List.flatten
      (List.map (fun ds -> ds#x_over ctx ~src ~dst) datasets)

  method draw ctx ~src ~dst =
    List.iter (fun ds -> ds#draw ctx ~src ~dst) datasets


  method draw_legend ctx ~x ~y =
    List.iter (fun ds -> ds#draw_legend ctx ~x ~y) datasets

  method legend_dimensions ctx =
    List.fold_left (fun (w, h) ds ->
		      let width, height = ds#legend_dimensions ctx in
		      let w' = if width > w then width else w
		      and h' = if height > h then height else h
		      in w', h')
      (0., 0.) datasets

  method avg_slope =
    (List.fold_left (fun accum ds ->
		       let ds_slope = ds#avg_slope in
			 match classify_float ds_slope with
			   | FP_nan -> accum
			   | _ -> accum +. ds_slope)
       0. datasets) /. (float (List.length datasets))

end

let composite_dataset ?name datasets = new composite_dataset ?name datasets
