(** Box plots.

    @author eaburns
    @since 2010-05-21
*)

open Geometry
open Drawing


class boxplot_dataset ?(interval=true) ?(outliers=true)
	?(point_radius=Length.Pt 2.) name values =
object(self)

  inherit Num_by_nom_dataset.dataset name

  val box = Boxplot.create ~outliers ~point_radius values

  method dimensions = Boxplot.dimensions box


  method residual ctx ~src ~dst ~width ~x =
    Boxplot.residual ctx ~src ~dst ~width ~x box


  method draw ctx ~src ~dst ~width ~x =
    Boxplot.draw ctx ~interval ~src ~dst ~width:(width *. 0.66)
      ~x:(x +. (width /. 2.)) box

end

(** [boxplot_dataset ?outliers ?point_radius name values] makes a
    boxplot dataset. *)
let boxplot_dataset ?interval ?outliers ?point_radius name values =
  new boxplot_dataset ?interval ?outliers ?point_radius name values


(** [boxplot_datasets ?outliers ?point_radius name_vl_list] makes a
    set of boxplot datasets. *)
let boxplot_datasets ?interval ?outliers ?point_radius name_vl_list =
  List.map
    (fun (name, values) ->
       new boxplot_dataset ?interval ?outliers ?point_radius name values)
    name_vl_list
