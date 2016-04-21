(** Barcharts.

    @author jtd7 and eaburns
    @since 2010-05-26
*)

open Num_by_nom_dataset
open Drawing
open Geometry

let fcmp a b = if (a:float) < b then ~-1 else if a > b then 1 else 0
let fmin a b = if (a:float) < b then a else b
let fmax a b = if (a:float) > b then a else b

(** {1 Simple barcharts} *)

(** A simple barchart.  This is one bar with a name and a height. *)
class barchart_dataset fill_pattern ?(line_width=Length.Pt 1.) name value =
  let min_val = min 0. value
  and max_val = max 0. value in
object(self)

  inherit Num_by_nom_dataset.dataset name

  val style = { default_line_style with line_width = line_width; }


  method dimensions =
    { min = min_val; max = max_val}


  method residual ctx ~src ~dst ~width ~x =
    { min = 0.; max = 0.; }


  method draw ctx ~src ~dst ~width ~x =
    let tr = range_transform ~src ~dst in
    let y_min = tr (min 0. value)
    and y_max = tr (max 0. value) in
    let r = rectangle ~x_min:x ~x_max:(x +. width) ~y_min ~y_max in
      draw_fill_pattern ctx r fill_pattern;
      draw_line ctx ~style [point x y_min;
			    point x y_max;
			    point (x +. width) y_max;
			    point (x +. width) y_min;
			    point x y_min;]

end


(** [barchart_dataset fill_pattern ?line_width name data] makes a
    barchart dataset. *)
let barchart_dataset fill_pattern ?(line_width=Length.Pt 1.) name data =
  new barchart_dataset fill_pattern ~line_width name data


(** [barchart_datasets ?use_color ?line_width ?group values] makes a
    group of bars datasets. *)
let barchart_datasets
    ?(use_color=false)
    ?(line_width=Length.Pt 1.) ?group values =
  let next_fill =
    if use_color
    then Factories.default_color_fill_pattern_factory ()
    else Factories.default_fill_pattern_factory () in
  let bars =
    List.map
      (fun (nm,dt) -> new barchart_dataset (next_fill ()) ~line_width nm dt)
      values
  in
    match group with
      | Some name -> [ new Num_by_nom_dataset.dataset_group name bars ]
      | None -> bars


(** {1 Barcharts with error bars} *)


(** A barchart where the height of the bar is the mean of a set of
    values and error bars are computed to the 95\% confidence
    intervals. *)
class barchart_errbar_dataset
  fill_pattern ?(line_width=Length.Pt 1.) name values =
  let mean, conf_interval = Statistics.mean_and_interval values in
  let min_val = min 0. (mean -. conf_interval)
  and max_val = max 0. (mean +. conf_interval) in
object(self)

  inherit Num_by_nom_dataset.dataset name

  val style = { default_line_style with line_width = line_width; }


  method dimensions =
    { min = min_val; max = max_val}


  method residual ctx ~src ~dst ~width ~x =
    { min = 0.; max = 0.; }


  method draw ctx ~src ~dst ~width ~x =
    let tr = range_transform ~src ~dst in
    let y_min = tr (min 0. mean)
    and y_max = tr (max 0. mean)
    and center = (width /. 2.) +. x in
    let r = rectangle ~x_min:x ~x_max:(x +. width) ~y_min ~y_max in
      draw_fill_pattern ctx r fill_pattern;
      draw_line ctx ~style [point x y_min;
			    point x y_max;
			    point (x +. width) y_max;
			    point (x +. width) y_min;
			    point x y_min;];
      Errbar.draw_up ctx ~style ?cap_size:None ~src ~dst
	~x:center ~y:mean ~mag:conf_interval;
      Errbar.draw_down ctx ~style ?cap_size:None ~src ~dst
	~x:center ~y:mean ~mag:conf_interval;

end

(** [barchart_errbar_dataset fill_pattern ?line_width name data]
    makes a barchart dataset where the bar is the mean value with
    error bars. *)
let barchart_errbar_dataset fill_pattern ?(line_width=Length.Pt 1.) name data =
  new barchart_errbar_dataset fill_pattern ~line_width name data


(** [barchart_errbar_datasets ?use_color ?line_width ?gname
    values] makes a set of barcharts with error bars. *)
let barchart_errbar_datasets
    ?(use_color=false) ?(line_width=Length.Pt 1.) ?group values =
  let next_fill =
    if use_color
    then Factories.default_color_fill_pattern_factory ()
    else Factories.default_fill_pattern_factory () in
  let bars =
    List.map
      (fun (nm,dt) ->
	 new barchart_errbar_dataset (next_fill ()) ~line_width nm dt)
      values
  in
    match group with
      | Some name -> [ new Num_by_nom_dataset.dataset_group name bars ]
      | None -> bars


(** {1 Stacked barcharts} *)


class stacked_barchart_dataset next_pattern ?name ?(line_width=Length.Pt 1.)
  values =
  let values = (List.map (fun (nm,value) -> next_pattern(), nm, value)
		  (Array.to_list values)) in

  let pos = List.filter (fun (_,_,v) -> v >= 0.) values
  and neg = List.filter (fun (_,_,v) -> v < 0.) values in

  let min_val = List.fold_left (fun accum (_,_,value) -> accum +. value) 0. neg
  and max_val = List.fold_left (fun accum (_,_,value) -> accum +. value) 0. pos
  in
  let pos = List.map (fun (pt,nm,value) -> value, pt) pos
  and neg = List.map (fun (pt,nm,value) -> value, pt) neg
  and minor = (List.fold_left (fun accum (_,nm,_) -> nm ^ "," ^ accum) ""
		(List.rev values)) in
  let minor = String.sub minor 0 ((String.length minor - 1)) in
  let major_name = name in

object(self)

  inherit Num_by_nom_dataset.dataset
    (match major_name with None -> "" | Some n -> n)


  val style = { default_line_style with line_width = line_width; }

  method x_label_height ctx style width =
    match major_name with
      |	None -> fixed_width_text_height ctx ~style width minor
      | Some major -> ((fixed_width_text_height ctx ~style width minor) +.
			 (fixed_width_text_height ctx ~style width major))

  method draw_x_label ctx ~x ~y style ~width =
    let half_width = width /. 2. in
      match major_name with
	| None -> (draw_fixed_width_text
		     ctx ~x:(x +. half_width) ~y ~style ~width minor)
	| Some major ->(let height = self#x_label_height ctx style width in
			  draw_fixed_width_text ctx ~x:(x +. half_width)
			    ~y ~style ~width minor;
			  draw_fixed_width_text ctx ~x:(x +. half_width)
			    ~y:(y +. height /. 2.) ~style ~width major)

  method dimensions =
    { min = min_val; max = max_val}


  method residual ctx ~src ~dst ~width ~x =
    { min = 0.; max = 0.; }


  method draw ctx ~src ~dst ~width ~x =
    let tr = range_transform ~src ~dst
    and x_min = x
    and x_max = x +. width in
    let d start_y (value,fill) =
      let y_max = tr (start_y +. value)
      and y_min = tr start_y in
      let r = rectangle ~x_min ~x_max ~y_min ~y_max in
	draw_fill_pattern ctx r fill;
	draw_line ctx ~style [point x y_min;
			      point x y_max;
			      point (x +. width) y_max;
			      point (x +. width) y_min;
			      point x y_min;];
	start_y +. value in
      ignore (List.fold_left d 0. pos);
      ignore (List.fold_left d 0. neg)

end


let stacked_barchart_dataset
    ?(line_width=Length.Pt 1.) ?name ?fill_factory nm_data_array =
  let fill_factory = match fill_factory with
    | None -> Factories.default_fill_pattern_factory ()
    | Some f -> f in
  new stacked_barchart_dataset fill_factory ?name ~line_width nm_data_array


let stacked_barchart_datasets
    ?(line_width=Length.Pt 1.) ?group ?fill_factory mjr_by_nm_data_array_list =
  let fill_factory = match fill_factory with
    | None -> Factories.default_fill_pattern_factory ()
    | Some f -> f in
  let bars = List.map
    (fun  (name,nm_data_array) ->
       new stacked_barchart_dataset fill_factory ?name
	 ~line_width nm_data_array)
    mjr_by_nm_data_array_list in

    match group with
      | Some name -> [ new Num_by_nom_dataset.dataset_group name bars ]
      | None -> bars


(** {1 Layered barcharts} *)


class layered_barchart_dataset next_pattern ?(sort=true) ?name
  ?(line_width=Length.Pt 1.) values =
  let vls =
    let n = next_pattern in
    let cmp (_,_,a) (_,_,b) = fcmp (abs_float b) (abs_float a) in
    let vls = List.map (fun (nm,v) -> n (), nm, v) (Array.to_list values) in
    if sort then List.sort cmp vls else vls in
  let min_vl = List.fold_left (fun m (_,_,v) -> fmin v m) 0. vls in
  let max_vl = List.fold_left (fun m (_,_,v) -> fmax v m) 0. vls in
  let minor =
    let s = List.fold_left (fun s (_,nm,_) -> nm^","^s) "" (List.rev vls) in
    String.sub s 0 (String.length s - 1) in
  let vls = List.map (fun (pat,_,vl) -> vl, pat) vls in
  let major = name in
object(self)

  inherit Num_by_nom_dataset.dataset
    (match major with None -> "" | Some n -> n)

  val style = { default_line_style with line_width = line_width; }

  method x_label_height ctx style width =
    match major with
      |	None -> fixed_width_text_height ctx ~style width minor
      | Some major -> ((fixed_width_text_height ctx ~style width minor) +.
			  (fixed_width_text_height ctx ~style width major))

  method draw_x_label ctx ~x ~y style ~width =
    let half_width = width /. 2. in
    match major with
      | None -> (draw_fixed_width_text
		   ctx ~x:(x +. half_width) ~y ~style ~width minor)
      | Some major ->(let height = self#x_label_height ctx style width in
		      draw_fixed_width_text ctx ~x:(x +. half_width)
			~y ~style ~width minor;
		      draw_fixed_width_text ctx ~x:(x +. half_width)
			~y:(y +. height /. 2.) ~style ~width major)

  method dimensions =
    { min = min_vl; max = max_vl }

  method residual ctx ~src ~dst ~width ~x =
    { min = 0.; max = 0.; }

  method draw ctx ~src ~dst ~width ~x =
    let offset = (width /. 10.) in
    let tr = range_transform ~src ~dst in
    let max_offset = List.length vls in
    let width = width -. ((float max_offset) *. offset) in
    let d x_min (value,fill) =
      let x_max = x_min +. width
      and y_max = tr value
      and y_min = tr 0. in
      let r = rectangle ~x_min ~x_max ~y_min ~y_max in
      draw_fill_pattern ctx r fill;
      draw_line ctx ~style [point x_min y_min;
			    point x_min y_max;
			    point x_max y_max;
			    point x_max y_min;
			    point x_min y_min;];
      x_min +. offset in
    ignore (List.fold_left d x vls)

end


let layered_barchart_dataset
    ?(line_width=Length.Pt 1.) ?name ?sort ?fill_factory nm_data_array =
  let fill_factory = match fill_factory with
    | None -> Factories.default_fill_pattern_factory ()
    | Some f -> f
  in new layered_barchart_dataset fill_factory ?name ?sort
  ~line_width nm_data_array


let layered_barchart_datasets
    ?(line_width=Length.Pt 1.) ?group ?sort ?fill_factory nm_data_array_list =
  let fill_factory = match fill_factory with
    | None -> Factories.default_fill_pattern_factory ()
    | Some f -> f in
  let bars =
    List.map
      (fun (name,nm_data_array) ->
	new layered_barchart_dataset fill_factory ?name ?sort
	  ~line_width nm_data_array)
      nm_data_array_list in
  match group with
    | Some name -> [ new Num_by_nom_dataset.dataset_group name bars ]
    | None -> bars


(* EOF *)
