(** Defaualing with plot legends.

    @author eaburns
    @since 2010-05-03
*)

open Drawing
open Geometry
open Verbosity

type text_location =
  | Text_before
  | Text_after


type location =
  | At of text_location * float * float
  | Upper_left
  | Lower_left
  | Upper_right
  | Lower_right


(** Padding between legend text and the icons. *)
let padding = Length.Pt 4.


(** [max_height ctx style datasets] gets the max entry height. *)
let max_height ctx style datasets =
  let text_height = font_suggested_line_height ~style ctx in
    List.fold_left
      (fun h ds ->
	 let _, icon_height = ds#legend_dimensions ctx in
	   max h (max text_height icon_height))
      0. datasets


(** [max_widths ctx style datasets] gets the max entry height. *)
let max_widths ctx style datasets =
  let rec max_width tw iw = function
    | [] -> tw, iw
    | ds :: dss ->
	begin match ds#name with
	  | None -> max_width tw iw dss
	  | Some txt ->
	      let txt_w, _ = text_dimensions ctx ~style txt in
	      let ico_w, _ = ds#legend_dimensions ctx in
		max_width (max tw txt_w) (max iw ico_w) dss
	end
  in
    max_width 0. 0. datasets


(** [dimensions style ctx datasets] gets the dimensions of a legend
    for the given datasets. *)
let dimensions style ctx datasets =
  let padding = ctx.units padding in
  let ndatasets =
    float (List.length (List.filter (fun ds -> ds#name <> None) datasets)) in
  let ent_height = max_height ctx style datasets in
  let text_width, icon_width = max_widths ctx style datasets
  in (text_width +. icon_width +. padding), ent_height *. ndatasets


(** [locate ctx style dst datasest legend_loc] gets the location
    for drawing the plot legend of the given datasets. *)
let locate ctx style dst datasets = function
  | At (txt_loc, x, y) ->
      vprintf verb_debug "legend location: text %s icons (%f, %f)\n"
	(match txt_loc with Text_after -> "after" | _ -> "before")
	x y;
      txt_loc, x, y
  | loc ->
      let w, h = dimensions style ctx datasets in
      let txt_loc = match loc with
	| Upper_left | Lower_left -> Text_after
	| _ -> Text_before
      and x_loc = match loc with
	| Upper_left | Lower_left -> dst.x_min
	| _ -> dst.x_max -. w
      and y_loc = match loc with
	| Upper_left | Upper_right -> dst.y_max
	| _ -> dst.y_min -. h
      in
	vprintf verb_debug "legend location: text %s icons (%f, %f)\n"
	  (match txt_loc with Text_after -> "after" | _ -> "before")
	  x_loc y_loc;
	txt_loc, x_loc, y_loc


(** [sort_cmp a b] comparison function used to sort the legends. *)
let sort_cmp src a b =
  let ay, _ = a#mean_y_value src
  and by, _ = b#mean_y_value src in
    match classify_float ay, classify_float by with
      | FP_nan, FP_nan -> 0
      | _, FP_nan -> 1
      | FP_nan, _ -> ~-1
      | _, _ ->
	  if ay > by then ~-1 else if ay < by then 1 else 0


let do_draw_vertical ctx text_loc style datasets =
  let padding = ctx.units padding in
  let text_width, icon_width = max_widths ctx style datasets in
  let width = text_width +. icon_width +. padding in
  let entry_height = max_height ctx style datasets in
  let _ (* height *) =
    List.fold_left
      (fun y_top ds -> match ds#name with
	 | None -> y_top
	 | Some txt ->
	     let tw, _ = text_dimensions ctx ~style txt in
	     let y = y_top +. (entry_height /. 2.) in
	     let tx, ix = match text_loc with
	       | Text_before ->
		   (width -. padding -. icon_width -. (tw /. 2.),
		    width -. (icon_width /. 2.))
	       | Text_after ->
		   (icon_width +. padding +. (tw /. 2.),
		    icon_width /. 2.)
	     in
	       draw_text_line ctx ~style ~center:tx ~top:y_top txt;
	       ds#draw_legend ctx ~x:ix ~y;
	       y_top +. entry_height)
      0. datasets in
    ()


(** [draw_legend ctx sort src text_loc style datasets] draws the
    legend into the upper right corner of the unit square. *)
let draw ctx sort src text_loc style datasets =
  let datasets =
    (* Possibly sort the datasets in descending order. *)
    if sort then List.sort (sort_cmp src) datasets else datasets
  in
    do_draw_vertical ctx text_loc style datasets


let do_draw_horizontal ctx text_loc style datasets =
  let padding = ctx.units padding in
(*
  let text_width, icon_width = max_widths ctx style datasets in
*)
  let entry_height = max_height ctx style datasets in
  let _ (* height *) =
    List.fold_left
      (fun x_start ds -> match ds#name with
	 | None -> x_start
	 | Some txt ->
	     let tw, _ = text_dimensions ctx ~style txt in
	     let iw, _ = ds#legend_dimensions ctx in
	     let y = entry_height /. 2. in
	     let tx, ix = match text_loc with
	       | Text_before ->
		   x_start +. tw /. 2.,
		   x_start +. tw +. padding +. iw /. 2.
	       | Text_after ->
		   x_start +. iw +. padding +. tw /. 2.,
		   x_start +. iw /. 2.
	     in
	       draw_text_line ctx ~style ~center:tx ~top:0. txt;
	       ds#draw_legend ctx ~x:ix ~y;
	       x_start +. tw +. iw +. 4. *. padding)
      0. datasets in
    ()

