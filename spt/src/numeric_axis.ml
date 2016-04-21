(** Drawing of a numeric axis.

    @author eaburns
    @since 2010-04-24
*)

open Printf
open Geometry
open Drawing
open Verbosity

type axis = {
  src : range;
  ticks : (float * string option) list;
  label : string option;
  label_text_style : text_style;
  tick_text_style : text_style;
}

(** The line style of a numeric axis. *)
let axis_style =
  {
    line_color = black;
    line_dashes = [| |];
    line_width = Length.Pt 1.;
  }


(** The length of a tick mark. *)
let tick_length = Length.Pt 5.


(** The padding between a tick mark and the text labeling it. *)
let tick_text_pad = Length.Pt ((Length.as_pt tick_length) *. 0.5)


(** The line style of a tick mark. *)
let tick_style =
  {
    line_color = black;
    line_dashes = [| |];
    line_width = Length.Pt 0.5;
  }

(** [create ~label_text_style ~tick_text_style ~src ticks label]
    creates a new axis. *)
let create ~label_text_style ~tick_text_style ~src ticks label =
  {
    label = label;
    src = src;
    ticks = ticks;
    label_text_style = label_text_style;
    tick_text_style = tick_text_style;
  }


(** [recommended_ticks length] gets the recommended number of tick
    marks. *)
let recommended_ticks length =
  let n = (Length.as_cm length) *. (1. /. 4.) in
    if n < 2. then 2. else n


(** {1 Tick marks} *)

let sprintf_float n =
  sprintf "%g" n
(*
  let str = sprintf "%f" n in
  let non_zero = ref ((String.length str) - 1) in
    while str.[!non_zero] = '0' do decr non_zero done;
    if str.[!non_zero] = '.' then decr non_zero;
    String.sub str 0 (!non_zero + 1)
*)


(** [tick_list ~delta ~min ~max] gets a list of the tick marks. *)
let rec tick_list ~delta ~min ~max =
  let fst = (floor (min /. delta)) *. delta in
  let next = ref fst in
  let count = ref 0. in
  let lst = ref [] in
    while !next <= max do
      let n = !next in
	if n >= min && n <= max
	then begin
	  let t = n, Some (sprintf_float n) in
	    lst := t :: !lst;
	end;
	count := !count +. 1.;
	next := fst +. (!count *. delta);
    done;
    !lst


(** [tick_locations ?suggested_number rng] computes the location of
    tick marks on a numeric axis with the given range.
    [suggested_number] is the suggested number of major tick
    marks. *)
let tick_locations ?(suggested_number=2.) rng =
  let nticks = suggested_number in
  let min = rng.min and max = rng.max in
  let tens = 10. ** (floor (log10 ((max -. min) /. nticks))) in
  let ntens = (max -. min) /. tens in
  let multiple, minor_fact =
    let m = match truncate (ntens /. nticks) with
      | 7 -> 6
      | 9 -> 8
      | x -> x in
    let f = match m with
      | 3 | 6 -> 1. /. 3.
      | 5 -> 1. /. 5.
      | 7 | 9 -> assert false
      | x -> 1. /. 2.
    in float m, f
  in
  let delta = multiple *. tens in
  let major_ticks = tick_list ~delta ~min ~max in
  let minor_ticks' = tick_list ~delta:(delta *. minor_fact) ~min ~max in
  let minor_ticks =
    List.fold_left (fun l (v, s) ->
		      if List.exists (fun (_, r) -> r = s) major_ticks
		      then l
		      else (v, None) :: l)
      [] minor_ticks'
  in
    vprintf verb_debug "\n\nmin=%g, max=%g\n" min max;
    vprintf verb_debug "suggested=%g, nmajor=%d, nminor=%d\n"
      suggested_number (List.length major_ticks) (List.length minor_ticks);
    vprintf verb_debug "multiple=%g, minor_fact=%g\n" multiple minor_fact;
    vprintf verb_debug "delta=%g, minor_delta=%g\n"
      delta (delta *. minor_fact);
    major_ticks @ minor_ticks


(** [max_tick_text_width ctx style ticks] gets the maximum width of
    the text for the given ticks. *)
let max_tick_text_width ctx style ticks =
  List.fold_left (fun m (_, txt_opt) ->
		    match txt_opt with
		      | None -> m
		      | Some txt ->
			  let w = fst (text_dimensions ctx ~style txt) in
			    if w > m then w else m)
    0. ticks

(** [max_tick_text_height ctx style ticks] gets the maximum height of
    the text for the given ticks. *)
let max_tick_text_height ctx style ticks =
  List.fold_left (fun m (_, txt_opt) ->
		    match txt_opt with
		      | None -> m
		      | Some txt ->
			  let h = snd (text_dimensions ctx ~style txt) in
			    if h > m then h else m)
    0. ticks


(** {1 Drawing an x-axis} *)


let max_major_tick ticks =
  List.fold_left (fun ((vl, _) as max_tick) tick -> match tick with
		    | (v, Some _) -> if v > vl then tick else max_tick
		    | (_, None) -> max_tick)
    (0., None) ticks


(** [resize_for_x_axis ctx ~pad ~y_min ~dst axis] gets the new scale
    after making room for the x-axis tick marks and label.  [pad] is
    the padding between text.  The result is a new (y_min * x_max)
    that will have room for the x-axis and the x-tick label text. *)
let resize_for_x_axis ctx ~pad ~y_min ~dst axis =
  let tick_length = ctx.units tick_length in
  let tick_text_style = axis.tick_text_style in
  let tick_text_pad = ctx.units tick_text_pad in
  let label_room =
    match axis.label with
      | None -> 0.
      | Some label ->
	  snd (text_dimensions ctx ~style:axis.label_text_style label)
  in
  let ticks_with_text = List.filter (fun (_, t) -> t <> None) axis.ticks in
  let x_tick_txt_height =
    match ticks_with_text with
      | (_,  Some txt) :: tl ->
	  snd (text_dimensions ctx ~style:tick_text_style txt)
      | _ -> 0. in
  let y_min' =
    y_min
    -. label_room -. pad
    -. tick_length -. tick_text_pad -. x_tick_txt_height
  in
  let max_vl, max_txt = max_major_tick axis.ticks in
  let x_max' = match max_txt with
    | None -> dst.max
    | Some max_txt ->
	let width, _ = text_dimensions ctx ~style:tick_text_style max_txt in
	let half_w = ceil (width /. 2.) in
	let tr = range_transform ~src:axis.src ~dst in
	let tick_max = (tr max_vl) +. half_w in
	let over = if tick_max > dst.max then tick_max -. dst.max else 0. in
	  if over > 0. then
	    let tgt = dst.max -. half_w in
	    let max' = Geometry.find_new_dmax ~src:axis.src ~dst max_vl tgt in
	      max'
	  else dst.max
  in
    y_min', x_max'


(** [draw_x_tick ctx style ~y scale t] draws an x-tick with the top at
    the given [y] location.. *)
let draw_x_tick ctx style ~y scale (vl, t_opt) =
  let tick_length = ctx.units tick_length in
  let tick_text_pad = ctx.units tick_text_pad in
  let x = scale vl in
  let len = if t_opt = None then tick_length /. 2. else tick_length in
    draw_line ctx ~style:tick_style [ point x y; point x (y +. len) ];
    begin match t_opt with
      | Some txt ->
	  draw_text_centered_below ctx ~style x
	    (y +. len +. tick_text_pad) txt;
      | None -> ()
    end


(** [draw_x_axis ctx ~pad ~height ~dst axis] draws an x-axis. [scale]
    is a function that converts an x-value in the original data
    coordinates to the destination x-coordinate system. *)
let draw_x_axis ctx ~pad ~height ~dst axis =
  let tick_length = ctx.units tick_length in
  let tick_text_pad = ctx.units tick_text_pad in
  let tick_text_height =
    max_tick_text_height ctx axis.tick_text_style axis.ticks in
  let tr = range_transform ~src:axis.src ~dst in
  let h = match axis.label with
    | None -> 0.
    | Some label ->
	let h = snd (text_dimensions ctx ~style:axis.label_text_style label) in
	let x = (dst.max +. dst.min) /. 2. in
	  draw_text_centered_above ctx x height label;
	  h
  in
  let y' =
    height -. h -. pad -. tick_text_height -. tick_text_pad -. tick_length
  in
    List.iter (draw_x_tick ctx axis.tick_text_style ~y:y' tr) axis.ticks;
    draw_line ctx ~style:axis_style [ point dst.min y'; point dst.max y'; ]


(** {1 Drawing a y-axis} *)

(** [resize_for_y_axis ctx ~pad ~x_min axis] gets the new minimum and
    maximum x-values after making room for the y-axis tick marks and
    label.  [pad] is the padding between text. *)
let resize_for_y_axis ctx ~pad ~x_min axis =
  let tick_length = ctx.units tick_length in
  let tick_text_pad = ctx.units tick_text_pad in
  let label_room =
    match axis.label with
      | None -> 0.
      | Some label ->
	  snd (text_dimensions ctx ~style:axis.label_text_style label)
  in
  let tick_text_width =
    max_tick_text_width ctx axis.tick_text_style axis.ticks
  in
    x_min
    +. label_room +. pad
    +. tick_length +. tick_text_pad +. tick_text_width


(** [draw_y_tick ctx style ~x tr t] draws a y-tick with the left at
    the given [x] location.. *)
let draw_y_tick ctx style ~x tr (vl, t_opt) =
  let tick_length = ctx.units tick_length in
  let tick_text_pad = ctx.units tick_text_pad in
  let y = tr vl in
  let len = if t_opt = None then tick_length /. 2. else tick_length in
    draw_line ctx ~style:tick_style [ point x y; point (x -. len) y ];
    begin match t_opt with
      | Some txt ->
	  draw_text_centered_before ctx ~style
	    (x -. len -. tick_text_pad) y txt;
      | None -> ()
    end


(** [draw_y_axis ctx ~pad ~dst axis] draws a y-axis. *)
let draw_y_axis ctx ~pad ~dst axis =
  let tick_length = ctx.units tick_length in
  let tick_text_pad = ctx.units tick_text_pad in
  let tick_text_width =
    max_tick_text_width ctx axis.tick_text_style axis.ticks in
  let tr = range_transform ~src:axis.src ~dst in
  let h = match axis.label with
    | None -> 0.
    | Some label ->
	let h = snd (text_dimensions ctx ~style:axis.label_text_style label) in
	let y = (dst.max +. dst.min) /. 2. in
	  draw_text ctx ~angle:270. ~x:(h /. 2.) ~y label;
	  h
  in
  let x' = h +. pad +. tick_text_width +. tick_text_pad +. tick_length in
    List.iter (draw_y_tick ctx axis.tick_text_style ~x:x' tr) axis.ticks;
    draw_line ctx ~style:axis_style [ point x' dst.min; point x' dst.max; ]
