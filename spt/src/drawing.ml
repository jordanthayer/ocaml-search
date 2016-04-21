(** Some basic Cairo drawing primitives.

    @author eaburns
    @since 2010-04-19
*)


open Geometry


type context = {
  cairo : Cairo.t;
  units : Length.t -> float;
  width : Length.t;
  height : Length.t;
}

(** [drawing_context cairo units ~w ~h] creates a new drawing
    context.  [cairo] is the Cairo.t context. *)
let drawing_context cairo units ~w ~h =
  {
    cairo = cairo;
    units = units;
    width = w;
    height = h;
  }

(** {1 Transforms} *)

let save_transforms ctx = Cairo.save ctx.cairo

let restore_transforms ctx = Cairo.restore ctx.cairo

let translate ctx x y = Cairo.translate ctx.cairo x y

let scale ctx x y = Cairo.scale ctx.cairo x y

let rotate ctx theta = Cairo.rotate ctx.cairo theta


(** {1 Color} *)

type color = { r : float; g : float; b : float; a : float}


let string_of_color c =
  Printf.sprintf "r=%f,g=%f,b=%f,a=%f" c.r c.g c.b c.a

let font_slant_normal = Cairo.FONT_SLANT_NORMAL

let font_weight_normal = Cairo.FONT_WEIGHT_NORMAL

let black = { r = 0.; g = 0.; b = 0.; a = 1.; }

let white = { r = 1.; g = 1.; b = 1.; a = 1.; }

let red = { r = 1.; g = 0.; b = 0.; a = 1.; }

let dark_red = { r = 0.8; g = 0.; b = 0.; a = 1.; }

let green = { r = 0.; g = 1.; b = 0.; a = 1.; }

let dark_green = { r = 0.; g = 0.6; b = 0.; a = 1.; }

let blue = { r = 0.; g = 0.; b = 1.; a = 1.; }

let dark_blue = { r = 0.; g = 0.; b = 0.6; a = 1.; }

let gray = { r = 0.5; g = 0.5; b = 0.5; a = 1.; }

let purple = { r = 0.5; g = 0.; b = 0.5; a = 1.; }

let lavender = {r = 0.9; g = 0.9; b = 0.98; a = 1.; }

let fuchsia = { r = 1.0; g = 0.; b = 1.; a = 1.; }

let mustard = {r = 0.8; g = 0.8; b = 0.2; a = 1.; }

let dark_orange = {r = 1.0; g = 0.54; b = 0.; a = 1.; }

let dark_slate_blue = { r = 0.28; g = 0.24; b = 0.55; a = 1. }

let slate_blue = { r = 0.0; g = 0.5; b = 1.0; a = 1. }

let olive_drab = { r = 0.42; g = 0.56; b = 0.14; a = 1. }

let pink = { r = 1.; g = 0.; b = 0.4; a = 1.; }

let limeish = { r = 0.25; g = 1.; b = 0.25; a = 1.; }

let skyish = { r = 0.25; g = 0.8; b = 0.8; a = 1. }

(** [color ?a ~r ~g ~b] makes a new color. *)
let color ?(a=1.0) ~r ~g ~b = { r = r; g = g; b = b; a = a }


(** [set_color ctx color] sets the current color. *)
let set_color ctx color =
  Cairo.set_source_rgba ctx.cairo color.r color.g color.b color.a


(** {1 Text} *)

type text_style = {
  text_font : string;
  text_size : Length.t;
  text_slant : Cairo.font_slant;
  text_weight : Cairo.font_weight;
  text_color : color;
}

(** [set_text_stlye ctx style] sets the text style. *)
let set_text_style ctx style =
  set_color ctx style.text_color;
  Cairo.select_font_face ctx.cairo
    style.text_font style.text_slant style.text_weight;
  Cairo.set_font_size ctx.cairo (ctx.units style.text_size)


(** [set_text_style_option ctx style] sets the style if there was
    one specified. *)
let set_text_style_option ctx = function
  | None -> ()
  | Some style -> set_text_style ctx style


(** [draw_text ctx ?style ?angle ~x ~y str] displays the text at the
    given center point. *)
let draw_text ctx ?style ?(angle=0.) ~x ~y str =
  set_text_style_option ctx style;
  let te = Cairo.text_extents ctx.cairo str in
  let w = te.Cairo.text_width and h = te.Cairo.text_height in
  let x_offs = te.Cairo.x_bearing and y_offs = te.Cairo.y_bearing in
    Cairo.save ctx.cairo;
    Cairo.move_to ctx.cairo 0. 0.;
    Cairo.translate ctx.cairo x y;
    Cairo.rotate ctx.cairo (angle *. (pi /. 180.));
    Cairo.move_to ctx.cairo ~-.((w /. 2.) +. x_offs) ~-.((h /. 2.) +. y_offs);
    Cairo.show_text ctx.cairo str;
    Cairo.restore ctx.cairo


(** [text_dimensions ctx ?style str] gets the dimensions of the
    text.  *)
let text_dimensions ctx ?style str =
  set_text_style_option ctx style;
  let te = Cairo.text_extents ctx.cairo str in
    te.Cairo.text_width, te.Cairo.text_height


(** [font_suggested_line_height ?style ctx] gets the suggested line
    height for the given font.  This is useful when adding multiple
    lines of text.  If the text dimensions are used instead the
    spacing between different lines may not be uniform. *)
let font_suggested_line_height ?style ctx =
  set_text_style_option ctx style;
  let fe = Cairo.font_extents ctx.cairo in
    fe.Cairo.font_height


(** [text_rectangle ctx ~style ?pt txt] gets the bounding box around
    the given text optionally given its location. *)
let text_rectangle ctx ~style ?(pt=point 0. 0.) txt =
  let w, h = text_dimensions ctx ~style txt in
  let half_w = w /. 2. and half_h = h /. 2. in
  let x = pt.x and y = pt.y in
  let x_min = x -. half_w
  and x_max = x +. half_w
  and y_max = y -. half_h
  and y_min = y +. half_h in
    rectangle ~x_min ~x_max ~y_min ~y_max


(** [draw_text_centered_below ctx ?style ?angle x y str] draws the
    given string centered below the given location. *)
let draw_text_centered_below ctx ?style ?(angle=0.) x y str =
  set_text_style_option ctx style;
  let te = Cairo.text_extents ctx.cairo str in
    draw_text ctx ~angle ~x ~y:(y +. te.Cairo.text_height /. 2.) str


(** [draw_text_centered_above ctx ?style ?angle x y str] draws the
    given string centered above the given location. *)
let draw_text_centered_above ctx ?style ?(angle=0.) x y str =
  set_text_style_option ctx style;
  let te = Cairo.text_extents ctx.cairo str in
    draw_text ctx ~angle ~x ~y:(y -. te.Cairo.text_height /. 2.) str


(** [draw_text_centered_before ctx ?style ?angle x y str] draws the
    given string centered before the given location. *)
let draw_text_centered_before ctx ?style ?(angle=0.) x y str =
  set_text_style_option ctx style;
  let te = Cairo.text_extents ctx.cairo str in
    draw_text ctx ~angle ~x:(x -. te.Cairo.text_width /. 2.) ~y str


(** [draw_text_centered_after ctx ?style ?angle x y str] draws the
    given string centered after the given location. *)
let draw_text_centered_after ctx ?style ?(angle=0.) x y str =
  set_text_style_option ctx style;
  let te = Cairo.text_extents ctx.cairo str in
    draw_text ctx ~angle ~x:(x +. te.Cairo.text_width /. 2.) ~y str


(** {2 Formatted text} *)

(** [drawf ctx ?style ?angle x y fmt] displays the formatted text at
    the given center point. *)
let drawf ctx ?style ?(angle=0.) x y fmt =
  Printf.kprintf (draw_text ctx ?style ~angle ~x:x ~y:y) fmt


(** [dimensionsf ctx ?style fmt] gets the dimensions of the
    formatted text and returns the string.  *)
let dimensionsf ctx ?style fmt =
  let str_and_dims str =
    let w, h = text_dimensions ctx ?style str in
      str, w, h
  in Printf.kprintf str_and_dims fmt


(** {2 Fixed width text} *)


(** [hypenate_word ctx width word] hyphenates a word that is too
    long to fit across the given width. *)
let hypenate_word ctx width word =
  let partition word i =
    let fst = String.sub word 0 (i + 1) in
    let snd = String.sub word (i + 1) ((String.length word) - i - 1) in
      fst, snd
  in
  let n = String.length word in
  let rec do_hyphenate i =
    if i < n
    then begin
      let proposed, _ = partition word i in
      let w, h = text_dimensions ctx (proposed ^ "-") in
	if w > width
	then begin
	  let fst, snd = partition word (i - 1) in
	    fst ^ "-", snd
	end else do_hyphenate (i + 1)
    end else word, ""
  in do_hyphenate 1


(** [fixed_width_lines ctx width string] gets a list of lines that
    that will display within the given width.  Assumes the given
    style has already been set. *)
let fixed_width_lines ctx width string =
  let rec get_line accum cur_line = function
    | [] -> List.rev (cur_line :: accum)
    | hd :: tl when cur_line = "" ->
	let w, _ = text_dimensions ctx hd in
	  if w > width
	  then begin
	    let first, last = hypenate_word ctx width hd in
	      get_line (first :: accum) "" (last :: tl)
	  end else get_line accum hd tl
    | (hd :: tl) as words ->
	let proposed_line = cur_line ^ " " ^ hd in
	let w, h = text_dimensions ctx proposed_line in
	  if w > width
	  then get_line (cur_line :: accum) "" words
	  else get_line accum proposed_line tl
  in
  let words = Str.split (Str.regexp "[ \t\n\r]+") string in
    get_line [] "" words


(** [fixed_width_text_height ctx ?style width string] gets the
    height of the fixed width text.  *)
let fixed_width_text_height ctx ?style width string =
  set_text_style_option ctx style;
  let line_height = font_suggested_line_height ctx in
  let lines = fixed_width_lines ctx width string in
    line_height *. (float (List.length lines))


type text_alignment = Centered_text | Left_aligned_text | Right_aligned_text

(** [draw_text_line ctx ?style ~center ~top string] draws a line of
    text.  This function makes lines of text look better than the
    general draw_text_centered_XXX routines because this function
    does vertical alignment based on the font instead of the text
    string. *)
let draw_text_line ctx ?style ~center ~top string =
  set_text_style_option ctx style;
  let te = Cairo.text_extents ctx.cairo string in
  let fe = Cairo.font_extents ctx.cairo in
  let x_bearing = te.Cairo.x_bearing and w = te.Cairo.text_width in
  let font_height = fe.Cairo.font_height and descent = fe.Cairo.descent in
  let x = center -. (w /. 2.) -. x_bearing
  and y = top +. font_height -. descent in
    Cairo.move_to ctx.cairo x y;
    Cairo.show_text ctx.cairo string



(** [draw_fixed_width_text ctx ?style ~x ~y ~width string] displays
    the given fixed-width text where [x], [y] is the location of the
    top center. *)
let draw_fixed_width_text ctx ?style ~x ~y ~width string =
  set_text_style_option ctx style;
  let line_height = font_suggested_line_height ctx in
  let lines = fixed_width_lines ctx width string in
    ignore (List.fold_left (fun y line ->
			      draw_text_line ctx ~center:x ~top:y line;
			      y +. line_height)
	      y lines)

(** {1 Lines} *)


type line_style = {
  line_color : color;
  line_dashes : Length.t array;
  line_width : Length.t;
}

(** The default line style. *)
let default_line_style =
  {
    line_color = black;
    line_width = Length.Pt 1.;
    line_dashes = [||];
  }


(** [set_line_style ctx style] sets the line style. *)
let set_line_style ctx style =
  let dashes = Array.map ctx.units style.line_dashes in
    set_color ctx style.line_color;
    Cairo.set_dash ctx.cairo dashes 0.;
    Cairo.set_line_width ctx.cairo (ctx.units style.line_width)


(** [set_line_style_option ctx style] sets the line style if there
    is one *)
let set_line_style_option ctx = function
  | None -> ()
  | Some style -> set_line_style ctx style


(** [draw_line ctx ?box ?tr ?close_path ?style points] draws the
    given line optionally within the given bounding
    box. [close_path], if set to true, closes the path before
    drawing the stroke. *)
let draw_line ctx ?box ?(tr=(fun x -> x)) ?(close_path=false) ?style points =
  set_line_style_option ctx style;
  begin match box with
    | None ->
	begin match points with
	  | [] -> ()
	  | p :: tl ->
	      let p = tr p in
		Cairo.new_path ctx.cairo;
		Cairo.move_to ctx.cairo p.x p.y;
		List.iter (fun p ->
			     let p = tr p in
			       Cairo.line_to ctx.cairo p.x p.y) tl;
		if close_path then Cairo.close_path ctx.cairo;
		Cairo.stroke ctx.cairo;
	end;
    | Some box ->
	begin match clip_line box points with
	  | [] -> ()
	  | lst ->
	      List.iter
		(function
		   | [] | _ :: [] -> ()
		   | p :: ps ->
		       let p' = tr p in
			 Cairo.new_path ctx.cairo;
			 Cairo.move_to ctx.cairo p'.x p'.y;
			 List.iter (fun p ->
				      let p' = tr p in
					Cairo.line_to ctx.cairo p'.x p'.y)
			   ps;
			 if close_path then Cairo.close_path ctx.cairo;
			 Cairo.stroke ctx.cairo;)
		lst;
	end;
  end;
  Cairo.set_dash ctx.cairo [| |] 0.


(** {1 Points} *)

type glyph =
  | Circle_glyph
  | Ring_glyph
  | Cross_glyph
  | Plus_glyph
  | Square_glyph
  | Box_glyph
  | Triangle_glyph
  | Char_glyph of char
  | Point_glyph


(** The default line width when drawing glyphs. *)
let default_glyph_line_width = Length.Pt 0.5


let glyph_of_string = function
  | "circle" -> Circle_glyph
  | "ring" -> Ring_glyph
  | "cross" -> Cross_glyph
  | "plus" -> Plus_glyph
  | "square" -> Square_glyph
  | "box" -> Box_glyph
  | "triangle" -> Triangle_glyph
  | c when (String.length c) = 1 -> Char_glyph c.[0]
  | s -> failwith (Printf.sprintf "Invalid glyph name %s" s)


(** [make_draw_glyph ctx radius glyph] makes a function draws the
    given glyph.  Assumes that the text width and line width won't
    change between creating the draw_glyph function and its
    use. *)
let make_draw_glyph ctx radius = function
  | Point_glyph ->
      Cairo.set_line_width ctx.cairo (ctx.units default_glyph_line_width);
      (fun pt ->
	 let radius = 0.5 in
	 Cairo.new_path ctx.cairo;
	 Cairo.arc ctx.cairo pt.x pt.y radius 0. (2. *. pi);
	 Cairo.fill ctx.cairo)

  | Circle_glyph ->
      Cairo.set_line_width ctx.cairo (ctx.units default_glyph_line_width);
      (fun pt ->
	 Cairo.new_path ctx.cairo;
	 Cairo.arc ctx.cairo pt.x pt.y radius 0. (2. *. pi);
	 Cairo.fill ctx.cairo)
  | Ring_glyph ->
      Cairo.set_line_width ctx.cairo (ctx.units default_glyph_line_width);
      (fun pt ->
	 Cairo.new_path ctx.cairo;
	 Cairo.arc ctx.cairo pt.x pt.y radius 0. (2. *. pi);
	 Cairo.stroke ctx.cairo)
  | Cross_glyph ->
      let r = radius *. (sin (pi /. 4.)) in
	Cairo.set_line_width ctx.cairo (ctx.units default_glyph_line_width);
	(fun pt ->
	   let x = pt.x and y = pt.y in
	     Cairo.new_path ctx.cairo;
	     Cairo.move_to ctx.cairo (x -. r) (y +. r);
	     Cairo.line_to ctx.cairo (x +. r) (y -. r);
	     Cairo.move_to ctx.cairo (x -. r) (y -. r);
	     Cairo.line_to ctx.cairo (x +. r) (y +. r);
	     Cairo.stroke ctx.cairo)
  | Plus_glyph ->
      (fun pt ->
	 Cairo.set_line_width ctx.cairo (ctx.units default_glyph_line_width);
	 let x = pt.x and y = pt.y in
	   Cairo.new_path ctx.cairo;
	   Cairo.move_to ctx.cairo (x -. radius) y;
	   Cairo.line_to ctx.cairo (x +. radius) y;
	   Cairo.move_to ctx.cairo x (y -. radius);
	   Cairo.line_to ctx.cairo x (y +. radius);
	   Cairo.stroke ctx.cairo)
  | Box_glyph ->
      let r = radius *. (sin (pi /. 4.)) in
      let r2 = r *. 2. in
	Cairo.set_line_width ctx.cairo (ctx.units default_glyph_line_width);
	(fun pt ->
	   let x = pt.x and y = pt.y in
	     Cairo.rectangle ctx.cairo (x -. r) (y -. r) r2 r2;
	     Cairo.stroke ctx.cairo)
  | Square_glyph ->
      let r = radius *. (sin (pi /. 4.)) in
      let r2 = r *. 2. in
	Cairo.set_line_width ctx.cairo (ctx.units default_glyph_line_width);
	(fun pt ->
	   let x = pt.x and y = pt.y in
	     Cairo.rectangle ctx.cairo (x -. r) (y -. r) r2 r2;
	     Cairo.fill ctx.cairo)
  | Triangle_glyph ->
      Cairo.set_line_width ctx.cairo (ctx.units default_glyph_line_width);
      let s = radius *. (sin (pi /. 6.)) in
      let c = radius *. (cos (pi /. 6.)) in
	(fun pt ->
	   let x = pt.x and y = pt.y in
	     Cairo.new_path ctx.cairo;
	     Cairo.move_to ctx.cairo x (y -. radius);
	     Cairo.line_to ctx.cairo (x +. c) (y +. s);
	     Cairo.line_to ctx.cairo (x -. c) (y +. s);
	     Cairo.line_to ctx.cairo x (y -. radius);
	     Cairo.stroke ctx.cairo)
  | Char_glyph ch ->
      let str = " " in
	str.[0] <- ch;
	Cairo.select_font_face ctx.cairo
	  "Sans Serif" font_slant_normal font_weight_normal;
	Cairo.set_font_size ctx.cairo (radius *. 2.);
	(fun pt -> draw_text ctx pt.x pt.y str)


(** [draw_point ctx ?color radius glyph pt] draws a single point. *)
let draw_point ctx ?color radius glyph pt =
  begin match color with
    | None -> ()
    | Some color -> set_color ctx color
  end;
  make_draw_glyph ctx (ctx.units radius) glyph pt


(** [draw_points ctx ?color radius glyph points] draws a set of
    points.  This is a slightly faster way of drawing points that
    all have the same radius. *)
let draw_points ctx ?color radius glyph points =
  begin match color with
    | None -> ()
    | Some color -> set_color ctx color;
  end;
  let draw_pt = make_draw_glyph ctx (ctx.units radius) glyph in
    List.iter draw_pt points

(** {1 Rectangles} *)

type fill_pattern =
  | No_fill
  | Patterned_solid_fill of color * fill_pattern
  | Solid_fill of color
  | Vertical_fill of line_style * Length.t
  | Horizontal_fill of line_style * Length.t
  | Diagonal_fill of line_style * Length.t * Length.t
  | Hash_fill of line_style * Length.t * Length.t
  | Dotted_fill of color * Length.t * Length.t


(** [draw_solid_fill ctx r color] draws a solid fill *)
let draw_solid_fill ctx r color =
  set_color ctx color;
  Cairo.rectangle ctx.cairo
    r.x_min r.y_min (r.x_max -. r.x_min) (r.y_max -. r.y_min);
  Cairo.fill ctx.cairo


(** [draw_vertical_fill ctx r style delta] draws a fill of vertical
    bars. *)
let draw_vertical_fill ctx r style delta =
  set_line_style ctx style;
  let delta = ctx.units delta in
  let x = ref (r.x_min +. delta) in
    while !x < r.x_max do
      draw_line ctx [ point !x r.y_min; point !x r.y_max ];
      x := !x +. delta
    done


(** [draw_horizontal_fill ctx r style delta] draws a fill of
    horizontal bars. *)
let draw_horizontal_fill ctx r style delta =
  set_line_style ctx style;
  let y_min, y_max =
    if r.y_min < r.y_max then r.y_min, r.y_max else r.y_max, r.y_min
  in
  let delta = ctx.units delta in
  let y = ref (y_min +. delta) in
    while !y < y_max do
      draw_line ctx [ point r.x_min !y; point r.x_max !y ];
      y := !y +. delta
    done


(** [draw_diagonal_fill ctx r style slope delta] draws a fill of
    diagonal lines. *)
let draw_diagonal_fill ctx r style slope delta =
  set_line_style ctx style;
  let delta = ctx.units delta in
  let slope = ctx.units slope in
  let y_min, y_max =
    if r.y_min < r.y_max then r.y_min, r.y_max else r.y_max, r.y_min
  in
  let y_diff = (r.x_max -. r.x_min) *. slope in
  let y0, y1 =
    if slope > 0.
    then ref (y_min +. delta), ref (y_min +. delta -. y_diff)
    else ref (y_min +. delta +. y_diff), ref (y_min +. delta)
  in
    while !y0 < y_max || !y1 < y_max do
      draw_line ctx ~box:r [ point r.x_min !y0; point r.x_max !y1 ];
      y0 := !y0 +. delta;
      y1 := !y1 +. delta;
    done


(** [draw_dotted_fill ctx r color radius delta] draws a fill of
    dots. *)
let draw_dotted_fill ctx r color radius delta =
  set_color ctx color;
  let radius = ctx.units radius in
  let delta = ctx.units delta in
  let draw_dot = make_draw_glyph ctx radius Circle_glyph in
  let y_min, y_max =
    if r.y_min < r.y_max then r.y_min, r.y_max else r.y_max, r.y_min in
  let x_min, x_max =
    if r.x_min < r.x_max then r.x_min, r.x_max else r.x_max, r.x_min in
  let x = ref (x_min +. (2. *. radius)) in
    while !x <= (x_max -. radius) do
      let y = ref (y_min +. (2. *. radius)) in
	while !y <= (y_max -. radius) do
	  draw_dot (point !x !y);
	  y := !y +. delta;
	done;
	x := !x +. delta;
    done


(** [draw_fill_pattern ctx r] draws the given fill pattern in the
    rectangle. *)
let rec draw_fill_pattern ctx r = function
  | No_fill -> ()
  | Solid_fill c -> draw_solid_fill ctx r c
  | Vertical_fill (style, delta) -> draw_vertical_fill ctx r style delta
  | Horizontal_fill (style, delta) -> draw_horizontal_fill ctx r style delta
  | Diagonal_fill (style, slope, delta) ->
      draw_diagonal_fill ctx r style slope delta
  | Hash_fill (style, slope, delta) ->
      let s = Length.as_pt slope in
	draw_diagonal_fill ctx r style slope delta;
	draw_diagonal_fill ctx r style (Length.Pt ~-.s) delta;
  | Dotted_fill (color, radius, delta) ->
      draw_dotted_fill ctx r color radius delta
  | Patterned_solid_fill (color, pattern) ->
      match pattern with
	| Patterned_solid_fill (_, _) | Solid_fill _ ->
	    invalid_arg ("Patterned_solid_fill: pattern cannot be one of: "
			 ^ "Patterned_solid_fill or Solid_fill")
	| x ->
	    draw_solid_fill ctx r color;
	    draw_fill_pattern ctx r pattern


(** [draw_rectangle ctx ?style r] draws the given rectangle. *)
let draw_rectangle ctx ?style r =
  set_line_style_option ctx style;
  Cairo.rectangle ctx.cairo
    r.x_min r.y_min (r.x_max -. r.x_min) (r.y_max -. r.y_min);
  Cairo.stroke ctx.cairo


(** [draw_rectangle ctx color r] draws the given rectangle. *)
let fill_rectangle ctx ?(color=black) r =
  set_color ctx color;
  Cairo.rectangle ctx.cairo
    r.x_min r.y_min (r.x_max -. r.x_min) (r.y_max -. r.y_min);
  Cairo.fill ctx.cairo


(** {6 Circles} *)

(** [fill_circle ctx center r color] draws a filled in circle. *)
let fill_circle ctx center r color =
  let cairo = ctx.cairo in
  let cx = center.x and cy = center.y in
    set_color ctx color;
    Cairo.new_path cairo;
    Cairo.arc cairo cx cy r 0. (2. *. pi);
    Cairo.fill cairo


(** [slice_path ctx center ~r ~t ~dt ] makes the path for a slice.
    (see [draw_silce] and [fill_slice]. *)
let slice_path ctx center ~r ~t ~dt =
  let cairo = ctx.cairo in
  let cx = center.x and cy = center.y in
    Cairo.new_path cairo;
    if dt < two_pi
    then Cairo.move_to cairo cx cy;
    Cairo.arc cairo cx cy r t (t +. dt);
    Cairo.close_path cairo


(** [draw_sector ctx ?style center ~r ~t ~dt color] draws an outline
    around a pie-slice centered at [center] with a radius [r]
    beginning at angle [t] (in radians) and continuing for [dt]
    radians. *)
let draw_slice ctx ?style center ~r ~t ~dt color =
  set_line_style_option ctx style;
  set_color ctx color;
  slice_path ctx center ~r ~t ~dt;
  Cairo.stroke ctx.cairo


(** [fill_sector ctx center ~r ~t ~d color] fills in a pie-slice
    centered at [center] with a radius [r] beginning at angle [t]
    (in radians) and continuing for [dt] radians. *)
let fill_slice ctx center ~r ~t ~dt color =
  set_color ctx color;
  slice_path ctx center ~r ~t ~dt;
  Cairo.fill ctx.cairo


(** [sector_path ctx center ~r ~dr ~t ~dt] builds the path for a
    sector of a circle beginning at radius [r] at a width of [dr]
    beginning at angle [t] for a span of [dt]. *)
let sector_path ctx center ~r ~dr ~t ~dt =
  let cairo = ctx.cairo in
  let cx = center.x and cy = center.y in
  let r0 = r and r1 = r +. dr and t0 = t and t1 = t +. dt in
    Cairo.new_path cairo;
    Cairo.arc cairo cx cy r0 t0 t1;
    Cairo.arc_negative cairo cx cy r1 t1 t0;
    Cairo.close_path cairo


(** [fill_sector ctx center ~r ~dr ~t ~dt color] fills a sector of a
    circle beginning at radius [r] at a width of [dr] beginning at
    angle [t] for a span of [dt]. *)
let fill_sector ctx center ~r ~dr ~t ~dt color =
  set_color ctx color;
  sector_path ctx center ~r ~dr ~t ~dt;
  Cairo.fill ctx.cairo


(** [draw_sector ctx ?style center ~r ~dr ~t ~dt color] draws a sector
    of a circle beginning at radius [r] at a width of [dr] beginning
    at angle [t] for a span of [dt]. *)
let draw_sector ctx ?style center ~r ~dr ~t ~dt color =
  set_line_style_option ctx style;
  set_color ctx color;
  sector_path ctx center ~r ~dr ~t ~dt;
  Cairo.stroke ctx.cairo
