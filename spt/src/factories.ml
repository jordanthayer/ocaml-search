(**

    @author jtd7
    @since 2010-05-21
   Contains a bulk of the style, cloor, etc factories that the
   various plots are going to make use of

*)

open Drawing

(** [make_glyph_factory glyph_set ()] makes a glyph factory which
    returns a new glyph at each call. *)
let make_glyph_factory glyph_set () =
  let next = ref 0 in
  let n = Array.length glyph_set in
    (fun () ->
       let g = glyph_set.(!next) in
	 next := (!next + 1) mod n;
	 g)


(** [default_glyph_factory] gets the default glyph factory
    builder. *)
let default_glyph_factory =
  let glyph_set =
    [|
      Ring_glyph;
      Circle_glyph;
      Triangle_glyph;
      Box_glyph;
      Square_glyph;
      Cross_glyph;
      (*Plus_glyph;*)
    |]
  in make_glyph_factory glyph_set


(** [default_color_glyph_factory] gets the a glyph factory builder
    that looks good when the glyphs are colored. *)
let default_color_glyph_factory =
  let glyph_set =
    [|
      Ring_glyph;
      Triangle_glyph;
      Char_glyph 'S';
      Char_glyph 'H';
      Char_glyph '#';
      Box_glyph;
      (*Square_glyph;
	Circle_glyph;*)
      Cross_glyph;
    |]
  in make_glyph_factory glyph_set


(** [overlapping_glyph_factory] gets a glyph factory builder that
    makes glyphs that look good with overlapping data.

    This set is similar to the set recommended in ps_plot for
    overlapping data. *)
let overlapping_glyph_factory =
  let glyph_set =
    [|
      Plus_glyph;
      Char_glyph '<';
      Char_glyph 'S';
      Char_glyph 'W';
      Cross_glyph;
    |]
  in make_glyph_factory glyph_set


(** [numbered_glyph_factory] gets a glyph factory builder that
    returns numbers as the glyphs. *)
let numbered_glyph_factory =
  let glyph_set =
    [| Char_glyph '0';
       Char_glyph '1';
       Char_glyph '2';
       Char_glyph '3';
       Char_glyph '4';
       Char_glyph '5';
       Char_glyph '6';
       Char_glyph '7';
       Char_glyph '8';
       Char_glyph '9';
    |]
  in make_glyph_factory glyph_set


(** [make_dash_factory dash_set ()] makes a dash pattern factory. *)
let make_dash_factory dash_set () =
  let next = ref 0 in
  let n = Array.length dash_set in
    (fun () ->
       let d = dash_set.(!next) in
	 next := (!next + 1) mod n;
	 d)

(** [default_dash_factory] gets the default dash factory builder. *)
let default_dash_factory =
  let default_dash_set =
    [|
      [| |];
      [| Length.Pt 6.; Length.Pt 2.; |];
      [| Length.Pt 2.; Length.Pt 2.; |];
      [| Length.Pt 1.; Length.Pt 1.; |];
      [| Length.Pt 5.; Length.Pt 2.; Length.Pt 1.; Length.Pt 2.; |];
      [| Length.Pt 10.; Length.Pt 2.; Length.Pt 2.; Length.Pt 2.;
	 Length.Pt 2.; Length.Pt 2.; Length.Pt 2.; Length.Pt 2.; |];
      [| Length.Pt 10.; Length.Pt 2.; Length.Pt 2.; Length.Pt 2.; |];
      [| Length.Pt 5.; Length.Pt 2.; Length.Pt 5.; Length.Pt 2.;
	 Length.Pt 2.; Length.Pt 2.; Length.Pt 2.; Length.Pt 2.; |];
      [| Length.Pt 4.; Length.Pt 2.; Length.Pt 4.; Length.Pt 1.;
	 Length.Pt 1.; Length.Pt 1.; Length.Pt 1.; Length.Pt 1.;
	 Length.Pt 1.; Length.Pt 1.; |];
    |]
  in make_dash_factory default_dash_set


let make_color_factory color_list =
  let colors = ref color_list in
    (fun () ->
       match !colors with
	   hd::tl -> (colors := tl @ [hd];
		      hd)
	 | _ -> failwith "Unpossible")


let default_color_factory () =
  make_color_factory
    [ dark_red; dark_green; dark_blue; purple; dark_orange; slate_blue;
      olive_drab; skyish; gray; mustard; fuchsia; ]


(** [make_fill_pattern_factory fill_set] makes a factory for getting
    fill patterns. *)
let make_fill_pattern_factory fill_set =
  let next = ref 0 in
  let n = Array.length fill_set in
    (fun () ->
       let d = fill_set.(!next) in
	 next := (!next + 1) mod n;
	 d)

(** [default_fill_pattern_factory ?color ()] makes the default fill
    pattern factory. *)
let default_fill_pattern_factory ?(color=black) () =
  let line_style = { default_line_style with line_color = color } in
  make_fill_pattern_factory
    (Array.map
       (fun p -> Patterned_solid_fill (white, p))
       [|
	 No_fill;
	 Horizontal_fill (line_style, Length.Pt 4.);
	 Diagonal_fill (line_style, Length.Pt 1., Length.Pt 10.);
	 Diagonal_fill (line_style, Length.Pt ~-.1., Length.Pt 10.);
	 Hash_fill (line_style, Length.Pt ~-.1., Length.Pt 10.);
	 Dotted_fill (color, Length.Pt 1., Length.Pt 4.);
	 Vertical_fill (line_style, Length.Pt 4.);
	 Solid_fill color;
       |])


(** [default_color_fill_pattern_factory ?patterned ()] makes a
    factory that returns different colored fills. *)
let default_color_fill_pattern_factory ?(patterned=true) () =
  let line_style = { default_line_style with line_color = black } in
  let next_color = default_color_factory () in
  let next_pattern =
    make_fill_pattern_factory
      [|
	No_fill;
	Horizontal_fill (line_style, Length.Pt 4.);
	Diagonal_fill (line_style, Length.Pt 1., Length.Pt 10.);
	Diagonal_fill (line_style, Length.Pt ~-.1., Length.Pt 10.);
	Hash_fill (line_style, Length.Pt ~-.1., Length.Pt 10.);
	Dotted_fill (black, Length.Pt 1., Length.Pt 4.);
	Vertical_fill (line_style, Length.Pt 4.);
      |]
  in
    (fun () ->
       if not patterned
       then Solid_fill (next_color ())
       else Patterned_solid_fill ((next_color ()), next_pattern ()))


(* EOF *)
