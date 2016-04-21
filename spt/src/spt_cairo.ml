(** A bunch of calls to cairo - Currently just for saving files *)

open Verbosity

type files =
  | Postscript
  | PNG
  | PDF
  | Unknown of string


(* saving functionality *)
let as_png width height plot filename =
  let width_px = Length.as_px width and height_px = Length.as_px height in
  let surface = (Cairo.image_surface_create
		   Cairo.FORMAT_ARGB32 ~width:width_px ~height:height_px) in
  let ctx =
    Drawing.drawing_context (Cairo.create surface)
      Length.as_px_float ~w:width ~h:height
  in
    plot#set_size ~w:width ~h:height;
    plot#draw ctx;
    Cairo_png.surface_write_to_file surface filename


let as_ps width height plot filename =
  let width_pt = Length.as_pt width and height_pt = Length.as_pt height in
  let chan = open_out filename in
  let surface = (Cairo_ps.surface_create_for_channel chan
		   ~width_in_points:width_pt ~height_in_points:height_pt) in
  let ctx =
    Drawing.drawing_context (Cairo.create surface)
      Length.as_pt ~w:width ~h:height
  in
    plot#set_size ~w:width ~h:height;
    plot#draw ctx;
    Cairo.surface_finish surface;
    close_out chan


let as_pdf width height plot filename =
  let width_pt = Length.as_pt width and height_pt = Length.as_pt height in
  let chan = open_out filename in
  let surface = (Cairo_pdf.surface_create_for_channel chan
		   ~width_in_points:width_pt ~height_in_points:height_pt) in
  let ctx =
    Drawing.drawing_context (Cairo.create surface)
      Length.as_pt ~w:width ~h:height in
    plot#set_size ~w:width ~h:height;
    plot#draw ctx;
    Cairo.surface_finish surface;
    close_out chan



(* determining filetype and saving *)
let filetype file =
  let file_split = Str.split (Str.regexp "\\.") file in
    match (List.rev file_split) with
	[] -> Unknown ""
      | ext::tl -> (match (String.lowercase ext) with
		      | "ps" -> Postscript
		      | "png" -> PNG
		      | "pdf" -> PDF
		      | _ -> Unknown ext)


let save ?width ?height plot filename =
  let width = (match width with
		 | None -> plot#width
		 | Some w -> w)
  and height = (match height with
		  | None -> plot#height
		  | Some h -> h) in
    match (filetype filename) with
      | Postscript ->
	  vprintf verb_optional "saving as postscript\n";
	  as_ps width height plot filename
      | PNG ->
	  vprintf verb_optional "saving as png\n";
	  as_png width height plot filename
      | PDF ->
	  vprintf verb_optional "saving as PDF\n";
	  as_pdf width height plot filename
      | Unknown ext -> failwith ("Cannot save unknown filetype " ^ ext)



(* EOF *)
