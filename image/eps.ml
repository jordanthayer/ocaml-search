(** Exporting images to encapsulated postscript (.eps) files.

    @author Ethan Burns
*)

open Image
open Printf

(** Perform the various encodings. *)
let do_encode image =
  let flatten_data image =
    (* Convert the image data into a stream of characters. *)
    let buf = Buffer.create (image.width * image.height * 3) in
      Array.iter (fun (r, g, b) ->
		    Buffer.add_char buf r;
		    Buffer.add_char buf g;
		    Buffer.add_char buf b;)
	image.data;
      Stream.of_string (Buffer.contents buf)
  in
  let run_length = Run_length.encode (flatten_data image) in
    Ascii85.encode (Stream.of_string (Buffer.contents run_length))


(** Save a image to the given channel. *)
let export scale image filename chan =
  let w = image.width
  and h = image.height
  and color_components = 3		(* r,g,b *)
  and point_of_pixel p = (float_of_int p) *. 0.72 in
    (* Convert a PS point value to a pixel value. *)

    (* Print the EPS header info to the channel. *)
    Verb.pf Verb.always chan "%%!PS-Adobe-3.0\n";
    Verb.pf Verb.always chan
      "%%%%Creator: OCaml Encapsulated PostScript Canvas.\n";
    Verb.pf Verb.always chan "%%%%Title: %s\n" filename;
    Verb.pf Verb.always chan "%%%%BoundingBox: 0 0 %d %d\n"
      (Math.round ((point_of_pixel w) *. scale))
      (Math.round ((point_of_pixel h) *. scale));
    Verb.pf Verb.always chan "%%%%EndComments\n";

    (* Print the image data to the channel. *)
    Verb.pf Verb.always chan "%f %f scale\n"
      ((point_of_pixel w) *. scale) ((point_of_pixel h) *. scale);
    Verb.pf Verb.always chan "%d\n%d\n8\n" w h; (* width height color-depth *)
    Verb.pf Verb.always chan "[%d 0 0 %d 0 0]\n" w h;
    Verb.pf Verb.always chan
      "/datasource currentfile /ASCII85Decode filter /RunLengthDecode filter def\n";
    Verb.pf Verb.always chan "/datastring %d string def\n"
      (w * color_components);
    Verb.pf Verb.always chan "{datasource datastring readstring pop}\n";
    Verb.pf Verb.always chan "false\n";	(* single data source, rgb *)
    Verb.pf Verb.always chan "%d\n" color_components;
    Verb.pf Verb.always chan "colorimage\n";
    Buffer.output_buffer chan (do_encode image);
    Verb.pf Verb.always chan "\nshowpage\n";
    close_out chan
;;

add_exporter "eps" export
