(** A image for drawing images

    To ensure that the desired exporter is linked properly you should
    "open" the desired exporter in the module that uses it.  For
    example, to export to an eps file, you should "open Eps".

    @author Ethan Burns
*)

open Printf

type t = {
  width : int;
  height : int;
  data : (char * char * char) array			(* rgb array *)
}


(** Get a image data array index value from a pixel x,y
    coordinate. *)
let index image x y = y * image.width + x


(** Create a new image with the given width and height and
    optionally the given background color. *)
let create ?(bgcolor=(255, 255, 255)) w h =
  let r, g, b = bgcolor in
    { width = w;
      height = h;
      data = Array.make (w * h) (char_of_int (r land 0xff),
				 char_of_int (g land 0xff),
				 char_of_int (b land 0xff)); }


(** Set the color of the given pixel.
    Raise [Failure (x,y)] if the point is outside of the image. *)
let draw_point image x y (r, g, b) =
  if x >= image.width
    || y >= image.height
    || x < 0
    || y < 0
  then ()
  else image.data.(index image x y) <- (char_of_int (r land 0xff),
					char_of_int (g land 0xff),
					char_of_int (b land 0xff))



(** Draw a line from (x0,y0) to (x1,y1) of the given color. *)
let draw_line image x0 y0 x1 y1 color =
  let x0 = max 0 (min x0 (image.width - 1))
  and y0 = max 0 (min y0 (image.height - 1))
  and x1 = max 0 (min x1 (image.width - 1))
  and y1 = max 0 (min y1 (image.height - 1)) in
  let dx = abs (x1 - x0)
  and dy = abs (y1 - y0) in
  let shallow = (abs dx) > (abs dy) in
  let incr0 = if shallow then 2 * dy else 2 * dx
  and incr1 = if shallow then (2 * dy) - (2 * dx) else (2 * dx) - (2 * dy)
  and d = ref (if shallow then (2 * dy) - dx else (2 * dx) - dy)
  and j, jtill =
    if shallow
    then if x0 > x1 then (ref y1), y0 else (ref y0), y1
    else if y0 > y1 then (ref x1), x0 else (ref x0), x1
  and from, till =
    if shallow
    then if x0 > x1 then x1, x0 else x0, x1
    else if y0 > y1 then y1, y0 else y0, y1 in
    for i = from to till do
      let x, y = if shallow then i, !j else !j, i in
	draw_point image x y color;
	if !d > 0
	then
	  begin
	    d := !d + incr1;
	    j := !j + (if !j > jtill then -1 else 1)
	  end
	else d := !d + incr0
    done


(** Draw a box with one corner at (x0,y0) and opposite corner at
    (x1,y1) of the given color.*)
let draw_box image x0 y0 x1 y1 color =
  for i = 0 to (abs(x1-x0)) do
    draw_line image (x0 + i) y0 (x0 + i) y1 color;
  done


(** Function for saving images to files, and a function for adding
    export functions to export to various file types. *)
let export, add_exporter =
  (* this is needed to set the types for the exporters list reference. *)
  let default_export
      (scale:float)
      (image:t)
      (filename:string)
      (chan:out_channel) =
    () in
  let exporters = ref [("", default_export)] in
  let export filetype ?(scale=1.0) image filename =
    try
      let exp = List.assoc filetype !exporters in
      let file = open_out filename in
	exp scale image filename file;
	close_out file
    with Not_found ->
      invalid_arg (Wrutils.str "Filetype [%s] is not supported" filetype)
  in
  let add_exporter filetype savefun =
    if (List.exists (fun (a, _) -> a = filetype) !exporters) then
      invalid_arg (sprintf
		     "Attempt to add exporter %s when it already exists."
		     filetype)
    else exporters := (filetype, savefun) :: !exporters
  in export, add_exporter



(** A generic image scaling.  If the exporter type doesn't natively
support scaling (eps does, for example, but ppm does not) this
function can be used to get some type of scaling functionality. *)
let default_scale scale image =
  if scale <> (floor scale) then
    invalid_arg
      (Wrutils.str "do_cheap_scale: %f must be an even integer value" scale)
  else begin
    let scale = int_of_float scale in
    let w = image.width * scale
    and h = image.height * scale in
    let new_data = Array.make (w * h) ('\xff', '\xff', '\xff') in
    let new_img = {
      width = w;
      height = h;
      data = new_data;
    } in
      for x = 0 to image.width - 1 do
	for y = 0 to image.height - 1 do
	  let c = image.data.(index image x y) in
	    for i = 0 to scale - 1 do
	      for j = 0 to scale - 1 do
		let x = x * scale
		and y = y * scale in
		  new_data.(index new_img (x + i) (y + j)) <- c;
	      done
	    done
	done
      done;
      new_img
  end

(** Get an Image.t from the given Graphics.image. *)
let of_graphics_image gi =
  let colors = Graphics.dump_image gi in
  let h = Array.length colors
  and w = Array.length colors.(0) in
  let image = create w h in
    for i = 0 to w - 1 do
      for j = 0 to h - 1 do
	let c = colors.(j).(i) in
	let r = (c lsr 16) land 0xFF
	and g = (c lsr 8) land 0xFF
	and b = c land 0xFF in
	  draw_point image i j (r, g, b)
      done
    done;
    image


(** Get an Image.t from the current Graphics state. *)
let of_graphics () =
  let w = Graphics.size_x ()
  and h = Graphics.size_y () in
  let gi = Graphics.get_image 0 0 w h in
    of_graphics_image gi

