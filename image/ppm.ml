(** Exporting to portable pixmap files.

    @author Ethan Burns
*)

open Image
open Printf

let export scale image filename chan =
  let image = default_scale scale image in
  let w = image.width
  and h = image.height in
    Verb.pf Verb.always chan "P3\n";		(* 3 ascii color values *)
    Verb.pf Verb.always chan "%d %d\n" w h;
    Verb.pf Verb.always chan "255\n";		(* max color value *)
    for y = h - 1 downto 0 do
      for x = 0 to w - 1 do
	let r, g, b = image.data.(index image x y) in
	  Verb.pf Verb.always chan "%d %d %d\n"
	    (int_of_char r)
	    (int_of_char g)
	    (int_of_char b)
      done
    done
;;

add_exporter "ppm" export
