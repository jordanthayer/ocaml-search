(**
   Test the Image module.
*)

open Eps
open Ppm

let black = (0, 0, 0)
and red = (255, 0, 0)
and green = (0, 255, 0)
and blue = (0, 0, 255)

let main () =
  let image = Image.create ~bgcolor:(255,255,255) 10 10 in

    Image.draw_line image 9 8 0 9 red;
    Image.draw_line image 0 9 9 8 blue;

    Image.draw_line image 0 0 9 9 red;
    Image.draw_line image 9 9 0 0 green;

    Image.export ~scale:4.0 "eps" image "output.eps";
    Image.export ~scale:4.0 "ppm" image "output.ppm"
;;

let _ = main ()
