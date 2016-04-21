(** Set of constants for the visualizer *)

let dpq_height = ref 5
and grid_squareSize = ref 7
and dHeight = 800
and dWidth = 1200
and ts = 10
and offset = 20
and text_color = (65535, 65535, 65535) (* white *)
and background = (0, 0, 0)       (* black *)

and colors = [|(33000, 33000, 0);
	       (33000, 0, 33000);
	       (0, 33000, 33000);
	       (0, 65535, 0);       (* green *)
	       (0, 0, 65535);       (* blue *)
	       (65535, 0, 65535);     (* purple *)
	       (65535, 65535, 0);     (* orange *)
	       (65535, 0, 0);       (* red *)
	       (0, 65535, 65535);
	       (33000, 33000, 33000);   (* gray *) |]

and delay = ref 0.3
(* EOF *)
