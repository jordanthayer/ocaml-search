(* $Id: line_bars.ml,v 1.1 2003/07/18 21:10:24 ruml Exp ruml $

   plots with two numeric axes
*)


let glyphs = ["circle", circle;
	      "spot", spot;
	     "ring", ring;
	     "circle-line", circle_line;
	     "circle-dot", circle_dot;
	     "plus", plus;
	     "less", less;
	     "s", s;
	     "w", w;
	     "triangle", triangle;
	     "triangle-down", triangle_down;
	     "box", box;
	     "x", x]

(** specialized - don't use automatically *)
let other_glyphs = ["big-plus", big_plus;
		    "dot", dot;
		    "big-x", big_dot;
		    "label", label;
		    "spot-gray", spot_gray;
		    "spot-color", spot_color ]

let dashes = List.map (fun pat -> pat, 0.)
	       [(* solid *)
		 [];
		 (* long dash *)
		 [6; 2];
		 (* short dash *)
		 [2; 2];
		 (* dotted line *)
		 [1; 1];
		 (* long dashes with 1 dot *)
		 [5; 2; 1; 2];
		 (* line with 3 dots *)
		 [10; 2; 2; 2; 2; 2; 2; 2];
		 (* line with 1 dot *)
		 [10; 2; 2; 2];
		 (* large dashes, small dashes *)
		 [5; 2; 5; 2; 2; 2; 2; 2];
		 (* dashes and dots *)
		 [4; 2; 4; 1; 1; 1; 1; 1; 1; 1]]


let colors = [0.9,  0,    0;
	      0,    0.8,  0;
	      0,    0,    0.8;
	      (* purple *)
	      0.8,  0,    0.8;
	      (* orange *)
	      0.95, 0.55, 0;
	      (* aquamarine *)
	      0,    0.8,  1;
	      (* yellow is tricky *)
	      0.9,  0.9,  0]


(****** making plot ********)


let plot
  ?(fontname = "Palatino-Roman")
  ?(fontsize = 12)
  ?title
  ?(titlesize = fontsize)
  ?xlabel
  ?ylabel
  ch data = ()



(* EOF *)
