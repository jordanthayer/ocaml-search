(* $Id: problem.ml,v 1.2 2003/07/07 21:20:50 ruml Exp ruml $
   
   main interface.  most code is in other modules and merely called
   from here.
*)


open Box

   (*
     
(*********** line and errbars ***********)


let line_and_errbars path data
  ?(min_x = None) ?(max_x = None) ?(min_y = None) ?(max_y = None)
  ?(clip = false) ?(zero_line = false) title x_label y_label =
  (* Line_bars.plot path data x_label y_label title *)
  Ps_plot_interface.lines_and_bars path data
    min_x max_x min_y max_y title x_label y_label clip zero_line
  (* Ps_plot_interface.dump_psp_input path data title x_label y_label *)


let sample1 () =
  line_and_errbars
    (* Ps_plot_interface.dump_psp_input *)
    "/tilde/ruml/tmp/foo.ps"
    [
      [[|100.; 200.; 300.|],
       [|1.1; 1.2; 1.3|];
       
       [|100.; 200.; 300.|],
       [|1.1; 1.2; 1.3|];
       
       [|100.; 200.; 300.|],
       [|1.1; 1.2; 1.3|];
       
       [|100.; 200.; 300.|],
       [|1.1; 1.2; 1.3|];
       
      (*
	[|0.; 1.; 2.; 3.|],
	[|0.1; 1.0; 1.3; 1.2|];
	
	[|1.; 2.; 3.; 4.|],
	[|1.2; 1.1; 1.4; 1.4|]
      *)
      ], "set one"
    ]
    "A Title"
    "X Axis"
    "Y axis"


(*********** scatter ***********)


let scatter path data
  ?(non_prob_gray = false)
  ?(font_name = "Palatino")
  ?(font_size = 12)
  ?(x_scale = 1.)
  ?(y_scale = 1.)
  x_label y_label =
  (* data is something like (string * ((float * float) array)) list *)
  Ps_plot_interface.scatter path data
    non_prob_gray font_name font_size x_scale y_scale x_label y_label
    

   *)
   

(* EOF *)
