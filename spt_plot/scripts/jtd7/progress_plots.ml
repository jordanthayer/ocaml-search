(**

    @author jtd7
    @since 2011-12-30

   Code for plotting the progress of search algorithms
*)

let use_color = ref true
let loc = ref Legend.Upper_left
let log = ref false
let xlog = ref false
let lwidth = ref (Length.Pt 2.)
let lbl_sty = ref Spt.default_label_style
let ymax = ref 1.

let get_line df =
  let solved_at = 1. +. (float_of_string (Datafile.get_val df
					    "total nodes expanded")) in
  let estimates = Datafile.get_altcol df "estimated_remaining"
  and x_raw = Datafile.get_altcol df "expansion" in
  let max = Array.length estimates - 1 in
    Array.init (Array.length estimates)
      (fun i ->
	 Geometry.point
	   ~x:(solved_at -. x_raw.(max - i))
	   ~y:estimates.(max - i))


let get_percent_line df =
  let solved_at = 1. +. (float_of_string (Datafile.get_val df
					    "total nodes expanded")) in
  let estimates = Datafile.get_altcol df "estimated_percent"
  and x_raw = Datafile.get_altcol df "expansion" in
    Array.init (Array.length estimates + 1)
      (fun i ->
	 if i = 0 then Geometry.point ~x:0. ~y:0.
	 else
	 Geometry.point
	   ~x:(100. *. (x_raw.(i-1) /. solved_at))
	   ~y:estimates.(i-1))


let get_percent_err_line df =
  let solved_at = 1. +. (float_of_string (Datafile.get_val df
					    "total nodes expanded")) in
  let estimates = Datafile.get_altcol df "estimated_percent"
  and x_raw = Datafile.get_altcol df "expansion" in
    Array.init (Array.length estimates + 2)
      (fun i ->
	 if i = 0 then Geometry.point ~x:0. ~y:0.
	 else if (i = (Array.length estimates + 1))
	 then (Geometry.point ~x:100. ~y:0.)
	 else
	   let real_percent = x_raw.(i-1) /. solved_at in
	     Geometry.point
	       ~x:(100. *. real_percent)
	       ~y:(100. *. (abs_float(estimates.(i-1) -. real_percent))))


let get_lines ?(gl = get_line) dataset =
  let nm = Dataset.get_name dataset in
  let dfs = Dataset.get_dfs dataset in
    Verb.pe Verb.always "Getting line for %s\n%!" nm;
    Some nm,
  Array.of_list (List.fold_left (fun ac e -> (gl e) :: ac) [] dfs)


let plot ?(xlabel = "Expansions Remaining") ?(ylabel = "Estimate Remaining")
    ?(gl = get_line)
    ?(target = Num_by_num.function_dataset [||] Fn.identity)
    title datasets =
  Verb.pe Verb.always "Contsturcting Lines\n%!";
  let lines = (Line_errbar_dataset.line_errbar_datasets ~color:!use_color
		 ~line_width:!lwidth
		 (List.map (get_lines ~gl) datasets)) in
  let lines = target::lines in
    Verb.pe Verb.always "Lines constructed, starting plot\n%!";
  let plot = (Num_by_num.plot ~legend_loc:!loc
		~title
		~label_text_style:!lbl_sty
		(*~y_max:!ymax*)
		~xlabel:(if !xlog then "log10 " ^ xlabel else xlabel)
		~ylabel:(if !log
			 then "log10 "^ ylabel
			 else ylabel) lines) in
    plot#display


let l4grid salg alg_list =
  let dat = List.map
    (Jtd7_helpers.load_wrap Load_dataset.standard_life_4_grids) alg_list in
    Verb.pe Verb.always "Data loaded\n%!";
    plot
      (salg ^ "Life Four-way Grid")
      dat


let l4grid_percent salg alg_list =
  let dat = List.map
    (Jtd7_helpers.load_wrap Load_dataset.standard_life_4_grids) alg_list in
    Verb.pe Verb.always "Data loaded\n%!";
    plot ~xlabel:"Percent Complete"
      ~ylabel:"Estimated Percent Complete"
      ~gl:get_percent_line
      (salg ^ "Life Four-way Grid")
      dat


let l4grid_percent_err salg alg_list =
  let dat = List.map
    (Jtd7_helpers.load_wrap Load_dataset.standard_life_4_grids) alg_list in
    Verb.pe Verb.always "Data loaded\n%!";
    plot
      ~target:(Num_by_num.function_dataset [||] (fun _ -> 0.))
      ~xlabel:"Percent Complete"
      ~ylabel:"|Estimate - Truth|"
      ~gl:get_percent_err_line
      (salg ^ "Life Four-way Grid") dat



let u4grid salg alg_list =
  let dat = List.map
    (Jtd7_helpers.load_wrap Load_dataset.standard_unit_4_grids) alg_list in
    Verb.pe Verb.always "Data loaded\n%!";
    plot
      (salg ^ "Unit Four-way Grid")
      dat


let u4grid_percent salg alg_list =
  let dat = List.map
    (Jtd7_helpers.load_wrap Load_dataset.standard_unit_4_grids) alg_list in
    Verb.pe Verb.always "Data loaded\n%!";
    plot ~xlabel:"Percent Complete"
      ~ylabel:"Estimated Percent Complete"
      ~gl:get_percent_line
      (salg ^ "Unit Four-way Grid")
      dat


let u4grid_percent_err salg alg_list =
  let dat = List.map
    (Jtd7_helpers.load_wrap Load_dataset.standard_unit_4_grids) alg_list in
    Verb.pe Verb.always "Data loaded\n%!";
    plot
      ~target:(Num_by_num.function_dataset [||] (fun _ -> 0.))
      ~xlabel:"Percent Complete"
      ~ylabel:"Estimate - Truth"
      ~gl:get_percent_err_line
      (salg ^ "Unit Four-way Grid") dat


let tiles salg alg_list =
  let dat = List.map
    (Jtd7_helpers.load_wrap Load_dataset.korf_100_tiles) alg_list in
    Verb.pe Verb.always "Data loaded\n%!";
    plot (salg ^ " Korf's 100 15 Puzzles") dat


let tiles_percent salg alg_list =
  let dat = List.map
    (Jtd7_helpers.load_wrap Load_dataset.korf_100_tiles) alg_list in
    Verb.pe Verb.always "Data loaded\n%!";
    plot ~xlabel:"Percent Complete"
      ~ylabel:"Estimated Percent Complete"
      ~gl:get_percent_line
      (salg ^ " Korf's 100 15 Puzzles") dat


let tiles_percent_err salg alg_list =
  let dat = List.map
    (Jtd7_helpers.load_wrap Load_dataset.korf_100_tiles) alg_list in
    Verb.pe Verb.always "Data loaded\n%!";
    plot
      ~target:(Num_by_num.function_dataset [||] (fun _ -> 0.))
      ~xlabel:"Percent Complete"
      ~ylabel:"|Estimate - Truth|"
      ~gl:get_percent_err_line
      (salg ^ " Korf's 100 15 Puzzles") dat

(*
 Progress_plots.l4grid_percent
  ["greedy_est_rb", "Greedy Est RB", ["beam_width", "5"; "num", "42";];
   "astar_est_rb", "A* Est RB", ["beam_width", "5"; "num", "42"];];;
*)
