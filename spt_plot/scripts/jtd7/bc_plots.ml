(**
   @author jtd7
   @since 2010-05-28
*)

(*
Plot_by_weight.robots ~ykey:"total raw cpu time"
  ["wted_astar", "wA*", [];
   "aggressive_fhp_path_dd", "Skeptical Path", [];
   "aggressive_fhp_dd", "Skeptical Global", [];
   "aggressive_dd", "Optimistic 1.5", ["optimism", "1.5"];
   "aggressive_dd", "Optimistic 2", ["optimism", "2."];
   "aggressive_dd", "Optimistic 3", ["optimism", "3."];
   "aggressive_dd", "Optimistic 5", ["optimism", "5."];];;
*)

let use_color = ref true
let loc = ref Legend.Upper_right
let log = ref true
let xlog = ref false
let ymax = ref 3.
let lwidth = ref (Length.Pt 2.)
let lbl_sty = ref Spt.default_label_style


type sol_filt =
  | No_filt
  | Some_Solved
  | None_Soved


let xy_plot ?(xkey="wt") ?(xlabel = "Cost Over Optimal")
    ~title ~ylabel datasets ykey =
  let datasets = (if !log
		  then List.map (Dataset.numeric_transform ykey
				   (fun v -> (if v = 0. then -5. else
						log10 v))) datasets
		  else datasets) in
  let datasets = (if !xlog
		  then List.map (Dataset.numeric_transform xkey
				   (fun v -> (if v = 0. then -5. else
						log10 v))) datasets
		  else datasets) in
  let dset = Dataset_to_spt.line_errs ~line_width:!lwidth
    ~use_color:!use_color ~xkey ~ykey
    ~group_keys:[|"num"|] datasets in
(*  let y_equal_x = Num_by_num.function_dataset [||] ~name:"y=x" Fn.identity in*)
  let plot = (Num_by_num.plot ~legend_loc:!loc
		~title
		~label_text_style:!lbl_sty
		~y_max:!ymax
		~xlabel:(if !xlog then "log10 " ^ xlabel else xlabel)
		~ylabel:(if !log
			 then "log10 "^ ylabel
			 else ylabel) (dset)) in
    plot#display


let xy_plot_norm ?(xkey="wt") ?(xlabel = "Cost Bound")
    ~title ~ylabel datasets ykey =
  let datasets = Normalize_dataset.norm_min "num" ykey datasets in
  let datasets = (if !log
		  then List.map (Dataset.numeric_transform ykey
				   (fun v -> (if v = 0. then -5. else
						log10 v))) datasets
		  else datasets) in
  let datasets = (if !xlog
		  then List.map (Dataset.numeric_transform xkey
				   (fun v -> (if v = 0. then -5. else
						log10 v))) datasets
		  else datasets) in
  let dset = Dataset_to_spt.line_errs ~line_width:!lwidth
    ~use_color:!use_color ~xkey ~ykey
    ~group_keys:[|"num"|] datasets in
  let plot = (Num_by_num.plot ~legend_loc:!loc
		~title
		~label_text_style:!lbl_sty
		~y_max:!ymax
		~xlabel:(if !xlog then "log10 " ^ xlabel else xlabel)
		~ylabel:(if !log
			 then "log10 "^ ylabel
			 else ylabel) (dset)) in
    plot#display


(*************************** Normed plots ************************************)

let normed_tiles ?(ykey = "total nodes generated") alg_list =
  let alg_list = ("idastar", "IDA*", []) :: alg_list in
    match List.map
      (Jtd7_helpers.load_wrap Load_dataset.korf_100_tiles) alg_list with
	| (idastar::dat) ->
	    let dat = List.map (Dataset.filter float_of_string
				  (fun v -> v <= 5. && v >= 1.1) "wt") dat in
	    let dat = idastar::dat in
	    let normed = Normalize_dataset.norm_by_alg [|"num"|]
	      ykey  (List.hd dat) (List.tl dat) in
	      xy_plot ~title:"Korf 100 15 Puzzles"
		~ylabel:(ykey ^ " relative to IDA*") normed ykey
	| _ -> ()

let normed_u4grid ?(ykey = "total nodes generated") alg_list =
  let alg_list = ("astar", "A*", []) :: alg_list in
  match List.map
    (Jtd7_helpers.load_wrap Load_dataset.standard_unit_4_grids) alg_list with
      | astar::dat ->
	  let dat = List.map (Dataset.filter float_of_string
				(fun v -> v <= 5.) "wt") dat in
	  let dat = astar::dat in
	  let normed = Normalize_dataset.norm_by_alg [|"num"|]
	    ykey (List.hd dat) (List.tl dat) in
	    xy_plot ~title:"Unit Four-way Grid World"
	      ~ylabel:(ykey ^ " relative to A*") normed ykey
      | _ -> ()

let normed_u8grid ?(ykey = "total nodes generated") alg_list =
  let alg_list = ("astar", "A*", []) :: alg_list in
  match List.map
    (Jtd7_helpers.load_wrap Load_dataset.standard_unit_8_grids) alg_list with
      | astar::dat ->
	  let dat = List.map (Dataset.filter float_of_string
				(fun v -> v <= 3.) "wt") dat in
	  let dat = astar::dat in
	  let normed = Normalize_dataset.norm_by_alg [|"num"|]
	    ykey (List.hd dat) (List.tl dat) in
	    xy_plot ~title:"Unit Eight-way Grid World"
	      ~ylabel:(ykey ^ " relative to A*") normed ykey
      | _ -> ()


let normed_l4grid ?(ykey = "total nodes generated") alg_list =
  let alg_list = ("astar", "A*", []) :: alg_list in
    match List.map
      (Jtd7_helpers.load_wrap Load_dataset.standard_life_4_grids) alg_list with
	| astar::dat ->
	    let dat = List.map (Dataset.filter float_of_string
				  (fun v -> v <= 5.) "wt") dat in
	    let dat = astar::dat in
	    let normed = Normalize_dataset.norm_by_alg [|"num"|]
	      ykey (List.hd dat) (List.tl dat) in
	      xy_plot ~title:"Life Four-way Grid World"
		~ylabel:(ykey ^ " relative to A*") normed ykey
	| _ -> ()



let normed_small_life ?(ykey = "total nodes generated") alg_list =
  let alg_list = ("astar", "A*", []) :: alg_list in
    match List.map
      (Jtd7_helpers.load_wrap Load_dataset.small_life_4_grids) alg_list with
	| astar::dat ->
	    let dat = List.map (Dataset.filter float_of_string
				  (fun v -> v <= 3.) "wt") dat in
	    let dat = astar::dat in
	    let normed = Normalize_dataset.norm_by_alg [|"num"|]
	     ykey (List.hd dat) (List.tl dat) in
	      xy_plot ~title:"Life Four-way Grid World"
		~ylabel:(ykey ^ " relative to A*") normed ykey
	| _ -> ()


let normed_l8grid ?(ykey = "total nodes generated") alg_list =
  let alg_list = ("astar", "A*", []) :: alg_list in
  let dat = List.map
    (Jtd7_helpers.load_wrap Load_dataset.standard_life_8_grids) alg_list in
  let dat = List.map (Dataset.filter float_of_string
			(fun v -> v <= 3.) "wt") dat in
  let normed = Normalize_dataset.norm_by_alg [|"num"|]
    ykey (List.hd dat) (List.tl dat) in
    xy_plot ~title:"Life Eight-way Grid World"
      ~ylabel:(ykey ^ " relative to A*") normed ykey


let normed_pancakes ?(ykey = "total nodes generated") alg_list =
  let alg_list = ("astar", "A*", []) :: alg_list in
    match List.map
      (Jtd7_helpers.load_wrap Load_dataset.standard_pancakes) alg_list with
	| astar::dat ->
	    let dat = List.map (Dataset.filter float_of_string
				  (fun v -> v <= 3.) "wt") dat in
	    let dat = (astar::dat) in
	    let normed = Normalize_dataset.norm_by_alg [|"num"|]
	      ykey (List.hd dat) (List.tl dat) in
	      xy_plot ~title:"10 Pancake Puzzle"
		~ylabel:(ykey ^ " relative to A") normed ykey
	| _ -> ()


(* No other domains are optimally solvable in the standard configuration *)

(****************************** Raw plots ************************************)


let tiles ?(ykey = "total nodes generated") ?(xmin = 2.0) ?(xmax = 500.)
    alg_list =
  let dat = List.map
    (Jtd7_helpers.load_wrap (Load_dataset.korf_100_tiles ~cost:"unit"))
    alg_list in
  let dat = List.map (Dataset.filter float_of_string
			(fun v -> v <= xmax && v >= xmin) "wt") dat in
    xy_plot ~title:"Korf's 100 15 Puzzles" ~ylabel:ykey dat ykey



let macro_tiles ?(ykey = "total nodes generated") ?(xmin = 1.1) ?(xmax = 5.)
    alg_list =
  let dat = List.map
    (Jtd7_helpers.load_wrap Load_dataset.macro_korf_100_tiles) alg_list in
    xy_plot ~title:"Korf's 100 15 Puzzles Macro" ~ylabel:ykey dat ykey


let inv_tiles ?(ykey = "total nodes generated") ?(xmin = 1.) ?(xmax = 40.)
    alg_list =
  let dat = List.map
    (Jtd7_helpers.load_wrap (Load_dataset.korf_100_tiles ~cost:"inverse"))
    alg_list in
  let dat = List.map (Dataset.filter float_of_string
			(fun v -> v <= xmax && v >= xmin) "wt") dat in
    xy_plot ~title:"100 Inverse 15 Puzzles" ~ylabel:ykey dat ykey

let u4grid ?(ykey = "total nodes generated") alg_list =
  let dat = List.map
    (Jtd7_helpers.load_wrap Load_dataset.standard_unit_4_grids) alg_list in
    xy_plot ~title:"Unit Four-way Grid World" ~ylabel:ykey dat ykey


let l4grid ?(ykey = "total nodes generated")
    ?(xmin = 2_100_000.) ?(xmax = 2_400_000.) alg_list =
  let dat = List.map
    (Jtd7_helpers.load_wrap Load_dataset.standard_life_4_grids) alg_list in
  let dat = List.map (Dataset.filter float_of_string
			(fun v -> v >= xmin && v <= xmax) "wt") dat in
    xy_plot ~title:"Life Four-way Grid World" ~ylabel:ykey dat ykey



let vacuum_by_dirt  ?(ykey = "total nodes generated")
    ?(xmin = 5.) ?(xmax = 50.) alg_list =
  let dat = List.map
    (Jtd7_helpers.load_wrap Load_dataset.heavy_vacuum_nodirt) alg_list in
  let dat = List.map (Dataset.filter float_of_string
			(fun v -> v >= xmin && v <= xmax) "dirt") dat in
    xy_plot ~xkey:"dirt" ~title:"Vacuum World"
      ~xlabel:"Dirt Piles"
      ~ylabel:ykey dat ykey


let pancakes ?(ykey = "total nodes generated") alg_list =
  let dat = List.map
    (Jtd7_helpers.load_wrap Load_dataset.standard_pancakes) alg_list in
    xy_plot ~title:"10 Pancake Puzzle" ~ylabel:ykey dat ykey



let vacuum_standard ?(ykey = "total nodes generated") ?(xmin = 1.) ?(xmax = 5.)
    alg_list =
  let dat = List.map
    (Jtd7_helpers.load_wrap Load_dataset.improvedd_standard_vacuum) alg_list in
  let dat = List.map (Dataset.filter float_of_string
			(fun v -> xmin <= v && v <= xmax) "wt") dat in
    xy_plot  ~title:"Vacuum World" ~ylabel:ykey dat ykey


let heavy_vacuum ?(ykey = "total nodes generated") ?(xmin = 1000.)
    ?(xmax = 30_000.) alg_list =
  let dat = List.map
    (Jtd7_helpers.load_wrap Load_dataset.heavy_vacuum) alg_list in
  let dat = List.map (Dataset.filter float_of_string
			(fun v -> v >= xmin && v <= xmax) "wt") dat in
    List.iter (Dataset.add_final_value "total duplicates encountered"
		 "duplicates encountered") dat;
    xy_plot  ~title:"Heavy Vacuum World" ~ylabel:ykey dat ykey


let solved_heavy_vacuum ?(ykey = "total nodes generated")
    ?(xmin = 1000.) ?(xmax = 30_000.) alg_list =
  let dat = List.map
    (Jtd7_helpers.load_wrap Load_dataset.heavy_vacuum) alg_list in
  let dat = List.map (Dataset.filter float_of_string
			(fun v -> v >= xmin && v <= xmax) "wt") dat in
    xy_plot  ~title:"Heavy Vacuum World - Have Solution" ~ylabel:ykey dat ykey


let unsolved_heavy_vacuum ?(ykey = "total nodes generated")
    ?(xmin = 1000.) ?(xmax = 30_000.) alg_list =
  let dat = List.map
    (Jtd7_helpers.load_wrap Load_dataset.unsolved_heavy_vacuum) alg_list in
  let dat = List.map (Dataset.filter float_of_string
			(fun v -> v >= xmin && v <= xmax) "wt") dat in
    List.iter (Dataset.add_final_value "total duplicates encountered"
		 "duplicates encountered") dat;
    xy_plot  ~title:"Heavy Vacuum World - No Solution" ~ylabel:ykey dat ykey


let small_robots ?(ykey = "total nodes generated") ?(xmax = 3.)
    ?(xmin = 1.)  alg_list =
  let dat = List.map
    (Jtd7_helpers.load_wrap Load_dataset.smaller_robots) alg_list in
  let dat = List.map (Dataset.filter float_of_string
			(fun v -> v <= xmax && v >= xmin) "wt") dat in
    xy_plot  ~title:"Dynamic Robot Motion Planning" ~ylabel:ykey dat ykey


let robots ?(ykey = "total nodes generated") ?(xmax = 50.) alg_list =
  let dat = List.map
    (Jtd7_helpers.load_wrap Load_dataset.standard_robots) alg_list in
  let dat = List.map (Dataset.filter float_of_string
			(fun v -> v <= xmax) "wt") dat in
    xy_plot  ~title:"Dynamic Robot Motion Planinng" ~ylabel:ykey dat ykey


let dock_robot ?(ykey = "total raw cpu time") ?(xmin = 10.) ?(xmax = 50.)
    alg_list =
  let dat = List.map
    (Jtd7_helpers.load_wrap Load_dataset.dock_robot) alg_list in
  let dat = List.map (Dataset.filter float_of_string
			(fun v -> v >= xmin && v <= xmax) "wt") dat in
    xy_plot  ~title:"Dock Robot" ~ylabel:ykey dat ykey


let big_dock_robot ?(ykey = "total raw cpu time") ?(xmin = 10.) ?(xmax = 50.)
    alg_list =
  let dat = List.map
    (Jtd7_helpers.load_wrap Load_dataset.big_dock_robot) alg_list in
  let dat = List.map (Dataset.filter float_of_string
			(fun v -> v >= xmin && v <= xmax) "wt") dat in
    xy_plot  ~title:"Dock Robot" ~ylabel:ykey dat ykey



let blocks ?(num = ("num", "1")) ?(ykey = "total nodes generated") alg_list =
  let alg_list = List.map (fun (a,b,c) -> a,b,num::c) alg_list in
  let dat = List.map
    (Jtd7_helpers.load_wrap Load_dataset.blocks) alg_list in
    xy_plot  ~title:("Blocks World " ^ (snd num)) ~ylabel:ykey dat ykey


let tpl_logistics ?(num = ("num", "1"))
    ?(ykey = "total nodes generated") alg_list =
  let alg_list = List.map (fun (a,b,c) -> a,b,num::c) alg_list in
  let dat = List.map
    (Jtd7_helpers.load_wrap Load_dataset.tpl_logistics) alg_list in
    xy_plot  ~title:("Logistics " ^ (snd num)) ~ylabel:ykey dat ykey


let rovers ?(num = ("num", "1")) ?(ykey = "total nodes generated") alg_list =
  let alg_list = List.map (fun (a,b,c) -> a,b,num::c) alg_list in
  let dat = List.map
    (Jtd7_helpers.load_wrap Load_dataset.rovers) alg_list in
    xy_plot  ~title:("Rovers " ^ (snd num)) ~ylabel:ykey dat ykey


let satellite ?(num = ("num", "1")) ?(ykey = "total nodes generated") alg_list =
  let alg_list = List.map (fun (a,b,c) -> a,b,num::c) alg_list in
  let dat = List.map
    (Jtd7_helpers.load_wrap Load_dataset.satellite) alg_list in
    xy_plot  ~title:("Satellite " ^ (snd num)) ~ylabel:ykey dat ykey


let zeno ?(num = ("num", "1")) ?(ykey = "total nodes generated") alg_list =
  let alg_list = List.map (fun (a,b,c) -> a,b,num::c) alg_list in
  let dat = List.map
    (Jtd7_helpers.load_wrap Load_dataset.zenotravel) alg_list in
    xy_plot  ~title:("Zenotravel " ^ (snd num)) ~ylabel:ykey dat ykey


(* Aggregate plot *)
let low_res_weights = [ 5.; 4.; 3.; 2.; 1.75; 1.5; 1.2; 1.1; 1.01; 1.0;]

let plot_agregate ?(ykey = "final sol cost")
    talgs italgs l4algs vacalgs robalgs docalgs =
  let tdat = List.map (Jtd7_helpers.load_wrap
			 (Load_dataset.korf_100_tiles ~cost:"unit")) talgs in
    Verb.pe Verb.always "Tiles loaded!\n%!";
  let itdat = List.map (Jtd7_helpers.load_wrap
			  (Load_dataset.korf_100_tiles ~cost:"inverse")) italgs
  in Verb.pe Verb.always "Inverse Tiles Loaded!\n%!";
  let l4g = List.map
    (Jtd7_helpers.load_wrap Load_dataset.standard_life_4_grids) l4algs
  in Verb.pe Verb.always "L4grid Loaded!\n%!";
  let vac = List.map
    (Jtd7_helpers.load_wrap Load_dataset.heavy_vacuum) vacalgs
  in Verb.pe Verb.always "Vacuum Loaded!\n%!";
  let rob = List.map
    (Jtd7_helpers.load_wrap Load_dataset.standard_robots) robalgs
  in Verb.pe Verb.always "Dynamic Robot Loaded!\n%!";
  let doc = List.map
    (Jtd7_helpers.load_wrap Load_dataset.big_dock_robot) docalgs in
    Verb.pe Verb.always "All Datasets loaded\n%!";
  let agg_lst = [tdat; itdat; l4g; vac; rob; doc;] in
  let agg = List.fold_left (fun accum el ->
			      List.map2 (fun a b -> Dataset.merge [a;b])
				accum el) (List.hd agg_lst) (List.tl agg_lst) in
  let agg = List.map (Dataset.filter float_of_string
			(fun e -> List.mem e low_res_weights) "wt") agg in
    Verb.pe Verb.always "Datasets Merged, plotting\n%!";
    xy_plot ~title:"All Benchmarks" ~ylabel:ykey agg ykey


(* Tools for plotting solution quality on bounded suboptimal search algorithms*)

let nc_tiles ?(ykey = "final sol cost") ?(xmin = 1.1) ?(xmax = 5.)
    alg_list =
  let dat = List.map
    (Jtd7_helpers.load_wrap (Load_dataset.korf_100_tiles ~cost:"unit"))
    alg_list in
  let dat = List.map (Dataset.filter float_of_string
			(fun v -> v <= xmax && v >= xmin) "wt") dat in
    xy_plot_norm
      ~title:"Korf's 100 15 Puzzles" ~ylabel:"Solution Quality" dat ykey


let nc_inv_tiles ?(ykey = "final sol cost") ?(xmin = 1.1) ?(xmax = 5.)
    alg_list =
  let dat = List.map
    (Jtd7_helpers.load_wrap (Load_dataset.korf_100_tiles ~cost:"inverse"))
    alg_list in
  let dat = List.map (Dataset.filter float_of_string
			(fun v -> v <= xmax && v >= xmin) "wt") dat in
    xy_plot_norm
      ~title:"100 Inverse 15 Puzzles" ~ylabel:"Solution Quality" dat ykey


let nc_l4grid ?(ykey = "final sol cost") ?(xmin = 1.) ?(xmax = 5.)alg_list =
  let dat = List.map
    (Jtd7_helpers.load_wrap Load_dataset.standard_life_4_grids) alg_list in
  let dat = List.map (Dataset.filter float_of_string
			(fun v -> v >= xmin && v <= xmax) "wt") dat in
    xy_plot_norm ~title:"Life Four-way Grid World"
       ~ylabel:"Solution Quality" dat ykey


let nc_heavy_vacuum ?(ykey = "final sol cost") ?(xmin = 1.5) ?(xmax = 5.)
    alg_list =
  let dat = List.map
    (Jtd7_helpers.load_wrap Load_dataset.heavy_vacuum) alg_list in
  let dat = List.map (Dataset.filter float_of_string
			(fun v -> v >= xmin && v <= xmax) "wt") dat in
    List.iter (Dataset.add_final_value "total duplicates encountered"
		 "duplicates encountered") dat;
    xy_plot_norm  ~title:"Heavy Vacuum World"
      ~ylabel:"Solution Quality" dat ykey


let robots_nc ?(ykey = "final sol cost") ?(xmax = 5.) alg_list =
  let dat = List.map
    (Jtd7_helpers.load_wrap Load_dataset.standard_robots) alg_list in
  let dat = List.map (Dataset.filter float_of_string
			(fun v -> v <= xmax) "wt") dat in
    xy_plot_norm  ~title:"Dynamic Robot Motion Planinng"
      ~ylabel:"Solution Quality" dat ykey


let dock_robot_nc ?(ykey = "final sol cost") ?(xmin = 1.2) ?(xmax = 3.)
    alg_list =
  let dat = List.map
    (Jtd7_helpers.load_wrap Load_dataset.dock_robot) alg_list in
  let dat = List.map (Dataset.filter float_of_string
			(fun v -> v >= xmin && v <= xmax) "wt") dat in
    xy_plot_norm  ~title:"Dock Robot"  ~ylabel:"Solution Quality" dat ykey

(* EOF *)
