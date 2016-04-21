(** A plotting script for parallel search.

    @author eaburns
    @since 2010-08-16
*)

open Printf


let ps_plot_text_style =
  (** A text style matching that of Ps_plot title, label and legend
      text. *)
  {
    Drawing.text_font = "Palatino";
    Drawing.text_size = Length.Pt 12.;
    Drawing.text_slant = Drawing.font_slant_normal;
    Drawing.text_weight = Drawing.font_weight_normal;
    Drawing.text_color = Drawing.black;
  }


let ps_plot_tick_text_style =
  (** A text style matching that of Ps_plot tick marks, label and
      legend text. *)
  { ps_plot_text_style with Drawing.text_size = Length.Pt 9. }


let db_root = User_paths.data_root ^ "x86_64/"


let get_valid_keys keys ds =
  (** [get_valid_keys keys ds] filters out all of the keys in the
      [keys] list that do not appear in [ds].  The result is an array of
      the remaining keys. *)
  let valid_keys = List.filter (fun k -> Dataset.is_key k ds) keys in
    Array.of_list valid_keys


let add_log10 key ds =
  let safe_log10 x =
    if x = 0. then log10 epsilon_float else log10 x
  in
  if Dataset.is_key key ds
  then begin
    let new_key = "log10 " ^ key in
    let ds = Dataset.copy_key key new_key ds in
      Dataset.numeric_transform new_key safe_log10 ds
  end else ds


let add_keys ds =
  let default_match_keys = [ "num"; "threads"; "wt" ] in
  let match_keys = get_valid_keys default_match_keys ds in
  let fs =
    [
      add_log10 "total nodes expanded";
      add_log10 "total wall time";

      (********** nodes per CPU second **********)
      (fun ds ->
	 let ds =
	   Dataset.copy_key "total raw cpu time" "nodes per cpu second" ds
	 in
	   Dataset.transform_with match_keys ds "nodes per cpu second"
	     ~with_key:"total nodes expanded" (fun nodes sec -> nodes /. sec)
	     ds);

      (********** generations per thread **********)
      (fun ds ->
	 let ds =
	   Dataset.copy_key "total nodes generated" "generations per thread"
	     ds
	 in
	   Dataset.transform_with match_keys ds "generations per thread"
	     ~with_key:"threads" (fun thr gens -> gens /. thr) ds);

      (********** waiting ratio **********)
      (fun ds ->
	 if Dataset.is_key "thread wait time" ds
	 then begin
	   let ds =
	     Dataset.copy_key "thread wait time" "thread wait percent" ds
	   in
	     Dataset.transform_with match_keys ds "thread wait percent"
	       ~with_key:"total wall time" (fun wall wait ->
					      (wait /. wall) *. 100.)
	       ds
	 end else ds);

      add_log10 "thread wait percent";

      (********** locking ratio **********)
      (fun ds ->
	 if Dataset.is_key "thread lock time" ds
	 then begin
	   let ds =
	     Dataset.copy_key "thread lock time" "thread lock percent" ds
	   in
	     Dataset.transform_with match_keys ds "thread lock percent"
	       ~with_key:"total wall time" (fun wall lock ->
					      (lock /. wall) *. 100.)
	       ds
	 end else ds);

      add_log10 "thread lock percent";

      (********** coord ratio **********)
      (fun ds ->
	 if Dataset.is_key "total coord time" ds then begin
	   let ds =
	     Dataset.copy_key "total coord time" "thread coord percent" ds
	   in
	     Dataset.transform_with match_keys ds "thread coord percent"
	       ~with_key:"total wall time" (fun wall sum ->
					      (sum /. wall) *. 100.) ds
	 end else ds );

      (********** max wait ratio **********)
      (fun ds ->
	 if Dataset.is_key "max time waiting" ds
	 then begin
	   let ds =
	     Dataset.copy_key "max time waiting" "max wait ratio" ds
	   in
	     Dataset.transform_with match_keys ds "max wait ratio"
	       ~with_key:"total wall time" (fun wall wait -> wait /. wall)
	       ds
	 end else ds);

      add_log10 "max wait ratio";

      (********** max lock ratio **********)
      (fun ds ->
	 if Dataset.is_key "max time acquiring locks" ds
	 then begin
	   let ds =
	     Dataset.copy_key "max time acquiring locks" "max lock ratio" ds
	   in
	     Dataset.transform_with match_keys ds "max lock ratio"
	       ~with_key:"total wall time" (fun wall lock -> lock /. wall)
	       ds
	 end else ds);

      add_log10 "max lock ratio";

      (********** wall time without wasted time **********)

      (fun ds ->
	 if Dataset.is_key "threads" ds
	 then begin
	   let ds =
	     Dataset.copy_key "total wall time" "wall time times threads" ds
	   in
	     Dataset.transform_with match_keys ds "wall time times threads"
	       ~with_key:"threads" ( *. ) ds
	 end else ds);

      (fun ds ->
	 if Dataset.is_key "total time acquiring locks" ds
	 then begin
	   let ds =
	     Dataset.copy_key "total time acquiring locks"
	       "minus lock time" ds
	   in
	     Dataset.transform_with match_keys ds "minus lock time"
	       ~with_key:"wall time times threads"
	       (fun wall lock -> wall -. lock) ds
	 end else ds);

      (fun ds ->
	 if Dataset.is_key "total time waiting" ds
	 then begin
	   let ds =
	     Dataset.copy_key "total time waiting" "working cpu time" ds
	   in
	     Dataset.transform_with match_keys ds "working cpu time"
	       ~with_key:"minus lock time" (fun m w -> m -. w) ds
	 end else ds);
    ]
  in List.fold_left (fun ds f -> f ds) ds fs


let load_grids costs moves prob width height attrs name =
  let attrs' = attrs @ [ "costs", costs;
			 "moves", moves;
			 "prob", string_of_float prob;
			 "width", string_of_int width;
			 "height", string_of_int height; ]
  in add_keys (Dataset.load_from_rdb ~db:(db_root ^ "grid/") attrs' ~name)



let load_tiles model ?(rows=4) ?(cols=4) attrs name =
  let attrs' = attrs @ [ "model", model;
			 "rows", string_of_int rows;
			 "cols", string_of_int cols; ]
  in add_keys (Dataset.load_from_rdb ~db:(db_root ^ "tiles/") attrs' ~name)


(************************************************************)
(* Tiles "Best" values                                      *)
(************************************************************)

let tiles_bests =
  [
    "astar",
    ("A*", [ "alg", "astar" ]);

    "safepbnf-instrumented",
    ("SafePBNF", [ "alg", "safepbnf-instrumented"; "wt", "1.";
		   "nblocks", "3360"; "min-expansions", "16" ]);

    "safepbnf",
    ("SafePBNF", [ "alg", "safepbnf-uninstrumented"; "wt", "1.";
		   "nblocks", "3360"; "min-expansions", "16" ]);

    "ahdastar-instrumented",
    ("AHDA*", [ "alg", "ahdastar-instrumented"; "wt", "1."; "nblocks", "123";
		"max-expansions", "32"]);

    "ahdastar",
    ("AHDA*", [ "alg", "ahdastar-uninstrumented"; "wt", "1."; "nblocks", "123";
		"max-expansions", "32"]);

    "bfpsdd-instrumented",
    ("BFPSDD", [ "alg", "bfpsdd-instrumented"; "nblocks", "123";
		 "min-expansions", "16" ]);
  ]


let tiles_best =
  (** [tiles_best ?extra_attrs model alg] looks up the data for the
      given algorithm. *)
  let cache = Cache.create 10 in
  let load_data (model, alg, extra_attrs, replace) =
    let name, attrs = List.assoc alg tiles_bests in
    let attrs' =
      List.map (fun ((vr, vl) as a) ->
		  try
		    let vl' = List.assoc vr replace in
		      vr, vl'
		  with Not_found -> a)
	attrs
    in
    let attrs'' = extra_attrs @ attrs' in
      Verb.pr Verb.toplvl "Loading with attributes: %s\n"
	(Rdb.human_str attrs'');
      load_tiles model attrs'' name
  in
    (fun ?(extra_attrs=[]) ?(replace=[]) model alg ->
       let ds =
	 Cache.find_or_compute cache
	   load_data (model, alg, extra_attrs, replace)
       in
	 Verb.pr Verb.toplvl "Loaded %s\n" alg;
	 ds)


(************************************************************)
(* Grid "Best" values                                      *)
(************************************************************)

let grid_bests =
  [
    "astar",
    ("A*", [ "alg", "astar" ]);

    "safepbnf-instrumented",
    ("SafePBNF", [ "alg", "safepbnf-instrumented"; "wt", "1.";
		   "nblocks", "62500"; "min-expansions", "32" ]);

    "safepbnf",
    ("SafePBNF", [ "alg", "safepbnf-uninstrumented"; "wt", "1.";
		   "nblocks", "62500"; "min-expansions", "32" ]);

    "ahdastar-instrumented",
    ("AHDA*", [ "alg", "ahdastar-instrumented"; "wt", "1.";
		"nblocks", "10000"; "max-expansions", "64"]);

    "ahdastar",
    ("AHDA*", [ "alg", "ahdastar-uninstrumented"; "wt", "1.";
		"nblocks", "10000"; "max-expansions", "64"]);
  ]


let grid_best =
  (** [grid_best ?extra_attrs costs moves prob width height alg] looks
      up the data for the given algorithm. *)
  let cache = Cache.create 10 in
  let load_data
      (costs, moves, prob, width, height, alg, extra_attrs, replace) =
    let name, attrs = List.assoc alg grid_bests in
    let attrs' =
      List.map (fun ((vr, vl) as a) ->
		  try
		    let vl' = List.assoc vr replace in
		      vr, vl'
		  with Not_found -> a)
	attrs
    in
    let attrs'' = extra_attrs @ attrs' in
      Verb.pr Verb.toplvl "Loading with attributes: %s\n"
	(Rdb.human_str attrs'');
      load_grids costs moves prob width height attrs'' name
  in
    (fun ?(extra_attrs=[]) ?(replace=[]) costs moves prob width height alg ->
       let ds =
	 Cache.find_or_compute cache load_data
	   (costs, moves, prob, width, height, alg, extra_attrs, replace)
       in
	 Verb.pr Verb.toplvl "Loaded %s\n" alg;
	 ds)


(************************************************************)
(* Boxplots of values at 1 thread.                          *)
(************************************************************)

let tiles_1thread_boxplot key model =
  let extra_attrs = [ "threads", "1" ] in
  let dss = [ tiles_best model "astar";
	      tiles_best model ~extra_attrs ~replace:["min-expansions", "1"]
		"safepbnf";
	      tiles_best model ~extra_attrs ~replace:["max-expansions", "1"]
		"ahdastar";
	    ] in
  let datasets = Dataset_to_spt.boxplots key dss in
  let plot =
    Num_by_nom.plot ~title:"snlemons easy 1 thread" ~ylabel:key datasets
  in plot#display

let tiles_8thread_boxs ~key ?(ylabel=key) model =
  let extra_attrs = [ "threads", "8"  ] in
  let dss = [ tiles_best model ~extra_attrs "safepbnf-instrumented";
	      tiles_best model ~extra_attrs "ahdastar-instrumented";
	    ] in
  let datasets = Dataset_to_spt.boxplots key dss in
  let plot =
    Num_by_nom.plot ~ylabel datasets ~y_min:0.
      ~legend_text_style:ps_plot_text_style
      ~label_text_style:ps_plot_text_style
      ~tick_text_style:ps_plot_tick_text_style
  in plot#display


let tiles_across_threads key model =
  let dss = [ tiles_best model "safepbnf"; tiles_best model"ahdastar"; ] in
  let datasets =
    Dataset_to_spt.line_errs ~group_keys:[|"num"|] ~ykey:key
      ~xkey:"threads" dss
  in
  let title = model in
  let plot =
    Num_by_num.plot ~title ~xlabel:"threads" ~ylabel:key datasets ~y_min:0.
  in plot#display


let tiles_total_nodes_1 = tiles_1thread_boxplot "total nodes expanded"
let tiles_nodes_per_second_1 = tiles_1thread_boxplot "nodes per cpu second"
let tiles_nodes_per_second = tiles_across_threads "nodes per cpu second"

let tiles_open_cpu_time8 =
  tiles_8thread_boxs ~ylabel:"Mean CPU time (seconds)" ~key:"mean pq cpu time"
let tiles_gens8 = tiles_8thread_boxs ~key:"total nodes generated"
let tiles_exps8 = tiles_8thread_boxs ~key:"total nodes expanded"

let grid_1thread_boxplot key costs moves prob width height =
  let extra_attrs = [ "threads", "1" ] in
  let dss = [ (*grid_best costs moves prob width height "astar"; *)

	      grid_best costs moves prob width height
		~extra_attrs "safepbnf";

	      grid_best costs moves prob width height
		~extra_attrs "ahdastar";
	    ] in
  let datasets = Dataset_to_spt.boxplots key dss in
  let title = sprintf "%dx%dx%f %s %s" width height prob costs moves in
  let plot = Num_by_nom.plot ~title ~ylabel:key datasets ~y_min:0.
  in plot#display


let grid_8thread_boxs ~key ?(ylabel=key) costs moves prob width height =
  let extra_attrs = [ "threads", "8"  ] in
  let dss = [ (*grid_best costs moves prob width height "astar"; *)

    grid_best costs moves prob width height
      ~extra_attrs "safepbnf-instrumented";

    grid_best costs moves prob width height
      ~extra_attrs "ahdastar-instrumented";
  ] in
  let datasets = Dataset_to_spt.boxplots key dss in
  let plot =
    Num_by_nom.plot ~ylabel datasets ~y_min:0.
      ~legend_text_style:ps_plot_text_style
      ~label_text_style:ps_plot_text_style
      ~tick_text_style:ps_plot_tick_text_style
  in plot#display

let grid_open_cpu_time8 =
  grid_8thread_boxs ~ylabel:"Mean CPU time (seconds)" ~key:"mean pq cpu time"
let grid_gens8 = grid_8thread_boxs ~key:"total nodes generated"
let grid_exps8 = grid_8thread_boxs ~key:"total nodes expanded"


let grid_across_threads key costs moves prob width height =
  let dss = [
    grid_best costs moves prob width height "safepbnf";
    grid_best costs moves prob width height "ahdastar";
  ] in
  let datasets =
    Dataset_to_spt.line_errs ~group_keys:[|"num"|] ~ykey:key
      ~xkey:"threads" dss
  in
  let title = sprintf "%dx%dx%f %s %s" width height prob costs moves in
  let plot =
    Num_by_num.plot ~title ~xlabel:"threads" ~ylabel:key datasets ~y_min:0.
  in plot#display


let grid_nodes_per_second_1 = grid_1thread_boxplot "nodes per cpu second"
let grid_nodes_per_second = grid_across_threads "nodes per cpu second"
let grid_exp_line = grid_across_threads "total nodes expanded"
let grid_gen_line = grid_across_threads "total nodes generated"


let open_list_boxplots () =
  let extra_attrs = [ "threads", "8"  ] in
  let key = "mean pq cpu time" in
  let ylabel = "Mean CPU time (seconds)" in
  let model = "snlemons_easy" in
  let tiles = [ tiles_best model ~extra_attrs "safepbnf-instrumented";
		tiles_best model ~extra_attrs "ahdastar-instrumented";
	      ] in
  let width = 5000 and height = 5000 in
  let costs = "Unit" and moves = "Four-way" and prob = 0.35 in
  let grid = [ grid_best costs moves prob width height
		 ~extra_attrs "safepbnf-instrumented";
	       grid_best costs moves prob width height
		 ~extra_attrs "ahdastar-instrumented";
	     ] in
  let grid_group =
    Num_by_nom.dataset_group "Grid pathfinding"
      (Dataset_to_spt.boxplots key grid) in
  let tiles_group =
    Num_by_nom.dataset_group "15-puzzle"
      (Dataset_to_spt.boxplots key tiles) in
  let plot =
    Num_by_nom.plot ~ylabel [grid_group; tiles_group] ~y_min:0.
      ~legend_text_style:{ ps_plot_text_style with
			     Drawing.text_size = Length.Pt 10. }
      ~label_text_style:{ ps_plot_text_style with
			    Drawing.text_size = Length.Pt 10. }
      ~tick_text_style:ps_plot_tick_text_style
  in
    plot#set_size ~w:(Length.In 4.) ~h:(Length.In 2.5);
    plot#display

(************************************************************)
(************************************************************)

let tiles_best_wall_time model =
  let dss = [ tiles_best model "safepbnf";
	      tiles_best model "ahdastar"; ] in
  let datasets =
    Dataset_to_spt.line_errs ~use_color:true ~xkey:"threads"
      ~ykey:"total wall time" ~group_keys:[|"num"|] dss
  in
  let plot =
    Num_by_num.plot ~title:"snlemons easy 8 threads"
      ~ylabel:"total wall time (seconds)" ~xlabel:"threads" datasets
  in plot#display


let tiles_best_expansions model =
  let dss = [ tiles_best model "safepbnf"; tiles_best model "ahdastar"; ] in
  let datasets =
    Dataset_to_spt.line_errs ~use_color:true ~xkey:"threads"
      ~ykey:"total nodes expanded" ~group_keys:[|"num"|] dss
  in
  let plot =
    Num_by_num.plot ~title:"snlemons easy 8 threads"
      ~ylabel:"total nodes expanded" ~xlabel:"threads" datasets
  in plot#display


let tiles_best_expansions_8 model =
  let extra_attrs = [ "threads", "8"; ] in
  let dss = [ tiles_best model ~extra_attrs "safepbnf";
	      tiles_best model ~extra_attrs "ahdastar"; ]
  in
  let datasets =
    Dataset_to_spt.boxplots ~key:"total nodes expanded" dss
  in
  let plot =
    Num_by_nom.plot ~title:"snlemons easy 8 threads"
      ~ylabel:"total nodes expanded" datasets
  in plot#display


let tiles_best_queue_size_1 model =
  let extra_attrs = [ "threads", "1"; ] in
  let dss = [ tiles_best model ~extra_attrs "safepbnf-instrumented";
	      tiles_best model ~extra_attrs "ahdastar-instrumented"; ]
  in
  let datasets =
    Dataset_to_spt.boxplots ~key:"average open size" dss
  in
  let plot =
    Num_by_nom.plot ~title:"snlemons easy 1 threads"
      ~ylabel:"average open size" datasets
  in plot#display


(************************************************************)
(************************************************************)

let grid_best_wall_time costs moves prob width height =
  let dss = [ grid_best costs moves prob width height "safepbnf";
	      grid_best costs moves prob width height "ahdastar"; ] in
  let datasets =
    Dataset_to_spt.line_errs ~use_color:true ~xkey:"threads"
      ~ykey:"total wall time" ~group_keys:[|"num"|] dss
  in
  let title = sprintf "%dx%dx%f %s %s" width height prob costs moves in
  let plot =
    Num_by_num.plot ~title ~ylabel:"total wall time (seconds)"
      ~xlabel:"threads" datasets
  in plot#display


let grid_best_expansions costs moves prob width height =
  let dss = [ grid_best costs moves prob width height "safepbnf-instrumented";
	      grid_best costs moves prob width height "ahdastar-instrumented"; ] in
  let datasets =
    Dataset_to_spt.line_errs ~use_color:true ~xkey:"threads"
      ~ykey:"total nodes expanded" ~group_keys:[|"num"|] dss
  in
  let title = sprintf "%dx%dx%f %s %s" width height prob costs moves in
  let plot =
    Num_by_num.plot ~title ~ylabel:"total nodes expanded"
      ~xlabel:"threads" datasets
  in plot#display


let grid_best_expansions_8 costs moves prob width height =
  let extra_attrs = [ "threads", "8"; ] in
  let dss = [ grid_best ~extra_attrs costs moves prob width height "safepbnf";
	      grid_best ~extra_attrs costs moves prob width height "ahdastar";
	    ] in
  let datasets =
    Dataset_to_spt.boxplots ~key:"total nodes expanded" dss
  in
  let title = sprintf "%dx%dx%f %s %s" width height prob costs moves in
  let plot =
    Num_by_nom.plot ~title ~ylabel:"total nodes expanded" datasets
  in plot#display


let grid_best_queue_size_1 costs moves prob width height =
  let extra_attrs = [ "threads", "1"; ] in
  let dss = [ grid_best ~extra_attrs costs moves prob width height
		"safepbnf-instrumented";
	      grid_best ~extra_attrs costs moves prob width height
		"ahdastar-instrumented";
	    ] in
  let datasets =
    Dataset_to_spt.boxplots ~key:"average open size" dss
  in
  let title =
    sprintf "%dx%dx%f %s %s 1 thread" width height prob costs moves in
  let plot =
    Num_by_nom.plot ~title ~ylabel:"average open size" datasets
      ~y_min:0.
  in plot#display


(************************************************************)
(************************************************************)

let tiles_wall_time model =
  let extra_attrs = [ "threads", "8" ] in
  let dss = [ tiles_best model ~extra_attrs "safepbnf-instrumented";
	      tiles_best model ~extra_attrs "ahdastar-instrumented"; ]
  in
  let datasets =
    Dataset_to_spt.scatters ~use_color:true ~xkey:"log10 total nodes expanded"
      ~ykey:"total wall time" dss
  in
  let plot =
    Num_by_num.plot ~title:"snlemons easy 8 threads"
      ~ylabel:"total wall time (seconds)" ~xlabel:"log10 nodes expanded"
      ~legend_loc:Legend.Upper_left datasets
  in plot#display


let tiles_mean_open_size model =
  let extra_attrs = [ "threads", "8" ] in
  let dss = [ tiles_best model ~extra_attrs "safepbnf-instrumented";
	      tiles_best model ~extra_attrs "ahdastar-instrumented"; ]
  in
  let datasets =
    Dataset_to_spt.scatters ~use_color:true ~xkey:"log10 total nodes expanded"
      ~ykey:"average open size" dss
  in
  let plot =
    Num_by_num.plot ~title:"snlemons easy 8 threads average open list size"
      ~ylabel:"nodes" ~xlabel:"log10 nodes expanded"
      ~legend_loc:Legend.Upper_left datasets
  in plot#display


let tiles_max_open_size model =
  let extra_attrs = [ "threads", "8" ] in
  let dss = [ tiles_best model ~extra_attrs "safepbnf-instrumented";
	      tiles_best model ~extra_attrs "ahdastar-instrumented"; ]
  in
  let datasets =
    Dataset_to_spt.scatters ~use_color:true ~xkey:"log10 total nodes expanded"
      ~ykey:"max open size" dss
  in
  let plot =
    Num_by_num.plot ~title:"snlemons easy 8 threads max open list size"
      ~ylabel:"nodes" ~xlabel:"log10 nodes expanded"
      ~legend_loc:Legend.Upper_left datasets
  in plot#display


let tiles_working_time model =
  let extra_attrs = [ "threads", "8" ] in
  let safepbnf = tiles_best model ~extra_attrs "safepbnf-instrumented" in
  let ahdastar = tiles_best model ~extra_attrs "ahdastar-instrumented" in
  let safepbnf =
    Dataset_to_spt.scatter ~color:Drawing.dark_red ~glyph:Drawing.Ring_glyph
      ~xkey:"log10 total nodes expanded" ~ykey:"working cpu time"
      (Dataset.with_name "SafePBNF less" safepbnf)
  in
  let ahdastar =
    Dataset_to_spt.scatter ~color:Drawing.dark_green ~glyph:Drawing.Cross_glyph
      ~xkey:"log10 total nodes expanded" ~ykey:"working cpu time"
      (Dataset.with_name "AHDA* less" ahdastar)
  in
  let plot =
    Num_by_num.plot ~title:"snlemons easy 8 threads"
      ~ylabel:"(wall time - wasted time) * threads"
      ~xlabel:"log10 nodes expanded"
      ~legend_loc:Legend.Upper_left
      [ safepbnf; ahdastar; ]
  in plot#display


let doing_nothing_8 model cost moves prob width height =
  let extra_attrs = [ "threads", "8" ] in
  let safepbnf_tiles = tiles_best model ~extra_attrs "safepbnf-instrumented" in
  let ahdastar_tiles = tiles_best model ~extra_attrs "ahdastar-instrumented" in
(*
  let bfpsdd_tiles = tiles_best model ~extra_attrs "bfpsdd-instrumented" in
*)
  let safepbnf_grids =
    grid_best cost moves prob width height ~extra_attrs
      "safepbnf-instrumented" in
  let ahdastar_grids =
    grid_best cost moves prob width height ~extra_attrs
      "ahdastar-instrumented" in
  let ykey_lock = "thread lock percent" in
  let ykey_wait = "thread wait percent" in
  let ykey_sum = "thread coord percent" in
  let ylabel = "percentage of wall time" in
  let safepbnf_lock_tiles =
    Dataset_to_spt.boxplot ~key:ykey_lock ~outliers:false safepbnf_tiles
  in
  let safepbnf_wait_tiles =
    Dataset_to_spt.boxplot ~key:ykey_wait ~outliers:false safepbnf_tiles
  in
  let safepbnf_sum_tiles =
    Dataset_to_spt.boxplot ~key:ykey_sum ~outliers:false safepbnf_tiles
  in
  let ahdastar_lock_tiles =
    Dataset_to_spt.boxplot ~key:ykey_lock ~outliers:false ahdastar_tiles
  in
  let ahdastar_wait_tiles =
    Dataset_to_spt.boxplot ~key:ykey_wait ~outliers:false ahdastar_tiles
  in
  let ahdastar_sum_tiles =
    Dataset_to_spt.boxplot ~key:ykey_sum ~outliers:false ahdastar_tiles
  in
(*
  let bfpsdd_lock_tiles =
    Dataset_to_spt.boxplot ~key:ykey_lock ~outliers:false bfpsdd_tiles
  in
  let bfpsdd_wait_tiles =
    Dataset_to_spt.boxplot ~key:ykey_wait ~outliers:false bfpsdd_tiles
  in
  let bfpsdd_sum_tiles =
    Dataset_to_spt.boxplot ~key:ykey_sum ~outliers:false bfpsdd_tiles
  in
*)
  let safepbnf_lock_grids =
    Dataset_to_spt.boxplot ~key:ykey_lock ~outliers:false safepbnf_grids
  in
  let safepbnf_wait_grids =
    Dataset_to_spt.boxplot ~key:ykey_wait ~outliers:false safepbnf_grids
  in
  let safepbnf_sum_grids =
    Dataset_to_spt.boxplot ~key:ykey_sum ~outliers:false safepbnf_grids
  in
  let ahdastar_lock_grids =
    Dataset_to_spt.boxplot ~key:ykey_lock ~outliers:false ahdastar_grids
  in
  let ahdastar_wait_grids =
    Dataset_to_spt.boxplot ~key:ykey_wait ~outliers:false ahdastar_grids
  in
  let ahdastar_sum_grids =
    Dataset_to_spt.boxplot ~key:ykey_sum ~outliers:false ahdastar_grids
  in
  let grids =
    Num_by_nom.plot ~ylabel
      ~legend_text_style:{ ps_plot_text_style with
			     Drawing.text_size = Length.Pt 10. }
      ~label_text_style:{ ps_plot_text_style with
			    Drawing.text_size = Length.Pt 10. }
      ~tick_text_style:ps_plot_tick_text_style
      [ Num_by_nom.dataset_group "Locks" [ safepbnf_lock_grids;
					   ahdastar_lock_grids; ];
	Num_by_nom.dataset_group "Wait" [ safepbnf_wait_grids;
					  ahdastar_wait_grids; ];
	Num_by_nom.dataset_group "Sum" [ safepbnf_sum_grids;
					 ahdastar_sum_grids; ];
      ]
  in
  let tiles =
    Num_by_nom.plot ~ylabel
      ~legend_text_style:{ ps_plot_text_style with
			     Drawing.text_size = Length.Pt 10. }
      ~label_text_style:{ ps_plot_text_style with
			    Drawing.text_size = Length.Pt 10. }
      ~tick_text_style:ps_plot_tick_text_style
      [ Num_by_nom.dataset_group "Locks" [ safepbnf_lock_tiles;
					   ahdastar_lock_tiles;
					   (* bfpsdd_lock_tiles; *) ];
	Num_by_nom.dataset_group "Wait" [ safepbnf_wait_tiles;
					  ahdastar_wait_tiles;
					  (* bfpsdd_wait_tiles; *) ];
	Num_by_nom.dataset_group "Sum" [ safepbnf_sum_tiles;
					 ahdastar_sum_tiles;
					 (* bfpsdd_sum_tiles;*)];
      ]
  in
    grids#set_size ~w:(Length.In 5.) ~h:(Length.In 2.);
    grids#display;
    tiles#set_size ~w:(Length.In 5.) ~h:(Length.In 2.);
    tiles#display



(************************************************************)
(* f values.                                                *)
(************************************************************)

(* This code is for plotting f value histograms or densities by
   loading pairs of numbers from spt-it-out input files. *)

let load_points datafile =
  (** [load_points datafile] loads x,y pairs from the given
      datafile. *)
  let xs = ref [] in
    Wrio.with_infile datafile
      (fun ichan ->
	 try
	   while true do
	     let x, y = Scanf.fscanf ichan "%f %f\n" Fn.gather2 in
	       xs := (Geometry.point x y) :: !xs
	   done;
	 with End_of_file -> ());
    Array.of_list !xs


let tiles_plot_fs () =
  let pbnf_pts = load_points "tiles-pbnf-fs.dat" in
  let ahdastar_pts = load_points "tiles-ahdastar-fs.dat" in
  let datasets =
    Num_by_num.points_cdf_datasets ~normalize:false ~color:false
      [("Safe PBNF", pbnf_pts); ("AHDA*", ahdastar_pts)]
  in
  let plot = (Num_by_num.plot
(*
		~title:"snlemons easy tiles 8 threads: f values"
*)
		~legend_loc:Legend.Upper_left
		~xlabel:"Factor of optimal cost"
		~ylabel:"Cumulative expansions"
		~legend_text_style:ps_plot_text_style
		~label_text_style:ps_plot_text_style
		~tick_text_style:ps_plot_tick_text_style
		datasets)
  in
    plot#set_size ~w:(Length.In 4.) ~h:(Length.In 4.);
    plot#display


let unit4_plot_fs () =
  let pbnf_pts = load_points "unit4-pbnf-fs.dat" in
  let ahdastar_pts = load_points "unit4-ahdastar-fs.dat" in
  let datasets =
    Num_by_num.points_cdf_datasets ~normalize:false ~color:false
      [("Safe PBNF", pbnf_pts); ("AHDA*", ahdastar_pts)]
  in
  let plot = (Num_by_num.plot
(*
		~title:"unit4 grids 5000x5000x0.35 8 threads: f values"
*)
		~legend_loc:Legend.Upper_left
		~xlabel:"Factor of optimal cost"
		~ylabel:"Cumulative expansions"
		~legend_text_style:ps_plot_text_style
		~label_text_style:ps_plot_text_style
		~tick_text_style:ps_plot_tick_text_style
		datasets)
  in
    plot#set_size ~w:(Length.In 4.) ~h:(Length.In 4.);
    plot#display

(************************************************************)
(* modeling performance on open list size.                  *)
(************************************************************)

let build_model ds =
  (** [build_model ds] builds the model given the dataset. *)
  let keys = [| "total wall time"; "average open size";
		"generations per thread" |] in
  let cols = Dataset.get_column_vector float_of_string keys ds in
  let wall_times = cols.(0) in
  let open_sizes = cols.(1) in
  let total_gens = cols.(2) in
  let xs =
    Array.init (Array.length wall_times)
      (fun i ->
	 let open_size = open_sizes.(i) and total_gen = total_gens.(i) in
	   [| 1.; (Math.log2 open_size) *. total_gen; total_gen |])
  in
  let p = 100 in
  let coeffs = Offline_lms.least_median_of_squares ~eps:0.05 p xs wall_times in
    printf "%s: %g + %g g lg s + %g g\n" (Dataset.name ds)
      coeffs.(0) coeffs.(1) coeffs.(2);
    coeffs

(*

let build_model ds =
  (** [build_model ds] builds the model given the dataset. *)
  let keys = [| "total wall time"; "average open size";
		"generations per thread" |] in
  let cols = Dataset.get_column_vector float_of_string keys ds in
  let wall_times = cols.(0) in
  let open_sizes = cols.(1) in
  let total_gens = cols.(2) in
  let size = Array.length total_gens in
  let xs =
(*
    Array.init (Array.length wall_times)
      (fun i ->
	 let open_size = open_sizes.(i) and total_gen = total_gens.(i) in
	   [| 1.; (Math.log2 open_size) *. total_gen; total_gen |])
*)
   [| Array.create size 1.;
      Wrarray.map2 (fun s g -> (Math.log2 s) *. g) open_sizes total_gens;
      total_gens;
   |]
  in
  let a = Wrarray.flatten xs in
  let b = wall_times in
  let m = size and n = 3 in
  let dims = m, n in
  let bl = Array.create n 0. in
  let bu = Array.create n infinity in
    assert ((Array.length a) = (m * n));
    let coeffs = Bvls.bvls dims a b bl bu in
      printf "%s: %g + %g g lg s + %g g\n%!" (Dataset.name ds)
	coeffs.(0) coeffs.(1) coeffs.(2);
      coeffs
*)

let tiles_build_model model alg =
  (** [tiles_build_model model alg] builds a model of generations per second
      based on the open list size. *)
  let extra_attrs = [ "threads", "1" ] in
  let ds = tiles_best model ~extra_attrs alg in
    build_model ds


let grid_build_model costs moves prob width height alg =
  (** [grid_build_model costs moves prob width height alg] builds a
      model of generations per second based on the open list size. *)
  let extra_attrs = [ "threads", "1" ] in
  let ds = grid_best costs moves prob width height ~extra_attrs alg in
    build_model ds


let add_estimated_wall coeffs ds =
  (** [add_estimated_wall coeffs ds] add a key to estimate the wall
      time based on the open list size. *)
  let match_keys = [| "num"; "threads"; "wt" |] in
  let ds = Dataset.copy_key "average open size" "estimated wall time" ds in
    Dataset.transform_with match_keys ds "estimated wall time"
      ~with_key:"generations per thread"
      (fun gens_per_thr size ->
	 coeffs.(0)
	 +. coeffs.(1) *. (Math.log2 size) *. gens_per_thr
	 +. coeffs.(2) *. gens_per_thr)
      ds


let add_estimated_overhead ds =
  (** [add_estimated_overhead ds] adds a key that estimations the
      communication-related overhead by subtracting the estimated wall
      time (due to openlist) from the actual wall time. *)
  let match_keys = [| "num"; "threads"; "wt" |] in
  let ds = Dataset.copy_key "estimated wall time" "estimated overhead" ds in
    Dataset.transform_with match_keys ds "estimated overhead"
      ~with_key:"total wall time" (fun actual est -> actual -. est) ds


let tiles_wall_time_model model =
  let extra_attrs = [ "threads", "1" ] in
  let safepbnf = tiles_best model ~extra_attrs "safepbnf-instrumented" in
  let safe_model = tiles_build_model model "safepbnf-instrumented" in
  let safepbnf = add_estimated_wall safe_model safepbnf in
  let ahdastar = tiles_best model ~extra_attrs "ahdastar-instrumented" in
  let ahdastar_model = tiles_build_model model "ahdastar-instrumented" in
  let ahdastar = add_estimated_wall ahdastar_model ahdastar in
  let safepbnf_time =
    Dataset_to_spt.scatter ~color:Drawing.dark_red ~glyph:Drawing.Ring_glyph
      ~xkey:"total nodes generated" ~ykey:"total wall time"
      (Dataset.with_name "SafePBNF wall time" safepbnf)
  in
  let safepbnf_est =
    Dataset_to_spt.scatter ~color:Drawing.dark_red ~glyph:Drawing.Cross_glyph
      ~xkey:"total nodes generated" ~ykey:"estimated wall time"
      (Dataset.with_name "SafePBNF estimated time" safepbnf)
  in
  let ahdastar_time =
    Dataset_to_spt.scatter ~color:Drawing.dark_green ~glyph:Drawing.Ring_glyph
      ~xkey:"total nodes generated" ~ykey:"total wall time"
      (Dataset.with_name "AHDA* wall time" ahdastar)
  in
  let ahdastar_est =
    Dataset_to_spt.scatter ~color:Drawing.dark_green ~glyph:Drawing.Cross_glyph
      ~xkey:"total nodes generated" ~ykey:"estimated wall time"
      (Dataset.with_name "AHDA* estimated time" ahdastar)
  in
  let plot =
    Num_by_num.plot ~title:"snlemons easy 1 thread"
      ~ylabel:"wall time (seconds)" ~xlabel:"total nodes generated"
      ~legend_loc:Legend.Upper_left [ safepbnf_time; safepbnf_est;
				      ahdastar_time; ahdastar_est; ]
  in
    plot#set_size ~w:(Length.In 10.5) ~h:(Length.In 8.);
    plot#display


let tiles_estimate_wall_time model =
  let extra_attrs = [ "threads", "8" ] in
  let safepbnf = tiles_best model ~extra_attrs "safepbnf-instrumented" in
  let safe_model = tiles_build_model model "safepbnf-instrumented" in
  let safepbnf = add_estimated_wall safe_model safepbnf in
  let ahdastar = tiles_best model ~extra_attrs "ahdastar-instrumented" in
  let ahdastar_model = tiles_build_model model "ahdastar-instrumented" in
  let ahdastar = add_estimated_wall ahdastar_model ahdastar in
  let safepbnf_time =
    Dataset_to_spt.scatter ~color:Drawing.dark_red ~glyph:Drawing.Ring_glyph
      ~xkey:"total nodes generated" ~ykey:"total wall time"
      (Dataset.with_name "SafePBNF wall time" safepbnf)
  in
  let safepbnf_est =
    Dataset_to_spt.scatter ~color:Drawing.dark_red ~glyph:Drawing.Cross_glyph
      ~xkey:"total nodes generated" ~ykey:"estimated wall time"
      (Dataset.with_name "SafePBNF estimated time" safepbnf)
  in
  let ahdastar_time =
    Dataset_to_spt.scatter ~color:Drawing.dark_green ~glyph:Drawing.Ring_glyph
      ~xkey:"total nodes generated" ~ykey:"total wall time"
      (Dataset.with_name "AHDA* wall time" ahdastar)
  in
  let ahdastar_est =
    Dataset_to_spt.scatter ~color:Drawing.dark_green ~glyph:Drawing.Cross_glyph
      ~xkey:"total nodes generated" ~ykey:"estimated wall time"
      (Dataset.with_name "AHDA* estimated time" ahdastar)
  in
  let plot =
    Num_by_num.plot ~title:"snlemons easy 8 threads"
      ~ylabel:"wall time (seconds)" ~xlabel:"total nodes generated"
      ~legend_loc:Legend.Upper_left [ safepbnf_time; safepbnf_est;
				      ahdastar_time; ahdastar_est; ]
  in
    plot#set_size ~w:(Length.In 10.5) ~h:(Length.In 8.);
    plot#display


let tiles_estimate_overhead model =
  let extra_attrs = [ "threads", "8" ] in
  let safepbnf = tiles_best model ~extra_attrs "safepbnf-instrumented" in
  let safe_model = tiles_build_model model "safepbnf-instrumented" in
  let safepbnf =
    add_estimated_overhead (add_estimated_wall safe_model safepbnf) in
  let ahdastar = tiles_best model ~extra_attrs "ahdastar-instrumented" in
  let ahdastar_model = tiles_build_model model "ahdastar-instrumented" in
  let ahdastar =
    add_estimated_overhead (add_estimated_wall ahdastar_model ahdastar) in
  let safepbnf_time =
    Dataset_to_spt.boxplot ~key:"total wall time" safepbnf
  in
  let safepbnf_est =
    Dataset_to_spt.boxplot ~key:"estimated wall time" safepbnf
  in
  let safepbnf_overhead =
    Dataset_to_spt.boxplot ~key:"estimated overhead" safepbnf
  in
  let ahdastar_time =
    Dataset_to_spt.boxplot ~key:"total wall time" ahdastar
  in
  let ahdastar_est =
    Dataset_to_spt.boxplot ~key:"estimated wall time" ahdastar
  in
  let ahdastar_overhead =
    Dataset_to_spt.boxplot ~key:"estimated overhead" ahdastar
  in
  let wall_time_group =
    Num_by_nom.dataset_group "Wall time" [ safepbnf_time; ahdastar_time; ]
  in
  let pq_time_group =
    Num_by_nom.dataset_group "Estimated generation time"
      [ safepbnf_est; ahdastar_est; ]
  in
  let overhead_group =
    Num_by_nom.dataset_group "Estimated communication overhead"
      [ safepbnf_overhead; ahdastar_overhead; ]
  in
  let plot =
    Num_by_nom.plot ~title:"snlemons easy 8 threads"
      ~ylabel:"wall time (seconds)"
      [ wall_time_group; pq_time_group; overhead_group; ]
  in
    plot#set_size ~w:(Length.In 10.5) ~h:(Length.In 8.);
    plot#display


let grid_wall_time_model costs moves prob width height =
  let extra_attrs = [ "threads", "1" ] in
  let safepbnf =
    grid_best costs moves prob width height ~extra_attrs
      "safepbnf-instrumented" in
  let safe_model =
    grid_build_model costs moves prob width height "safepbnf-instrumented" in
  let safepbnf = add_estimated_wall safe_model safepbnf in
  let ahdastar =
    grid_best costs moves prob width height ~extra_attrs
      "ahdastar-instrumented" in
  let ahdastar_model =
    grid_build_model costs moves prob width height "ahdastar-instrumented" in
  let ahdastar = add_estimated_wall ahdastar_model ahdastar in
  let safepbnf_time =
    Dataset_to_spt.scatter ~color:Drawing.dark_red ~glyph:Drawing.Ring_glyph
      ~xkey:"total nodes generated" ~ykey:"total wall time"
      (Dataset.with_name "SafePBNF wall time" safepbnf)
  in
  let safepbnf_est =
    Dataset_to_spt.scatter ~color:Drawing.dark_red ~glyph:Drawing.Cross_glyph
      ~xkey:"total nodes generated" ~ykey:"estimated wall time"
      (Dataset.with_name "SafePBNF estimated time" safepbnf)
  in
  let ahdastar_time =
    Dataset_to_spt.scatter ~color:Drawing.dark_green ~glyph:Drawing.Ring_glyph
      ~xkey:"total nodes generated" ~ykey:"total wall time"
      (Dataset.with_name "AHDA* wall time" ahdastar)
  in
  let ahdastar_est =
    Dataset_to_spt.scatter ~color:Drawing.dark_green ~glyph:Drawing.Cross_glyph
      ~xkey:"total nodes generated" ~ykey:"estimated wall time"
      (Dataset.with_name "AHDA* estimated time" ahdastar)
  in
  let title = sprintf "%dx%dx%f %s %s" width height prob costs moves in
  let plot =
    Num_by_num.plot ~title ~ylabel:"wall time (seconds)"
      ~xlabel:"total nodes generated" ~legend_loc:Legend.Upper_left
      [ safepbnf_time; safepbnf_est; ahdastar_time; ahdastar_est; ]
  in
    plot#set_size ~w:(Length.In 10.5) ~h:(Length.In 8.);
    plot#display


let grid_estimate_overhead costs moves prob width height =
  let extra_attrs = [ "threads", "8" ] in
  let safepbnf =
    grid_best costs moves prob width height ~extra_attrs
      "safepbnf-instrumented" in
  let safe_model =
    grid_build_model costs moves prob width height "safepbnf-instrumented" in
  let safepbnf =
    add_estimated_overhead (add_estimated_wall safe_model safepbnf) in
  let ahdastar =
    grid_best costs moves prob width height ~extra_attrs
      "ahdastar-instrumented" in
  let ahdastar_model =
    grid_build_model costs moves prob width height "ahdastar-instrumented" in
  let ahdastar =
    add_estimated_overhead (add_estimated_wall ahdastar_model ahdastar) in
  let safepbnf_time =
    Dataset_to_spt.boxplot ~key:"total wall time" safepbnf
  in
  let safepbnf_est =
    Dataset_to_spt.boxplot ~key:"estimated wall time" safepbnf
  in
  let safepbnf_overhead =
    Dataset_to_spt.boxplot ~key:"estimated overhead" safepbnf
  in
  let ahdastar_time =
    Dataset_to_spt.boxplot ~key:"total wall time" ahdastar
  in
  let ahdastar_est =
    Dataset_to_spt.boxplot ~key:"estimated wall time" ahdastar
  in
  let ahdastar_overhead =
    Dataset_to_spt.boxplot ~key:"estimated overhead" ahdastar
  in
  let wall_time_group =
    Num_by_nom.dataset_group "Wall time" [ safepbnf_time; ahdastar_time; ]
  in
  let pq_time_group =
    Num_by_nom.dataset_group "Estimated generation time"
      [ safepbnf_est; ahdastar_est; ]
  in
  let overhead_group =
    Num_by_nom.dataset_group "Estimated communication overhead"
      [ safepbnf_overhead; ahdastar_overhead; ]
  in
  let title = sprintf "%dx%dx%f %s %s" width height prob costs moves in
  let plot =
    Num_by_nom.plot ~title ~ylabel:"wall time (seconds)"
      [ wall_time_group; pq_time_group; overhead_group; ]
  in
    plot#set_size ~w:(Length.In 10.5) ~h:(Length.In 8.);
    plot#display


let grid_estimate_wall_time costs moves prob width height =
  let extra_attrs = [ "threads", "8" ] in
  let safepbnf =
    grid_best costs moves prob width height ~extra_attrs
      "safepbnf-instrumented" in
  let safe_model =
    grid_build_model costs moves prob width height "safepbnf-instrumented" in
  let safepbnf = add_estimated_wall safe_model safepbnf in
  let ahdastar =
    grid_best costs moves prob width height ~extra_attrs
      "ahdastar-instrumented" in
  let ahdastar_model =
    grid_build_model costs moves prob width height "ahdastar-instrumented" in
  let ahdastar = add_estimated_wall ahdastar_model ahdastar in
  let safepbnf_time =
    Dataset_to_spt.scatter ~color:Drawing.dark_red ~glyph:Drawing.Ring_glyph
      ~xkey:"total nodes generated" ~ykey:"total wall time"
      (Dataset.with_name "SafePBNF wall time" safepbnf)
  in
  let safepbnf_est =
    Dataset_to_spt.scatter ~color:Drawing.dark_red ~glyph:Drawing.Cross_glyph
      ~xkey:"total nodes generated" ~ykey:"estimated wall time"
      (Dataset.with_name "SafePBNF estimated time" safepbnf)
  in
  let ahdastar_time =
    Dataset_to_spt.scatter ~color:Drawing.dark_green ~glyph:Drawing.Ring_glyph
      ~xkey:"total nodes generated" ~ykey:"total wall time"
      (Dataset.with_name "AHDA* wall time" ahdastar)
  in
  let ahdastar_est =
    Dataset_to_spt.scatter ~color:Drawing.dark_green ~glyph:Drawing.Cross_glyph
      ~xkey:"total nodes generated" ~ykey:"estimated wall time"
      (Dataset.with_name "AHDA* estimated time" ahdastar)
  in
  let title = sprintf "%dx%dx%f %s %s" width height prob costs moves in
  let plot =
    Num_by_num.plot ~title ~ylabel:"wall time (seconds)"
      ~xlabel:"total nodes generated" ~legend_loc:Legend.Upper_left
      [ safepbnf_time; safepbnf_est; ahdastar_time; ahdastar_est; ]
  in
    plot#set_size ~w:(Length.In 10.5) ~h:(Length.In 8.);
    plot#display
