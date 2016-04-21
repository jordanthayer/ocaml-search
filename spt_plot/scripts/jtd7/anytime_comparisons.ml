(**

    @author jtd7
    @since 2010-05-28

   Macros to do my sweet anytime comparisons, based on the IPC-2008 scoring
   criteria.

*)

let use_color = ref true
let log10x = ref false
let loc = ref (*(Legend.At (Legend.Text_before, 250., 220.))*)
  Legend.Upper_right
let maxx = ref 600.
let plot_max = ref 50000.

let xy_plot ~title ~xlabel ~xkey datasets ykey =
  let datasets = (if !log10x
		  then List.map (Dataset.numeric_transform xkey
				   (fun v -> if v = 0. then -2. else log10 v))
		    datasets
		  else datasets) in
  let dset = (Dataset_to_spt.padded_line_errs ~use_color:!use_color
		~maxx:!maxx
		~line_width:(Length.Pt 2.)
		~xkey ~ykey ~group_keys:[| "num" |] datasets) in
  let xlabel = if !log10x then "Log10 " ^ xlabel else xlabel in
  let plot = (Num_by_num.plot
		~legend_loc:!loc
		~x_max:(if !log10x then log10 !maxx else !maxx)
		~title ~xlabel
		~ylabel:(if ykey = "nodes generated" then ykey else
			   "Solution Quality")
		dset) in
    plot#display


let xy_diff_plot ~title ~xlabel ~xkey data ykey =
  let datasets = (if !log10x
		  then List.map (Dataset.numeric_transform xkey
				   (fun v -> if v = 0. then -2. else log10 v))
		    data
		  else data) in
  let get_xs = Dataset.get_values float_of_string xkey
  and get_ys = Dataset.get_values float_of_string ykey
  and get_num ds = (let nums = Dataset.get_values int_of_string "num" ds in
		      match nums with
			| [| i |] -> i
			| _ -> failwith "Should have only been one array.")
  and sort_num = List.sort (fun (_,a,_,_) (_,b,_,_) -> compare a b) in
  let data = List.map
    (fun ds ->
       let grouped = Dataset.group_by [|"num"|] ds
       and name = Dataset.get_name ds in
	 List.map (fun by_num ->
		     let xar = get_xs by_num
		     and yar = get_ys by_num in
		     let last_y = yar.((Array.length yar) - 1) in
		       name, get_num by_num,
		     Wrarray.extend xar 1 300., Wrarray.extend yar 1 last_y)
	   grouped) datasets in
    match data with
      |	[] -> failwith "Not Enough data"
      | base::algs ->
	  (let base = sort_num base
	   and algs = List.map sort_num algs in
	   let algs = List.map (fun alg ->
				  List.map2
				    (fun (_,bnum,xbs,ybs) (nm,num,xar,yar) ->
				       assert (bnum = num);
				       Verb.pe Verb.debug "nm: %i\n%!" num;
				       nm,xar, Line_diff.sing_instp xbs ybs xar yar)
				    base alg) algs in
	   let algs = (List.map
			 (fun alg ->
			    let (nm,_,_) = List.hd alg in
			      Some nm, Array.of_list
				(List.map
				   (fun (_,xar,yar) ->
				      Wrarray.map2 (fun x y ->
						      Geometry.point ~x ~y)
					xar yar) alg)) algs) in
	   let dset = (Line_errbar_dataset.line_errbar_datasets
			 ~line_width:(Length.Pt 2.) ~color:!use_color algs) in
	   let xlabel = if !log10x then "Log10 " ^ xlabel else xlabel in
	   let plot = (Num_by_num.plot
			 ~legend_loc:!loc
			 ~x_max:!maxx
			 ~y_min:0.5
			 ~title ~xlabel
			 ~ylabel:(ykey  ^ " vs. Baseline") dset) in
	     plot#display)


let def_time = ref 60.

let final_sol_histo time ~title ~xlabel ~xkey datasets ykey =
  (* separate out the instances *)
  let by_instance = List.map (Dataset.group_by [|"num"|]) datasets in
    (* get the vectors for each number *)
  let data = (List.map
		(fun ds_list ->
		   List.map (fun ds ->
			       Dataset.get_column_vector
				 float_of_string [|xkey; ykey|] ds)
		     ds_list) by_instance) in
    let data = (List.map2 (fun ds_list ds ->
			     Dataset.get_name ds,
			     Array.of_list
			       (List.map (fun inst ->
					    let sol_index =
					      (Wrarray.rfind (fun t -> t < time)
						 inst.(0)) in
					      inst.(1).(sol_index))
				  ds_list)) data datasets) in

    let dset = Boxplot_dataset.boxplot_datasets data in
    let plot = (Num_by_nom.plot ~title:(ykey ^ " at time " ^
					  (string_of_float time))
		  ~ylabel:ykey dset)  in
      plot#display


let load_compare ld title ?(ykey = "sol cost") xkey data =
  let ld = Jtd7_helpers.load_wrap ld in
  let data = List.map ld data in
    xy_diff_plot ~title ~xlabel:xkey ~xkey data ykey


let load_agnostic ld title ?(ykey = "sol cost") xkey alg_list =
  let dat = List.map (Jtd7_helpers.load_wrap ld) alg_list in
    Verb.pe Verb.debug "Data loaded\n%!";
  let ndat = Normalize_dataset.norm_min "num" ykey dat in
    Verb.pe Verb.debug "Data normalized\n%!";
    xy_plot ~title ~xlabel:xkey ~xkey ndat ykey


let load_filtered ld title ?(ykey = "sol cost") xkey alg_list =
  let dat = List.map (Jtd7_helpers.load_wrap ld) alg_list in
    List.iter (fun d -> (Verb.pe Verb.always "%i\t" (Dataset.size d))) dat;
    Verb.pe Verb.always "\n%!";
  let dat = Dataset_macros.filter_all_unsolved dat in
    List.iter (fun d -> (Verb.pe Verb.always "%i\t" (Dataset.size d))) dat;
    Verb.pe Verb.always "\n%!";
    (*final_sol_histo !def_time*)
    xy_plot ~title ~xlabel:xkey ~xkey dat ykey


let load_diff ld title ?(ykey = "sol cost") xkey alg_list =
  let dat = List.map (Jtd7_helpers.load_wrap ld) alg_list in
  let dat = Dataset_macros.filter_all_unsolved dat in
  let get_max key = (List.fold_left (fun accum ds ->
				       max accum (Dataset.get_min key ds))
		       neg_infinity dat) in
  let max_fuel = get_max "fuel_burned" in
  let tank_offset = 10. ** (ceil (log10 max_fuel)) in
    Verb.pe Verb.always "max fuel %f offset %f\n%!" max_fuel tank_offset;
    let dat = List.map (Dataset.group_by [|"num"|]) dat in
    let y_vals = List.map
      (List.map
	 (fun ds ->
	    let tanks = (Dataset.get_column_vector float_of_string
			   [|"tankers_used"|] ds)
	    and burned = (Dataset.get_column_vector float_of_string
			    [|"fuel_burned"|] ds) in
	      Wrarray.map2 (fun ta ba ->
			      Wrarray.map2 (fun a b->
					      a *. tank_offset +. b) ta ba)
		tanks burned)) dat in
    let x_vals = List.map (List.map
			     (fun ds -> Dataset.get_column_vector
				float_of_string [|"raw cpu time"|] ds)) dat in
    let pairs = List.map2
      (List.map2
	 (fun x_ar_ar y_ar_ar ->
	    Wrarray.map2 (fun xar yar ->
			    Wrarray.filter
			      (fun p ->
				 match classify_float p.Geometry.x,
				   classify_float p.Geometry.y with
				     | FP_nan, _ | _, FP_nan ->
					 false
				     | FP_infinite, _ | _, FP_infinite ->
					 false
				     | _, _ -> true)
			      (Wrarray.map2 (fun x y ->
					       Geometry.point ~x ~y) xar yar))
	      x_ar_ar y_ar_ar)) x_vals y_vals in
      (* extend points to arbirtrary cutoff *)
    let pairs = List.map (List.map
			    (fun points_ar ->
			       Array.map
				 (fun ar ->
				    let last = ar.((Array.length ar) - 1) in
				      Wrarray.extend
					ar 1 (Geometry.point ~x:300.
						~y:(last.Geometry.y)))
				 points_ar)) pairs in
      Verb.pe Verb.always "do the diff\n%!";
      let pairs2 = (match pairs with
		      | [sing] -> pairs
		      | dfs::tl ->
			  List.map
			    (fun dset ->
			       List.map2
				 (Wrarray.map2
				    (fun dfs_ins ins ->
				       for i = 0 to (Array.length ins) - 1 do
					 let x = ins.(i).Geometry.x
					 and y = ins.(i).Geometry.y in
					   try
					     let dfs_e = dfs_ins.
					       ((Wrarray.rfind
						   (fun p -> p.Geometry.x <= x)
						   dfs_ins))
					     in
					       ins.(i) <-
						 (Geometry.point ~x
						    ~y:(y -. dfs_e.Geometry.y))
					   with Not_found ->
					     ins.(i) <- Geometry.point
					       ~x ~y:0.
				       done;
				       ins)) dfs dset
			    ) tl
		      | _ -> pairs) in
      let dat = List.map2 (fun (_, alg_name, _) points ->
			     points, alg_name;) (List.tl alg_list) pairs2 in

      let next_color = Dataset_to_spt.make_color_factory true in
      let next_dash = (Factories.make_dash_factory [|[||]|] ())  in
      let next_style = (Dataset_to_spt.make_or_use_line_err_factory
			  next_dash None) in

      let dat = List.map (fun (bdat , nm) ->
			    Array.of_list
			      (List.map
				 (fun a -> a.(0)) bdat), nm) dat in
      let dsets =
	(List.map (fun (data,name) ->
		     Num_by_num.line_errbar_dataset
		       (next_style ()) ~name
		       ~line_width:(Length.Pt 2.)
		       ~color:(next_color()) data) dat) in
      let plot = (Num_by_num.plot
		    (*~legend_loc:Legend.Lower_right*)
		    ~legend_loc:!loc
		    ~title ~xlabel:"time (s)"
		    ~ylabel:"cost over baseline" dsets)
      in
	plot#display;
	dat




(************************************************************************)

let tiles ?(xkey = "raw cpu time") alg_list =
  load_agnostic Load_dataset.korf_100_tiles "Korf's 100 15 Puzzles"
    xkey alg_list

let tiles_gen ?(xkey = "raw cpu time") alg_list =
  (load_filtered ~ykey:"nodes generated")
    Load_dataset.korf_100_tiles "Korf's 100 15 Puzzles"
    xkey alg_list


let inv_tiles ?(xkey = "raw cpu time") alg_list =
  load_agnostic (Load_dataset.korf_100_tiles ~cost:"inverse")
    "15 Puzzles - Inverse Cost"
    xkey alg_list



let macro_tiles ?(xkey = "raw cpu time") alg_list =
  load_agnostic Load_dataset.macro_korf_100_tiles "Macro 15 Puzzles"
    xkey alg_list

let inv_tiles ?(xkey = "raw cpu time") alg_list =
  load_agnostic (Load_dataset.korf_100_tiles ~cost:"inverse")
    "Korf's 100 15 Puzzles - Inverse Cost"
    xkey alg_list

let u4grid ?(xkey = "raw cpu time") alg_list =
  load_agnostic Load_dataset.standard_unit_4_grids
    "Unit Four-way Grids 35% Obstacles" xkey alg_list


let u8grid ?(xkey = "raw cpu time") alg_list =
  load_agnostic Load_dataset.standard_unit_8_grids
    "Unit Eight-way Grids 45% Obstacles" xkey alg_list


let l4grid ?(xkey = "raw cpu time") alg_list =
  load_agnostic Load_dataset.standard_life_4_grids
    "Life Four-way Grids 35% Obstacles" xkey alg_list


let l4grid_gen ?(xkey = "raw cpu time") alg_list =
  (load_filtered ~ykey:"nodes generated")  Load_dataset.standard_life_4_grids
    "Life Four-way Grids 35% Obstacles" xkey alg_list



let l8grid ?(xkey = "raw cpu time") alg_list =
  load_agnostic Load_dataset.standard_life_8_grids
    "Life Eight-way Grids 45% Obstacles" xkey alg_list


let dyn_robots ?(xkey = "raw cpu time") alg_list =
  load_agnostic Load_dataset.standard_robots "Dynamic Robot Navigation"
    xkey alg_list


let dock_robots ?(xkey = "raw cpu time") alg_list =
  load_agnostic Load_dataset.dock_robot "Dock Robot"
    xkey alg_list


let big_dock_robots ?(xkey = "raw cpu time") alg_list =
  load_agnostic Load_dataset.big_dock_robot "Dock Robot"
    xkey alg_list


let pancakes ?(xkey = "raw cpu time") alg_list =
  load_agnostic Load_dataset.standard_pancakes "Pancake Puzzle"
    xkey alg_list


let pkhard ?(xkey = "raw cpu time") alg_list =
  load_agnostic Load_dataset.standard_pkhard "100 City TSP - PK Hard"
    xkey alg_list


let usquare ?(xkey = "raw cpu time") alg_list =
  load_agnostic Load_dataset.standard_usquare "100 City TSP - Unit Square"
    xkey alg_list


let vacuum ?(xkey = "raw cpu time") alg_list =
  load_agnostic Load_dataset.heavy_vacuum "Vacuum Problem"
    xkey alg_list


let big_vacuum ?(xkey = "raw cpu time") alg_list =
  load_agnostic Load_dataset.big_heavy_vacuum "Vacuum Problem"
    xkey alg_list


(* Boeing Plots *)
let boeing_supplied ?(xkey = "raw cpu time") ykey size alg_list =
  let alg_list = (List.map (fun (fst,snd,thrd) -> fst,snd,
			      ("size", size)::thrd) alg_list) in
  let dat = List.map (Jtd7_helpers.load_wrap
			Load_dataset.tanker_supplied) alg_list in
    Verb.pe Verb.debug "Data loaded\n%!";

  let dset = (Dataset_to_spt.padded_lines ~use_color:!use_color
		~line_width:(Length.Pt 2.)
		~xkey ~ykey dat) in
  let plot = (Num_by_num.plot
		(*~legend_loc:Legend.Lower_right*)
		~legend_loc:!loc
		~title:("Boeing Supplied " ^ size)
		~xlabel:xkey ~ylabel:ykey dset) in
    plot#display


let boeing_random ?(xkey = "raw cpu time") ykey gap_min gap_max req_min req_max
    dur_min dur_max prob alg_list =
  load_filtered (Load_dataset.random_tanker gap_min gap_max req_min
		   req_max dur_min dur_max prob) ""
    ~ykey xkey alg_list


let boeing_new_random ?(xkey = "raw cpu time") ykey dur alg_list =
  load_filtered (Load_dataset.newrandom_tanker dur) ("New Random " ^ dur)
    ~ykey xkey alg_list


let boeing_agnostic ?(xkey = "raw cpu time") ykey dur alg_list =
  load_agnostic (Load_dataset.newrandom_tanker dur) ("New Random " ^ dur)
    ~ykey xkey alg_list

let boeing_compare ?(xkey = "raw cpu time") ykey dur alg_list =
  load_compare (Load_dataset.newrandom_tanker dur) ("New Random " ^ dur)
    ~ykey xkey alg_list


(* EOF *)
