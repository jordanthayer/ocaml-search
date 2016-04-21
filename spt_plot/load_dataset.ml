(**

    @author jtd7
    @since 2010-05-28

   Loading tools for datasets
*)

(****** Dynamic Robot Loaders *****)

let robots attrs name =
  Dataset.load_from_rdb_with_domain ~skip_fails:true ~domain:"dyn_robot"
    (attrs @ ["type", "run"]) ~name


let smaller_robots attrs name =
  robots (attrs @ ["obstacles", "liney"; "width", "200"; "height", "200";
		   "num_lines", "25"; ]) name


let standard_robots attrs name =
  robots (attrs @ ["obstacles", "liney"; "width", "500"; "height", "500";
		   "num_lines", "75"; "line_length", "0.1"; ]) name


(****** Grid Loaders ******)

let grids ?(skip_fails=true) attrs name =
  Dataset.load_from_rdb_with_domain ~skip_fails ~domain:"grid" attrs ~name


let unit_4_grids attrs name =
  grids (attrs @ ["obstacles","uniform";
		 "costs", "Unit"; "moves", "Four-way"]) name


let life_4_grids attrs name =
  grids (attrs @ ["obstacles","uniform";
		 "costs", "Life"; "moves", "Four-way"]) name


let unit_8_grids attrs name =
  grids (attrs @ ["obstacles","uniform";
		 "costs", "Unit"; "moves", "Eight-way"]) name


let life_8_grids attrs name =
  grids (attrs @ ["obstacles","uniform";
		 "costs", "Life"; "moves", "Eight-way"]) name


let standard_unit_4_grids attrs name =
  unit_4_grids (attrs @ ["height", "1200";
			 "width", "2000";
			 "prob", "0.35"]) name


let standard_unit_8_grids attrs name =
  unit_8_grids (attrs @ ["height", "1200"; "width", "2000";
			 "prob", "0.35"]) name


let standard_life_4_grids attrs name =
  life_4_grids (attrs @ ["height", "1200"; "width", "2000";
			 "prob", "0.35";
			 (*"heuristic", "manhattan";*)]) name

let standard_life_4_grids_noh attrs name =
  life_4_grids (attrs @ ["height", "1200"; "width", "2000";
			 "prob", "0.35";]) name


let corrupt_life_4_grids attrs name =
  life_4_grids (attrs @ ["height", "1200"; "width", "2000";
			 "prob", "0.35";
			 "heuristic", "online_corrupted";]) name


let small_life_4_grids attrs name =
  life_4_grids (attrs @ ["height", "200"; "width", "200";
			 "prob", "0.35"]) name

let standard_life_8_grids attrs name =
  life_8_grids (attrs @ ["height", "1200"; "width", "2000";
			 "prob", "0.35"]) name


(****** chess *******)


let chess ?(skip_fails = false)
    rows
    cols
    n_pieces
    attrs
    name =
  Dataset.load_from_rdb_with_domain ~skip_fails:skip_fails
    ~domain:"solitaire_chess"
    (
      attrs @
	[
	  "rows",string_of_int rows;
	  "cols",string_of_int cols;
	  "n_pieces",string_of_int n_pieces;
	]
    ) ~name


(****** topspin ******)
let topspin
    ?(skip_fails = false)
    ?(heuristic="h_b")
    ~disks
    ~flipper
    attrs
    name =
  Dataset.load_from_rdb_with_domain ~skip_fails:skip_fails ~domain:"topspin"
    (attrs @
       ["flipper", string_of_int flipper;
	"disks", string_of_int disks;
(*	"heuristic",heuristic;*)
       ]) ~name


(****** hanoi *******)

let hanoi ?(skip_fails = false) ?(pdb_type="Felner_packed")
    up
    down
    pegs
    disks
    attrs
    name =
  Dataset.load_from_rdb_with_domain ~skip_fails:skip_fails ~domain:"hanoi"
    (attrs @
       ["npegs", string_of_int pegs;
	"ndisks", string_of_int disks;
	"pdb_type","Felner";
	"pdb_up", string_of_int up;
	"pdb_down",string_of_int down;
       ]) ~name


let hanoi_12 attrs name =
  Dataset.load_from_rdb_with_domain ~domain:"hanoi"
    (attrs @
       [
	 "npegs","4";
	 "ndisks","12";
	 "pdb_up","8";
	 "pdb_down","8"
       ]) ~name


let hanoi_22 attrs name =
  Dataset.load_from_rdb_with_domain ~domain:"hanoi"
    (attrs @
       [
	 "npegs","4";
	 "ndisks","22";
	 "pdb_up","13";
	 "pdb_down","0"
       ]) ~name


(****** Job Shop Loader *****)

let jobshop attrs name =
  Dataset.load_from_rdb_with_domain ~domain:"jobshop" attrs ~name

(****** Logistics Loader *****)

let log attrs name =
  Dataset.load_from_rdb_with_domain ~domain:"logistics" attrs ~name


(****** MultipleSequence Loader ******)

let msa attrs name =
  Dataset.load_from_rdb_with_domain ~domain:"msa" attrs ~name


(****** Pancake Loader *****)

let pancakes attrs name =
  Dataset.load_from_rdb_with_domain ~domain:"pancake" attrs ~name


let standard_pancakes attrs name =
  pancakes (attrs @ ["cost", "sum"; "pdb", "7"; "ncakes", "10";]) name

(****** Rucksack Loaders *****)

let rucksack attrs name =
  Dataset.load_from_rdb_with_domain ~domain:"rucksack" attrs ~name


(****** Setcover Loader *****)



(****** Sokoban Loaders *****)

let sokoban attrs name =
  Dataset.load_from_rdb_with_domain ~domain:"sokoban" attrs ~name


(****** Synth Loaders ******)

let synth attrs name =
  Dataset.load_from_rdb_with_domain ~domain:"synth" attrs ~name


let uniform_tree ~costs ~max_sol_br ~min_depth ~max_depth attrs name =
  let costs_str =
    Wrarray.fold_lefti (fun s i c ->
			  if i = 0
			  then Printf.sprintf "%s%g" s c
			  else Printf.sprintf "%s,%g" s c)
      "" costs
  in
  let attrs = [ "model", "uniform-tree";
		"costs", costs_str;
		"max-sol-branching", string_of_int max_sol_br;
		"min-depth", string_of_int min_depth;
		"max-depth", string_of_int max_depth; ] @ attrs
  in synth attrs name


(****** Tiles Loaders ******)

let tiles ?(skip_fails=true) attrs name =
  Dataset.load_from_rdb_with_domain
    ~skip_fails:skip_fails
    ~domain:"sliding_tiles" attrs ~name


let eight_puzzles bucket attrs name =
  Dataset.load_from_rdb_with_domain ~domain:"tiles"
    (attrs @ ["bucket", string_of_int bucket]) ~name


let korf_100_tiles ?(cost = "unit") attrs name =
  tiles (attrs @ ["cost", cost;
		  (*"moves", "standard";*)
		  "model", "korf";
		  "rows", "4";
		  "cols", "4"]) name

let wted_korf_100_tiles cost attrs name =
  tiles (attrs @ ["cost", cost;
		  "model", "korf"; "rows", "4"]) name


let snlemons_easy_tiles attrs name =
  tiles (attrs @ ["cost", "unit";
		  "model", "snlemons_easy"; "rows", "4"]) name


let macro_korf_100_tiles attrs name =
  tiles (attrs @ ["cost", "unit";
		  "moves", "macro";
		  "model", "korf"; "rows", "4"]) name


let seven_tiles ?(skip_fails=false) ?(heuristic="manhattan") attrs name =
  tiles ~skip_fails:skip_fails (attrs @ ["cost", "unit";
					 "heuristic",heuristic;
		  "model", "random"; "rows", "7";]) name


let rwalk_7_7 ?(skip_fails=false) ?(heuristic="manhattan")
    ?(cost="unit")
    attrs name =
  tiles ~skip_fails:skip_fails (attrs @
[
				    "model", "random_walk";
				    "cost",cost;
				    "heuristic",heuristic;
				    "rows", "7";
				    "cols", "7";
				    "length", "1_000_000";
				  ]
			       ) name


let custom_tiles ?(skip_fails=false) ?(heuristic="manhattan")
    ?(cost="unit")
    model rows cols
    attrs name =
  tiles ~skip_fails:skip_fails (attrs @
[
				    "model", model;
				    "cost",cost;
				    "heuristic",heuristic;
				    "rows", string_of_int rows;
				    "cols", string_of_int cols;
				  ]
			       ) name


(****** TPlan Loaders *****)

let tplan attrs name =
  Dataset.load_from_rdb_with_domain ~domain:"tplan" attrs ~name


let blocks attrs name =
  tplan (attrs @ ["domainname", "blocksworld"]) name


let tpl_logistics attrs name =
  tplan (attrs @ ["domainname", "logistics"]) name


let rovers attrs name =
  tplan (attrs @ ["domainname", "rovers"]) name


let satellite attrs name =
  tplan (attrs @ ["domainname", "satellite"]) name


let zenotravel attrs name =
  tplan (attrs @ ["domainname", "zenotravel"]) name


(****** TSP Loaders ******)

let tsp attrs name =
  Dataset.load_from_rdb_with_domain ~domain:"tsp" attrs ~name



let dock_robot ?(skip_fails=true) attrs name =
  let base_attrs = ["heuristic", "sum_deep";
		    "model", "usquare";
		    "ncranes", "3";
		    "ncontainers", "10";
		    "npiles", "3";
		    "nrobots", "1";
		    "nlocations", "3";] in

  Dataset.load_from_rdb_with_domain ~skip_fails ~domain:"dock_robot"
		    (attrs @ base_attrs) ~name

let big_dock_robot attrs name =
  let base_attrs = ["heuristic", "sum_deep";
		    "model", "usquare";
		    "ncranes", "3";
		    "ncontainers", "15";
		    "npiles", "3";
		    "nrobots", "1";
		    "nlocations", "3";] in

  Dataset.load_from_rdb_with_domain ~skip_fails:true ~domain:"dock_robot"
		    (attrs @ base_attrs) ~name



let pkhard attrs name =
  tsp (attrs @ ["model", "pkhard"]) name


let usquare attrs name =
  tsp (attrs @ ["model", "usquare"]) name


let standard_pkhard attrs name =
  pkhard (attrs @ ["symmetric", "true"; "size", "100"]) name


let standard_usquare attrs name =
  usquare (attrs @ ["symmetric", "true"; "size", "100"]) name


(****** Vacuum Loaders *******)

let vacuum attrs name =
  Dataset.load_from_rdb_with_domain
    ~skip_fails:true ~domain:"vacuum" attrs ~name

let solved_vacuum attrs name =
  Dataset.filter (fun s -> s) (fun s -> s = "yes") "found solution" (vacuum attrs name)


let unsolved_vacuum attrs name =
  Dataset.filter (fun s -> s) (fun s -> s = "no") "found solution" (vacuum attrs name)


let standard_vacuum attrs name =
  vacuum (attrs @ ["obstacles", "uniform";
		   "width", "500";
		   "height", "500";
		   "obst_prob", "0.35";
		   "dirt", "20";]) name

let heavy_vacuum attrs name =
  vacuum (attrs @ ["obstacles", "uniform";
		   "width", "200";
		   "height", "200";
		   "obst_prob", "0.35";
		   "cost", "heavy";
		   "dirt", "10";]) name

let big_heavy_vacuum attrs name =
  vacuum (attrs @ ["obstacles", "uniform";
		   "width", "500";
		   "height", "500";
		   "obst_prob", "0.35";
		   "cost", "heavy";
		   "dirt", "20";]) name


let solved_heavy_vacuum attrs name =
  solved_vacuum (attrs @ ["obstacles", "uniform";
		   "width", "200";
		   "height", "200";
		   "obst_prob", "0.35";
		   "cost", "heavy";
		   "dirt", "10";]) name

let unsolved_heavy_vacuum attrs name =
  unsolved_vacuum (attrs @ ["obstacles", "uniform";
		   "width", "200";
		   "height", "200";
		   "obst_prob", "0.35";
		   "cost", "heavy";
		   "dirt", "10";]) name


let heavy_vacuum_nodirt attrs name =
  vacuum (attrs @ ["obstacles", "uniform";
		   "width", "200";
		   "height", "200";
		   "obst_prob", "0.35";
		   "cost", "heavy";]) name


let vacuum_nodirt attrs name =
  vacuum (attrs @ ["obstacles", "uniform";
		   "width", "200";
		   "height", "200";
		   "obst_prob", "0.35";]) name

let heavy_nodirt attrs name =
  vacuum (attrs @ ["obstacles", "uniform";
		   "width", "200";
		   "height", "200";
		   "cost", "heavy";
		   "obst_prob", "0.35";]) name


let heavy_nodirt attrs name =
  vacuum (attrs @ ["obstacles", "uniform";
		   "width", "200";
		   "height", "200";
		   "obst_prob", "0.35";
		   "cost", "heavy";]) name



let small_vacuum attrs name =
  vacuum (attrs @ ["obstacles", "uniform";
		   "width", "200";
		   "height", "200";
		   "obst_prob", "0.35";
		   "dirt", "10";
		   "cost", "unit"; ]) name


let tiny_vacuum attrs name =
  vacuum (attrs @ ["obstacles", "uniform";
		   "width", "200";
		   "height", "200";
		   "obst_prob", "0.35";
		   "dirt", "5";
		   "cost", "unit"; ]) name

let tiny_heavy_vacuum attrs name =
  vacuum (attrs @ ["obstacles", "uniform";
		   "width", "200";
		   "height", "200";
		   "obst_prob", "0.35";
		   "dirt", "5";
		   "cost", "heavy"; ]) name


let vacuum_maze attrs name =
  (* The compressed vacuum mazes. *)
  Dataset.load_from_rdb_with_domain ~domain:"vacuum_maze" attrs ~name


let standard_vacuum_maze w h p d attrs name =
  (* The compressed vacuum mazes. *)
  Dataset.load_from_rdb_with_domain ~domain:"vacuum_maze"
    (attrs @ [ "obstacles", "maze";
	       "width", string_of_int w;
	       "height", string_of_int h;
	       "cycle_prob", string_of_float p;
	       "dirt", string_of_int d; ])
    ~name


let chris_standard_vacuum attrs name =
  standard_vacuum (attrs @ ["heuristic", "chris";]) name


let spanning_standard_vacuum attrs name =
  standard_vacuum (attrs @ ["heuristic", "spanning tree";]) name


let improvedd_standard_vacuum attrs name =
  standard_vacuum (attrs @ ["heuristic", "improved d";]) name


let greedy_standard_vacuum attrs name =
  standard_vacuum (attrs @ ["heuristic", "greedy";]) name


(****** Vis_Nav Loaders ******)

let visnav attrs name =
  Dataset.load_from_rdb_with_domain ~domain:"vis_nav" attrs ~name


(****** Boeing Loaders *******)

let tanker_supplied attrs name =
  Dataset.load_from_rdb_with_domain ~domain:"boeing"
    (attrs @ ["instance_type", "supplied"]) ~name

let random_tanker gap_min gap_max req_min req_max dur_min dur_max prob
    (* you'll basically curry out this whole line so that the sig's match*)
    attrs name =
  Dataset.load_from_rdb_with_domain ~domain:"boeing"
    (attrs @ ["instance_type", "random";
	      "tracks", "fixed";
	      "bases", "fixed";
	      "gap_floor", string_of_int gap_min;
	      "gap_ceil", string_of_int gap_max;
	      "req_floor", string_of_int req_min;
	      "req_ceil", string_of_int req_max;
	      "dur_floor", string_of_int dur_min;
	      "dur_ceil", string_of_int dur_max;
	      "task_prob", string_of_float prob;]) ~name


let newrandom_tanker dur attrs name =
  Dataset.load_from_rdb_with_domain ~domain:"boeing"
    (attrs @ ["instance_type", "new_random";
	      "duration", dur;]) ~name


let load_biased_sample attrs_name_list =
  let loader = Dataset.load_from_rdb ~skip_fails:true
   ~db:"./Documents/FBiasPaper/anytime_data"
 in
    List.map (fun (attrs,name) -> loader attrs ~name) attrs_name_list


(* EOF *)
