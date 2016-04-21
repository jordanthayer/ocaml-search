(**

    @author jtd7
    @since 2010-06-27

  Some box plot comparisons of algorithms, meant to demo box plots
   and some simple concetps (like h vs d on life)
*)

let h_d_life ?(ykey = "total nodes generated")
    ?(alg_list = ["greedier", "cost", ["heuristic", "manhattan";];
		  "speedier", "distance", ["heuristic", "nearest";]]) () =
  let data = List.map (Jtd7_helpers.load_wrap ~o_attrs:["prob", "0.35";
						        "width", "2000";
						        "height", "1200";]
			 Load_dataset.life_4_grids) alg_list in
  let dset = Dataset_to_spt.boxplots ~key:ykey data in
  let plot = Num_by_nom.plot ~title:"Greedy Search on Cost vs Distance"
    ~ylabel:ykey dset in
    plot#display


let h_hhat_vacuum ?(ykey = "total nodes generated") ?(log10y = true)
    ?(alg_list = ["greedy", "admissible h", ["heuristic", "spanning tree";];
		  "greedy", "inadmissible h", ["heuristic", "greedy";]]) () =
  let data = List.map (Jtd7_helpers.load_wrap
			 Load_dataset.standard_vacuum) alg_list in
  let data = (if log10y
	      then List.map (Dataset.numeric_transform ykey
			       (fun n -> log10 (n+.1.))) data
	      else data) in
  let dset = Dataset_to_spt.boxplots ~key:ykey data in
  let plot = Num_by_nom.plot ~title:"Vacuum World: Greedy Search Guidance"
    ~ylabel:(if log10y then "log10 " ^ ykey else ykey) dset in
    plot#display


let h_d_tsp ?(ykey = "total nodes generated")
    ?(alg_list = ["greedy", "h", [];
		  "speedy", "d", []]) () =
  let data = List.map (Jtd7_helpers.load_wrap
			 Load_dataset.standard_pkhard) alg_list in
  let dset = Dataset_to_spt.boxplots ~key:ykey data in
  let plot = Num_by_nom.plot ~title:"Grids: h vs d" ~ylabel:ykey dset in
    plot#display


let h_d_robots ?(ykey = "total nodes generated")
    ?(alg_list = ["greedy", "h", [];
		  "speedy", "d", []]) () =
  let data = List.map (Jtd7_helpers.load_wrap
			 Load_dataset.standard_robots) alg_list in
  let dset = Dataset_to_spt.boxplots ~key:ykey data in
  let plot = Num_by_nom.plot ~title:"Grids: h vs d" ~ylabel:ykey dset in
    plot#display


let converge_eightpuzzles ?(ykey = "total raw cpu time")
    ?(alg_list = [ "astar", "A*", [];
		   "arastar", "ARA* w=2", ["wt", "2."];
		   "arastar", "ARA* w=3", ["wt", "3."];
		   "arastar", "ARA* w=5", ["wt", "5."];]) () =
  let load bucket = (Jtd7_helpers.load_wrap
		       (Load_dataset.eight_puzzles bucket)) in
    for i = 0 to 499
    do
      (Verb.pe Verb.toplvl "Doing bucket %i\n%!" i;
       let data = List.map (load i) alg_list in
       let dset = Dataset_to_spt.boxplots ~key:ykey data in
       let plot = Num_by_nom.plot ~title:"Eight Puzzle Convergence"
	 ~ylabel:ykey dset in
	 plot#output (Wrutils.str
			"./epb/bucket_%i.ps" i))
    done


let tiles ?(ykey = "total nodes generated") alg_list =
  let data = List.map (Jtd7_helpers.load_wrap
			 Load_dataset.korf_100_tiles) alg_list in
  let dset = Dataset_to_spt.boxplots ~key:ykey data in
  let plot = Num_by_nom.plot ~title:"Tiles: Speedy vs Speediest"
    ~ylabel:ykey dset in
    plot#display


let boot_tiles ?(ykey = "total nodes generated") alg_list =
  let data = List.map (Jtd7_helpers.load_wrap
			 (Load_dataset.custom_tiles "boot_aij" 5 5))
			 alg_list in
  let data = List.map (Dataset.numeric_transform ykey (log10)) data in
  let dset = Dataset_to_spt.boxplots ~key:ykey data in
  let plot = Num_by_nom.plot ~title:"Tiles: Speedy vs Speediest"
    ~ylabel:("log " ^ ykey) dset in
    plot#display

let boot_diff ?(ykey = "total nodes generated")
    ?(loader = Load_dataset.custom_tiles "boot_aij" 5 5) title ylabel =
  let speedier = loader ["alg", "speedier";] "speedy_dd"
  and speediest = loader ["alg", "speediest_dd";]  "speediest_dd" in
(*  and speediest_path = loader ["alg", "speediest_path_adapt_dd";]  "speediest_dd" in*)
  let sp_vls = Dataset.get_values (float_of_string) ykey speedier in
  let spist_vls = Dataset.get_values (float_of_string) ykey speediest in
(*  let spip_vls = Dataset.get_values (float_of_string) ykey speediest_path in*)
  let vals = Array.init (Array.length spist_vls)
    (fun i -> spist_vls.(i) -. sp_vls.(i)) in
(*  let vals2 = Array.init (Array.length spist_vls)
    (fun i -> spip_vls.(i) -. sp_vls.(i)) in*)
  let dset = Boxplot_dataset.boxplot_dataset "Speediest" vals in
(*  let dset2 = Boxplot_dataset.boxplot_dataset "Speediest d^" vals2 in*)
  let plot = Num_by_nom.plot ~title ~ylabel [dset; (*dset2*)] in
    plot#display


let boot_tiles_div ?(ykey = "total nodes generated") () =
  let speedier = Load_dataset.custom_tiles "boot_aij" 5 5 ["alg", "speedier";]
    "speedy_dd"
  and speediest = Load_dataset.custom_tiles "boot_aij" 5 5
    ["alg", "speediest_dd";]  "speediest_dd" in
  let sp_vls = Dataset.get_values (float_of_string) ykey speedier in
  let spist_vls = Dataset.get_values (float_of_string) ykey speediest in
  let vals = Array.init (Array.length spist_vls)
    (fun i -> log (spist_vls.(i) /. sp_vls.(i))) in
  let dset = Boxplot_dataset.boxplot_dataset "diff" vals in
  let plot = Num_by_nom.plot ~title:"Tiles: Speediest - Speedy"
    ~ylabel:("log (spdiest/spdy) " ^ ykey) [dset] in
    plot#display

let inv_tiles_diff () =
  boot_diff
    ~loader:(Load_dataset.korf_100_tiles ~cost:"inverse")
    "Inverse Tiles" "Speediest - Speedier: Nodes"

let tiles_diff () =
  boot_diff
    ~loader:(Load_dataset.korf_100_tiles)
    "Korf's 100 Tiles" "Speediest - Speedier: Nodes"

let random_tiles_diff () =
  boot_diff
    ~loader:(Load_dataset.rwalk_7_7)
    "Random 7x7 Tiles" "Speediest - Speedier: Nodes"


let macro_tiles_diff () =
  boot_diff
    ~loader:(Load_dataset.macro_korf_100_tiles)
    "Macro Tiles" "Speediest - Speedier: Nodes"

let life_diff () =
  boot_diff
    ~loader:(Load_dataset.life_4_grids)
    "Life Grids" "Speediest - Speedier: Nodes"

let vacuum_diff () =
  boot_diff
    ~loader:(Load_dataset.heavy_vacuum)
    "Heavy Vacuum" "Speediest - Speedier: Nodes"

(* EOF *)
