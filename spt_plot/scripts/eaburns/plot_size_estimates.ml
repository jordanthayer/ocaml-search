(** Plots some size estimations.

    @author eaburns
    @since 2010-05-20
*)

open Printf


let cfpartial_astar_0_9 =
  "Continuing Partial A* with f", [ "alg", "continuingf_par_astar";
				    "initial p", "0.9";
				    "p decrease rate", "0.99"; ]
let cbestfpartial_astar_0_9 =
  "Continuing Partial A* with best f", [ "alg", "continuing_bestf_par_astar";
					 "initial p", "0.9";
					 "p decrease rate", "0.99"; ]
let cpartial_astar_0_9 =
  "Continuing Partial A*", [ "alg", "continuing_par_astar";
			     "initial p", "0.9";
			     "p decrease rate", "0.99"; ]

let rpartial_astar_0_9 =
  "Restarting Partial A*", [ "alg", "restarting_par_astar";
			     "initial p", "0.9";
			     "p decrease rate", "0.99"; ]

let knuth_sampling =
  "Knuth's Sampling", [ "alg", "knuth_sampling"; ]

let kilby_counting =
  "Kilby Counting", [ "alg", "kilby_counting"; ]

let knuth_counting =
  "Knuth Counting", [ "alg", "knuth_counting"; ]


let algs = [
(*
  kilby_counting;
  knuth_counting;
*)
  knuth_sampling;
  rpartial_astar_0_9;
  cpartial_astar_0_9;
]

let additional_keys astar_data inst_key = [
  (fun ds ->
     (* log10 of the final estimation *)
     Dataset.numeric_transform "log10 final estimation" log10
       (Dataset.copy_key ~key:"final estimation"
	  ~new_key:"log10 final estimation" ds));
  (fun ds ->
     (* log10 of the total nodes *)
     Dataset.numeric_transform "log10 total nodes expanded plus one"
       (fun x -> log10 (x +. 1.))
       (Dataset.copy_key ~key:"total nodes expanded"
	  ~new_key:"log10 total nodes expanded plus one" ds));
  (fun ds ->
     (* astar nodes *)
     Dataset.transform_with [| inst_key |] astar_data
       "astar nodes expanded" ~with_key:"total nodes expanded"
       (fun tru _ -> tru)
       (Dataset.copy_key ~key:"final sol cost"
	  ~new_key:"astar nodes expanded" ds));
  (fun ds ->
     (* log10 of the astar nodes *)
     Dataset.numeric_transform "log10 astar nodes expanded" log10
       (Dataset.copy_key ~key:"astar nodes expanded"
	  ~new_key:"log10 astar nodes expanded" ds));
  (fun ds ->
     (* absolute estimation difference *)
     Dataset.transform_with [| inst_key |] astar_data
       "estimation difference" ~with_key:"total nodes expanded"
       (fun tru est -> abs_float (est -. tru))
       (Dataset.copy_key ~key:"final estimation"
	  ~new_key:"estimation difference" ds));
  (fun ds ->
     (* log10 of the absolute estimation difference *)
     Dataset.numeric_transform "log10 estimation difference" log10
       (Dataset.copy_key ~key:"estimation difference"
	  ~new_key:"log10 estimation difference" ds));
  (fun ds ->
     (* estimation to truth ratio estimated nodes / A* nodes *)
     Dataset.transform_with [| inst_key |] astar_data
       "estimation to truth ratio" ~with_key:"total nodes expanded"
       (fun tru est -> est /. tru)
       (Dataset.copy_key ~key:"final estimation"
	  ~new_key:"estimation to truth ratio" ds));
  (fun ds ->
     (* log10 estimation to truth ratio estimated nodes / A* nodes *)
     Dataset.numeric_transform "log10 estimation to truth ratio" log10
       (Dataset.copy_key ~key:"estimation to truth ratio"
	  ~new_key:"log10 estimation to truth ratio" ds))
]


let load_unit4 alg_attrs name =
  let astar_data =
    Load_dataset.unit_4_grids [ "alg", "astar"; ] "A*" in
  let ds = Load_dataset.unit_4_grids alg_attrs name
  in List.fold_left (fun ds k_fun -> k_fun ds)
       ds (additional_keys astar_data "num")

let load_unit4_17 alg_attrs name =
  let astar_data =
    Load_dataset.unit_4_grids [ "alg", "astar"; ] "A*" in
  let ds = Load_dataset.unit_4_grids (("num", "17") :: alg_attrs) name
  in List.fold_left (fun ds k_fun -> k_fun ds)
       ds (additional_keys astar_data "num")


let load_korf alg_attrs name =
  let astar_data =
    Dataset.load_from_rdb
      ~db:"./ai/data/astar_lower_bound/korf100"
      ["alg", "astar"] ~name:"A*" in
  let ds = Load_dataset.korf_100_tiles alg_attrs name
  in List.fold_left (fun ds k_fun -> k_fun ds)
       ds (additional_keys astar_data "num")


let load_korf_easy_12 alg_attrs name =
  let astar_data =
    Dataset.load_from_rdb
      ~db:"./ai/data/hierarchical/korf100"
      ["alg", "astar"] ~name:"A*" in
  let ds =
    Load_dataset.tiles
      (alg_attrs @ ["num", "12"; "cost", "unit"; "moves", "standard";
		    "model", "korf_25_easy"; "rows", "4";
		    "cols", "4"])
      name
  in List.fold_left (fun ds k_fun -> k_fun ds)
       ds (additional_keys astar_data "num")


let plot_ratio_boxes title load =
  let dss = List.map (fun (n, attrs) -> load attrs n) algs in
  let groups =
    Dataset_to_spt.boxplot_groups ~key:"log10 estimation to truth ratio"
      ~cmp:(fun a b -> compare (float_of_string a) (float_of_string b))
      ~group_key:"fraction of astar time"
      ~group_name_fun:(fun v -> sprintf "%g%% of A*'s time"
			 ((float_of_string v) *. 100.))
      dss
  in
  let plot =
    Num_by_nom.plot ~title ~ylabel:"log10 estimation to truth ratio"
      ~horiz_lines:[0.] groups
  in
    plot#set_size ~w:(Length.In 10.) ~h:(Length.In 7.5);
(*
      plot#output (User_paths.plot_root ^ title ^ ".pdf")
*)
    plot#display


let plot_expanded_boxes title load =
  let dss = List.map (fun (n, attrs) -> load attrs n) algs in
  let groups =
    Dataset_to_spt.boxplot_groups ~key:"log10 total nodes expanded plus one"
      ~cmp:(fun a b -> compare (float_of_string a) (float_of_string b))
      ~group_key:"fraction of astar time"
      ~group_name_fun:(fun v -> sprintf "%g%% of A*'s time"
			 ((float_of_string v) *. 100.))
      dss
  in
  let plot =
    Num_by_nom.plot ~title ~ylabel:"log10 total nodes expanded plus one" groups
  in
    plot#set_size ~w:(Length.In 10.) ~h:(Length.In 7.5);
(*
      plot#output (User_paths.plot_root ^ title ^ ".pdf")
*)
    plot#display


let plot_ratio_lines title load =
  let dss = List.map (fun (n, attrs) -> load attrs n) algs in
  let lines =
    Dataset_to_spt.line_errs
      ~xkey:"fraction of astar time"
      ~ykey:"log10 estimation to truth ratio"
      ~group_keys:[|"num"|]
      dss
  in
  let plot =
    Num_by_num.plot ~title
      ~ylabel:"log10 estimation to truth ratio"
      ~xlabel:"fraction of astar time"
      ~y_min:0.
      (Num_by_num.function_dataset ~name:"y=0" [||] (fun _ -> 0.)
       :: lines)
  in
    plot#set_size ~w:(Length.In 10.) ~h:(Length.In 7.5);
    (*
      plot#output (User_paths.plot_root ^ title ^ ".pdf")
    *)
    plot#display



let plot_subtree_size_scatter title load =
  let ds = load ["alg", "count_subtrees"] "Count Subtrees" in
  let scatter =
    Dataset_to_spt.scatter
      ~xkey:"subtree relative size" ~ykey:"subtree depth" ds
  in
  let plot =
    Num_by_num.plot ~title
      ~ylabel:"subtree size normalized to smallest sibling"
      ~xlabel:"subtree depth"
      [scatter]
  in
    plot#set_size ~w:(Length.In 10.) ~h:(Length.In 7.5);
    plot#output (User_paths.plot_root ^ title ^ ".pdf")


let plot_subtree_size_histogram title load =
  let ds = load ["alg", "count_subtrees"] "Count Subtrees" in
  let hist =
    Dataset_to_spt.histogram ~key:"subtree relative size" ~dash:[||] ds
  in
  let plot =
    Num_by_num.plot ~title
      ~ylabel:"Count"
      ~xlabel:"Subtree size normalized to smallest sibling"
       [hist]
  in
    plot#set_size ~w:(Length.In 10.) ~h:(Length.In 7.5);
(*
    plot#output (User_paths.plot_root ^ title ^ ".pdf")
*)
    plot#display


let plot_duplicate_frac_boxes title load =
  let algs = [ kilby_counting; knuth_counting ] in
  let dss = List.map (fun (n, attrs) -> load attrs n) algs in
  let boxes = Dataset_to_spt.boxplots ~key:"duplicate fraction" dss in
  let plot =
    Num_by_nom.plot ~title ~ylabel:"state to node ratio"
      ~horiz_lines:[1.] boxes
  in
    plot#set_size ~w:(Length.In 10.) ~h:(Length.In 7.5);
(*
      plot#output (User_paths.plot_root ^ title ^ ".pdf")
*)
    plot#display
