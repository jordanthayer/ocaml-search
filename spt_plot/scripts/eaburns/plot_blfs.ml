(** A plotting script for BLFS.

    @author eaburns
    @since 2010-12-06
*)

open Printf
open Fn

(** Determines how the x values will be padded-out *)
type xfinal =
  | Nil
  | Key of string
      (* pad out the x-values to the given key for each dataset. *)
  | Val of float
      (* pad out the x-values to the given value. *)
  | Max_vl of string
      (* pad out the x-values theh value that is the max of the given
	 key over all of the datasets. *)


let log_fields = [ "total nodes expanded";
		   "nodes expanded";
		   "sol cost";
		   "final sol cost"; ]


let make_log_field ds key =
  let new_key = "log10 " ^ key in
    Dataset.numeric_transform new_key log10
      (Dataset.copy_key ~key:key ~new_key:new_key ds)

(** [algs] is the list of the various algorithms by their
    attributes. *)
let algs =
  [
    "DFS", ["alg", "dfs";];
    "DDS", ["alg", "dds";];

(*
    "ILDS (top)", ["alg", "ilds_top";];
*)

    "ILDS", ["alg", "ilds_bottom";];

(*
    "RBFS Ind.", ["alg", "indecision_rbfs"; "norm", "false";];
*)

    "OWDS", ["alg", "wds_bottom"; "max-bins", "100"];

(*
    "Ind. CR", ["alg", "indecision_cr_bottom"; "max-bins", "100"];
    "Ind. CR2", ["alg", "indecision_cr2_bottom"; "max-bins", "100"];
*)

    "Indecision",
    ["alg", "indecision_sum_bottom";
     "norm", "false";
     "max-bins", "2000";];

    "Quadratic",
    ["alg", "blfs_poly";
     "learner", "nlms_flow_2";
     "learning rate", "0.01";
     "degree", "2"; ];
(*

    "Quad. r=0.05",
    ["alg", "blfs_poly";
     "learner", "nlms_flow_2";
     "learning rate", "0.05";
     "degree", "2"; ];
*)

(*
    "ID Ind. (top)",
    ["alg", "indecision_sum_top";
     "norm", "false";node_spectrum_t
     "max-bins", "2000";];
*)

(*
    "wInd. RBFS", ["alg", "windecision_rbfs"; ];
*)

(*
    "wInd. P Sum (bottom)",
    ["alg", "windecision_sum_bottom"; "traversal", "probe";
     "max-bins", "2000"; ];

    "wInd. P Sum (top)",
    ["alg", "windecision_sum_top"; "traversal", "probe";
     "max-bins", "2000"; ];
*)

(*
    "wInd. D Sum (bottom)",
    ["alg", "windecision_sum_bottom"; "traversal", "discr";
     "max-bins", "2000"; ];

    "wInd. D Sum (top)",
    ["alg", "windecision_sum_top"; "traversal", "discr";
     "max-bins", "2000"; ];
*)

  ]

(************************************************************)
(** {1 Building line errs} *)

(** Gets a set of lines with errorbars.  This is used for creating
    anytime profiles using different methods for specifying the final x
    value. *)
let profiles ?(names=true) ?(group_keys=[|"num"|]) xkey xfinal ykey dss =
  let dss = if names then dss else List.map (Dataset.with_name "") dss in
  let use_color = true in
    match xfinal with
      | Nil ->
	  Dataset_to_spt.line_errs ~group_keys ~use_color ~xkey ~ykey dss
      | Key k ->
	  Dataset_to_spt.line_errs ~group_keys ~x_final_key:k
	    ~use_color ~xkey ~ykey dss
      | Max_vl k ->
	  let maxx = Dataset.max_of_datasets k dss in
	    Dataset_to_spt.padded_line_errs ~group_keys ~maxx ~use_color
	      ~xkey ~ykey dss
      | Val maxx ->
	  Dataset_to_spt.padded_line_errs ~group_keys ~maxx ~use_color
	    ~xkey ~ykey dss


(************************************************************)
(** {1 Normalization} *)

(** Finds the best solution for a given set of keys ([group_keys]). *)
let find_best_sols group_keys key ?(is_better=( > )) dss =
  let tbl = Hashtbl.create 149 in
  let grouped = List.map (Dataset.group_by group_keys) dss in
  let handle_region ds =
    let group = Array.map (fun k -> Dataset.get_group_value k ds) group_keys in
    let b = Dataset.get_max key ds in
      try
	let best = Hashtbl.find tbl group in
	  if is_better b best then
	    Hashtbl.replace tbl group b;
      with Not_found ->
	let str = Array.fold_left (sprintf "%s %s") "" group in
	  Printf.printf "Adding group: %s\n" str;
	  Hashtbl.add tbl group b
  in
    List.iter (List.iter handle_region) grouped;
    tbl


(** Create a 'norm sol cost' key that is the solution cost normalized
    to the best solution found across all algorithms for the same set
    of keys ([group_keys]). *)
let norm_best group_keys cost_key final_cost_key ?is_better dss =
  let best = find_best_sols group_keys final_cost_key ?is_better dss in
  let get_val df key =
    (* stupid hack to make numbers end with a '.' *)
    let v = Datafile.get_val df key in
      try string_of_float (float_of_string v) with _ -> v
  in
  let norm df cost =
    let group = Array.map (get_val df) group_keys in
    let b =
      try
	Hashtbl.find best group
      with _ ->
	let str = Array.fold_left (sprintf "%s %s") "" group in
	  Printf.printf "Group not found: %s" str;
	  exit 1;
    in
    let n = cost /. b in
      match classify_float n with
	| FP_normal -> n
	| _ -> assert false
  in
    List.map (Dataset.copy_key ~key:cost_key ~new_key:"norm sol cost") dss
      |> List.map (Dataset.transform "norm sol cost" norm)


(************************************************************)
(** {1 Taillard JSP instances} *)

(** Loads the dataset for a set of JSP instances with the given
    algorithm. *)
let load_jsp inst_attrs alg_attrs =
  let attrs = inst_attrs @ alg_attrs in
  let name = Rdb.attrs_str [] alg_attrs in
  let ds =
    Dataset.load_from_rdb_with_domain ~domain:"job_shop" attrs ~name:name
  in List.fold_left make_log_field ds log_fields


(** Loads the dataset for a set of JSP instances with the given
    algorithm. *)
let load_jsp_old inst_attrs alg_attrs =
  let attrs = inst_attrs @ alg_attrs in
  let name = Rdb.attrs_str [] alg_attrs in
  let ds =
    Dataset.load_from_rdb_with_domain ~domain:"job_shop.old_impl" attrs
      ~name:name
  in
    List.fold_left make_log_field ds log_fields


(** Loads the [n]x[m] Taillard instances. *)
let load_taillard load_fun n m alg_attrs =
  load_fun ["model", "taillard";
	    "num_jobs", string_of_int n;
	    "num_machines", string_of_int m;]
    alg_attrs


(** [normalize_to_dfs n m ds] adds a column which is the solution
    cost normalized to the first solution found by DFS. *)
let normalize_to_dfs n m ds =
  let dfs = load_taillard load_jsp n m ["alg", "dfs_first"] in
  let ds = Dataset.copy_key ~key:"sol cost" ~new_key:"norm sol cost" ds in
    Dataset.transform_with [|"time seed"; "machine seed"|]
      dfs "norm sol cost" ~with_key:"final sol cost" (fun a b -> b /. a) ds


(** [get_taillard_dss n m] gets a set of Datasets.t's (one for each
    alg in [algs]) for Taillard's JSP instances. *)
let get_taillard_dss n m =
  (*
    List.map (fun a -> normalize_to_taillard_lb n m (load_taillard n m a)) algs
    List.map (load_taillard n m) algs
  *)
  List.map (fun (name, attrs) ->
	      load_taillard load_jsp n m attrs
	    |> Dataset.with_name name
(*
	    |> normalize_to_dfs n m)
*)
	   )
    algs


(** [get_taillard_lines xkey xfinal ykey n m] gets a set of lines
    (one for each alg in [algs] for Taillard's instances). *)
let get_taillard_lines ?(names=true) xkey xfinal ykey n m =
  let dss =
    let dss = get_taillard_dss n m in
      if names then
	dss
      else
	List.map (Dataset.with_name "") dss
  in
  let group_keys = [| "time seed"; "machine seed" |] in
  let use_color = true in
    match xfinal with
      | Nil ->
	  Dataset_to_spt.line_errs ~group_keys ~use_color ~xkey ~ykey dss
      | Key k ->
	  Dataset_to_spt.line_errs ~group_keys ~x_final_key:k
	    ~use_color ~xkey ~ykey dss
      | Max_vl k ->
	  let maxx = Dataset.max_of_datasets k dss in
	    printf "max %s = %g\n%!" k maxx;
	    Dataset_to_spt.padded_line_errs ~group_keys ~maxx ~use_color
	      ~xkey ~ykey dss
      | Val maxx ->
	  Dataset_to_spt.padded_line_errs ~group_keys ~maxx ~use_color
	    ~xkey ~ykey dss


let plot_taillard ?(display=true) ?x_min ?x_max ?y_min ?y_max
    xkey ?(xlabel=xkey) xfinal ykey ?(ylabel=ykey) n m =
  let lines = get_taillard_lines ~names:display xkey xfinal ykey n m in
  let title = sprintf "Taillard %dx%d" n m in
  let plot =
    Num_by_num.plot ?x_min ?x_max ?y_min ~xlabel ~ylabel
(*
      ~legend_loc:Legend.Lower_left
*)
      ~title lines
  in
    plot#set_size ~w:(Length.In 2.2) ~h:(Length.In 2.2);
    if display then plot#display;
    plot


let plot_taillard_time ?display n m =
  plot_taillard ?display "raw cpu time" ~xlabel:"CPU time (seconds)"
    (* (Key "time limit") *) (Val 300.) "sol cost" ~ylabel:"makespan" n m


let plot_taillard_leaves ?display n m =
  plot_taillard ?display "leaves seen"
    (Val 10.) (* (Max_vl "total leaves seen") *)
    "sol cost" n m


let plot_taillard_nodes ?display n m =
  plot_taillard ?display "nodes expanded" (Max_vl "total nodes expanded")
    "norm sol cost" n m


let jsp_montage () =
  let display = true in
  let time_10_10 = plot_taillard_time ~display 10 10 in
(*
  let leaves_10_10 = plot_taillard_leaves ~display 10 10 in
*)
  let time_15_15 = plot_taillard_time ~display 15 15 in
(*
  let leaves_15_15 = plot_taillard_leaves ~display 15 15 in
*)
  let time_20_20 = plot_taillard_time ~display 20 20 in
(*
  let leaves_20_20 = plot_taillard_leaves ~display 20 20 in
*)
  let s =
    Plot_sheet.us_letter_montage ~nrows:3 ~ncols:1 ~landscape:false
      [ time_10_10; (* leaves_10_10; *)
	time_15_15; (* leaves_15_15; *)
	time_20_20; (* leaves_20_20; *)
      ]
  in
  let legend =
    let lines =
      get_taillard_lines "raw cpu time" (Key "time limit") "sol cost" 15 15
    in
      Num_by_num.standalone_legend ~text_before:true ~vertical:false lines
  in
      legend#set_size ~w:(Length.In 6.4) ~h:(Length.In 0.025);
    (*
    (* vertical legend size *)
    legend#set_size ~w:(Length.In 1.4) ~h:(Length.In 1.4);
    *)
    legend#display;
    legend#output "jsp_legend.ps";
    time_10_10#output "jsp_10_10.ps";
    time_15_15#output "jsp_15_15.ps";
    time_20_20#output "jsp_20_20.ps";
    begin match s with
      | [] -> ()
      | s :: _ ->
	  s#output "jsp-montage.pdf";
	  s#display;
    end


let plot_jsp_comparison n m =
  let x_final_key = "total raw cpu time" in
  let xkey = "raw cpu time" in
  let ykey = "sol cost" in
  let color = Factories.default_color_factory () in
  let alg l (name, attrs) =
    let o = load_taillard load_jsp_old n m attrs in
    let oname = Dataset.name o in
    let o = Dataset.with_name (oname ^ " old") o in
    let n = load_taillard load_jsp n m attrs in
    let lold =
      Dataset_to_spt.line_err ~group_keys:[| "time seed"; "machine seed" |]
	~x_final_key ~color:(color ()) ~xkey ~ykey o in
    let lnew =
      Dataset_to_spt.line_err ~group_keys:[| "time seed"; "machine seed" |]
	~x_final_key ~color:(color ()) ~xkey ~ykey n
    in lold :: lnew :: l
  in
  let ls = List.fold_left alg [] algs in
  let title = sprintf "Old vs New JSP impl. Taillard %d %d" n m in
  let plot = Num_by_num.plot ~xlabel:xkey ~ylabel:ykey ~title ls in
    plot#display


let plot_jsp_comparison2 n m =
  let ykey = "total nodes expanded" in
  let xkey = "total raw cpu time" in
  let color = Factories.default_color_factory () in
    (*
      let alg l (name, attrs) =
      let o = load_taillard load_jsp_old n m attrs in
      let oname = Dataset.name o in
      let o = Dataset.with_name (oname ^ " old") o in
      let n = load_taillard load_jsp n m attrs in
      let lold =
      Dataset_to_spt.scatter_err ~group_keys:[| "time seed"; "machine seed" |]
      ~color:(color ()) ~xkey ~ykey o in
      let lnew =
      Dataset_to_spt.scatter_err ~group_keys:[| "time seed"; "machine seed" |]
      ~color:(color ()) ~xkey ~ykey n
      in lold :: lnew :: l
      in
      let ls = List.fold_left alg [] algs in
    *)
  let algs =
    [ "dfs"; "ilds_top"; "ilds_bottom"; "indecision_sum_bottom";
      "indecision_sum_top"; "indecision_rbfs"; ] in
  let od =
    load_taillard load_jsp_old n m []
      |> Dataset.with_name "old"
      |> Dataset.filter identity (fun a -> List.mem a algs) "alg" in
  let nw =
    load_taillard load_jsp n m []
      |> Dataset.with_name "new"
      |> Dataset.filter identity (fun a -> List.mem a algs) "alg" in
  let lold =
    Dataset_to_spt.scatter_err ~xloc:Num_by_num.Label_before
      ~group_keys:[| "alg"; |] ~color:(color ())
      ~glyph:Drawing.Triangle_glyph ~xkey ~ykey od in
  let lnew =
    Dataset_to_spt.scatter_err ~group_keys:[| "alg"; |]
      ~color:(color ()) ~glyph:Drawing.Box_glyph ~xkey ~ykey nw in
  let ls = [ lold; lnew; ] in
  let title = sprintf "Old vs New JSP impl. Taillard %d %d" n m in
  let plot = Num_by_num.plot ~xlabel:xkey ~ylabel:ykey ~title ls in
    plot#output "cmp.pdf";
    plot#display;
    plot


let taillard_paired_test n m (aname, aattrs) (bname, battrs) =
  let dsa = load_taillard load_jsp n m aattrs |> Dataset.with_name aname in
  let dsb = load_taillard load_jsp n m battrs |> Dataset.with_name bname in
  let pair_keys = [| "time seed"; "machine seed" |] in
  let test_key = "final sol cost" in
  let diff_med = Dataset_utils.med_paired_diff pair_keys test_key dsa dsb in
  let p =
    Dataset_utils.wilcoxon_signed_rank_test pair_keys test_key dsa dsb
  in
    printf "\n";
    printf "positive is better for %s\n" bname;
    printf "negative is better for %s\n" aname;
    printf "median diff: %g\np: %g\n" diff_med p


let taillard_estimations ?(display=true) n m =
  let key = "estimation factor" in
  let title = sprintf "taillard %dx%d" n m in
  let dss =
    get_taillard_dss n m |> List.filter (Dataset.is_key key)
  in
  let scatters = Dataset_to_spt.boxplots ~key dss in
  let plot =
    Num_by_nom.plot ~horiz_lines:[1.] ~ylabel:"estimation factor"
      ~title scatters
  in
    if display then plot#display;
    plot


let taillard_estimation_montage () =
  let plot =
    Plot_sheet.us_letter_montage ~nrows:3 ~ncols:1
      [ taillard_estimations ~display:false 10 10;
	taillard_estimations ~display:false 15 15;
	taillard_estimations ~display:false 20 20;
      ]
  in
    if plot = [] then failwith "Too few pages";
    List.iter (fun p -> p#output "jsp-est-montage.pdf"; p#display) plot


(************************************************************)
(** {1 Random JSP instances} *)

(** [load_random n m alg_attrs] loads the [n]x[m] Taillard
    instances. *)
let load_random n m alg_attrs =
  load_jsp
    ["model", "random";
     "num_jobs", string_of_int n;
     "num_machines", string_of_int m;]
    alg_attrs


(** [normalize_to_dfs_random n m ds] adds a column which is the
    solution cost normalized to the first solution found by DFS. *)
let normalize_to_dfs_random n m ds =
  let dfs = load_random n m ["alg", "dfs_first"] in
  let ds = Dataset.copy_key ~key:"sol cost" ~new_key:"norm sol cost" ds in
    Dataset.transform_with [|"num";|]
      dfs "norm sol cost" ~with_key:"final sol cost" (fun a b -> b /. a) ds


(** [get_random_dss n m] gets a set of Datasets.t's (one for each
    alg in [algs]) for Taillard's JSP instances. *)
let get_random_dss n m =
  List.map (fun (name, attrs) ->
	      load_random n m attrs
	    |> Dataset.with_name name
	    |> normalize_to_dfs_random n m)
    algs


(** [get_random_lines xkey xfinal ykey n m] gets a set of lines (one
    for each alg in [algs] for random instances). *)
let get_random_lines xkey xfinal ykey n m =
  let dss = get_random_dss n m in
  let group_keys = [| "num" |] in
  let use_color = true in
    match xfinal with
      | Nil ->
	  Dataset_to_spt.line_errs ~group_keys ~use_color ~xkey ~ykey dss
      | Key k ->
	  Dataset_to_spt.line_errs ~group_keys ~x_final_key:k
	    ~use_color ~xkey ~ykey dss
      | Max_vl k ->
	  let maxx = Dataset.max_of_datasets k dss in
	    printf "max %s = %g\n%!" k maxx;
	    Dataset_to_spt.padded_line_errs ~group_keys ~maxx ~use_color
	      ~xkey ~ykey dss
      | Val maxx ->
	  Dataset_to_spt.padded_line_errs ~group_keys ~maxx ~use_color
	    ~xkey ~ykey dss


let plot_random ?x_min ?x_max ?y_min ?y_max xkey xfinal ykey n m =
  let lines = get_random_lines xkey xfinal ykey n m in
  let title = Wrutils.str "%s vs %s on Random %d %d" xkey ykey n m in
  let plot =
    Num_by_num.plot ?x_min ?x_max ?y_min ~xlabel:xkey ~ylabel:ykey ~title lines
  in
    plot#display


let plot_random_time n m =
  plot_random "raw cpu time" (Key "time limit") "sol cost" n m



let plot_random_enum () =
  let ds =
    (*
      load_random 10 10 ["num", "50"; "alg", "indecision_enum"]
    *)
    load_taillard load_jsp 20 20
      ["time seed", "5997802"; "alg", "indecision_enum"]
    |> Dataset.copy_key ~key:"best leaf" ~new_key:"log10 best leaf"
    |> Dataset.numeric_transform "log10 best leaf" log10
  in
  let module To_spt = Dataset_to_spt in
  let x = "discrepancies" and y = "best leaf" in
  let s =
    To_spt.scatter ~glyph:Drawing.Cross_glyph ~xkey:x ~ykey:y ds in
  let p =
    Num_by_num.plot ~xlabel:x ~ylabel:y ~legend_loc:Legend.Lower_right [ s; ]
  in
    p#display


(************************************************************)
(** {1 Darts} *)


let load_darts_by_ndarts ndarts (name, alg_attrs) =
  let attrs = [ "num darts", string_of_int ndarts ] @ alg_attrs in
    Dataset.load_from_rdb_with_domain ~domain:"darts" attrs ~name
    |> Dataset.copy_key ~key:"final sol cost" ~new_key:"log10 final sol cost"
    |> Dataset.numeric_transform "log10 final sol cost" log10


let plot_final_sol_by_regions ndarts =
  let dss = List.map (load_darts_by_ndarts ndarts) algs in
  let xkey = "num regions" in
  let ykey = "final sol cost" in
  let lines = Dataset_to_spt.lines ~xkey ~ykey ~use_color:true dss in
  let plot =
    Num_by_num.plot ~xlabel:xkey ~ylabel:ykey ~legend_loc:Legend.Upper_left
      lines
  in
    plot#display


let find_best_sols dss =
  let tbl = Hashtbl.create 149 in
  let by_regions = List.map (Dataset.group_by [| "num regions" |]) dss in
  let handle_region ds =
    let rstr = Dataset.get_group_value "num regions" ds in
    let r = truncate (float_of_string rstr) in
    let b = Dataset.get_max "final sol cost" ds in
    let best = try Hashtbl.find tbl r with Not_found -> neg_infinity in
      if b > best then Hashtbl.replace tbl r b;
  in
    List.iter (List.iter handle_region) by_regions;
    tbl


(** [norm_best dss] create a 'norm sol cost' key that is the solution
    cost normalized to the best solution found across all algorithms
    for the same number of regions. *)
let darts_norm_best dss =
  let best = find_best_sols dss in
  let norm df cost =
    let r = int_of_string (Datafile.get_val df "num regions") in
    let b = Hashtbl.find best r in
    let n = cost /. b in
      match classify_float n with
	| FP_normal -> n
	| _ -> assert false
  in
    List.map (Dataset.copy_key ~key:"sol cost" ~new_key:"norm sol cost") dss
    |> List.map (Dataset.transform "norm sol cost" norm)


let darts_plot_anytime ?(display=true) ?(y_min=0.9) ndarts =
  let xkey = "raw cpu time" in
  let xlabel = "CPU time (seconds)" in
(*
  let ykey = "sol cost" in
  let ylabel = "minimum unattainable value" in
*)
  let ykey = "norm sol cost" in
  let ylabel = "fraction of best solution" in
  let group_keys = [| "num regions" |] in
  let lines =
    List.map (load_darts_by_ndarts ndarts) algs
    |> darts_norm_best
    |> List.map (fun ds -> if display then ds else Dataset.with_name "" ds)
    |> Dataset_to_spt.padded_line_errs ~minx:(0.,0.) ~maxx:300.
	~xkey ~ykey ~group_keys ~use_color:true
  in
  let title = Printf.sprintf "%d stamps" ndarts in
  let plot =
    Num_by_num.plot ~x_min:0. ~y_min ~xlabel ~ylabel
      ~title ~legend_loc:Legend.Lower_right lines
  in
    plot#set_size ~w:(Length.In 2.2) ~h:(Length.In 2.2);
    if display then plot#display;
    plot


let legend () =
  let xkey = "raw cpu time" in
  let ykey = "norm sol cost" in
  let group_keys = [| "num regions" |] in
  let lines =
    List.map (load_darts_by_ndarts 4) algs
    |> darts_norm_best
    |> Dataset_to_spt.padded_line_errs ~minx:(0.,0.) ~maxx:300.
	~xkey ~ykey ~group_keys ~use_color:true
  in
    Num_by_num.standalone_legend ~vertical:false lines


let darts_montage () =
  let display = false in
  let three = darts_plot_anytime ~display 3 in
  let four = darts_plot_anytime ~display 4 in
  let five = darts_plot_anytime ~display 5 in
  let six = darts_plot_anytime ~display ~y_min:0.95 6 in
  let s =
    Plot_sheet.us_letter_montage ~landscape:false ~ncols:2 ~nrows:2
      [ three; four; five; six ]
  in
  let legend = legend () in
    legend#set_size ~w:(Length.In 6.5) ~h:(Length.In 0.025);
    legend#display;
    legend#output "darts_legend.ps";
    three#output "darts_3.ps";
    four#output "darts_4.ps";
    five#output "darts_5.ps";
    six#output "darts_6.ps";
    match s with
      | s :: [] ->
	  s#output "darts-montage.pdf";
	  s#display
      | [] -> failwith "Montage has no pages"
      | _ -> failwith "Montage has multiple pages"



let plot_darts_enum () =
  let ds =
    load_darts_by_ndarts 3 ("Ind. Enum", ["alg", "indecision_enum"])
    |> Dataset.copy_key ~key:"best leaf" ~new_key:"log10 best leaf"
    |> Dataset.numeric_transform "log10 best leaf" log10
  in
  let module To_spt = Dataset_to_spt in
  let x = "discrepancies" and y = "best leaf" in
  let s =
    To_spt.scatter ~glyph:Drawing.Cross_glyph ~xkey:x ~ykey:y ds in
  let p =
    Num_by_num.plot ~xlabel:x ~ylabel:y ~legend_loc:Legend.Lower_right [ s; ]
  in
    p#display


(************************************************************)
(** {1 TSP} *)


(** Loads the TSP data for the given algorithm. *)
let load_tsp model ncities (alg_name, alg_attrs) =
  let domain = "tsp" in
    (* TSP doesn't use the 'norm' attribute for indecision. *)
  let alg_attrs = Rdb.filter_attrs ["norm"] alg_attrs in
  let attrs = alg_attrs @ [ "model", model; "size", string_of_int ncities] in
    Dataset.load_from_rdb_with_domain ~domain attrs ~name:alg_name


(** Loads all of the TSP data. *)
let tsp_dss model ncities =
    List.map (load_tsp model ncities) algs



(** Gets a set of lines (one for each alg in [algs]). *)
let tsp_lines ?(names=true) xkey xfinal ykey model ncities =
  let dss =
    let dss = tsp_dss model ncities in
      if names then
	dss
      else
	List.map (Dataset.with_name "") dss
  in
  let group_keys = [| "num"; |] in
  let use_color = true in
    match xfinal with
      | Nil ->
	  Dataset_to_spt.line_errs ~group_keys ~use_color ~xkey ~ykey dss
      | Key k ->
	  Dataset_to_spt.line_errs ~group_keys ~x_final_key:k
	    ~use_color ~xkey ~ykey dss
      | Max_vl k ->
	  let maxx = Dataset.max_of_datasets k dss in
	    Dataset_to_spt.padded_line_errs ~group_keys ~maxx ~use_color
	      ~xkey ~ykey dss
      | Val maxx ->
	  Dataset_to_spt.padded_line_errs ~group_keys ~maxx ~use_color
	    ~xkey ~ykey dss


let plot_tsp ?x_max ?(display=true) xkey ?(xlabel=xkey) xfinal ykey
    ?(ylabel=ykey) model ncities =
  let ls = tsp_lines ~names:display xkey xfinal ykey model ncities in
  let title = sprintf "%s with %d cities" model ncities in
  let p =
    Num_by_num.plot ?x_max ~title ~xlabel ~ylabel ls
  in
    p#set_size ~w:(Length.In 2.2) ~h:(Length.In 2.2);
    if display then p#display;
    p


let tsp_montage (* xkey ?xlabel xfinal ykey ?ylabel *) () =
  let display = false in
  let xkey = "nodes expanded" and xlabel = "nodes expanded" in
  let ykey = "sol cost" and ylabel = "tour cost" in
(*
  let xfinal = Val 30. in
*)
(*
  let xfinal = Val 1e7 in
*)
  let xfinal = Key "total nodes expanded" in
  let pk_100 =
    plot_tsp ~display xkey ~xlabel xfinal ykey ~ylabel "pkhard" 100 in
  let us_100 =
    plot_tsp ~display xkey ~xlabel xfinal ykey ~ylabel "usquare" 100 in
(*
  let xfinal = Val 60. in
*)
  let us_50 =
    plot_tsp ~display xkey ~xlabel xfinal ykey ~ylabel "usquare" 50 in
(*
  let xfinal = Val 300. in
*)
(*
  let us_500 =
    plot_tsp ~display xkey ~xlabel xfinal ykey ~ylabel "usquare" 500 in
*)
  let legend =
    let lines = tsp_lines xkey xfinal ykey "usquare" 100 in
      Num_by_num.standalone_legend ~vertical:false lines
  in
  let s =
    Plot_sheet.us_letter_montage ~nrows:3 ~ncols:1
      [ us_50; us_100; (* us_500; *) pk_100; ]
  in
    legend#set_size ~w:(Length.In 5.1) ~h:(Length.In 0.025);
    legend#display;
    legend#output "tsp_legend.ps";
    us_50#output "us_50.ps";
    us_100#output "us_100.ps";
(*
    us_500#output "us_500.ps";
*)
    pk_100#output "pk_100.ps";
    match s with
      | s :: [] ->
	  s#output "tsp-montage.pdf";
	  s#display
      | [] -> failwith "No pages"
      | _ -> failwith "Too many pages"


let tsp_estimations ?(display=true) model ncities =
  let key = "estimation factor" in
  let title = sprintf "%s %d" model ncities in
  let dss =
    tsp_dss model ncities |> List.filter (Dataset.is_key key)
  in
  let scatters = Dataset_to_spt.boxplots ~key dss in
  let plot =
    Num_by_nom.plot ~horiz_lines:[1.] ~ylabel:"estimation factor"
      ~title scatters
  in
    if display then plot#display;
    plot


let tsp_estimation_montage () =
  let plot =
    Plot_sheet.us_letter_montage ~nrows:2 ~ncols:2
      [
	tsp_estimations ~display:false "usquare" 50;
	tsp_estimations ~display:false "usquare" 100;
	tsp_estimations ~display:false "usquare" 500;
	tsp_estimations ~display:false "pkhard" 100;
      ]
  in
    if plot = [] then failwith "Too few pages";
    List.iter (fun p -> p#output "tsp-est-montage.pdf"; p#display) plot


(************************************************************)
(** {1 Knapsack} *)

(** Loads the knapsack data for the given algorithm. *)
let load_knapsack ?(model="random_max_val") ?(value=200.) ?(weight=200.) size
    nitems (alg_name, alg_attrs) =
  let domain = "rucksack" in
  let attrs = alg_attrs @ [ "model", model;
			    "value", string_of_float value;
			    "weight", string_of_float weight;
			    "sack size", string_of_float size;
			    "item count", string_of_int nitems; ]
  in
    Dataset.load_from_rdb_with_domain ~domain attrs ~name:alg_name


(** Loads all of the knapsack data. *)
let knapsack_dss ?model ?value ?weight size nitems =
  List.map (load_knapsack ?model ?value ?weight size nitems) algs
      |> norm_best [| "model"; "value"; "weight"; "sack size"; "item count"; |]
	  "sol cost" "final sol cost" ~is_better:( < )



(** Gets a set of lines (one for each alg in [algs]). *)
let knapsack_lines ?names xkey xfinal ykey ?model ?value ?weight
    size nitems =
  let dss = knapsack_dss ?model ?value ?weight size nitems in
    profiles ?names xkey xfinal ykey dss


let plot_knapsack ?(display=true) xkey ?(xlabel=xkey) xfinal ykey
    ?(ylabel=ykey) ?model ?value ?weight size nitems =
  let ls =
    knapsack_lines ~names:display xkey xfinal ykey ?model ?value ?weight
      size nitems in
  let title = sprintf "size=%g nitems=%d" size nitems in
  let p = Num_by_num.plot ~title ~xlabel ~ylabel ls in
    p#set_size ~w:(Length.In 2.2) ~h:(Length.In 2.2);
    if display then p#display;
    p


let knapsack_montage () =
  let display = true in
  let xkey = "raw cpu time" and xlabel = "CPU time (seconds)" in
  let xfinal = Val 30. in
(*
  let xkey = "nodes expanded" and xlabel = "nodes" in
  let xfinal = Val 4000. in
*)
  let ykey = "norm sol cost" and ylabel = "normalized solution cost" in
  let fv_0_4_50 =
    plot_knapsack ~display xkey ~xlabel xfinal ykey ~ylabel 0.4 50 in
  let fv_0_5_50 =
    plot_knapsack ~display xkey ~xlabel xfinal ykey ~ylabel 0.5 50 in
  let fv_0_6_50 =
    plot_knapsack ~display xkey ~xlabel xfinal ykey ~ylabel 0.6 50 in
  let fv_0_4_100 =
    plot_knapsack ~display xkey ~xlabel xfinal ykey ~ylabel 0.4 100 in
  let fv_0_5_100 =
    plot_knapsack ~display xkey ~xlabel xfinal ykey ~ylabel 0.5 100 in
  let fv_0_6_100 =
    plot_knapsack ~display xkey ~xlabel xfinal ykey ~ylabel 0.6 100 in
  let fv_0_4_200 =
    plot_knapsack ~display xkey ~xlabel xfinal ykey ~ylabel 0.4 200 in
  let fv_0_5_200 =
    plot_knapsack ~display xkey ~xlabel xfinal ykey ~ylabel 0.5 200 in
  let fv_0_6_200 =
    plot_knapsack ~display xkey ~xlabel xfinal ykey ~ylabel 0.6 200 in
  let legend =
    let lines = knapsack_lines xkey xfinal ykey 0.5 100 in
      Num_by_num.standalone_legend ~vertical:false lines
  in
  let s =
    Plot_sheet.us_letter_montage ~nrows:3 ~ncols:3
      [
	fv_0_4_50; fv_0_5_50; fv_0_6_50;
	fv_0_4_100; fv_0_5_100; fv_0_6_100;
	fv_0_4_200; fv_0_5_200; fv_0_6_200; ]
  in
    legend#set_size ~w:(Length.In 5.5) ~h:(Length.In 0.025);
    legend#display;
    legend#output "knapsack_legend.ps";
    fv_0_4_50#output "fv_0_4_50.ps";
    fv_0_5_50#output "fv_0_5_50.ps";
    fv_0_6_50#output "fv_0_6_50.ps";
    fv_0_4_100#output "fv_0_4_100.ps";
    fv_0_5_100#output "fv_0_5_100.ps";
    fv_0_6_100#output "fv_0_6_100.ps";
    fv_0_4_200#output "fv_0_4_200.ps";
    fv_0_5_200#output "fv_0_5_200.ps";
    fv_0_6_200#output "fv_0_6_200.ps";
    match s with
      | s :: [] ->
	  s#output "knapsack-montage.pdf";
	  s#display
      | [] -> failwith "No pages"
      | _ -> failwith "Too many pages"


(************************************************************)
(** {1 Setcover} *)

(** Loads the data for the given algorithm. *)
let load_setcover ?(model="uniform") ~objects ~subsets ~min_vl ~max_vl ~max_p
    (alg_name, alg_attrs) =
  let domain = "setcover" in
  let attrs = alg_attrs @ [ "model", model;
			    "objects", string_of_int objects;
			    "subsets", string_of_int subsets;
			    "min value", string_of_float min_vl;
			    "max value", string_of_float max_vl;
			    "max proportion", string_of_float max_p; ]
  in
    Dataset.load_from_rdb_with_domain ~domain attrs ~name:alg_name


(** Loads all of the data. *)
let setcover_dss ?model ~objects ~subsets ~min_vl ~max_vl ~max_p =
  List.map
    (load_setcover ?model ~objects ~subsets ~min_vl ~max_vl ~max_p) algs
      |> norm_best [| "model"; "objects"; "subsets"; "max value";
		      "max proportion"; |]
	  "sol cost" "final sol cost" ~is_better:( < )



(** Gets a set of lines (one for each alg in [algs]). *)
let setcover_lines ?names xkey xfinal ykey ?model ~objects ~subsets
    ~min_vl ~max_vl ~max_p =
  let dss = setcover_dss ?model ~objects ~subsets ~min_vl ~max_vl ~max_p in
    profiles ?names xkey xfinal ykey dss


let plot_setcover ?(display=true) xkey ?(xlabel=xkey) xfinal ykey
    ?(ylabel=ykey) ?model ~objects ~subsets ~min_vl ~max_vl ~max_p =
  let ls =
    setcover_lines ~names:display xkey xfinal ykey ?model ~objects
      ~subsets ~min_vl ~max_vl ~max_p in
  let title =
    sprintf "obj=%d, ssets=%d, min=%g, max=%g, max_p=%g" objects subsets
      min_vl max_vl max_p in
  let p = Num_by_num.plot ~title ~xlabel ~ylabel ls in
    p#set_size ~w:(Length.In 2.2) ~h:(Length.In 2.2);
    if display then p#display;
    p


(*
  plot_setcover "raw cpu time" (Val 120.) "sol cost" 1000 500 0. 100. 0.1;;
*)

let sc_montage (* xkey ?xlabel xfinal ykey ?ylabel *) () =
  let display = true in
  let xkey = "raw cpu time" and xlabel = "CPU time (seconds)" in
  let ykey = "sol cost" and ylabel = "cost" in
  let xfinal = Val 120. in
  let objects = 1000 and subsets = 500 and max_p = 0.1 in
  let model = "uniform" in
  let hundred =
    plot_setcover ~display xkey xfinal ~xlabel ykey ~ylabel ~objects
      ~subsets:500 ~min_vl:0. ~max_vl:100. ~max_p ~model in
  let ten =
    plot_setcover ~display xkey xfinal ~xlabel ykey ~ylabel ~objects
      ~subsets ~min_vl:0. ~max_vl:10. ~max_p ~model in
  let one =
    plot_setcover ~display xkey xfinal ~xlabel ykey ~ylabel ~objects
      ~subsets ~min_vl:0. ~max_vl:1. ~max_p ~model in
  let nine =
    plot_setcover ~display xkey xfinal ~xlabel ykey ~ylabel ~objects
      ~subsets ~min_vl:9. ~max_vl:10. ~max_p ~model in
  let one_1k =
    plot_setcover ~display xkey xfinal ~xlabel ykey ~ylabel ~objects
      ~subsets:1000 ~min_vl:0. ~max_vl:1. ~max_p ~model in
  let one_1h =
    plot_setcover ~display xkey xfinal ~xlabel ykey ~ylabel ~objects
      ~subsets:100 ~min_vl:0. ~max_vl:1. ~max_p ~model in
  let legend =
    let lines = setcover_lines xkey xfinal ykey 1000 500 0. 100. 0.1 in
      Num_by_num.standalone_legend ~vertical:false lines
  in
  let s =
    Plot_sheet.us_letter_montage ~nrows:3 ~ncols:2
      [ hundred; ten; one; nine; one_1k; one_1h; ]
  in
    (*
      legend#set_size ~w:(Length.In 5.5) ~h:(Length.In 0.025);
    *)
    legend#set_size ~w:(Length.In 6.5) ~h:(Length.In 0.025);
    legend#display;
    legend#output "sc_legend.ps";
    hundred#output "sc_100.ps";
    ten#output "sc_10.ps";
    one#output "sc_1.ps";
    nine#output "sc_9_10.ps";
    one_1k#output "sc_one_1k.ps";
    one_1h#output "sc_one_1h.ps";
    match s with
      | s :: [] ->
	  s#output "sc-montage.pdf";
	  s#display
      | [] -> failwith "No pages"
      | _ -> failwith "Too many pages"


(************************************************************)
(** CSP legend} *)

let csp_legend () =
  let algs = [ "DFS"; "DDS"; "ILDS"; "WDS"; "Indecision"; "YIELDS";] in
  let lines = List.map (fun str -> Some str, [||]) algs in
  let dss = Num_by_num.line_datasets ~color:true lines in
  let legend = Num_by_num.standalone_legend ~vertical:false dss in
    legend#set_size ~w:(Length.In 5.) ~h:(Length.In 0.15);
    legend#output "csp_legend.ps";
    legend#display
