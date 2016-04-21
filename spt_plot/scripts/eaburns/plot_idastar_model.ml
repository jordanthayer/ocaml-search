(** A plotting script for dealing with my IDA* model.
    @author eaburns
    @since 2010-07-28
*)

open Printf
open Fn
open Geometry
open Drawing


(** {1 Algorithms} ****************************************)


let idastar_im =
  "IDA* IM",
  [ "alg", "idastar_im"; "max-bins", "100"; "control-factor", "2."; ]

let idastar_im_c_hist =
  "IDA* IM",
  [ "alg", "idastar_im_c_hist"; "max-bins", "100"; "control-factor", "2."; ]

let idastar_im_c_hist_500 =
  "IDA* IM",
  [ "alg", "idastar_im_c_hist"; "max-bins", "500"; "control-factor", "2."; ]

let idastar_im_garray_c_hist =
  "IDA* IM Garray C Hist",
  [ "alg", "idastar_im_garray_c_hist"; "max-bins", "100";
    "control-factor", "2."; ]

let idastar_im_lazy_vec_c_hist =
  "IDA* IM Lazy_vec C Hist",
  [ "alg", "idastar_im_lazy_vec_c_hist"; "max-bins", "100";
    "control-factor", "2."; ]

let idastar_cr =
  "IDA* CR",
  [ "alg", "idastar_cr"; "max-bins", "100"; "control-factor", "2."; ]

let idastar= "IDA*", [ "alg", "idastar"; ]

let rbfs= "RBFS", [ "alg", "rbfs"; ]

let frontier_astar= "Frontier A*", [ "alg", "frontier_astar"; ]

let algs = [ idastar_im;
	     idastar_cr;
	     idastar;
	     frontier_astar;
	   ]


(** {1 Loading} ****************************************)


let additional_keys inst_key = [
  (fun ds -> (* log10 of the total nodes expanded *)
     let new_key = "log10 total nodes expanded" in
       Dataset.numeric_transform new_key (fun x -> log10 x)
	 (Dataset.copy_key ~key:"total nodes expanded" ~new_key ds));
  (fun ds ->
     let new_key = "log10 total raw cpu time" in
       Dataset.numeric_transform new_key (fun x -> log10 x)
	 (Dataset.copy_key ~key:"total raw cpu time" ~new_key ds));
]


let additional_keys_offline = [
  (fun ds ->
     if Dataset.is_key "cdp estimation" ds then
       let new_key = "cdp err" in
	 Dataset.transform_with [| "num" |] ds ~with_key:"idastar expansions"
	   new_key (fun act est -> act -. est)
	   (Dataset.copy_key ~key:"cdp estimation" ~new_key ds)
     else ds);
  (fun ds ->
     let new_key = "im err" in
       Dataset.transform_with [| "num" |] ds ~with_key:"idastar expansions"
	 new_key (fun act est -> act -. est)
	 (Dataset.copy_key ~key:"im estimation" ~new_key ds));
  (fun ds ->
     let new_key = "log10 idastar expansions" in
       Dataset.numeric_transform new_key (fun x -> log10 x)
	 (Dataset.copy_key ~key:"idastar expansions" ~new_key ds));
  (fun ds ->
     let new_key = "log10 im estimation" in
       Dataset.numeric_transform new_key (fun x -> log10 x)
	 (Dataset.copy_key ~key:"im estimation" ~new_key ds));
  (fun ds ->
     if Dataset.is_key "cdp estimation" ds then
       let new_key = "log10 cdp estimation" in
	 Dataset.numeric_transform new_key (fun x -> log10 x)
	   (Dataset.copy_key ~key:"cdp estimation" ~new_key ds)
     else ds);
  (fun ds ->
     let new_key = "log10 im fraction" in
       Dataset.numeric_transform new_key (fun x -> log10 x)
	 (Dataset.copy_key ~key:"im fraction" ~new_key ds));
  (fun ds ->
     if Dataset.is_key "cdp estimation" ds then
       let new_key = "log10 cdp fraction" in
	 Dataset.numeric_transform new_key (fun x -> log10 x)
	   (Dataset.copy_key ~key:"cdp fraction" ~new_key ds)
     else ds);
]


let load_korf cost alg_attrs name =
  let ds =
    Dataset.load_from_rdb_with_domain ~domain:"sliding_tiles"
      (alg_attrs @ ["cost", cost; "model", "korf"; "rows", "4"]) ~name
  in List.fold_left (fun ds k_fun -> k_fun ds) ds (additional_keys "num")


let load_vacuum_maze w h p d alg_attrs name =
  let ds = Load_dataset.standard_vacuum_maze w h p d alg_attrs name
  in List.fold_left (fun ds k_fun -> k_fun ds) ds (additional_keys "num")


let load_offline_korf cost training_size =
  let attrs = [ "alg", "offline_estimations";
		"training set size", string_of_int training_size;
		"cost", cost;
		"model", "korf"; "rows", "4"; "cols", "4";
		"moves", "standard";
	      ]
  in
    List.fold_left (fun ds f -> f ds)
      (Load_dataset.tiles attrs "offline estimations")
      (additional_keys_offline)


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
  in Load_dataset.synth attrs name

let load_uniform_tree ?(costs=[|0.5;5.;10.|]) ?(max_sol_br=1) ?(min_depth=18)
    ?(max_depth=18) attrs alg =
    List.fold_left
      (fun ds f -> f ds)
      (uniform_tree ~costs ~max_sol_br ~min_depth ~max_depth attrs alg)
      (additional_keys "num")


(** {1 Percent CDFs} ****************************************)

let percent_cdf dash color ?(filter_key="found solution") ?(filter_val="yes")
    key ds =
  let total = Dataset.size ds in
  let ds =
    let ds' = Dataset.filter identity (( = ) filter_val) filter_key ds in
      if Dataset.is_key "optimal solution" ds' then
	Dataset.filter identity (( = ) "true") "optimal solution" ds'
      else
	ds
  in
  let one = 100. /. (float total) in
  let values = Dataset.get_values float_of_string key ds in
  let points = Array.map (fun v -> Geometry.point v one) values in
    Num_by_num.points_cdf_dataset (dash ()) ~name:(Dataset.name ds)
      ~color:(color ()) points


let percent_cdfs ?filter_key ?filter_val key dss =
  let color = Factories.default_color_factory () in
  let dash = Factories.default_dash_factory () in
    List.map (percent_cdf dash color ?filter_key ?filter_val key) dss


(** {1 Between Iteration Growth} ****************************************)

let between_iteration_growths ds =
  (** [between_iteration_growths ds] computes the growth values
      between iterations. *)
  let insts = Dataset.group_by [| "num" |] ds in
  let iters =
    List.map (fun ds ->
		(Dataset.get_group_value "num" ds),
		(Dataset.get_row_vector float_of_string
		   ~sort:Dataset.Ascending
		   [| "iter no"; "iter expanded"; |] ds))
      insts
  in
  let growths = ref [] in
    List.iter
      (fun (num, iters) ->
	 let n = Array.length iters in
	   for i = 0 to n - 3 do
	     let cur_no = iters.(i).(0) and nxt_no = iters.(i + 1).(0) in
	     let cur = iters.(i).(1) and nxt = iters.(i + 1).(1) in
	       if cur_no <> nxt_no -. 1. then
		 failwith (sprintf "iteration %f then iteration %f"
			     cur_no nxt_no);
	       growths := (cur_no, nxt /. cur) :: !growths;
	   done
      )
      iters;
    !growths


let growth_log10_boxplot ds =
  (** [growth_log10_boxplot ds] get a boxplot of the between iteration
      growths. *)
  let growths = between_iteration_growths ds in
  let growths = List.map (Fn.compose log10 snd) growths in
    Num_by_nom.boxplot_dataset ~outliers:false
      (Dataset.name ds) (Array.of_list growths)


let median_growth_line ds =
  let growths = between_iteration_growths ds in
  let tbl = Hashtbl.create 149 in
  let module Floatset = Set.Make(struct
				   type t = float
				   let compare a b = compare b a
				 end) in
  let keys = ref Floatset.empty in
    List.iter (fun (no, grow) ->
		 keys := Floatset.add no !keys;
		 Hashtbl.add tbl no grow;)
      growths;
    Array.of_list
      (Floatset.fold (fun k accum ->
			let vls = Hashtbl.find_all tbl k in
			  (*
			let n = List.length vls in
			let sum = List.fold_left ( +. ) 0. vls in
			let p = Geometry.point k (sum /. (float n)) in
			  *)
			let ary = Array.of_list vls in
			  Array.sort compare ary;
			  let med = Stats.median ary in
			  (Geometry.point k med) :: accum)
	 !keys [])


let growth_lines ds =
  (** [growth_lines ds] computes lines of the growth values between
      iterations. *)
  let insts = Dataset.group_by [| "num" |] ds in
  let iters =
    List.map (fun ds ->
		Dataset.get_row_vector float_of_string
		  ~sort:Dataset.Ascending
		  [| "iter no"; "iter expanded" |] ds)
      insts
  in
  let lines =
    List.map
      (fun iters ->
	 let n = Array.length iters in
	   Array.init (n - 2)
	     (fun i ->
		let cur_no = iters.(i).(0) and nxt_no = iters.(i + 1).(0) in
		let cur = iters.(i).(1) and nxt = iters.(i + 1).(1) in
		  if cur_no <> nxt_no -. 1. then
		    failwith (sprintf "iteration %f then iteration %f"
				cur_no nxt_no);
		  Geometry.point cur_no (nxt /. cur)))
      iters
  in
    Array.of_list lines


(** {1 Offline Estimation} ****************************************)

let frac_box_group ds =
  (** [frac_box_group ds] gets a boxplot group for the current size of
      the estimation fraction for CDP and the incremental model. *)
  let im_box =
    Dataset_to_spt.boxplot ~key:"log10 im fraction" (Dataset.with_name "IM" ds)
  and cdp_box =
    Dataset_to_spt.boxplot ~key:"log10 cdp fraction"
      (Dataset.with_name "CDP" ds)
  in
  let training_sizes = Dataset.get_values Fn.identity "training set size" ds in
  let group_name = sprintf "%g samples" (float_of_string training_sizes.(0)) in
    assert (Wrarray.for_all (( = ) training_sizes.(0)) training_sizes);
    Num_by_nom.dataset_group group_name [ im_box; cdp_box ]


let plot_offline_frac_boxes () =
  let sizes = [ (* 1_000_000_000; *) 10_000_000_000; ] in
  let dss = List.map (load_offline_korf "unit") sizes in
  let groups = List.map frac_box_group dss in
  let title = "estimation accuracy" in
  let ylabel = "log10 (estimation / truth)" in
  let plot =
    Num_by_nom.plot ~title ~ylabel ~horiz_lines:[0.] groups
      ~y_min:~-.1. ~y_max:3.5
  in
    plot#set_size ~w:(Length.In 3.) ~h:(Length.In 3.);
    plot#display


let estimation_scatters
    ?point_radius
    ?(im_key="log10 im estimation")
    ?(cdp_key="log10 cdp estimation") cost ds =
  let xkey = "log10 idastar expansions" in
  let im_scatter =
    Dataset_to_spt.scatter ~color:Drawing.dark_red ?point_radius
      ~glyph:Drawing.Cross_glyph
      ~ykey:im_key ~xkey (Dataset.with_name "IM" ds)
  in
    match cost with
      | "unit" ->
	  let cdp_scatter =
	    Dataset_to_spt.scatter ~color:Drawing.dark_green ?point_radius
	      ~glyph:Drawing.Ring_glyph
	      ~ykey:cdp_key ~xkey (Dataset.with_name "CDP" ds)
	  in
	    [ im_scatter; cdp_scatter ]
      | "sqrt" -> [ im_scatter ]
      | c -> failwith (sprintf "Unknown cost function: %s" c)


let plot_offline_scatter ?(size=10_000_000_000) cost =
  let point_radius = Length.Pt 3. in
  let ds = load_offline_korf cost size in
  let scatters = estimation_scatters ~point_radius cost ds in
  let diag = Num_by_num.function_dataset ~name:"y=x" [||] Fn.identity in
  let title = sprintf "%s cost tiles: %g samples" cost (float size) in
  let ylabel = "log10 estimation" in
  let xlabel = "log10 IDA* expansions" in
  let plot =
    Num_by_num.plot ~title ~xlabel ~ylabel (diag :: scatters)
      ~x_min:4.5 ~x_max:11. ~y_min:4.5 ~y_max:11.
      ~legend_loc:Legend.Lower_right
      ~legend_text_style:{ Spt.default_legend_style with
			     Drawing.text_size = Length.Pt 8. }
      ~label_text_style:{ Spt.default_legend_style with
			    Drawing.text_size = Length.Pt 10. }
      ~tick_text_style:{ Spt.default_legend_style with
			   Drawing.text_size = Length.Pt 7. }
  in
  let sheet = Plot_sheet.us_letter ~landscape:true plot in
    sheet#output (sprintf "%s-offline-estimation.pdf" cost);
    plot#set_size ~w:(Length.In 3.) ~h:(Length.In 3.);
    plot#output (sprintf "%s-offline-estimation-small.pdf" cost);
    plot#display


let plot_offline_factor_scatter ?(size=10_000_000_000) cost =
  let point_radius = Length.Pt 3. in
  let ds = load_offline_korf cost size in
  let im_key = "log10 im fraction" and cdp_key = "log10 cdp fraction" in
  let scatters = estimation_scatters ~point_radius ~im_key ~cdp_key cost ds in
  let horiz =
    Num_by_num.function_dataset (* ~name:"y=0" *) [||] (fun _ -> 0.)
  in
    (*
      let title = sprintf "%s cost tiles: %g samples" cost (float size) in
    *)
  let ylabel = if cost = "unit" then Some "log10 (estimate / actual)" else None in
  let xlabel = "log10 nodes" in
  let x_min, x_max, y_min, y_max =
    if cost = "sqrt" then
      4., 10., ~-.1.5, 2.
    else
      5., 10.5, ~-.1.5, 2.
  in
  let plot =
    Num_by_num.plot (* ~title *) ~xlabel ?ylabel (horiz :: scatters)
      ~x_min ~x_max ~y_min ~y_max
      ~legend_loc:Legend.Upper_left
      ~legend_text_style:{ Spt.default_legend_style with
			     Drawing.text_size = Length.Pt 8. }
      ~label_text_style:{ Spt.default_legend_style with
			    Drawing.text_size = Length.Pt 8. }
      ~tick_text_style:{ Spt.default_legend_style with
			   Drawing.text_size = Length.Pt 7. }
  in
  let sheet = Plot_sheet.us_letter ~landscape:true plot in
    sheet#output (sprintf "%s-offline-estimation.ps" cost);
    if cost = "unit" then
      plot#set_size ~w:(Length.In 1.65) ~h:(Length.In 2.)
    else
      plot#set_size ~w:(Length.In 1.65) ~h:(Length.In 2.);
    plot#output (sprintf "%s-offline-estimation-small.ps" cost);
    plot#display


let test_offline_data () =
  let cost = "unit" in
  let size = 10_000_000_000 in
  let ds = load_offline_korf cost size in
  let im_key = "im fraction" and cdp_key = "cdp fraction" in
  let p =
    Dataset_utils.wilcoxon_signed_rank_test [|"num"|] im_key
      ~test_key2:cdp_key ds ds
  in
  let im_med = Dataset.get_median im_key ds in
  let cdp_med = Dataset.get_median cdp_key ds in
    Printf.printf "IM %s median: %f\n" im_key im_med;
    Printf.printf "CDP %s median: %f\n" cdp_key cdp_med;
    Printf.printf "Difference test: p=%g\n" p


let test_offline_data_sqrt  () =
  let cost = "sqrt" in
  let size = 50_000_000_000 in
  let ds = load_offline_korf cost size in
  let im_key = "im fraction" in
  let im_med = Dataset.get_median im_key ds in
    Printf.printf "IM %s median: %f\n" im_key im_med;;

(** {1 Sliding tiles} ****************************************)

let tiles_scatter cost label key alga alga_attrs algb algb_attrs =
  let dsa = load_korf cost alga_attrs alga in
  let dsb = load_korf cost algb_attrs algb in
  let b_key = "b key" in
  let ds =
    Dataset.transform_with [| "num" |] dsb b_key ~with_key:key (fun b _ -> b)
      (Dataset.copy_key ~key ~new_key:"b key" dsa)
  in
  let min = min (Dataset.get_min key ds) (Dataset.get_min b_key ds) in
  let max = max (Dataset.get_max key ds) (Dataset.get_max b_key ds) in
    Dataset_to_spt.scatter ~xkey:key ~ykey:b_key ~glyph:Drawing.Ring_glyph
      (Dataset.with_name label ds), min, max


let plot_tiles_time ?(cost="sqrt") alga alga_attrs algb algb_attrs =
  let key = "log10 total raw cpu time" in
  let scatter, min, max =
    tiles_scatter cost "seconds" key alga alga_attrs algb algb_attrs in
  let y_equal_x = Num_by_num.function_dataset [||] ~name:"y=x" Fn.identity in
  let plot =
    Num_by_num.plot ~title:"CPU time" ~xlabel:(alga ^ " CPU time")
      ~ylabel:(algb ^ " CPU time") ~legend_loc:Legend.Upper_left
      ~x_min:min ~x_max:max ~y_min:min ~y_max:max
      [ scatter; y_equal_x ]
  in
    plot#display


let plot_tiles_nodes ?(cost="sqrt") alga alga_attrs algb algb_attrs =
  let key = "log10 total nodes expanded" in
  let scatter, min, max =
    tiles_scatter cost "nodes" key alga alga_attrs algb algb_attrs in
  let y_equal_x = Num_by_num.function_dataset [||] ~name:"y=x" Fn.identity in
  let plot =
    Num_by_num.plot ~title:"CPU time" ~xlabel:(alga ^ " CPU time")
      ~ylabel:(algb ^ " CPU time") ~legend_loc:Legend.Upper_left
      ~x_min:min ~x_max:max ~y_min:min ~y_max:max
      [ scatter; y_equal_x ]
  in
    plot#display


let tiles_estimation ?(cost="sqrt") (alg, attrs) =
  let ds = load_korf cost attrs alg in
  let ds =
    Dataset.numeric_transform "log10 actual nodes" log10
      (Dataset.copy_key "actual nodes" "log10 actual nodes" ds)
  in
  let ds =
    Dataset.numeric_transform "log10 expected nodes" log10
      (Dataset.copy_key "expected nodes" "log10 expected nodes" ds)
  in
  let scatter =
    Dataset_to_spt.scatter ~point_radius:(Length.Pt 2.)
      ~glyph:Drawing.Cross_glyph
      ~xkey:"log10 actual nodes"
      ~ykey:"log10 expected nodes" ds
  in
  let diag = Num_by_num.function_dataset [||] ~name:"y=x" Fn.identity in
  let plot =
    Num_by_num.plot (* ~title *) ~xlabel:"log10 actual nodes"
      ~ylabel:"log10 expected nodes"
      ~legend_text_style:{ Spt.default_legend_style with
			     Drawing.text_size = Length.Pt 8. }
      ~label_text_style:{ Spt.default_legend_style with
			    Drawing.text_size = Length.Pt 10. }
      ~tick_text_style:{ Spt.default_legend_style with
			   Drawing.text_size = Length.Pt 7. }
      ~x_min:0. ~y_min:0. ~x_max:11. ~y_max:11.
      ~legend_loc:Legend.Lower_right [diag; scatter]
  in
  let med = Dataset.get_median "estimation factor" ds in
  let mean = Dataset.get_mean "estimation factor" ds in
    Printf.printf "median estimation factor: %g\n" med;
    Printf.printf "mean estimation factor: %g\n" mean;
    plot#set_size ~w:(Length.In 2.) ~h:(Length.In 2.);
    plot#output "tiles-estimation.pdf";
    plot#display

let tiles_growth_boxes ?(cost="sqrt") algs =
  let dss =
    List.map (fun (alg, attrs) ->
		Dataset.filter Fn.identity (fun s -> s = "yes")
		  "found solution" (load_korf cost attrs alg))
      algs
  in
  let boxes = List.map growth_log10_boxplot dss in
  let plot =
    Num_by_nom.plot ~title:"sliding tiles" ~ylabel:"log10 growth factor"
      ~horiz_lines:[log10 2.]
      ~legend_text_style:{ Spt.default_legend_style with
			     Drawing.text_size = Length.Pt 8. }
      ~label_text_style:{ Spt.default_legend_style with
			    Drawing.text_size = Length.Pt 10. }
      ~tick_text_style:{ Spt.default_legend_style with
			   Drawing.text_size = Length.Pt 7. }
      ~y_min:(~-.1.5) ~y_max:1.
      boxes
  in
    plot#set_size ~w:(Length.In 2.33) ~h:(Length.In 2.);
    plot#output "tiles-growths.pdf";
    plot#display


let tiles_growth_lines ?(cost="sqrt") ?title algs =
  let dss =
    List.map (fun (alg, attrs) ->
		Dataset.filter Fn.identity (fun s -> s = "yes")
		  "found solution" (load_korf cost attrs alg))
      algs
  in
  let line_data = List.map (fun ds ->
			      let line = median_growth_line ds in
				Dataset.name ds, line)
    dss
  in
  let next = Factories.default_dash_factory () in
  let color = Factories.default_color_factory () in
  let two =
    Num_by_num.function_dataset (next ()) ~name:"y=2" (Fn.constantly1 2.)
  in
  let lines =
    List.map
      (fun (n, ps) ->
	 Num_by_num.line_dataset ~color:(color ()) (next ()) ~name:n ps)
      line_data
  in
  let ylabel = if cost = "unit" then Some "median growth factor" else None in
  let plot =
    Num_by_num.plot ?title ~xlabel:"iteration number"
      ~axis_padding:(Length.Pt 1.) ?ylabel
      ~legend_text_style:{ Spt.default_legend_style with
			     Drawing.text_size = Length.Pt 8. }
      ~label_text_style:{ Spt.default_legend_style with
			    Drawing.text_size = Length.Pt 10. }
      ~tick_text_style:{ Spt.default_legend_style with
			   Drawing.text_size = Length.Pt 7. }
      ~y_min:0. (* ~y_max:20. *)
      (two::lines)
  in
    plot#set_size ~w:(Length.In 1.7) ~h:(Length.In 1.7);
    plot#output (cost ^ "-tiles-growths.ps");
    plot#display



let tiles_cdfs key ?(xlabel=key) ?title cost algs =
(*
  let dss =
    List.map (fun (alg, attrs) ->
		Dataset.filter Fn.identity (fun s -> s = "yes")
		  "found solution" (load_korf cost attrs alg))
      algs
  in
  let cdfs =
    Dataset_to_spt.cdfs (* ~dash_factory *) ~normalize:false
      ~key ~use_color:true dss
  in
*)
  let dss = List.map (fun (alg, attrs) -> load_korf cost attrs alg) algs in
  let cdfs = percent_cdfs key dss in
  let ylabel = if cost = "unit" then Some "% solved" else None in
  let plot =
    Num_by_num.plot ?title ~xlabel:xlabel ?ylabel
      ~axis_padding:(Length.Pt 1.)
      ~legend_text_style:{ Spt.default_legend_style with
			     Drawing.text_size = Length.Pt 8. }
      ~label_text_style:{ Spt.default_legend_style with
			    Drawing.text_size = Length.Pt 10. }
      ~tick_text_style:{ Spt.default_legend_style with
			   Drawing.text_size = Length.Pt 7. }
      ~y_min:80.
      ~x_min:0. (* ~x_max:3600.*) ~legend_loc:Legend.Lower_right cdfs
  in
    plot#set_size ~w:(Length.In 1.7) ~h:(Length.In 1.7);
    plot#output (cost ^ "-tiles-cdf.ps");
    plot#display


(** {1 Vacuum maze} ****************************************)

let vacuum_scatter w h p d label key alga alga_attrs algb algb_attrs =
  let dsa = load_vacuum_maze w h p d alga_attrs alga in
  let dsb = load_vacuum_maze w h p d algb_attrs algb in
  let b_key = "b key" in
  let ds =
    Dataset.transform_with [| "num" |] dsb b_key ~with_key:key (fun b _ -> b)
      (Dataset.copy_key ~key ~new_key:"b key" dsa)
  in
  let min = min (Dataset.get_min key ds) (Dataset.get_min b_key ds) in
  let max = max (Dataset.get_max key ds) (Dataset.get_max b_key ds) in
    Dataset_to_spt.scatter ~xkey:key ~ykey:b_key ~glyph:Drawing.Ring_glyph
      (Dataset.with_name label ds), min, max


let plot_vacuum_time ?(w=500) ?(h=500) ?(p=0.) ?(d=10)
    alga alga_attrs algb algb_attrs =
  let key = "log10 total raw cpu time" in
  let scatter, min, max =
    vacuum_scatter w h p d "seconds" key alga alga_attrs algb algb_attrs in
  let y_equal_x = Num_by_num.function_dataset [||] ~name:"y=x" Fn.identity in
  let plot =
    Num_by_num.plot ~title:"CPU time" ~xlabel:(alga ^ " CPU time")
      ~ylabel:(algb ^ " CPU time") ~legend_loc:Legend.Upper_left
      ~x_min:min ~x_max:max ~y_min:min ~y_max:max
      [ scatter; y_equal_x ]
  in
    plot#display


let plot_vacuum_nodes ?(w=500) ?(h=500) ?(p=0.) ?(d=10)
    alga alga_attrs algb algb_attrs =
  let key = "log10 total nodes expanded" in
  let scatter, min, max =
    vacuum_scatter w h p d "nodes" key alga alga_attrs algb algb_attrs in
  let y_equal_x = Num_by_num.function_dataset [||] ~name:"y=x" Fn.identity in
  let plot =
    Num_by_num.plot ~title:"CPU time" ~xlabel:(alga ^ " CPU time")
      ~ylabel:(algb ^ " CPU time") ~legend_loc:Legend.Upper_left
      ~x_min:min ~x_max:max ~y_min:min ~y_max:max
      [ scatter; y_equal_x ]
  in
    plot#display

let vacuum_estimation ?(w=500) ?(h=500) ?(p=0.) ?(d=10) (alg, attrs) =
  let ds = load_vacuum_maze w h p d attrs alg in
  let ds =
    Dataset.numeric_transform "log10 actual nodes" log10
      (Dataset.copy_key "actual nodes" "log10 actual nodes" ds)
  in
  let ds =
    Dataset.numeric_transform "log10 expected nodes" log10
      (Dataset.copy_key "expected nodes" "log10 expected nodes" ds)
  in
  let scatter =
    Dataset_to_spt.scatter ~point_radius:(Length.Pt 2.)
      ~glyph:Drawing.Cross_glyph
      ~xkey:"log10 actual nodes"
      ~ykey:"log10 expected nodes" ds
  in
  let diag = Num_by_num.function_dataset [||] ~name:"y=x" Fn.identity in
  let plot =
    Num_by_num.plot (* ~title *) ~xlabel:"log10 actual nodes"
      ~ylabel:"log10 expected nodes"
      ~legend_text_style:{ Spt.default_legend_style with
			     Drawing.text_size = Length.Pt 8. }
      ~label_text_style:{ Spt.default_legend_style with
			    Drawing.text_size = Length.Pt 10. }
      ~tick_text_style:{ Spt.default_legend_style with
			   Drawing.text_size = Length.Pt 7. }
      ~x_min:0. ~y_min:0. ~x_max:11. ~y_max:11.
      ~legend_loc:Legend.Lower_right [diag; scatter]
  in
  let med = Dataset.get_median "estimation factor" ds in
  let mean = Dataset.get_mean "estimation factor" ds in
    Printf.printf "median estimation factor: %g\n" med;
    Printf.printf "mean estimation factor: %g\n" mean;
    plot#set_size ~w:(Length.In 2.) ~h:(Length.In 2.);
    plot#output "vacuum-estimation.pdf";
    plot#display


let vacuum_growth_lines ?title ?(w=500) ?(h=500) ?(p=0.) ?(d=10) algs =
  let dss =
    List.map (fun (alg, attrs) ->
		Dataset.filter Fn.identity (fun s -> s = "yes")
		  "found solution"
		  (load_vacuum_maze w h p d attrs alg))
      algs
  in
  let line_data = List.map (fun ds ->
			      let line = median_growth_line ds in
				Dataset.name ds, line)
    dss
  in
  let next = Factories.default_dash_factory () in
  let color = Factories.default_color_factory () in
  let two =
    Num_by_num.function_dataset (next ()) ~name:"y=2" (Fn.constantly1 2.)
  in
  let lines =
    List.map
      (fun (n, ps) ->
	 Num_by_num.line_dataset ~color:(color ()) (next ()) ~name:n ps)
      line_data
  in
  let plot =
    Num_by_num.plot ?title ~xlabel:"iteration number"
      ~axis_padding:(Length.Pt 1.)
      (* ~ylabel:"median growth factor" *)
      ~legend_text_style:{ Spt.default_legend_style with
			     Drawing.text_size = Length.Pt 8. }
      ~label_text_style:{ Spt.default_legend_style with
			    Drawing.text_size = Length.Pt 10. }
      ~tick_text_style:{ Spt.default_legend_style with
			   Drawing.text_size = Length.Pt 7. }
      ~y_min:0. ~x_max:50.
      (two::lines)
  in
    plot#set_size ~w:(Length.In 1.7) ~h:(Length.In 1.7);
    plot#output "vacuum-growths.ps";
    plot#display



let vacuum_cdfs key ?title ?(xlabel=key) ?(w=500) ?(h=500) ?(p=0.) ?(d=10)
    algs =
(*
  let dss =
    List.map (fun (alg, attrs) ->
		Dataset.filter Fn.identity (fun a -> a = "yes")
		  "found solution"
		  (load_vacuum_maze w h p d attrs alg))
      algs
  in
  let cdfs = Dataset_to_spt.cdfs ~normalize:false ~key ~use_color:true dss in
*)
  let dss =
    List.map (fun (alg, attrs) -> load_vacuum_maze w h p d attrs alg) algs
  in
  let cdfs = percent_cdfs key dss in
  let plot =
    Num_by_num.plot ~xlabel ?title
      (* ~ylabel:"instances solved" *)
      ~axis_padding:(Length.Pt 1.)
      (* ~x_min:0. ~x_max:3600. *) ~legend_loc:Legend.Lower_right cdfs
      ~legend_text_style:{ Spt.default_legend_style with
			     Drawing.text_size = Length.Pt 8. }
      ~label_text_style:{ Spt.default_legend_style with
			    Drawing.text_size = Length.Pt 10. }
      ~tick_text_style:{ Spt.default_legend_style with
			   Drawing.text_size = Length.Pt 7. }
  in
    plot#set_size ~w:(Length.In 1.7) ~h:(Length.In 1.7);
    plot#output "vacuum-cdf.ps";
    plot#display




(** {1 Uniform Tree} ****************************************)

let string_of_costs costs =
  (** [string_of_costs costs] gets a string representation of costs. *)
  Wrarray.fold_lefti (fun s i c ->
			if i = 0
			then Printf.sprintf "%s%g" s c
			else Printf.sprintf "%s,%g" s c)
    "" costs


let tree_estimation ?(costs=[|1.;20.;100.|]) ?(max_sol_br=1)
    ?(min_depth=19) ?(max_depth=19) (alg, attrs) =
  let ds =
    load_uniform_tree ~costs ~max_sol_br ~min_depth ~max_depth attrs alg
  in
  let ds =
    Dataset.numeric_transform "log10 actual nodes" log10
      (Dataset.copy_key "actual nodes" "log10 actual nodes" ds)
  in
  let ds =
    Dataset.numeric_transform "log10 expected nodes" log10
      (Dataset.copy_key "expected nodes" "log10 expected nodes" ds)
  in
  let scatter =
    Dataset_to_spt.scatter ~point_radius:(Length.Pt 2.)
      ~glyph:Drawing.Cross_glyph
      ~xkey:"log10 actual nodes"
      ~ykey:"log10 expected nodes" ds
  in
  let diag = Num_by_num.function_dataset [||] ~name:"y=x" Fn.identity in
  let plot =
    Num_by_num.plot (* ~title *) ~xlabel:"log10 actual nodes"
      ~ylabel:"log10 expected nodes"
      ~legend_text_style:{ Spt.default_legend_style with
			     Drawing.text_size = Length.Pt 8. }
      ~label_text_style:{ Spt.default_legend_style with
			    Drawing.text_size = Length.Pt 10. }
      ~tick_text_style:{ Spt.default_legend_style with
			   Drawing.text_size = Length.Pt 7. }
      ~x_min:0. ~y_min:0. ~x_max:11. ~y_max:11.
      ~legend_loc:Legend.Lower_right [diag; scatter]
  in
  let med = Dataset.get_median "estimation factor" ds in
  let mean = Dataset.get_mean "estimation factor" ds in
    Printf.printf "median estimation factor: %g\n" med;
    Printf.printf "mean estimation factor: %g\n" mean;
    plot#set_size ~w:(Length.In 2.) ~h:(Length.In 2.);
    plot#output "tree-estimation.pdf";
    plot#display



let tree_growth_lines ?title ?(costs=[|1.;5.;10.|]) ?(max_sol_br=1)
    ?(min_depth=18) ?(max_depth=18) algs =
  let dss =
    List.map (fun (alg, attrs) ->
		Dataset.filter Fn.identity (( = ) "yes")
		  "found solution"
		  (load_uniform_tree ~costs ~max_sol_br ~min_depth
		     ~max_depth attrs alg))
      algs
  in
  let line_data = List.map (fun ds ->
			      let line = median_growth_line ds in
				Dataset.name ds, line)
    dss
  in
  let next = Factories.default_dash_factory () in
  let color = Factories.default_color_factory () in
  let two =
    Num_by_num.function_dataset (next ()) ~name:"y=2" (Fn.constantly1 2.)
  in
  let lines =
    List.map
      (fun (n, ps) ->
	 Num_by_num.line_dataset ~color:(color ()) (next ()) ~name:n ps)
      line_data
  in
  let plot =
    Num_by_num.plot ?title ~xlabel:"iteration number"
      ~axis_padding:(Length.Pt 1.)
      (* ~ylabel:"median growth factor" *)
      ~legend_text_style:{ Spt.default_legend_style with
			     Drawing.text_size = Length.Pt 8. }
      ~label_text_style:{ Spt.default_legend_style with
			    Drawing.text_size = Length.Pt 10. }
      ~tick_text_style:{ Spt.default_legend_style with
			   Drawing.text_size = Length.Pt 7. }
      ~y_min:0. (* ~y_max:20. *)
      ~y_max:10.
      ~x_max:30.
      (two::lines)
  in
    plot#set_size ~w:(Length.In 1.7) ~h:(Length.In 1.7);
    plot#output "tree-growths.ps";
    plot#display


let tree_cdfs key ?title ?(xlabel=key) ?(costs=[|1.;5.;10.|])
    ?(max_sol_br=1) ?(min_depth=18) ?(max_depth=18) algs =
  (*
    let dss =
    List.map (fun (alg, attrs) ->
    Dataset.filter Fn.identity (fun a -> a = "yes")
    "found solution"
    (load_uniform_tree ~costs ~max_sol_br ~min_depth
    ~max_depth attrs alg))
    algs
    in
    let cdfs =
    Dataset_to_spt.cdfs ~normalize:false ~key ~use_color:true dss
    in
  *)
  let dss =
    List.map (fun (alg, attrs) ->
		(load_uniform_tree ~costs ~max_sol_br ~min_depth
		   ~max_depth attrs alg))
      algs
  in
  let cdfs = percent_cdfs key dss in
  let x_max = if key = "total nodes expanded" then 2e10 else 18000. in
  let ylabel =
    if key = "total nodes expanded" then Some "% solved" else None in
  let plot =
    Num_by_num.plot ~xlabel
      ?ylabel ~x_max ?title
      ~axis_padding:(Length.Pt 1.) ~x_min:0.
      ~legend_text_style:{ Spt.default_legend_style with
			     Drawing.text_size = Length.Pt 8. }
      ~label_text_style:{ Spt.default_legend_style with
			    Drawing.text_size = Length.Pt 10. }
      ~tick_text_style:{ Spt.default_legend_style with
			   Drawing.text_size = Length.Pt 7. }
      ~legend_loc:(Legend.At (Legend.Text_after, 80., 40.))
      (* This legend looks good in the .ps, but not in the GUI. *)
      cdfs
  in
    if key = "total nodes expanded" then
      plot#set_size ~w:(Length.In 2.) ~h:(Length.In 2.)
    else
      plot#set_size ~w:(Length.In 1.7) ~h:(Length.In 1.7);
    plot#output "tree-cdf.ps";
    plot#display


(************************************************************)

let load_pair_data () =
  Dataset.load_from_rdb_with_domain ~domain:"sliding_tiles"
    [ "alg", "pair_test" ] ~name:"pair data"


let correct_line key ds =
  let handle_group ds =
    let x = float_of_string (Dataset.get_group_value "sample size" ds) in
    let total = Dataset.size ds in
    let num =
      Dataset.size (Dataset.filter identity (( = ) "true") key ds) in
    let p = (float num) /. (float total) in
    let low, high = Stats.binomial_confidence_interval p total 0.95 in
    let logx = log10 x in
      Geometry.point logx p, Geometry.triple logx low high
  in
  let by_size =
    Dataset.group_by ~sort:Dataset.Ascending [| "sample size" |] ds
  in
  let pts, conf = List.split (List.map handle_group by_size) in
    Array.of_list pts, Array.of_list conf


let plot_offline_pairs () =
  let ds = load_pair_data () in
  let im_num, im_conf = correct_line "im correct" ds in
  let cdp_num, cdp_conf = correct_line "cdp correct" ds in
  let dash = Factories.default_dash_factory () in
  let data =
    [
      Num_by_num.vert_errbar_dataset ~color:Drawing.dark_red im_conf;
      Num_by_num.vert_errbar_dataset ~color:Drawing.dark_green cdp_conf;
      Num_by_num.line_dataset ~name:"CDP" (dash ())
	~color:Drawing.dark_green cdp_num;
      Num_by_num.line_dataset ~name:"IM" (dash ())
	~color:Drawing.dark_red im_num;
    ] in
  let xlabel = "log10 sample size" in
  let ylabel = "fraction correct" in
  let title = None (* Some "Ordering 15-puzzles by difficulty" *) in
  let plot =
    Num_by_num.plot ?title ~xlabel ~ylabel ~legend_loc:Legend.Lower_right data
      ~text_padding:(Length.Pt 2.)
      ~legend_text_style:{ Spt.default_legend_style with
			     Drawing.text_size = Length.Pt 8. }
      ~label_text_style:{ Spt.default_legend_style with
			    Drawing.text_size = Length.Pt 8. }
      ~tick_text_style:{ Spt.default_legend_style with
			   Drawing.text_size = Length.Pt 7. }
  in
    plot#set_size ~w:(Length.In 1.55) ~h:(Length.In 1.55);
(*
    plot#set_size ~w:(Length.In 3.) ~h:(Length.In 3.);
*)
    plot#output "pairs.ps";
    plot#display


let estimation_factors alg name dash glyph color ds =
  let hard_rows =
    Dataset.get_row_vector float_of_string
      [| "sample size"; alg ^ " hard frac"; |] ds in
  let easy_rows =
    Dataset.get_row_vector float_of_string
      [| "sample size"; alg ^ " easy frac"; |] ds in
  let rows = Array.append hard_rows easy_rows in
  let pts = Array.map (fun r -> point (log10 r.(0)) (log10 r.(1))) rows in
    Num_by_num.y_errbar_dataset (dash ()) glyph ~point_radius:(Length.Pt 0.)
      ~color ~name pts


let plot_offline_pair_factors () =
  let ds = load_pair_data () in
  let dash = Factories.default_dash_factory () in
  let im_bars = estimation_factors "im" "IM" dash Cross_glyph dark_red ds in
  let cdp_bars =
    estimation_factors "cdp" "CDP" dash Ring_glyph dark_green ds in
  let xlabel = "log10 sample size" in
  let ylabel = "log10 (estimate / actual)" in
  let zero_line = Num_by_num.function_dataset [||] (constantly1 0.) in
  let plot =
    Num_by_num.plot ~xlabel ~ylabel ~legend_loc:Legend.Lower_right
      ~text_padding:(Length.Pt 2.)
      [ im_bars; cdp_bars; zero_line; ]
      ~legend_text_style:{ Spt.default_legend_style with
			     Drawing.text_size = Length.Pt 8. }
      ~label_text_style:{ Spt.default_legend_style with
			    Drawing.text_size = Length.Pt 8. }
      ~tick_text_style:{ Spt.default_legend_style with
			   Drawing.text_size = Length.Pt 7. }
  in
    plot#set_size ~w:(Length.In 1.55) ~h:(Length.In 1.55);
    plot#output "pair-factors.ps";
    plot#display


(*
let frac size ds =
  let ds' = Dataset.filter Fn.identity (( = ) size) "sample size" ds in
  let e = Dataset.get_values float_of_string "im easy frac" ds' in
  let h = Dataset.get_values float_of_string "im hard frac" ds' in
  let f = Array.append e h in
    Array.sort compare f;
    (f.(100) +. f.(101)) /. 2.
*)


(************************************************************

plot_offline_factor_scatter "unit";;
plot_offline_factor_scatter ~size:50000000000 "sqrt";;

tree_cdfs "total raw cpu time" ~xlabel:"CPU time" ~costs:[|1.;20.;100.|] ~min_depth:19 ~max_depth:19 [ idastar_im_c_hist_500; idastar_cr; idastar; ];;


tree_growth_lines ~title:"Uniform tree" ~costs:[|1.;20.;100.|] ~min_depth:19 ~max_depth:19 [ idastar_im_c_hist_500; idastar_cr; idastar; ];;


vacuum_cdfs "total raw cpu time" ~xlabel:"CPU time" [idastar_im_c_hist; idastar_cr;];;


vacuum_growth_lines ~title:"Vacuum maze" [idastar_im_c_hist; idastar_cr];;


tiles_cdfs "total raw cpu time" ~xlabel:"CPU time"  "sqrt" [idastar_im_c_hist; idastar_cr];;

tiles_growth_lines ~title:"Square root tiles" [idastar_im_c_hist; idastar_cr];;

tiles_cdfs "total raw cpu time" ~xlabel:"CPU time" "unit" [idastar_im_c_hist; idastar_cr; idastar];;

tiles_growth_lines ~cost:"unit"  ~title:"Sliding tiles" [idastar_im_c_hist; idastar_cr; idastar];;

*)


(*

tree_cdfs ~title:"1,10,100 tree" "total nodes expanded" ~xlabel:"nodes expanded" ~costs:[|1.;10.;100.|] ~min_depth:22 ~max_depth:22 [ idastar_im_c_hist_500; idastar_cr; idastar; ];;

tiles_cdfs ~title:"unit 15-puzzles" "total nodes expanded" ~xlabel:"nodes expanded" "unit" [idastar_im_c_hist; idastar_cr; idastar];;


tiles_cdfs ~title:"sqrt 15-puzzles" "total nodes expanded" ~xlabel:"nodes expanded"  "sqrt" [idastar_im_c_hist; idastar_cr];;

vacuum_cdfs ~title:"vacuum maze" "total nodes expanded" ~xlabel:"nodes expanded" [idastar_im_c_hist; idastar_cr;];;
*)

