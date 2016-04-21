(** A plotting script for anticipatory planning.

    @author eaburns
    @since 2010-10-20
*)

open Printf

let filter_zero_lower_bound =
  Dataset.filter
    float_of_string
    (fun p -> match classify_float p with | FP_zero -> false | _ -> true)
    "lower bound"


let add_keys ?norm match_keys ds =
  List.fold_left (fun ds f -> f ds)
    ds
    ([
       (fun ds ->
	  Dataset.numeric_transform "log10 cpu time"
	    (fun v ->
	       let v = if Math.is_zero v then epsilon_float else v in
		 log10 v)
	    (Dataset.copy_key "total raw cpu time" "log10 cpu time" ds));
     ]
     @ (match norm with
	  | Some norm ->
	      [
		(fun ds ->
		   Dataset.transform_with match_keys norm "cost diff"
		     ~with_key:"final sol cost" (-.)
		     (Dataset.copy_key "final sol cost" "cost diff" ds));
		(fun ds ->
		   Dataset.transform_with match_keys norm "cost frac"
		     ~with_key:"final sol cost"
		     (fun r c -> c /. r)
		     (Dataset.copy_key "final sol cost" "cost frac" ds))
	      ]
	  | None -> []))

let load_ambulance w h p steps (alg_name, attrs) =
  (** [load_ambulance w h p steps (alg_name, attrs)] loads the given
      ambulance instance set. *)
  let attrs' =
    [ "domain", "ambulance";
      "width", string_of_int w;
      "height", string_of_int h;
      "prob", string_of_float p;
      "steps", string_of_int steps; ]
    @ attrs
  in
    filter_zero_lower_bound
      (Dataset.load_from_rdb_with_domain ~domain:"anticipatory"
	 attrs' ~name:alg_name)


let load_ambulance_all_prob w h steps (alg_name, attrs) =
  (** [load_ambulance w h steps (alg_name, attrs)] loads the given
      ambulance instance set. *)
  let attrs' =
    [ "domain", "ambulance";
      "width", string_of_int w;
      "height", string_of_int h;
      "steps", string_of_int steps; ]
    @ attrs
  in
    filter_zero_lower_bound
      (Dataset.load_from_rdb_with_domain ~domain:"anticipatory"
	 attrs' ~name:alg_name)


let load_grocery steps ?(prob_attrs=[]) (alg_name, attrs) =
  (** [load_grocery steps ?prob_attrs (alg_name, attrs)] loads grocery
      data. *)
  let attrs' =
    ("domain", "grocery2")
    :: ("steps", string_of_int steps)
    :: prob_attrs @ attrs
  in
    filter_zero_lower_bound
      (Dataset.load_from_rdb_with_domain ~domain:"anticipatory"
	 attrs' ~name:alg_name)


let reactionary =
  "reactionary", ["alg", "reactionary"]


let oracle =
  "oracle", ["alg", "oracle"]


let hindsight ?horiz ?helpful_actions samples =
  let attrs = [ "alg", "anticipatory";
		"samples", string_of_int samples; ] in
  let attrs = match horiz with
    | Some h -> ("horizon", string_of_int h) :: attrs
    | None -> attrs
  in
  let attrs = match helpful_actions with
    | Some h -> ("helpful actions", string_of_bool h) :: attrs
    | None -> attrs
  in
    "anticipatory", attrs


let anticipatory ?horiz ?helpful_actions ?samples () =
  let attrs = [ "alg", "anticipatory"; ] in
  let attrs = match horiz with
    | Some h -> ("horizon", string_of_int h) :: attrs
    | None -> attrs
  in
  let attrs = match helpful_actions with
    | Some h -> ("helpful actions", string_of_bool h) :: attrs
    | None -> attrs
  in
  let attrs = match samples with
    | Some s -> ("samples", string_of_int s) :: attrs
    | None -> attrs
  in
    "anticipatory", attrs


(************************************************************)
(* Verifying all solvers get the same problem.              *)
(************************************************************)


let verify_grocery steps =
  (** [verify_grocery steps] load all grocery instances and make sure
      that the goal arrivials match for all matching instances. *)
  let root = Filename.concat User_paths.data_root "anticipatory/grocery2" in
  let combos = Hashtbl.create 149 in
  let all_attrs = Rdb.matching_attrs root [] in
    List.iter (fun attrs ->
		 let gp = ref "" and wp = ref "" and nitems = ref "" in
		 let seed = ref "" in
		   List.iter (function
				| "grocery prob", v -> gp := v
				| "work prob", v -> wp := v
				| "nitems", v -> nitems := v
				| "seed", v -> seed := v
				| _ -> ())
		     attrs;
		   Hashtbl.replace combos (!gp, !wp, !nitems, !seed) true)
      all_attrs;
    Hashtbl.iter
      (fun (gp, wp, nitems, seed) _ ->
	 let attrs =
	   [ "grocery prob", gp; "work prob", wp; "nitems", nitems;
	     "seed", seed ]
	 in
	 let paths = Rdb.matching_paths root attrs in
	 let dss = List.map Datafile.load paths in
	   match dss with
	     | fst :: rest ->
		 let gtime = Datafile.get_col fst "grocery request time" in
		 let gitem = Datafile.get_col fst "grocery request time" in
		 let wtime = Datafile.get_col fst "work task time" in
		   List.iter
		     (fun ds ->
			let gt= Datafile.get_col fst "grocery request time" in
			let gi= Datafile.get_col fst "grocery request time" in
			let wt= Datafile.get_col fst "work task time" in
			  assert (gt = gtime);
			  assert (gi = gitem);
			  assert (wt = wtime);)
		     rest
	     | _ -> ())
      combos


(************************************************************)
(* Plots in the style of the workshop paper.                *)
(************************************************************)

let ambulance_lines w h steps samples =
  let algs = [
    reactionary;
    "anticipatory h=1", snd (hindsight ~horiz:1 samples);
    "anticipatory h=4", snd (hindsight ~horiz:4 samples);
    "anticipatory h=12", snd (hindsight ~horiz:12 samples);
    "anticipatory h=16", snd (hindsight ~horiz:16 samples);
  ]
  in
  let dss = List.map (load_ambulance_all_prob w h steps) algs in
  let dss = List.map (add_keys [|"seed"; "prob"|]) dss in
  let lines =
    Dataset_to_spt.line_errs ~xkey:"prob" ~ykey:"frac over bound"
      ~group_keys:[|"seed"|] ~use_color:true dss
  in
  let title = sprintf "%d x %d ambulance: %d steps" w h steps in
  let xlabel = "probability of a new patient" in
  let ylabel = "fraction of cost above the lower bound" in
  let plot =
    Num_by_num.plot ~title ~xlabel ~ylabel ~legend_loc:Legend.Upper_left lines
  in
    plot#display


let grocery_lines_const_work wp steps samples =
  let algs =
    [ reactionary;
      "anticipatory", snd (hindsight ~helpful_actions:true samples);
    ] in
  let prob_attrs = [ "work prob", string_of_float wp ] in
  let dss = List.map (load_grocery steps ~prob_attrs) algs in
  let dss =
    List.map (add_keys [|"seed"; "work prob"; "grocery prob"; "nitems" |]) dss
  in
  let lines =
    Dataset_to_spt.line_errs ~xkey:"grocery prob" ~ykey:"frac over bound"
      ~group_keys:[|"seed"|] ~use_color:true dss
  in
  let title = sprintf "grocery: %.1f work prob, %d steps" wp steps in
  let xlabel = "grocery probability " in
  let ylabel = "fraction of cost above the lower bound" in
  let plot =
    Num_by_num.plot ~title ~xlabel ~ylabel ~legend_loc:Legend.Upper_right lines
  in
    plot#display


let grocery_lines_const_groc gp steps samples =
  let algs = [ reactionary; "anticipatory", snd (hindsight samples); ] in
  let prob_attrs = [ "grocery prob", string_of_float gp ] in
  let dss = List.map (load_grocery steps ~prob_attrs) algs in
  let dss = List.map (add_keys [|"seed"; "prob"|]) dss in
  let lines =
    Dataset_to_spt.line_errs ~xkey:"work prob" ~ykey:"frac over bound"
      ~group_keys:[|"seed"|] ~use_color:true dss
  in
  let title = sprintf "grocery: %.1f grocery prob, %d steps" gp steps in
  let xlabel = "work probability" in
  let ylabel = "fraction of cost above the lower bound" in
  let plot =
    Num_by_num.plot ~title ~xlabel ~ylabel ~legend_loc:Legend.Upper_right lines
  in
    plot#display


(************************************************************)
(* Boxplots.                                                *)
(************************************************************)

let ambulance_cost_boxes w h steps =
  let algs = [
    "react", snd reactionary;
    (*
      "h=1", snd (anticipatory ~helpful_actions:true ~horiz:1 ());
      "h=4", snd (anticipatory ~helpful_actions:true ~horiz:4 ());
      "h=16", snd (anticipatory ~helpful_actions:true ~horiz:16 ());
    *)
      "h=10", snd (anticipatory ~helpful_actions:true ~horiz:10 ());
      "h=20", snd (anticipatory ~helpful_actions:true ~horiz:20 ());
      "h=30", snd (anticipatory ~helpful_actions:true ~horiz:30 ());
  ]
  in
  let filter_amb ds =
    let probs = [ "0.02"; "0.04"; (* "0.08"; *) ] in
    let samples = [ "1."; (* "10."; *) "20."; (* "30."; *) ] in
    let ds' =
      Dataset.filter Fn.identity (fun p -> List.mem p probs) "prob" ds
    in
      if Dataset.is_key "samples" ds' then
	Dataset.filter Fn.identity (fun p -> List.mem p samples) "samples" ds'
      else ds'
  in
  let dss =
    List.map (fun a -> filter_amb (load_ambulance_all_prob w h steps a)) algs
  in
  let oracle = load_ambulance_all_prob w h steps oracle in
  let dss = List.map (add_keys [|"seed"; "prob"|] ~norm:oracle) dss in
  let groups =
    Dataset_to_spt.nested_groups
      [|
	Dataset_to_spt.group_key
	  ~name:(fun p -> sprintf "samples=%d" (truncate (float_of_string p)))
	  ~sort:Dataset.Ascending "samples";
	Dataset_to_spt.group_key ~name:(fun p -> sprintf "prob=%s" p)
	  ~sort:Dataset.Ascending "prob";
	(*
	  Dataset_to_spt.group_key ~name:(fun p -> sprintf "wp=%s" p)
	  ~sort:Dataset.Ascending "work prob";
	*)
      |]
      (Dataset_to_spt.boxplot ~key:"cost frac")
      dss
  in
  let ylabel = "factor of optimal cost" in
  let plot =
    Num_by_nom.plot (* ~horiz_lines:[1.] *) groups ~ylabel ~y_max:2.
      ~y_axis_padding:(Length.Pt 2.)
      ~legend_text_style:{ Spt.default_legend_style with
			     Drawing.text_size = Length.Pt 8. }
      ~label_text_style:{ Spt.default_legend_style with
			    Drawing.text_size = Length.Pt 8. }
      ~tick_text_style:{ Spt.default_legend_style with
			   Drawing.text_size = Length.Pt 8. }
  in
    plot#set_size ~w:(Length.In 7.) ~h:(Length.In 1.75);
    plot#output (sprintf "ambulance_%d_%d_boxes.ps" w h);
    plot#display


let ambulance_median_cost w h prob steps alg =
  let ds = load_ambulance w h prob steps alg in
  let oracle = load_ambulance w h prob steps oracle in
  let ds = add_keys [|"seed"; "prob"|] ~norm:oracle ds in
  let med = Dataset.get_median "cost frac" ds in
    Printf.printf "median factor of optimal: %g\n" med


let ambulance_time_boxes w h steps samples =
  let key = "log10 cpu time" in
  let horiz_group ds =
    let dss =
      Dataset.group_by [| "helpful actions" |]
	~name_fun:(fun _ _ v -> if v = "true" then "ha" else "") ds
    in
    let h = Dataset.get_group_value "horizon" ds in
      Num_by_nom.dataset_group (sprintf "%d horiz"
				  (truncate (float_of_string h)))
	(Dataset_to_spt.boxplots ~key dss)
  in
  let prob_group ds =
    let p = float_of_string (Dataset.get_group_value "prob" ds) in
    let react = load_ambulance w h p steps ("react", snd reactionary) in
    let react = add_keys [|"seed"; "prob"|] react in
    let react_box = Dataset_to_spt.boxplot ~key react in
    let dss = Dataset.group_by ~sort:Dataset.Ascending [| "horizon" |] ds in
    let boxes = react_box :: (List.map horiz_group dss) in
      Num_by_nom.dataset_group (sprintf "%.3f prob" p) boxes
  in
  let groups =
    let ds = load_ambulance_all_prob w h steps (hindsight samples) in
    let ds = add_keys [|"seed"; "prob"|] ds in
    let ds =
      Dataset.filter float_of_string (fun h -> h = 1. || h = 20.) "horizon" ds
    in
      List.map prob_group
	(Dataset.group_by ~sort:Dataset.Ascending [| "prob" |] ds)
  in
  let title = sprintf "%d x %d ambulance: %d steps" w h steps in
  let ylabel = "log10 CPU time (seconds)" in
  let plot = Num_by_nom.plot ~y_min:(~-.2.) ~title ~ylabel groups in
  let sheet = Plot_sheet.us_letter ~landscape:true plot in
    sheet#output "ambulance_boxes.pdf";
    plot#display

(************************** Grocery ******************************)


let grocery_cost_boxes steps =
  let prob_attrs =
    [ "nitems", "2"; (* "grocery prob", "0.4"; *) "work prob", "0.4" ]
  in
  let algs = [
    "react", snd reactionary;
    "h=1", snd (anticipatory ~helpful_actions:true ~horiz:1 (*~samples:10 *) ());
    "h=4", snd (anticipatory ~helpful_actions:true ~horiz:4 (*~samples:10 *) ());
(*
    "h=16", snd (anticipatory ~helpful_actions:true ~horiz:16 (*~samples:10 *) ());
*)
  ] in
  let filter_groc ds =
    let probs = [ "0.4"; "0.8"; ] in
    let samples = [ "1."; "10."; ] in
    let ds' =
      Dataset.filter Fn.identity (fun p -> List.mem p probs)
	"work prob"
	(Dataset.filter Fn.identity (fun p -> List.mem p probs)
	   "grocery prob" ds)
    in
      if Dataset.is_key "samples" ds' then
	Dataset.filter Fn.identity (fun p -> List.mem p samples) "samples" ds'
      else ds'
  in
  let dss =
    List.map (fun ds -> filter_groc (load_grocery ~prob_attrs steps ds)) algs
  in
  let oracle = load_grocery ~prob_attrs steps oracle in
  let dss =
    List.map (add_keys [| "seed"; "nitems"; "work prob"; "grocery prob"; |]
		~norm:oracle)
      dss
  in
  let groups =
    Dataset_to_spt.nested_groups
      [|
	Dataset_to_spt.group_key
	  ~name:(fun p -> sprintf "samples=%d" (truncate (float_of_string p)))
	  ~sort:Dataset.Ascending "samples";
	Dataset_to_spt.group_key ~name:(fun p -> sprintf "gp=%s" p)
	  ~sort:Dataset.Ascending "grocery prob";
	(*
	  Dataset_to_spt.group_key ~name:(fun p -> sprintf "wp=%s" p)
	  ~sort:Dataset.Ascending "work prob";
	*)
      |]
      (Dataset_to_spt.boxplot ~key:"cost frac")
      dss
  in
  let ylabel = "factor of opimal cost" in
  let title = None (* Some "Grocery domain: plan costs" *) in
  let plot =
    Num_by_nom.plot (* ~horiz_lines:[1.] *) groups ~ylabel ?title
      ~y_axis_padding:(Length.Pt 2.)
      ~legend_text_style:{ Spt.default_legend_style with
			     Drawing.text_size = Length.Pt 8. }
      ~label_text_style:{ Spt.default_legend_style with
			    Drawing.text_size = Length.Pt 8. }
      ~tick_text_style:{ Spt.default_legend_style with
			   Drawing.text_size = Length.Pt 8. }
  in
    plot#set_size ~w:(Length.In 7.) ~h:(Length.In 1.75);
    plot#output (sprintf "grocery_boxes.ps");
    plot#display


let grocery_median_cost gp steps alg =
  let prob_attrs =
    [ "nitems", "2"; "grocery prob", string_of_float gp; "work prob", "0.4" ]
  in
  let ds = load_grocery ~prob_attrs steps alg in
  let oracle = load_grocery ~prob_attrs steps oracle in
  let ds = add_keys [|"seed"; |] ~norm:oracle ds in
  let med = Dataset.get_median "cost frac" ds in
    Printf.printf "median factor of optimal: %g\n" med



let grocery_time_boxes wp steps samples =
  let key = "log10 cpu time" in
  let prob_attrs = [ "work prob", string_of_float wp ] in
  let filter_groc ds =
    let groc_probs = [ "0.2"; "0.5"; ] in
      Dataset.filter Fn.identity (fun p -> List.mem p groc_probs)
	"grocery prob" ds
  in
  let horiz_group ds =
    let dss =
      Dataset.group_by [| "helpful actions" |]
	~name_fun:(fun _ _ v -> if v = "true" then "ha" else "") ds
    in
    let h = Dataset.get_group_value "horizon" ds in
      Num_by_nom.dataset_group (sprintf "%d horiz"
				  (truncate (float_of_string h)))
	(Dataset_to_spt.boxplots ~key dss)
  in
  let prob_group ds =
    let p = float_of_string (Dataset.get_group_value "grocery prob" ds) in
    let react =
      filter_groc (load_grocery steps ~prob_attrs ("react", snd reactionary))
    in
    let react = add_keys [|"seed"; "grocery prob"; "work prob"; |] react in
    let react_box = Dataset_to_spt.boxplot ~key react in
    let dss = Dataset.group_by ~sort:Dataset.Ascending [| "horizon" |] ds in
    let boxes = react_box :: (List.map horiz_group dss) in
      Num_by_nom.dataset_group (sprintf "%.3f grocery prob" p) boxes
  in
  let groups =
    let ds =
      filter_groc (load_grocery steps ~prob_attrs (hindsight samples)) in
    let ds = add_keys [|"seed"; "grocery prob"; "work prob"|] ds in
    let ds =
      Dataset.filter float_of_string (fun h -> h = 1. || h = 16.) "horizon" ds
    in
      List.map prob_group
	(Dataset.group_by ~sort:Dataset.Ascending [| "grocery prob" |] ds)
  in
  let title = sprintf "grocery: %.3f work prob, %d steps" wp steps in
  let ylabel = "log10 CPU time (seconds)" in
  let plot = Num_by_nom.plot ~title ~ylabel groups in
  let sheet = Plot_sheet.us_letter ~landscape:true plot in
    sheet#output "grocery_boxes.pdf";
    plot#display


(************************************************************)
(* Line err.                                                *)
(************************************************************)

let ambulance_frac_line w h steps samples =
  let algs = [
    "h=1", snd (hindsight ~horiz:1 samples);
    "h=4", snd (hindsight ~horiz:4 samples);
    "h=12", snd (hindsight ~horiz:12 samples);
    "h=16", snd (hindsight ~horiz:16 samples);
  ]
  in
  let dss = List.map (load_ambulance_all_prob w h steps) algs in
  let react = load_ambulance_all_prob w h steps reactionary in
  let dss = List.map (add_keys [|"seed"; "prob"|] ~norm:react) dss in
  let line_errs =
    Dataset_to_spt.line_errs ~xkey:"prob" ~ykey:"cost frac"
      ~group_keys:[|"seed"|] ~use_color:true dss
  in
  let title = sprintf "%d x %d ambulance: %d steps" w h steps in
  let ylabel = "fraction of reactionary cost" in
  let xlabel = "probability of new patient arrival" in
  let plot =
    Num_by_num.plot ~title ~xlabel ~ylabel line_errs
  in
  let sheet = Plot_sheet.us_letter ~landscape:true plot in
    sheet#output "ambulance_frac_line.pdf";
    plot#display



let grocery_frac_line_groc_x steps samples =
  let algs = [
    "wp=0.1", ("work prob", "0.1") :: snd (hindsight samples);
    "wp=0.5", ("work prob", "0.5") :: snd (hindsight samples);
    "wp=0.9", ("work prob", "0.9") :: snd (hindsight samples);
  ] in
  let dss = List.map (fun ds -> load_grocery steps ds) algs in
  let react = load_grocery steps reactionary in
  let dss =
    List.map (add_keys [|"seed"; "work prob"; "grocery prob"|] ~norm:react)
      dss
  in
  let line_errs =
    Dataset_to_spt.line_errs ~use_color:true ~xkey:"grocery prob"
      ~ykey:"cost frac" ~group_keys:[|"seed"|] dss
  in
  let title = sprintf "grocery: %d steps" steps in
  let ylabel = "fraction of reactionary cost" in
  let xlabel = "grocery probability" in
  let plot = Num_by_num.plot ~title ~xlabel ~ylabel line_errs in
  let sheet = Plot_sheet.us_letter ~landscape:true plot in
    sheet#output "grocery_frac_line.pdf";
    plot#display


let grocery_frac_line_work_x steps samples =
  let algs = [
    "gp=0.1", ("grocery prob", "0.1") :: snd (hindsight samples);
    "gp=0.5", ("grocery prob", "0.5") :: snd (hindsight samples);
    "gp=0.9", ("grocery prob", "0.9") :: snd (hindsight samples);
  ] in
  let dss = List.map (fun ds -> load_grocery steps ds) algs in
  let react = load_grocery steps reactionary in
  let dss =
    List.map (add_keys [|"seed"; "work prob"; "grocery prob"|] ~norm:react)
      dss
  in
  let line_errs =
    Dataset_to_spt.line_errs ~use_color:true ~xkey:"work prob"
      ~ykey:"cost frac" ~group_keys:[|"seed"|] dss
  in
  let title = sprintf "grocery: %d steps" steps in
  let ylabel = "fraction of reactionary cost" in
  let xlabel = "work probability" in
  let plot = Num_by_num.plot ~title ~xlabel ~ylabel line_errs in
  let sheet = Plot_sheet.us_letter ~landscape:true plot in
    sheet#output "grocery_frac_line.pdf";
    plot#display


(************************************************************)
(* Cost scatter plot.                                       *)
(************************************************************)

let grocery_scatter gp wp nitems steps samples =
  let algs = [
    reactionary;
    oracle;
    "ha, h=4", snd (hindsight ~helpful_actions:true ~horiz:4 samples);
    "ha, h=12", snd (hindsight ~helpful_actions:true ~horiz:12 samples);
(*
    "h=1", snd (hindsight ~helpful_actions:false ~horiz:1 samples);
    "h=16", snd (hindsight ~helpful_actions:false ~horiz:16 samples);
*)
] in
  let prob_attrs = [ "grocery prob", string_of_float gp;
		     "work prob", string_of_float wp;
		     "nitems", string_of_int nitems; ] in
  let dss = List.map (load_grocery steps ~prob_attrs) algs in
  let dss =
    List.map (add_keys [|"seed"; "grocery prob"; "work prob"; "nitems"; |])
      dss
  in
  let lines =
    Dataset_to_spt.scatters ~xkey:"seed" ~ykey:"final sol cost"
      ~use_color:true dss
  in
  let title = sprintf "grocery: gp=%.1f, wp=%.1f, nitems=%d, %d steps"
    gp wp nitems steps in
  let xlabel = "seed" in
  let ylabel = "final solution cost" in
  let plot =
    Num_by_num.plot ~title ~xlabel ~ylabel ~legend_loc:Legend.Upper_right lines
  in
    plot#display

(************************************************************)
(* Mean factor improvement.                                 *)
(************************************************************)

let ambulance_helpful_action_factor w h steps samples =
  let load = load_ambulance_all_prob w h steps in
  let with_ha = load (hindsight ~horiz:20 ~helpful_actions:true samples) in
  let without_ha = load (hindsight ~horiz:20 ~helpful_actions:false samples) in
  let ds = Dataset.copy_key "total raw cpu time" "frac time" with_ha in
  let ds =
    Dataset.transform_with [| "seed"; "horizon"; "samples"; "prob" |]
      without_ha "frac time" ~with_key:"total raw cpu time" ( /. ) ds
  in
  let med = Dataset.get_median "frac time" ds in
  let mean = Dataset.get_mean "frac time" ds in
  let stddev = Dataset.get_stdev "frac time" ds in
    Printf.printf "med=%f\nmean=%f\nstddev=%f\n" med mean stddev;
    ()


let grocery_helpful_action_factor steps samples =
  let filter_groc ds =
(*
    Dataset.filter float_of_string (fun p -> p > 0.1) "grocery prob" ds
*)
    ds
  in
  let load alg = filter_groc (load_grocery steps alg) in
  let with_ha = load (hindsight ~horiz:16 ~helpful_actions:true samples) in
  let without_ha = load (hindsight ~horiz:16 ~helpful_actions:false samples) in
  let ds = Dataset.copy_key "total raw cpu time" "frac time" with_ha in
  let ds =
    Dataset.transform_with
      [| "seed"; "horizon"; "nitems"; "samples";
	 "grocery prob"; "work prob"; |]
      without_ha "frac time" ~with_key:"total raw cpu time" ( /. ) ds
  in
  let med = Dataset.get_median "frac time" ds in
  let mean = Dataset.get_mean "frac time" ds in
  let stddev = Dataset.get_stdev "frac time" ds in
    Printf.printf "med=%f\nmean=%f\nstddev=%f\n" med mean stddev;
    ()


let ambulance_reactionary_factor w h steps samples =
  let load = load_ambulance_all_prob w h steps in
  let with_ha = load (hindsight ~horiz:20 ~helpful_actions:true samples) in
  let react = load reactionary in
  let ds = Dataset.copy_key "total raw cpu time" "frac time" react in
  let ds =
    Dataset.transform_with [| "seed"; "prob" |]
      with_ha "frac time" ~with_key:"total raw cpu time"
      ( /. ) ds
  in
  let med = Dataset.get_median "frac time" ds in
  let mean = Dataset.get_mean "frac time" ds in
  let stddev = Dataset.get_stdev "frac time" ds in
    Printf.printf "antic. med=%f\n"
      (Dataset.get_median "total raw cpu time" with_ha);
    Printf.printf "react med=%f\n"
      (Dataset.get_median "total raw cpu time" react);
    Printf.printf "med=%f\nmean=%f\nstddev=%f\n" med mean stddev;
    ()


let grocery_reactionary_factor steps samples =
  let filter_groc ds =
    Dataset.filter float_of_string (fun p -> p > 0.1) "grocery prob" ds
  in
  let load alg = filter_groc (load_grocery steps alg) in
  let with_ha = load (hindsight ~horiz:16 ~helpful_actions:true samples) in
  let react = load reactionary in
  let ds = Dataset.copy_key "total raw cpu time" "frac time" react in
  let ds =
    Dataset.transform_with
      [| "seed"; "grocery prob"; "work prob"; "nitems"; |]
      with_ha "frac time" ~with_key:"total raw cpu time" ( /. ) ds
  in
  let med = Dataset.get_median "frac time" ds in
  let mean = Dataset.get_mean "frac time" ds in
  let stddev = Dataset.get_stdev "frac time" ds in
    Printf.printf "antic. med=%f\n"
      (Dataset.get_median "total raw cpu time" with_ha);
    Printf.printf "react med=%f\n"
      (Dataset.get_median "total raw cpu time" react);
    Printf.printf "med=%f\nmean=%f\nstddev=%f\n" med mean stddev;
    ()
