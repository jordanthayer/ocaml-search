(** A plotting script for anticipatory planning.

    @author eaburns
    @since 2010-10-20
*)

open Printf
open Fn

(** The handsight data. *)
let hindsight ?(name=sprintf "hop s=%d h=%d") () =
  let alg s h =
    name s h, ["alg", "hindsight"; "samples", string_of_int s;
	       "horizon", string_of_int h; ]; in
  let samples = [ (* 8; *) (* 16;*) 32; (* 64 *) ] in
  let horizons = [ (* 4; *) 8; 16; (* 32;*) (* 64 *) ] in
  List.flatten (List.map (fun s -> List.map (alg s) horizons) samples)


let oracle = "oracle", ["alg", "oracle"]

let greedy = "greedy", ["alg", "greedy"]

let greedy_lrtdp = "greedy-lrtdp", ["alg", "greedy.lrtdp"]

let reactive = "react", ["alg", "reactive"]

let lrtdp = "lrtdp", ["alg", "lrtdp"; "init cpu time", "30.";]

(** Normalize the dataset solution costs to that of the oracle. *)
let normalize_to_oracle match_keys oracle dss =
  let handle_ds ds =
    Dataset.copy_key "final sol cost" "cost over optimal" ds
    |> Dataset.transform_with match_keys oracle "cost over optimal"
	~with_key:"final sol cost" (fun oracle sol -> sol -. oracle)

    |> Dataset.copy_key ~key:"final sol cost" ~new_key:"optimal cost"
    |> Dataset.transform_with match_keys oracle "optimal cost"
	~with_key:"final sol cost" (fun oracle _sol -> oracle)

(*
    |> Dataset.filter float_of_string (fun f -> f <> 0.) "optimal cost"
*)
  in
  List.map handle_ds dss

(** Normalize the dataset solution costs to that of the reactive
    planner. *)
let normalize_to_reactive match_keys reactive dss =
  let handle_ds ds =
    Dataset.copy_key "final sol cost" "under reactive" ds
    |> Dataset.transform_with match_keys reactive "under reactive"
	~with_key:"final sol cost" (fun reactive sol -> reactive -. sol)
    |> Dataset.copy_key ~key:"final sol cost" ~new_key:"reactive cost"
    |> Dataset.transform_with match_keys reactive "reactive cost"
	~with_key:"final sol cost" (fun reactive _sol -> reactive)
  in
  List.map handle_ds dss

(** Normalize the dataset solution costs to that of the greedy
    solution. *)
let normalize_to_greedy match_keys ~oracle ~greedy dss =
  let handle_ds ds =
    Dataset.copy_key "final sol cost" "greedy cost" ds
    |> Dataset.transform_with match_keys greedy "greedy cost"
	~with_key:"final sol cost" (fun greedy _sol -> greedy)

    |> Dataset.copy_key ~key:"final sol cost" ~new_key:"final less greedy"
    |>  Dataset.transform_with match_keys greedy "final less greedy"
	~with_key:"final sol cost" (fun greedy final -> final -. greedy)

    |> Dataset.copy_key ~key:"optimal cost" ~new_key:"opt less greedy"
    |> Dataset.transform_with match_keys greedy "opt less greedy"
	~with_key:"final sol cost" (fun greedy opt -> opt -. greedy)

    |> Dataset.copy_key ~key:"final less greedy" ~new_key:"normalized reward"
    |> (fun ds -> Dataset.transform_with match_keys ds "normalized reward"
      ~with_key:"opt less greedy" (fun opt fin -> fin /. opt) ds)

    |> Dataset.filter float_of_string (fun f -> f <> 0.) "opt less greedy" in
  List.map handle_ds (normalize_to_oracle match_keys oracle dss)

(** Build a hash table indexed on the values of the given keys to find
    the optimal solution cost for that instance. *)
let opt_by_keys keys oracle =
  let opt_tbl = Hashtbl.create 149 in
  let opt_groups = Dataset.group_by keys oracle in
  let hash_cost ds =
    let opt_costs = Dataset.get_values identity "final sol cost" ds in
    let ks = Array.map (fun k -> Dataset.get_group_value k ds) keys in
    if Array.length opt_costs > 1 then begin
      printf "%s, multiple datafiles\n" (Dataset.get_name ds);
      assert false;
    end;
    Hashtbl.add opt_tbl ks (float_of_string opt_costs.(0))
  in
  List.iter hash_cost opt_groups;
  opt_tbl


(** Ensure that the cost values are not better than optimal. *)
let check_costs keys oracle dss =
  let opt_tbl = opt_by_keys keys oracle in
  let handle_grp ds =
    try
      let ks = Array.map (fun k -> Dataset.get_group_value k ds) keys in
      let opt_cost = Hashtbl.find opt_tbl ks in
      let costs = Dataset.get_values identity "final sol cost" ds in
      if Array.length costs > 1 then begin
	printf "%s, multiple datafiles\n" (Dataset.get_name ds);
	assert false;
      end else if (float_of_string costs.(0)) < opt_cost then begin
	printf "%s, better than optimal %s < %g\n" (Dataset.get_name ds)
	  costs.(0) opt_cost;
	assert false;
      end;
    with Not_found ->
      printf "%s, no optimal cost\n" (Dataset.get_name ds);
      assert false; in
  let handle_alg ds =
    let groups = Dataset.group_by keys ds in List.iter handle_grp groups
  in
  List.iter handle_alg dss

let opt_sol_cost ds =
  let sols =
    Dataset.get_row_vector identity [| "alg"; "final sol cost"; |] ds in
  let ind = Wrarray.find (fun a -> a.(0) = "oracle") sols in
  float_of_string sols.(ind).(1)

let layered_bars ?(size=20) ds =
  let cmp ds0 ds1 =
    let op0 = opt_sol_cost ds0 and op1 = opt_sol_cost ds1 in
    compare op0 op1 in
  let dss =
    Dataset.group_by ~name_fun:(fun o _ _ -> o) [|"seed"|] ds
    |> Wrlist.sample size
    |> Array.to_list
    |> List.sort cmp in
  let name_key = "alg" and value_key = "final sol cost" in
  Dataset_to_spt.layered_barcharts ~name_key ~sort:false
    ~name_fun:(fun s -> String.sub s 0 1) ~value_key dss


(************************************************************)
(** {1 Taxi} *)

let load_taxi ?w ?h ?p ?ncabs ?fhorizon (name, alg_attrs) =
  let attrs =
    [ "domain", "taxi"; ]
    @ (match w with Some w -> ["width", string_of_int w] | None -> [])
    @ (match h with Some h -> ["height", string_of_int h] | None -> [])
    @ (match p with Some p -> ["prob", string_of_float p] | None -> [])
    @ (match fhorizon with
      | Some s -> ["future horizon", string_of_int s]
      | None -> [])
    @ (match ncabs with Some n -> ["num cabs", string_of_int n] | None -> [])
    @ alg_attrs in
  let ds =
    Dataset.load_from_rdb_with_domain ~domain:"anticipatory" attrs ~name in
  Verb.pr Verb.always "Loaded %s\n" name;
  ds

let taxi_keys = [|"seed"; "prob"; "width"; "height"; "num cabs" |]

let taxi_bars w h p fhorizon =
  let load = load_taxi ~w ~h ~p ~ncabs:1 ~fhorizon in
  let hindsight =
    "hindsight", ["alg", "hindsight"; "samples", "32"; "horizon", "16"; ] in
  let dss = List.map load [reactive; greedy; hindsight; oracle] in
  let ds = Dataset.with_name "" (Dataset.merge dss) in
  let bars = layered_bars ds in
  let plot = Num_by_nom.plot ~horiz_lines:[0.] ~ylabel:"cost" bars in
  let sheet = Plot_sheet.us_letter ~landscape:true plot in
  sheet#output "bars.pdf";
  plot#set_size ~w:(Length.In 7.) ~h:(Length.In 2.);
  plot#output "bars.ps";
  plot#display

let taxi_cost_boxes w h fhorizon =
  let ykey = "normalized reward" in
  let load = load_taxi ~w ~h ~fhorizon in
  let algs = greedy_lrtdp :: reactive :: (hindsight ~name:(fun _ h -> sprintf "h=%d" h) ()) in
  let oracle = load oracle and greedy = load greedy in
  let dss =
    normalize_to_greedy taxi_keys ~oracle ~greedy (List.map load algs) in
  let _ = check_costs taxi_keys oracle dss in
  let group k =
    { Dataset_to_spt.key = k;
      Dataset_to_spt.name = (fun v -> k ^ " " ^ v);
      Dataset_to_spt.sort = Dataset.Ascending; } in
  let num_cabs =
    let uav_name v =
      let f = float_of_string v in
      if f = 1. then "1 UAV" else sprintf "%g UAVs" f in
    { Dataset_to_spt.key = "num cabs";
      Dataset_to_spt.name = uav_name;
      Dataset_to_spt.sort = Dataset.Ascending; } in
  let groups = [| group "prob"; num_cabs |] in
  let boxplot = Dataset_to_spt.boxplot ~outliers:false ~key:ykey in
  let datasets =
    Dataset_to_spt.nested_groups ~singletons:false groups boxplot dss in
  let plot =
    Num_by_nom.plot ~horiz_lines:[0.] ~ylabel:ykey ~y_min:(~-.1.) ~y_max:1.
    datasets in
  let sheet = Plot_sheet.us_letter ~landscape:true plot in
  sheet#output "uav.pdf";
  plot#set_size ~w:(Length.In 7.) ~h:(Length.In 1.75);
  plot#output "uav.ps";
  plot#display

let taxi_cost_boxes_mdp w h fhorizon =
  let ykey = "normalized reward" in
  let load = load_taxi ~w ~h ~ncabs:1 ~fhorizon in
  let algs =
    reactive :: lrtdp :: (hindsight ~name:(fun _ h -> sprintf "h=%d" h) ()) in
  let oracle = load oracle and greedy = load greedy in
  let dss =
    normalize_to_greedy taxi_keys ~oracle ~greedy (List.map load algs) in
  let _ = check_costs taxi_keys oracle dss in
  let group k =
    { Dataset_to_spt.key = k;
      Dataset_to_spt.name = (fun v -> k ^ " " ^ v);
      Dataset_to_spt.sort = Dataset.Ascending; } in
  let groups = [| group "prob"; |] in
  let boxplot = Dataset_to_spt.boxplot ~outliers:false ~key:ykey in
  let datasets =
    Dataset_to_spt.nested_groups ~singletons:false groups boxplot dss in
  let plot =
    Num_by_nom.plot ~ylabel:ykey ~horiz_lines:[0.] ~y_min:(~-.1.) ~y_max:1.
    datasets
      ~legend_text_style:{ Spt.default_legend_style with
	Drawing.text_size = Length.Pt 7. }
      ~label_text_style:{ Spt.default_legend_style with
	Drawing.text_size = Length.Pt 8. }
      ~tick_text_style:{ Spt.default_legend_style with
	Drawing.text_size = Length.Pt 6. }
  in
  plot#set_size ~w:(Length.In 3.25) ~h:(Length.In 1.25);
  plot#output "uav_mdp.ps";
  plot#display

(************************************************************)
(** {1 Grocery} *)

let load_groc ?gp ?wp ?nitems ?fhorizon (name, alg_attrs) =
  let attrs =
    [ "domain", "grocery"; ]
    @ (match nitems with
      | Some nitems -> ["nitems", string_of_int nitems] | None -> [])
    @ (match gp with
      | Some gp -> ["grocery prob", string_of_float gp] | None -> [])
    @ (match wp with
      | Some wp -> ["work prob", string_of_float wp] | None -> [])
    @ (match fhorizon with
      | Some s -> ["future horizon", string_of_int s] | None -> [])
    @ alg_attrs in
  let ds =
    Dataset.load_from_rdb_with_domain ~domain:"anticipatory" attrs ~name in
  Verb.pr Verb.always "Loaded %s\n" name;
  ds

let groc_keys = [|"seed"; "grocery prob"; "work prob"; (* "nitems"; *) |]

let groc_cost_boxes nitems fhorizon =
  let ykey = "normalized reward" in
  let load = load_groc ~nitems ~fhorizon in
  let algs = reactive :: (hindsight ~name:(fun _ h -> sprintf "h=%d" h) ()) in
  let oracle = load oracle and greedy = load greedy in
  let dss =
    List.map
      (fun alg ->
	load alg
	  |> Dataset.filter identity (fun s -> s <> "0.8") "work prob"
	  |> Dataset.filter identity (fun s -> s <> "0.8") "grocery prob")
      algs in
  let dss =
    normalize_to_greedy groc_keys ~oracle ~greedy dss in
  let _ = check_costs groc_keys oracle dss in
  let work_prob =
    { Dataset_to_spt.key = "work prob";
      Dataset_to_spt.name = (fun v -> v ^ " widget");
      Dataset_to_spt.sort = Dataset.Ascending; } in
  let groc_prob =
    { Dataset_to_spt.key = "grocery prob";
      Dataset_to_spt.name = (fun v -> v ^ " damage");
      Dataset_to_spt.sort = Dataset.Ascending; } in
  let groups = [| work_prob; groc_prob; |] in
  let boxplot = Dataset_to_spt.boxplot ~outliers:false ~key:ykey in
  let datasets =
    Dataset_to_spt.nested_groups ~singletons:false groups boxplot dss in
  let plot =
    Num_by_nom.plot ~ylabel:ykey ~horiz_lines:[0.] ~y_min:0.5 ~y_max:1.
    datasets
      ~legend_text_style:{ Spt.default_legend_style with
	Drawing.text_size = Length.Pt 7. }
      ~label_text_style:{ Spt.default_legend_style with
	Drawing.text_size = Length.Pt 8. }
      ~tick_text_style:{ Spt.default_legend_style with
	Drawing.text_size = Length.Pt 6. } in
  plot#set_size ~w:(Length.In 3.25) ~h:(Length.In 1.75);
  plot#output "manufacturing.ps";
  plot#display

let groc_bars nitems wp gp fhorizon =
  let load = load_groc ~nitems ~wp ~gp ~fhorizon in
  let hindsight =
    "hindsight", ["alg", "hindsight"; "samples", "32"; "horizon", "16"; ] in
  let dss = List.map load [reactive; greedy; hindsight; oracle] in
  let ds = Dataset.with_name "" (Dataset.merge dss) in
  let bars = layered_bars ds in
  let plot = Num_by_nom.plot ~horiz_lines:[0.] ~ylabel:"cost" bars in
  let sheet = Plot_sheet.us_letter ~landscape:true plot in
  sheet#output "bars.pdf";
  plot#set_size ~w:(Length.In 7.) ~h:(Length.In 2.);
  plot#output "bars.ps";
  plot#display

