(* In the case of BUGSY!, 'wt' means the weight on the search time in
   the utility fnction U(n) = U_{default} - wf * f(n) + wt * t(n).
   'wt' is not short for 'weight' in these contexts.  On the other
   hand, 'wtsched' is short-hand fore 'weight schedule'. *)

open Fn
open Printf

let load_grid moves costs w h prob name args =
  Dataset.load_from_rdb_with_domain ~domain:"grid"
    (["width", string_of_int w; "height",string_of_int  h;
      "prob", string_of_float prob; "costs", costs;
      "moves", moves; "heuristic", "manhattan"] @ args) ~name

let load_unit4 name args =
  load_grid "Four-way" "Unit" 5000 5000 0.35 name args

let load_life4 name args =
  load_grid "Four-way" "Life" 5000 5000 0.35 name args

let transform1 key f key' ds =
  Dataset.copy_key key key' ds |> Dataset.numeric_transform key' f

let transform2 ?(group=[|"num"|]) key0 key1 f key' ds =
  let ds' = Dataset.copy_key ~key:key1 ~new_key:key' ds in
  Dataset.transform_with group ds' key' ~with_key:key0 f ds'

let other_keys ds =
  transform2 "total nodes expanded" "total raw cpu time" ( /. )
    "nodes per second" ds

let util_key wf wt ds =
  transform1 "final sol cost" (fun f -> wf *. f) "plan cost" ds
  |> transform1 "total raw cpu time" (fun t -> wt *. t) "time cost"
  |> transform2 "plan cost" "time cost" (fun f t -> ~-. (f  +. t)) "utility"

let logify ds =
  transform1 "total raw cpu time" log10 "log10 total raw cpu time" ds
  |> transform1 "total nodes expanded" log10 "log10 total nodes expanded"

let util_rows ds =
  let rows = Dataset.get_row_vector Fn.identity [|"num"; "utility"|] ds in
  let sols = Array.map (fun r -> r.(0), float_of_string r.(1)) rows in
  Dataset.name ds, Array.to_list sols

let anytime_util ?(inst="num") wf wt ds =
  let dss = Dataset.group_by [| inst |] ds in
  let util ds =
    let best_util =
      Dataset.get_row_vector float_of_string [|"sol cost"; "raw cpu time"|] ds
      |> Array.map (fun r -> ~-. (wf *. r.(0) +. wt *. r.(1)))
      |> Array.fold_left Math.fmax neg_infinity in
    Dataset.get_group_value inst ds, best_util in
  Dataset.name ds, List.map util dss

let anytime_mon_util ?(inst="num") wf wt ds =
  let dss = Dataset.group_by [| inst |] ds in
  let util ds =
    let best_util =
      Dataset.get_row_vector float_of_string
	[|"final sol cost"; "total raw cpu time"; "monitor policy sol time"; |]
	ds
      |> Array.map (fun r -> ~-. (wf *. r.(0) +. wt *. (r.(1) -. r.(2))))
      |> Array.fold_left Math.fmax neg_infinity in
    Dataset.get_group_value inst ds, best_util in
  Dataset.name ds, List.map util dss

(** Accepts data in the following format: (name:string * (num:string *
    utility:float) list).  The utilities are normalized to the best
    utility for the given instance. *)
let normalize algs =
  let best = Hashtbl.create 149 in
  let best_sol name (num, sol) =
    try
      let b, _ = Hashtbl.find best num in
      if sol > b then Hashtbl.replace best num (sol, name)
    with Not_found ->
      Hashtbl.add best num (sol, name) in
  List.iter (fun (name, sols) -> List.iter (best_sol name) sols) algs;
  let norm_alg (name, insts) =
    let norm_inst (n, sol) =
      let b, _ = Hashtbl.find best n in
      (* divide 'in reverse' since the utility estimates are negative *)
      let sol' = b /. sol in
      n, sol' in
    name, List.map norm_inst insts in
  List.map norm_alg algs

(** Accepts data in the following format: (name:string * (num:string *
    utility:float) list). The output is a bunch of boxplots. *)
let boxes algs =
  let box (name, sols) =
    let vls = List.map snd sols in
    Num_by_nom.boxplot_dataset name (Array.of_list vls) in
  List.map box algs

let unit_utils = [
  0., 1., "time only";
  (* 1., 1e6, "1 musec"; *)
  (*   2., 1e6, "2 msec"; *)
  4., 1e6, "4 musec";
  (*   8., 1e6, "8 msec"; *)
  (* 16., 1e6, "16 musec"; *)
  (*  32., 1e6, "32 msec"; *)
  64., 1e6, "64 musec";
  128., 1e6, "128 musec";
  512., 1e6, "512 musec";
  1., 0., "cost only";
]

let life_utils = [
  0., 1., "time only";
(*  100., 1e12, "100 psec"; *)
(*  200., 1e12, "200 psec"; *)
  400., 1e12, "400 psec";
(* 800., 1e12, "800 psec"; *)
  16., 1e9, "16 nsec";
(*  32., 1e9, "32 nsec"; *)
  64., 1e9, "64 nsec";
  1., 0., "cost only";
]

let plot_util_boxes utils load_fn =
  let ld (wf, wt, name) =
    printf "wf=%g, wt=%g, name=%s\n" wf wt name;
    let bugsy_ed_dd =
      (load_fn "BGSY"
	 ["alg", "bugsy"; "wf", string_of_float wf; "wt", string_of_float wt;
	  "correct h", "true"; "drop dups", "true";
	  "estimate delay", "false"; ]) |> util_key wf wt
         |> util_rows in
    let bugsy_old =
      (load_fn "orig"
	 ["alg", "bugsy-old"; "wf", string_of_float wf;
	  "wt", string_of_float wt]) |> util_key wf wt |> util_rows in
    let arastar =
      load_fn "ARA*" ["alg", "aras_mon"] |> anytime_mon_util wf wt in
    let astar =
      load_fn "A*" ["alg", "astar"] |> util_key wf wt |> util_rows in
    let speedy =
      load_fn "spd" ["alg", "speedy"] |> util_key wf wt |> util_rows in
    let utils =
      normalize [ bugsy_ed_dd; bugsy_old; arastar; astar; speedy; ] in
    let boxes = boxes utils in
    Num_by_nom.dataset_group name boxes in
  let groups = List.map ld utils in
  let plot =
    Num_by_nom.plot ~ylabel:"normalized utility" ~seps:true groups in
  let sheet = Plot_sheet.us_letter ~landscape:true plot in
  sheet#output "util.pdf";
  plot#set_size ~w:(Length.In 12.) ~h:(Length.In 8.);
  plot#output "util.ps";
  plot#display


let plot_boxes key utils load_fn =
  let ld (wf, wt, name) =
    printf "wf=%g, wt=%g, name=%s\n" wf wt name;
    (*
      let bugsy_simp =
      (load_fn "bugsy-simp"
      ["alg", "bugsy-simple"; "wf", string_of_float wf;
      "wt", string_of_float wt]) |> util_key wf wt in
      let bugsy_delay =
      (load_fn "bugsy-delay"
      ["alg", "bugsy-simple-delay"; "wf", string_of_float wf;
      "wt", string_of_float wt]) |> util_key wf wt in
    *)
    let bugsy_dd_c=
      (load_fn "dd/c"
	 ["alg", "bugsy"; "wf", string_of_float wf; "wt", string_of_float wt;
	  "correct h", "true"; "drop dups", "true";
	  "estimate delay", "false"; ]) |> util_key wf wt
       |> other_keys |> logify in
    let bugsy_ed_dd_c=
      (load_fn "dd/c"
	 ["alg", "bugsy"; "wf", string_of_float wf; "wt", string_of_float wt;
	  "correct h", "true"; "drop dups", "true";
	  "estimate delay", "true"; ]) |> util_key wf wt
       |> other_keys |> logify in
    let bugsy_old =
      (load_fn "old"
	 ["alg", "bugsy-old"; "wf", string_of_float wf;
	  "wt", string_of_float wt]) |> util_key wf wt
       |> other_keys |> logify in
    (*
      let astar =
      load_fn "astar" ["alg", "astar"] |> util_key wf wt in
      let speedy =
      load_fn "speedy" ["alg", "speedy"] |> util_key wf wt |> logify in
    *)
    let algs = [ bugsy_ed_dd_c; bugsy_dd_c; bugsy_old; ] in
    let boxes = Dataset_to_spt.boxplots ~key algs in
    Num_by_nom.dataset_group name boxes in
  let groups = List.map ld utils in
  let plot = Num_by_nom.plot ~seps:true ~ylabel:key groups in
  let sheet = Plot_sheet.us_letter ~landscape:true plot in
  sheet#output "key.pdf"


let plot_wastar ykey load_fn =
  let subtree =
    load_fn "wastar-s" ["alg", "wastar"; "update subtree", "true";] in
  let node = load_fn "wastar" ["alg", "wastar"; "update subtree", "false";] in
  let lines =
    Dataset_to_spt.line_errs ~xkey:"wt" ~ykey ~group_keys:[|"num"|]
      ~use_color:true [subtree; node] in
  let plot = Num_by_num.plot ~ylabel:ykey ~xlabel:"weight" lines in
  plot#display
