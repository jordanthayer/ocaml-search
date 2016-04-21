(** Compare the various tiles domains. *)

open Fn

let load_korf cost alg_attrs name =
  Load_dataset.korf_100_tiles ~cost alg_attrs name


let plot_time () =
  let tiles =
    let ds = load_korf "unit" ["alg", "idastar"] "tiles" in
      Dataset.numeric_transform "log10 nodes expanded" log10
	(Dataset.copy_key "total nodes expanded" "log10 nodes expanded" ds)
  in
  let sliding_tiles =
    let ds = Dataset.load_from_rdb_with_domain
      ~domain:"sliding_tiles" ["alg", "idastar"; "model", "korf";
			       "cost", "unit";
			       "rows", "4"; "cols", "4" ] ~name:"sliding_tiles"
    in
      Dataset.numeric_transform "log10 nodes expanded" log10
	(Dataset.copy_key "total nodes expanded" "log10 nodes expanded" ds)
  in
  let old_tiles =
    let ds = load_korf "unit" ["alg", "idastar_jtd7"] "old tiles" in
      Dataset.numeric_transform "log10 nodes expanded" log10
	(Dataset.copy_key "total nodes expanded" "log10 nodes expanded" ds)
  in
  let dss = [ tiles; old_tiles; sliding_tiles ] in
  let scatters =
    Dataset_to_spt.scatters ~xkey:"log10 nodes expanded"
      ~ykey:"total raw cpu time" ~use_color:true dss
  in
  let p =
    Num_by_num.plot ~title:"Tiles domains: IDA*" ~xlabel:"log10 nodes expanded"
      ~ylabel:"CPU time (seconds)" ~legend_loc:Legend.Upper_left scatters
  in
    p#set_size ~w:(Length.In 10.) ~h:(Length.In 7.5);
    p#output "tiles_times.pdf";
    p#display


let plot_mem () =
  let tiles =
    let ds = load_korf "unit" ["alg", "idastar"] "tiles" in
      Dataset.numeric_transform "log10 nodes expanded" log10
	(Dataset.copy_key "total nodes expanded" "log10 nodes expanded" ds)
  in
  let sliding_tiles =
    let ds = Dataset.load_from_rdb_with_domain
      ~domain:"sliding_tiles" ["alg", "idastar"; "model", "korf";
			       "cost", "unit";
			       "rows", "4"; "cols", "4" ] ~name:"sliding_tiles"
    in
      Dataset.numeric_transform "log10 nodes expanded" log10
	(Dataset.copy_key "total nodes expanded" "log10 nodes expanded" ds)
  in
  let dss = [ tiles; sliding_tiles ] in
  let scatters =
    Dataset_to_spt.scatters ~xkey:"log10 nodes expanded"
      ~ykey:"peak virtual mem usage kb" ~use_color:true dss
  in
  let p =
    Num_by_num.plot ~title:"Tiles domains: IDA*" ~xlabel:"log10 nodes expanded"
      ~ylabel:"peak mem usage (kilobytes)" ~y_min:0.
      ~legend_loc:Legend.Lower_left scatters
  in
    p#set_size ~w:(Length.In 10.) ~h:(Length.In 7.5);
    p#output "tiles_mem.pdf";
    p#display

(** {1 Compare IDA* implementations} *)

let plot_idastar () =
  let key = "total raw cpu time" in
  let op = load_korf "unit" ["alg", "idastar"] "out of place" in
  let ip = load_korf "unit" ["alg", "idastar.inplace"] "in place" in
  let xs = Hashtbl.create 149 and ys = Hashtbl.create 149 in
  let nums = ref [] in
  Array.iter (fun a -> Hashtbl.add xs a.(0) a.(1); nums := a.(0) :: !nums)
    (Dataset.get_row_vector float_of_string [|"num"; key|] op);
  Array.iter (fun a -> Hashtbl.add ys a.(0) a.(1))
    (Dataset.get_row_vector float_of_string [|"num"; key|] ip);
  let pts =
    let pt n = Geometry.point (Hashtbl.find xs n) (Hashtbl.find ys n) in
    Array.of_list (List.map pt !nums) in
  let scatter = Num_by_num.scatter_dataset Drawing.Ring_glyph pts in
  let diag = Num_by_num.function_dataset [||] (fun x -> x) in
  let plot =
    Num_by_num.plot ~xlabel:"out of place" ~ylabel:"in place"
      ~title:key [scatter; diag] in
  plot#display

