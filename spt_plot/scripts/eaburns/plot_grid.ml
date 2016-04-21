let load_unit4 name args =
  Dataset.load_from_rdb_with_domain ~domain:"grid"
    (["width", "5000"; "height", "5000"; "prob", "0.35"; "costs", "Unit";
      "moves", "Four-way"; "heuristic", "manhattan"] @ args) ~name

let plot_nodes_vs_time () =
  let astar = load_unit4 "A*" ["alg", "astar"] in
  let fast = load_unit4 "UCS-fast" ["alg", "uniform-fast"] in
  let xkey = "total nodes expanded" in
  let ykey = "total raw cpu time" in
  let glyph_factory =
    Factories.make_glyph_factory
      [| Drawing.Ring_glyph; Drawing.Cross_glyph|] () in
  let pts =
    Dataset_to_spt.scatters ~xkey ~ykey ~use_color:true ~glyph_factory
      [astar; fast] in
  let plot = Num_by_num.plot ~xlabel:xkey ~ylabel:ykey pts in
  plot#output "nodes_vs_time.pdf";
  plot#display

let nodes_per_sec ds =
  Dataset.transform_with [|"num"|] ds "nodes per second"
    ~with_key:"total nodes expanded" ( /. )
    (Dataset.copy_key ~key:"total raw cpu time" ~new_key:"nodes per second" ds)

let plot_nodes_per_sec () =
  let astar = nodes_per_sec (load_unit4 "A*" ["alg", "astar"]) in
  let fast = nodes_per_sec (load_unit4 "UCS-fast" ["alg", "uniform-fast"]) in
  let key = "nodes per second" in
  let boxes = Dataset_to_spt.boxplots ~key [astar; fast] in
  let plot = Num_by_nom.plot ~ylabel:key boxes in
  plot#output "nodes_per_second.pdf";
  plot#display
