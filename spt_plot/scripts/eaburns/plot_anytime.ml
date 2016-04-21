let load name args =
  Dataset.load_from_rdb_with_domain ~domain:"grid"
    (["width", "5000"; "height", "5000"; "prob", "0.35"; "costs", "Unit";
      "moves", "Four-way"; "heuristic", "manhattan"] @ args) ~name

let norm astar ds =
  let keys = [ "nodes expanded", "total nodes expanded";
	       "nodes generated", "total nodes generated";
	       "sol cost", "final sol cost" ] in
  let tr ds (key, tot_key) =
    let nkey = "norm " ^ key in
    Dataset.transform_with [|"num"|] astar nkey ~with_key:tot_key
      (fun a b -> b /. a)
      (Dataset.copy_key ~key ~new_key:nkey ds) in
  List.fold_left tr ds keys

let plot () =
  let astar = load "A*" ["alg", "astar"] in
  let ocaml = norm astar (load "ARA*-OCaml" ["alg", "arastar"]) in
  let silvia = norm astar (load "ARA*-Richter" ["alg", "arastar-richter"]) in
  let xkey = "norm nodes expanded" in
  let ykey = "norm sol cost" in
  let group_keys = [| "num" |] in
  let lines =
    Dataset_to_spt.padded_line_errs ~xkey ~maxx:2. ~ykey ~group_keys
      [ocaml; silvia] in
  let plot = Num_by_num.plot ~xlabel:xkey ~ylabel:ykey ~x_max:2. lines in
  plot#display
