(**

    @author jtd7
    @since 2011-01-10
*)

let get_grid_data problems =
  let merged =
  List.fold_left
    (fun accum problem ->
       let problem = Grid_instance.load problem in
       let iface = Grid_interfaces.default_interface problem [Limit.Never] in
       (Offline_contract_study.dups iface [||]):: accum) [] problems in
    List.split merged


let grid_problems cmodel =
  List.map (Printf.sprintf "./group/data/grid_instances/uniform/instance/%s/Four-way/0.35/2000/1200/%i" cmodel)
    (Wrlist.range 20)


let get_tiles_data problems =
  let merged =
  List.fold_left
    (fun accum problem ->
       let problem = Sliding_tiles_inst.load problem in
       let iface = (Sliding_tiles_interfaces.default_interface "unit"
		      problem [Limit.Never]) in
       (Offline_contract_study.dups iface [||]):: accum) [] problems in
    List.split merged


let tiles_problems =
  Rdb.matching_paths "./group/data/tiles_instances/korf_25_easy/4/4"
    []


let by_depth_average depths =
  let opt_arrays = (List.fold_left
		      (fun accum e ->
			 (Array.of_list (snd (List.split e)))::accum)
		      [] depths) in
  let get_opts arrays index = List.fold_left (fun accum e ->
						if (Array.length e) > index
						then (e.(index))::accum
						else accum) [] arrays in
  let max_index = (List.fold_left (fun accum e -> max accum (Array.length e))
		     0 opt_arrays) in
  Array.map (fun lst -> let length = List.length lst in
	       (float (List.fold_left (+) 0 lst)) /. (float length))
    (Array.init max_index (fun i -> get_opts opt_arrays i))



let learn_from_depths ?(max = 10) show_ex get_pat depths =
  let data = List.fold_left (@) [] depths in
  let data = Array.of_list data in
    for i = 0 to max do
      (Wrarray.shuffle data;
       Array.iter
	 (fun (pat, target) ->
	    let fpat = float pat in
	      ignore (show_ex (get_pat fpat) (float target)))
	 data)
    done


let offline_lms get_pat depths =
  let data = List.fold_left (@) [] depths in
  let depths, targets = List.split data in
  let depths = Array.of_list (List.map (fun d -> get_pat (float d)) depths)
  and targets = Array.of_list (List.map (fun v -> [| float v|]) targets) in
  let mats = Do_regression.pairs_to_mats (depths, targets) in
    Do_regression.get_least_squares mats



(** I'm just not getting good values out of the regression.  I could take
    straight averages per depth and just roll with that? *)

(* EOF *)
