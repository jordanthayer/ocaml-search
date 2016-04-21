(**

    @author jtd7
    @since 2012-01-24
*)


let get_mean_time dset =
  let times = Dataset.get_values float_of_string "raw cpu time" dset in
  let values = ref [] in
    for i = ((Array.length times) - 1) downto 1
    do
      values := (times.(i) -. times.(i-1))::!values
    done;
    (1800. -. times.((Array.length times) - 1))::!values


let sol_count dset =
  let times = Dataset.get_values float_of_string "raw cpu time" dset in
    Array.length times


let rec append l1 = function
  | [] -> l1
  | hd::tl -> append (hd::l1) tl

let do_alg_time dset =
  let dsets = Dataset.group_by [|"num"|] dset in
  let values = List.map get_mean_time dsets in
  let all_vals = List.fold_left append [] values in
  let total = List.fold_left (+.) 0. all_vals in
    total /. (float (List.length all_vals))


let do_alg_count dset =
  let dsets = Dataset.group_by [|"num"|] dset in
  let values = List.map sol_count dsets in
  let total = List.fold_left (+) 0 values in
    (float total) /. (float (List.length values))


let do_domain_time loader alg_list =
  let dsets = List.map (fun alg -> loader ["alg", alg] alg) alg_list in
  List.iter2 (fun dset nm ->
		let mean = do_alg_time dset in
		  Printf.printf "%s\t%f\n%!" nm mean) dsets alg_list

let do_domain_count loader alg_list =
  let dsets = List.map (fun alg -> loader ["alg", alg] alg) alg_list in
  List.iter2 (fun dset nm ->
		let mean = do_alg_count dset in
		  Printf.printf "%s\t%f\n%!" nm mean) dsets alg_list

