(** Some utilities for datasets.

    @author eaburns
    @since 2010-01-04
*)

open Printf

module StrSet = Set.Make(String)

let instances_run_on_all dss =
  (** [instances_run_on_all dss] filter the datasets so that they only
      contain instances ("num"s) which have data for each dataset in
      the list.  This is useful if some algorithms were run on a
      subset of the instances as another.  This will ensure that only
      instances that every algorithm has data for are accounted. *)
  let num_set ds =
    (* gets the set of "num"s in the daataset. *)
    Array.fold_left
      (fun accum num -> StrSet.add num accum)
      StrSet.empty
      (Dataset.get_values Fn.identity "num" ds)
  in
  let in_common =
    List.fold_left
      (fun i ds -> StrSet.inter (num_set ds) i)
      (num_set (List.hd dss))
      (List.tl dss)
  in
    List.map
      (Dataset.filter
	 Fn.identity
	 (fun num_str -> StrSet.mem num_str in_common)
	 "num")
      dss


let same_values group_by key dss =
  (** [same_values group_by key value dss] checks that the value for
      [key] is the same for all datasets in a group when grouped by
      the [group_by] keys array. *)
  let ds =
    Dataset.concat_data
      (Dataset.empty "")
      (Wrlist.mapcan Dataset.raw_dfs dss) in
  let groups = Dataset.group_by group_by ds in
    List.for_all
      (fun ds ->
	 let vls = Dataset.get_values Fn.identity key ds in
	   Wrarray.for_all (fun vl ->
			      if vls.(0) <> vl
			      then
				failwith
				  (Wrutils.str
				     "Not the same costs: %s and %s on %s\n"
				     vls.(0) vl (Dataset.name ds);)
			      else true)

	     vls)
      groups


(************************************************************)
(* Sorting.                                                 *)
(************************************************************)

let sort_by_mean key dss =
  (** [sort_by_mean key dss] sorts the dataset list in ascending order
      of the mean value for [key]. *)
  let with_mean = List.map (fun ds -> Dataset.get_mean key ds, ds) dss in
  let sorted_with_mean =
    List.sort (fun (meana,_) (meanb,_) -> Math.fcompare meana meanb) with_mean
  in List.map snd sorted_with_mean


(************************************************************)
(* Fetching datasets from a list.                           *)
(************************************************************)

let by_name dss name =
  (** [by_name dss name] gets a dataset from the list of
      datasets by its name. *)
  match List.filter (fun ds -> (Dataset.name ds) = name) dss with
    | ds :: [] -> ds
    | _ -> invalid_arg "by_name: Invalid algorithm name."


let by_min ?(less=(<)) dss key f =
  (** [by_min ?less dss key f] gets the dataset that minimizes the [f]
      function on [key].  The result is a tuple (min_ds, rest).  Order
      is not reserved.  If the [f] function throws an exception for a
      datafile in the list then it is assumed not to have the minimum
      value and is added to the [rest] list. *)
  let rec do_by_min min min_f rest = function
      (* [do_by_min min rest to_check] *)
    | [] -> min, rest
    | hd :: tl ->
	try
	  let hd_f = try f key hd with _ -> failwith "by_min: f failed" in
	    if try less hd_f min_f with _ -> failwith "by_min: less failed"
	    then do_by_min hd hd_f (min :: rest) tl
	    else do_by_min min min_f (hd :: rest) tl
	with _ -> do_by_min min min_f (hd :: rest) tl
  in
  match dss with
    | [] -> invalid_arg "by_min: Empty dataset list"
    | hd :: tl -> do_by_min hd (f key hd) [] tl


(************************************************************)
(* Statistics.                                              *)
(************************************************************)

let difference_from match_keys key diff_ds dss =
  (** [difference_from match_keys key diff_ds dss] creates a new key
      in each dataset in [dss] which represents the the value of [key]
      in [diff_ds] minus the value of [key] for each matching element
      in each dataset in [dss].  The result is a tuple: (new_key_name
      * new_dss).

      Example: To create a key which represents the nodes expanded by
      DFS minus the nodes expanded by each alg in an array [dss]:

      [difference_from [| "num" |] "total nodes expanded" dfs_ds dss]
  *)
  let diff_name = Dataset.name diff_ds in
  let new_key = Wrutils.str "%s diff from %s" key diff_name in
  let dss = List.map (Dataset.copy_key ~key:key ~new_key:new_key) dss in
    (new_key,
     List.map
       (Dataset.transform_with match_keys diff_ds new_key ~with_key:key (-.))
       dss)

let two_sample_t_tests ?(tails=Stats.Both) key ds1 ds2 =
  (** [two_sample_t_tests ?tails key ds1 ds2] performs a two sample
      t-test between [ds1] and [ds2] for [key]. *)
  let dat1 = Dataset.get_values float_of_string key ds1
  and dat2 = Dataset.get_values float_of_string key ds2
  in ds1, Stats.t_test_two_sample ~tails:tails dat1 dat2


let two_sample_t_tests_paired ?(tails=Stats.Both) pair_key test_key ds1 ds2 =
  (** [two_sample_t_tests_paired ?tails pair_key test_key ds1 ds2]
      performs a paired two sample t-test between [ds1] and [ds2] for
      [key]. *)
  let get_data ds =
    let rows = (Dataset.get_row_vector float_of_string
		  ~sort:Dataset.Ascending [|pair_key; test_key|]) ds
    in Array.map (fun r -> r.(1)) rows
  in
  let dat1 = get_data ds1
  and dat2 = get_data ds2 in
  let diff_ary = Wrarray.map2 (fun a b -> a -. b) dat1 dat2 in
  let zero_ary = Array.map (Fn.constantly1 0.) diff_ary
  in Stats.t_test_two_sample ~tails:tails diff_ary zero_ary


let sign_test pair_key test_key ds1 ds2 =
  (** [sign_test pair_key test_key ds1 ds2] performs a sign test on
      the data.  This will give a p-value without requiring that the data
      is distributed in any particular way. *)
  let a_row =
    Dataset.get_row_vector Fn.identity ~sort:Dataset.Ascending
      [| pair_key; test_key |] ds1
  and b_row =
    Dataset.get_row_vector Fn.identity ~sort:Dataset.Ascending
      [| pair_key; test_key |] ds2
  in
    Wrarray.iter2 (fun a b -> assert (a.(0) = b.(0))) a_row b_row;
    let a = Array.map (fun r -> float_of_string r.(1)) a_row
    and b = Array.map (fun r -> float_of_string r.(1)) b_row
    in Stats.sign_test a b


(** Performs a Wilcoxon signed-rank test on the data.  This will
    give a p-value without requiring that the data is distributed in
    any particular way.  The test evaluates the null hypothesis that
    the median difference is zero.  *)
let wilcoxon_signed_rank_test pair_keys test_key ?(test_key2=test_key)
    ds1 ds2 =
  let a_row =
    Dataset.get_row_vector Fn.identity ~sort:Dataset.Ascending
      (Wrarray.extend pair_keys 1 test_key) ds1
  and b_row =
    Dataset.get_row_vector Fn.identity ~sort:Dataset.Ascending
      (Wrarray.extend pair_keys 1 test_key2) ds2
  in
  let n = Array.length pair_keys in
    Wrarray.iter2
      (fun a b -> for i = 0 to n - 1 do assert (a.(i) = b.(i)) done)
      a_row b_row;
    let a = Array.map (fun r -> float_of_string r.(n)) a_row
    and b = Array.map (fun r -> float_of_string r.(n)) b_row
    in Stats.wilcoxon_signed_rank_test a b


(** Median paired difference between a key of two different datasets.
    This is the median that is tested by the Wilcoxon Signed-Rank
    test.*)
let med_paired_diff pair_keys ~test_key ?(test_key2=test_key) ~ds1 ~ds2 =
  let a_row =
    Dataset.get_row_vector Fn.identity ~sort:Dataset.Ascending
      (Wrarray.extend pair_keys 1 test_key) ds1
  and b_row =
    Dataset.get_row_vector Fn.identity ~sort:Dataset.Ascending
      (Wrarray.extend pair_keys 1 test_key2) ds2
  in
  let n = Array.length pair_keys in
    Wrarray.iter2
      (fun a b -> for i = 0 to n - 1 do assert (a.(i) = b.(i)) done)
      a_row b_row;
    let diffs = Wrarray.map2 (fun a b ->
				float_of_string a.(n)
				-. float_of_string b.(n) )
      a_row b_row
    in
      if Verb.level Verb.debug then begin
	printf "\n";
	Array.iter (fun a -> printf "%g " (float_of_string a.(n))) a_row;
	printf "\n";
	Array.iter (fun b -> printf "%g " (float_of_string b.(n))) b_row;
	printf "\n";
	Array.sort compare diffs;
	Array.iter (printf "%g ") diffs;
	printf "\n";
      end;
      Stats.median diffs


(************************************************************)
(* Solved instance filtering.                               *)
(************************************************************)


module StringSet = Set.Make(String)


let instances_solved_by_someone inst_key dss =
  (** [instances_solved_by_all dss] returns a list of instances
      (specified by [inst_key]) that were solved in all of the datasets
      in the list [dss]. *)
  let solved =
    List.map (fun ds ->
		let solved =
		  Dataset.filter Fn.identity (( = ) "yes") "found solution" ds
		in
		let insts = Dataset.get_values Fn.identity inst_key solved
		in
		  Array.fold_left
		    (fun set inst -> StringSet.add inst set)
		    StringSet.empty
		    insts)
      dss
  in
    StringSet.fold (fun i l -> i :: l)
      (List.fold_left StringSet.union (List.hd solved) (List.tl solved))
      []


let instances_solved_by_all inst_key dss =
  (** [instances_solved_by_all dss] returns a list of instances
      (specified by [inst_key]) that were solved in all of the datasets
      in the list [dss]. *)
  let solved =
    List.map (fun ds ->
		let solved =
		  Dataset.filter Fn.identity (( = ) "yes") "found solution" ds
		in
		let insts = Dataset.get_values Fn.identity inst_key solved
		in
		  Array.fold_left
		    (fun set inst -> StringSet.add inst set)
		    StringSet.empty
		    insts)
      dss
  in List.fold_left StringSet.inter (List.hd solved) (List.tl solved)


let solved_by_all inst_key dss =
  (** [solved_by_all inst_key dss] filters out all instances that were
      not solved in every dataset and also gives the list of instances
      not filtered. *)
  let solved = instances_solved_by_all inst_key dss in
  let solved_list = StringSet.fold (fun i l -> i :: l) solved [] in
  let dss' =
    List.map (Dataset.filter Fn.identity
		(fun i -> StringSet.mem i solved)
		inst_key)
      dss
  in dss', solved_list


let solved_df df =
  (Datafile.get_val df "found solution") = "yes"


let all_solved_dfs dfs =
  (* Were all of the problems in this list of data files solved?*)
  List.fold_left (fun accum e ->
		    (solved_df e) && accum)
    true dfs

let get_unsolved_nums dfs =
  List.fold_left (fun accum e ->
		    if (solved_df e)
		    then accum
		    else (Datafile.get_val e "num")::accum) [] dfs

let min_wt dfwtlist =
  (* finds the minimum weight at which all instances were solved *)
  List.fold_left (fun accum (wt,dflist) ->
		    if wt < accum
		    then (if all_solved_dfs dflist
			  then wt
			  else accum)
		    else accum) infinity dfwtlist


let form_wt_by_dflist ds =
  (* forms a list to be used by min_wt *)
  let datasets_by_weight = Dataset.group_by [|"wt"|] ds in
  let weights =
    List.map
      (fun ds -> float_of_string (Dataset.get_group_value "wt" ds))
      datasets_by_weight
  in
    (List.map2 (fun ds wt -> wt, Dataset.raw_dfs ds)
       datasets_by_weight weights)


let min_solved_wt_dataset d =
  (* finds the minmum weight at which the algorithm(s) in the dataset
     solved all instances *)
  min_wt (form_wt_by_dflist d)


let min_all_solved_by_wt ds =
  (* finds the minimum weight at which all algorithmms represented by
     all of the datasets in [ds] solved everything *)
  List.fold_left (fun acc e ->
		    Math.fmax acc (min_solved_wt_dataset e)) 0. ds

let get_unsolvedlist ds =
  get_unsolved_nums (Dataset.raw_dfs ds)


let solved_count ?i_count ds =
  let instances =
    match i_count with
	None ->  float_of_int (Dataset.size ds)
      | Some v -> float_of_int v in
  let unsolved = float_of_int (List.length (get_unsolvedlist ds)) in
    instances -. unsolved


let solved_percentage ?i_count ds =
  let instances =
    match i_count with
	None ->  float_of_int (Dataset.size ds)
      | Some v -> float_of_int v in
  let unsolved = float_of_int (List.length (get_unsolvedlist ds)) in
    (1. -. (unsolved /. instances)) *. 100.


let filter_instancelist instances ds =
  Dataset.filter (fun n -> int_of_string n) (fun n -> not (List.mem n instances))
    "num" ds

let filter_in_instancelist instances ds =
  Dataset.filter (fun n -> int_of_string n) (fun n -> (List.mem n instances))
    "num" ds


let solved_instances ds =
  filter_instancelist (List.map int_of_string (get_unsolvedlist ds)) ds


let filter_non_numeric_instance ds =
  Dataset.filter (fun n -> n)
    (fun n -> try (ignore(float_of_string n);true)
     with _ -> false)
    "num" ds


let transform_setvalue_on_failed set_val datafile value =
  if Datafile.get_val datafile "found solution" = "yes"
  then value else set_val

let add v datafile value =
    v +. value


let log10_transform datafile value =
  let v = log10 value in
    if Math.finite_p v then v else 0.


let get_min_ys data y_value =
  (* assumes that all datasets are the same size, even though they might
     not be *)
  let data = List.filter (Dataset.is_key "num") data in
    if List.length data = 0 then [||]
    else
      (let data = Dataset.merge data in
       let data =
	 Dataset.group_by
	   ~compare:(fun a b ->
		       try
			 compare (float_of_string a) (float_of_string b)
		       with _ -> Verb.pe Verb.always "Comparefail: %s %s\n" a b; 0)
	   ~sort:Dataset.Ascending [|"num"|] data in
       let mins = List.map (fun ds ->
			      try Dataset.get_min y_value ds
			      with _ -> infinity) data in
	 Verb.pe Verb.always "%i min vals\n" (List.length mins);
	 Array.of_list mins)


let normy_transform yvalues =
  (* Sometimes num is zero index, as in pancakes.
     comment out the -1 to get it to work there*)
  (fun datafile value ->
     let zero_index = Datafile.has_val datafile "ncakes" in
     let best_y_ind = (int_of_string (Datafile.get_val datafile "num")) in
     let best_y_ind = if not zero_index then best_y_ind - 1 else best_y_ind in
     let to_ret =
       if Math.finite_p yvalues.(best_y_ind)
       then (if Math.finite_p value
	     then (if value = 0.
		   then (Verb.pe Verb.always "Value was 0\n";
			 1.)
		   else yvalues.(best_y_ind) /. value)
	     else 0.)
       else (Verb.pe Verb.debug "No one solved instance %i\n" best_y_ind; 1.)
     in
       if not (Math.finite_p to_ret) || to_ret > 1.
       then (Verb.pe Verb.always "%f /. %f\n" yvalues.(best_y_ind) value);
       to_ret)


let last_of_vector_transform key =
  (fun datafile value ->
     let col = Datafile.get_col datafile key in
       col.((Array.length col) - 1))


(* EOF *)
