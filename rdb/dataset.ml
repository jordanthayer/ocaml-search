(** A dataset allows for manipulating whole sets of datafiles at the
    same time.

    @author eaburns
    @since 2009-10-30
*)

module StringMap = Map.Make(String)

type t = {
  dfs : Datafile.t list;
  (* The datafiles that comprise this dataset. *)

  name : string;
  (* The name for this set of data. *)

  transforms : (Datafile.t -> float -> float) StringMap.t;
  (* Transforms for the various keys in the dataset. *)

  group_keys : string StringMap.t;
  (* A set of key=value pairs that all of the datafiles in this
     dataset have in common due to a group_by operation. *)

  new_keys : string StringMap.t;
  (* User defined keys which are defined in terms of previously
     existing keys. *)
}

type sorting =
  | Unsorted
  | Ascending
  | Descending
  | First
  | Last

let get_name ds =
  ds.name


let with_name n ds = { ds with name = n }

let identity _ n = n

(******************** From old ps_plot/dfutils.ml ********************)

let found_solution df = (Datafile.get_val df "found solution") = "yes"
and getvl f vl df =
  try
    f (Datafile.get_val df vl)
  with Not_found ->
    Verb.pe Verb.always
      "Dfutils.getvl: Failed to find key '%s' in: %s\n%!"
      vl (Datafile.get_name df);
    raise Not_found

let getstr = getvl (fun x -> x)

let get_dfs t =
  t.dfs


let make_df_lookup dfs key =
  (** [make_df_lookup dfs key] returns a function for doing a quick
      lookup in the [dfs] list for the datafile with the [key] field
      matching a given value.  This can be used to more efficiently
      perform lookups on optimal data sets (for example, getting the
      matching instance). *)
  let tbl = Hashtbl.create (List.length dfs) in
    List.iter (fun df -> let vl = getstr key df in
                 Hashtbl.add tbl vl df)
      dfs;
    (fun vl ->
       try
         Hashtbl.find_all tbl vl
       with Not_found ->
         Verb.pe Verb.always
           "make_df_lookup: Unable to find a matching instance: %s\n%!" vl;
         raise Not_found)

(************************************************************)

let merge datasets =
  let names = List.map (fun n -> n.name) datasets
  and transforms = List.map (fun n -> n.transforms) datasets
  and new_keys = List.map (fun n -> n.new_keys) datasets in
  {dfs = List.concat (List.map (fun n -> n.dfs) datasets);
   name = List.hd names; (*List.fold_left (fun accum str -> accum ^ "_" ^ str) "merged" names;*)
   transforms = List.fold_left (fun tr sm ->
				  (StringMap.fold (fun k v m ->
						     StringMap.add k v m)
				     tr sm)) StringMap.empty transforms;
   group_keys = StringMap.empty;
   new_keys = List.fold_left (fun tr sm ->
				  (StringMap.fold (fun k v m ->
						     StringMap.add k v m)
				     tr sm)) StringMap.empty new_keys;}

(******************** Low-level Datafile Access ********************)
(* You probably don't want to use these functions, instead see
   get_values, get_column_vector and get_row_vector below. *)
(*******************************************************************)

let read_df ?(nullfill=false) key df =
  (** [read_df key df] gets the values for [key] in [df].  If
      [key] is a single-value key then only one value is retrieved,
      however, if [key] is a column key then all values in that column
      are retrieved.  Note: all values are assumed to be floats.  This
      throws Not_found if the key is not in the datafile.

      The key name "" is special cased to result in a unit array with
      the value "0." for a single datafile. *)
  if key = ""
  then [| "0." |]
  else
    ((* Verb.pe Verb.debug "key was nonempty: %s\n%!" key; *)
     if Datafile.has_val df key
     then [| (Datafile.get_val df key) |]
     else
       (Verb.pe Verb.debug "datafile had no value %s\n%!" key;
	if Datafile.has_col df key
	then (Verb.pe Verb.debug "%s appears to be a column.\n%!" key;
	      Array.map string_of_float (Datafile.get_col df key))
	else if (nullfill) then ( [|""|] )
	else
	  (Verb.pe Verb.always "Unable to find key [%s] in datafile [%s]\n%!"
	     key (Datafile.get_name df);
	   raise Not_found)))


let try_transform kind tr df str =
  (** [try_transform kind tr df str] tries to transform the given
      value if it is able to convert the string into a float.  The
      result is transformed to the desired type of the [kind]
      function. *)
  try
    (let fl = float_of_string str in
       kind (string_of_float (tr df fl)))
  with Failure _ -> (Verb.pe Verb.debug "transfrom failed\n%!";
		     kind str)


let get_values_from_df ?(nullfill=false) kind tr key df =
  (** [get_values_from_df kind transform df key] gets the
      transformed values from the given datafile. *)
  let ar = (read_df ~nullfill:nullfill key df) in
(*
    Verb.pe Verb.debug "Read success!\n%!";
*)
    let tr = Array.map (try_transform kind tr df) ar in
(*
      Verb.pe Verb.debug "Transform success!\n%!";
*)
      tr


let get_value_from_df kind tr key df =
  (** [get_value_from_df kind transform df key] gets the transformed value
      from the given datafile.  [key] must be a value key. *)
  if Datafile.has_val df key
  then try_transform kind tr df (read_df key df).(0)
  else invalid_arg (Wrutils.str "[%s] is a not a value key" key)


let get_transform t key =
  (** [get_transform t key] gets the transformation function for the
      given key. *)
  try StringMap.find key t.transforms
  with Not_found -> (fun _ v -> v)


let rec get_base_key t key =
  (** [get_base_key t key] gets the base key for [key].  The base key
      is the one that is actually in the datafile. *)
  if StringMap.mem key t.new_keys
  then get_base_key t (StringMap.find key t.new_keys)
  else key


let get_transform_and_base t key =
  (** [get_transform_and_base t key] gets the transform for [key] and
      the base key in the datafile. *)
  get_transform t key, get_base_key t key


let is_value_key key t =
  (** [is_value_key key t] tests if the given string is a valid value
      key. *)
  let base_key = get_base_key t key in
    Verb.pe Verb.debug "Checking to see if \"%s\" is a key\n%!" base_key;
    t.dfs <> [] && (base_key = ""
	|| (Datafile.has_val (List.hd t.dfs) base_key))


let is_key key t =
  (** [is_key key t] tests if the given string is a valid key. *)
  let base_key = get_base_key t key in
    Verb.pe Verb.debug "Checking to see if \"%s\" is a key\n%!" base_key;
    t.dfs <> [] && (base_key = ""
	|| (Datafile.has_val (List.hd t.dfs) base_key)
	|| (Datafile.has_col (List.hd t.dfs) base_key))


let size t =
  List.length t.dfs


let add_value name (value_function:Datafile.t -> string) ds =
  List.iter
    (fun i -> Datafile.add_val i name (value_function i))
    (ds.dfs)


let add_final_value name key ds =
  let last_val df =
    let ar = Datafile.get_col df key in
      string_of_float (ar.((Array.length ar) - 1)) in
    add_value name last_val ds


let histogram value ds =
  let unique_keys = Hashtbl.create 100 in
  let coll = Hashtbl.create 100 in
    List.iter (fun df ->
		 Hashtbl.add coll (Datafile.get_val df value) 0;
		 Hashtbl.replace unique_keys (Datafile.get_val df value) 0;
	      ) ds.dfs;
    let to_return = ref [] in
      Hashtbl.iter
	(
	  fun key _ ->
	    let key_list = Hashtbl.find_all coll key in
	      to_return := (key,(List.length key_list)) :: !to_return;
	) unique_keys;
      !to_return


(******************** Loading ********************)

let empty name = { dfs = [];
		   name = name;
		   transforms = StringMap.empty;
		   group_keys = StringMap.empty;
		   new_keys = StringMap.empty; }


let make_dummy_on_fail path =
  let attrs = Rdb.attrs_for path in
    Datafile.dummy path attrs


let fail_on_fail path =
  failwith (Printf.sprintf "Failed to load [%s]" path)


let load_from_rdb ?(on_fail = fail_on_fail)
    ?(skip_fails = false) ~db attrs ~name =
  let attrs = List.filter (fun (_,b) -> (String.compare b
					   "UNSPECIFIED")  != 0) attrs in
  let paths = Rdb.matching_paths db attrs in
  let dfs = ref [] in

    List.iter (fun p ->
			Verb.pe Verb.debug "loading %s\n%!" p;
			if Datafile.seems_complete p
			then (dfs := (Datafile.load p):: !dfs)
			else if(skip_fails)
			then Verb.pe Verb.always "failed to load %s\n!" p
			else dfs := (on_fail p)::!dfs) paths;
    if !dfs = []
    then Verb.pr Verb.often "WARNING: loaded empty dataset %s.\n%!" name;
    { (empty name) with dfs = !dfs }


let load_from_rdb_with_domain ?(skip_fails = false) ~domain attrs ~name =
  load_from_rdb
    ~skip_fails:skip_fails
    ~db:(User_paths.data_root ^ domain)
    attrs
    ~name:name


(******************** Transforms ********************)

let concat_data t dfs = { t with dfs = t.dfs @ dfs }


let transform key tr t =
  if is_key key t
  then
    let tr df vl = tr df ((get_transform t key) df vl)
    in { t with transforms = StringMap.add key tr t.transforms }
  else invalid_arg (Wrutils.str "[%s] is not a value key for %s" key t.name)


let numeric_transform key tr t =
  if is_key key t
  then
    let tr df vl = tr ((get_transform t key) df vl)
    in { t with transforms = StringMap.add key tr t.transforms }
  else invalid_arg (Wrutils.str "[%s] is not a value key for %s" key t.name)



let get_invalid_value_keys keys t =
  (** [get_invalid_value_keys keys t] gets a list of invalid value
      keys in the array of keys. *)
  List.filter (fun k -> not (is_value_key k t)) (Array.to_list keys)


let transform_with group_by with_ds key ?(with_key=key) tr t =
  if is_key key t
    && is_key with_key with_ds
    && (get_invalid_value_keys group_by t) = []
    && (get_invalid_value_keys group_by with_ds) = []
  then
    let by_dfs = with_ds.dfs in
    let lookup_dfs = make_df_lookup by_dfs group_by.(0) in
    let tr df vl =
      let match_vls = Array.map (fun k -> getstr k df) group_by in
      let _, with_dfs =
	Array.fold_left
	  (fun (ind, dfs) k ->
	     ind + 1,
	     List.filter
	       (fun df -> (getstr k df) = match_vls.(ind)) dfs)
	  (0, lookup_dfs match_vls.(0)) group_by
      in
	match with_dfs with
	  | [] -> failwith
	      (Wrutils.str
		 "Unable to find 'with' datafile with matching keys %s %s"
		 with_key t.name)
	  | with_df :: tl -> begin
	      if tl <> []
	      then Verb.pr Verb.optional "Multiple 'with' dafafiles match";
	      let with_tr, with_base_key =
		get_transform_and_base with_ds with_key
	      in tr (get_value_from_df
		       float_of_string
		       with_tr
		       with_base_key
		       with_df) vl
	    end
    in transform key tr t
  else
    (* find all the invalid keys and give a nice invalid_arg. *)
    let str =
      List.fold_left (fun s k ->
			Wrutils.str "%s\n[%s] is not a valid value key in [%s]"
			  s k t.name)
	"" (get_invalid_value_keys group_by t) in
    let str =
      List.fold_left (fun s k ->
			Wrutils.str "%s\n[%s] is not a valid value key in [%s]"
			  s k with_ds.name)
	str (get_invalid_value_keys group_by with_ds) in
    let str =
      if not (is_key key t)
      then Wrutils.str "%s\n[%s] is not a key in [%s]" str key t.name
      else str in
    let str =
      if not (is_key with_key with_ds)
      then (Wrutils.str "%s\n[%s] is not a key in [%s]"
	      str with_key with_ds.name)
      else str
    in invalid_arg str


let copy_key ~key ~new_key t =
  if key = new_key
  then
    invalid_arg
      (Wrutils.str "Dataset.copy_key: key=[%s] and new_key=[%s] are the same"
	 key new_key)
  else
    let tr, base = get_transform_and_base t key in
      { t with
	  transforms = StringMap.add new_key tr t.transforms;
	  new_keys = StringMap.add new_key base t.new_keys;
      }


let new_key new_key f t =
  let ds = copy_key ~key:"" ~new_key:new_key t in
    transform new_key (fun df _ -> f df) ds


(******************** Grouping/filtering ********************)

let filter kind filter_fun key dataset =
  let tr, key = get_transform_and_base dataset key in
    { dfs = List.fold_left
	(fun accum element ->
	   if filter_fun (get_value_from_df kind tr key element)
	   then element::accum
	   else accum)
	[] dataset.dfs;
      name = dataset.name;
      transforms = dataset.transforms;
      group_keys = dataset.group_keys;
      new_keys = dataset.new_keys;}


let do_one_group ?(compare = compare) sort name_fun key t =
  (** [do_one_group sort name_fun key t] gets a list of datasets where
      each dataset has the same value for [key].  Note: [key] must not
      be a column key.  [name_fun] is called with the original name,
      the key name and the value of the grouping key to get the name
      of each group. *)
  if (is_value_key key t)
  then begin
    let tr = get_transform t key in
    let get_vl df = get_value_from_df Fn.identity tr key df in
    let groups = Hashtbl.create (List.length t.dfs) in
    let values = ref [] in
      List.iter (fun df ->
		   let vl = get_vl df in
		     if not (Hashtbl.mem groups vl)
		     then values := vl :: !values;
		     Hashtbl.add groups vl df)
	t.dfs;
      let values = match sort with
	| Unsorted -> !values
	| Ascending -> List.sort compare !values
	| Descending -> List.sort (fun a b -> compare b a) !values
	| First -> [List.hd !values]
	| Last -> [List.hd (List.rev !values)]
      in
	List.map (fun vl ->
		    let dfs = Hashtbl.find_all groups vl in
		      { t with
			  dfs = dfs;
			  name = name_fun t.name key vl;
			  group_keys = StringMap.add key vl t.group_keys;
		      })
	  values
  end else invalid_arg (Wrutils.str "[%s] is a not a value key to %s" key
			  t.name)


let group_by
    (* note that sorting on the instance number doesn't currently work *)
    ?(compare = compare)
    ?(sort=Unsorted)
    ?(name_fun=(fun n k v -> Wrutils.str "%s-%s-%s" n k v))
    keys t =
  Array.fold_left
    (fun groups k ->
       List.flatten (List.map (do_one_group ~compare:compare sort name_fun k)
		       groups)) [t] keys


let get_group_value key t =
  try StringMap.find key t.group_keys
  with Not_found -> begin
    Verb.pe Verb.toplvl "key [%s] was never used to group %s dataset" key
      t.name;
    raise Not_found;
  end

(******************** Accessing values ********************)

let get_values kind ?(nullfill = false) ?(sort=Unsorted) key t =
  let tr, key = get_transform_and_base t key in
  let ary =
    ref (Array.concat
	   (List.map (get_values_from_df ~nullfill:nullfill kind tr key) t.dfs))
  in
    Verb.pe Verb.debug "ary constructed\n%!";
    begin match sort with
      | Unsorted -> ()
      | Ascending -> Array.sort compare !ary
      | Descending -> Array.sort (fun a b -> compare b a) !ary
      | First -> ary := [| !ary.(0) |]
      | Last -> ary := [| !ary.((Array.length !ary) - 1) |]
    end;
    Verb.pe Verb.debug "returning values\n%!";
    !ary


let sort_rows sort rows =
  (** [sort_rows sort rows] sorts an array of rows lexicographically
      on the order of the columns. *)
  let cmp a b =
    if sort = Ascending
    then compare a b
    else compare b a
  in
  let rec row_cmp ?(i=0) a b =
    assert ((Array.length a) = (Array.length b));
    let n = Array.length a in
      if i < n
      then
	let c = cmp a.(i) b.(i) in
	  if c = 0
	  then row_cmp ~i:(i+1) a b
	  else c
      else 0
  in
    begin match sort with
      | Unsorted -> rows
      | _ ->
	  Array.sort row_cmp rows;
	  rows
    end



let get_row_vector kind ?(nullfill=false) ?(sort=Unsorted) keys t =
  Verb.pe Verb.debug "Getting cols\n%!";
  let cols = Array.map (fun k -> Verb.pe Verb.debug "for key %s\n%!" k;
			  get_values kind ~nullfill:nullfill
			    ~sort:Unsorted k t) keys in
  Verb.pe Verb.debug "Getting rows\n%!";
  let rows = Matrix.transpose cols in
    Verb.pe Verb.debug "Rows got, sorting\n%!";
    sort_rows sort rows


let get_column_vector kind ?(sort=Unsorted) keys t =
  let cols = Array.map (fun k -> get_values kind ~sort:Unsorted k t) keys in
    begin match sort with
      | Unsorted -> cols (* only transpose if we need to sort *)
      | _ ->
	  let rows = sort_rows sort (Matrix.transpose cols) in
	    Matrix.transpose rows
    end


let new_keys t = StringMap.fold (fun k _ accum -> k :: accum) t.new_keys []


let keys t =
  match t.dfs with
    | [] -> new_keys t
    | hd :: tl -> new_keys t @ Datafile.get_keys hd


let raw_dfs t = t.dfs

let name t = t.name

(******************** High Level Dataset access ************************)

let fold key f init t =
  let col = get_values float_of_string key t
  in Array.fold_left f init col


let get_min key t = fold key Math.fmin infinity t


let get_max key t = fold key Math.fmax neg_infinity t


let get_mean key t =
  let n, sum = fold key (fun (n, sum) vl -> n +. 1., vl +. sum) (0., 0.) t
  in sum /. n


let get_finite_mean key t =
  let n, sum = fold key (fun (n, sum) vl ->
			   if vl < infinity then (n +. 1., vl +. sum)
			   else (n,sum)) (0., 0.) t
  in sum /. n

open Printf

let get_median key t =
  let vls = get_values float_of_string ~sort:Ascending key t in
  let n = Array.length vls in
    assert (n > 0);
    if Math.divisible n 2
    then begin
      let a = vls.(n / 2) and b = vls.((n / 2) + 1) in
	(a +. b) /. 2.
    end else vls.(n / 2)


let get_variance key t =
  let mean = get_mean key t in
  let n, diff_sq =
    fold key
      (fun (n, diff_sq) vl ->
	n +. 1.,
	diff_sq +. Math.square (vl -. mean))
      (0., 0.) t
  in diff_sq /. (n -. 1.)


let get_stdev key t = sqrt (get_variance key t)


let get_stderr key t =
  let mean = get_mean key t in
  let n, diff_sq =
    fold key (fun (n, diff_sq) vl ->
		n +. 1., Math.square (vl -. mean))
      (0., 0.) t in
  let stdev = sqrt (diff_sq /. n)
  in stdev /. (sqrt n)


let min_of_datasets key ts =
  List.fold_left (fun accum next -> min accum next) infinity
    (List.map (get_min key) ts)


let max_of_datasets key ts =
  List.fold_left (fun accum next -> max accum next) (-.infinity)
    (List.map (get_max key) ts)


let count ds =
  List.length ds.dfs


(******************** Plotting ********************)

let to_line ~xkey ~ykey t =
  let ary =
    get_column_vector float_of_string ~sort:Ascending [| xkey; ykey |] t
  in
  let xs, ys = ary.(0), ary.(1) in
    Wrarray.combine xs ys, t.name


let to_scatter ?(glyph="") ~xkey ~ykey t =
  let ary =
    get_column_vector float_of_string ~sort:Ascending [| xkey; ykey |] t
  in
  let xs, ys = ary.(0), ary.(1) in
    t.name, glyph, Wrarray.combine xs ys


let to_line_and_errbars ?(sort = Ascending) ?final_xkey
    group_key ~xkey ~ykey t =
  let groups = group_by group_key t in
  let data =
    List.map (fun ds ->
		let vls =
		  get_column_vector
		    float_of_string ~sort:sort [| xkey; ykey |] ds
		in
		  match final_xkey with
		    | None -> vls.(0), vls.(1)
		    | Some key ->
			let final_xs = get_values float_of_string key ds in
			let final_x = final_xs.(0) in
			let final_y = vls.(1).((Array.length vls.(1)) - 1) in
			  (Wrarray.extend vls.(0) 1 final_x,
			   Wrarray.extend vls.(1) 1 final_y))
      groups
  in data, t.name


let to_csv_string ?(sort=Unsorted) ?(header=true) keys t =
  let b = Buffer.create ((Array.length keys) * (List.length t.dfs) * 4) in
  let rows = get_row_vector Fn.identity ~nullfill:true ~sort:sort keys t in
  let nkeys = Array.length keys in
  let try_to_float str =
    try let fl = float_of_string str in Printf.sprintf "%f" fl
    with Failure _ -> Printf.sprintf "\"%s\"" str
  in
    if(header) then (
      Array.iteri (fun i key ->
		     Buffer.add_string b (Wrutils.str "\"%s\"" key);
		     if i < (nkeys - 1)
		     then Buffer.add_string b ","
		     else Buffer.add_string b "\n")
	keys);
    Array.iter (fun row ->
		  let nvls = Array.length row in
		    Array.iteri (fun i vl ->
				   let vstr = try_to_float vl in
				     Buffer.add_string b vstr;
				     if i < (nvls - 1)
				     then Buffer.add_string b ","
				     else Buffer.add_string b "\n")
		      row)
      rows;
    Buffer.contents b

let ds_exists k v ds =
  List.exists (
    fun df ->
      let df_v = Datafile.get_val df k in
      let same = String.compare df_v v in
	same = 0
  ) ds.dfs


(******************** Scratch ********************)

(*
  let ds =
  Dataset.load_from_rdb_with_domain
  "tiles"
  ["alg", "astar";
  "model", "eight_puzzle";
  "bucket", "0"]
  "astar";;
*)
