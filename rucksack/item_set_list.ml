(** Representation of a set of items and some functions for
    generating them *)


type item =
    { weight : float;
      value  : float;
      id : int;}

type t =
    {items: item list;}


(*************************** Item Functions *********************************)
let value_density i =
  i.value /. i.weight

let item_equal i1 i2 =
  i1.id = i2.id && (* this line could be argued *)
  i1.weight = i2.weight &&
  i1.value = i2.value

let item_equiv i1 i2 =
  i1.weight = i2.weight &&
  i1.value = i2.value

let item_min_weight a b =
  if a.weight < b.weight
  then a else b

let item_min_value a b =
  if a.value < b.value
  then a else b

let item_max_weight a b =
  if a.weight > b.weight
  then a else b

let item_max_value a b =
  if a.value > b.value
  then a else b

(*************************** Item Set Functions *****************************)

let rec get_min_weight t =
  (** returns the minimum weight of all the items in [t] *)
  match t with
    | [] -> infinity
    | hd::tl -> Math.fmin hd.weight (get_min_weight tl)

let rec get_min_value t =
  (** returns the minimum value of all the items in [t] *)
  match t with
    | [] -> infinity
    | hd::tl -> Math.fmin hd.value (get_min_weight tl)

let rec get_min_pair t =
  (** returns the minimum weight and value of all the items in [t] *)
  let f i (a,b) = (Math.fmin i.weight a, Math.fmin i.value b) in
  match t with
    | [] -> (infinity, infinity)
    | hd::tl -> f hd (get_min_pair tl)

let rec get_max_weight t =
  (** returns the maximum weight of all the items in [t] *)
  match t with
    | [] -> -.infinity
    | hd::tl -> Math.fmax hd.weight (get_min_weight tl)

let rec get_max_value t =
  (** returns the maximum value of all the items in [t] *)
  match t with
    | [] -> -.infinity
    | hd::tl -> Math.fmax hd.value (get_min_weight tl)

let rec get_max_pair t =
  (** returns the maximum weight and value of all the items in [t] *)
  let f i (a,b) = (Math.fmax i.weight a, Math.fmax i.value b) in
  match t with
    | [] -> (-.infinity, -.infinity)
    | hd::tl -> f hd (get_min_pair tl)

let rec get_total_weight t =
  (** returns the sum of all weights of the items in [t] *)
  match t with
    | [] -> 0.
    | hd::tl -> hd.weight +. (get_total_weight tl)

let rec get_total_value t =
  (** returns the sum of all values of the items in [t] *)
  match t with
    | [] -> 0.
    | hd::tl -> hd.value +. (get_total_value tl)

let rec get_total_pair t =
  (** returns the sum of all weights and values of the items in [t] *)
  let f i (a,b) = (i.weight +. a, i.value +. b) in
  match t with
    | [] -> (0., 0.)
    | hd::tl -> f hd (get_total_pair tl)

let rec get_min_weight_tuple t =
  match t with
      [] -> failwith "Empty List in get min tuple"
    | [hd] -> hd
    | hd::tl -> item_min_weight hd (get_min_weight_tuple tl)

let rec get_max_value_tuple t =
  match t with
      [] -> failwith "Empty List in get min tuple"
    | [hd] -> hd
    | hd::tl -> item_max_value hd (get_max_value_tuple tl)

let select t item =
  (** Removes [item] from [t], returning the tuple of the item
      removed and the remaining list *)
  let rec fn prev list =
    match list with
      | [] ->
	  failwith (Wrutils.str "Invalid selection: %i not in list!" item.id)
      | hd::tl ->
	  if item.id == hd.id
	  then (item, prev @ tl)
	  else fn (hd::prev) tl in
  let selected,remainder = fn [] t.items in
    selected, {items = List.sort (fun a b -> b.id - a.id) remainder}

let remove t item =
  (** Removes [item] from [t] *)
  List.filter (fun a -> a.id <> item.id) t

let add set item =
  {items = item :: set.items}

let filter_non_fitting remaining_weight t =
  {items = List.filter (fun a -> a.weight <= remaining_weight)
      t.items}

let null_set = { items = [] }

(***************************** Search Functions *****************************)

let expand t =
  (** Generates all possible item sets resulting from removing
     a single element from the current item set [t] *)
  List.map (select t) t.items


let key t =
  (* Note, this will fail with large t's*)
  List.fold_left (fun accum list_ele -> accum + (Math.int_exp 2 list_ele.id))
    0 t.items


let equal t1 t2 =
  (* assumes that lists are sorted *)
  let rec fn t1 t2 =
  match t1 with
    | [] -> (match t2 with
	       | [] -> true
	       | _ -> false)
    | hd1::tl1 -> (match t2 with
		   | [] -> false
		   | hd2::tl2 -> (item_equal hd1 hd2) && fn tl1 tl2) in
    fn t1.items t2.items

(*********************** Set Creation Functions ******************************)
let epsilon = 0.1

let rec make_random_fixed_totals ?(seed = 314159) max_weight total_weight
    total_value count =
  Random.set_state !(Math.random_state_from seed);
  let rec fn remaining_weight remaining_value count =
    if count = 0
    then [{ weight = remaining_weight;
	    value = remaining_value;
	    id = 0;}]
    else
      (let wv = Random.float (Math.fmin
				(remaining_weight -.
				   (float_of_int count) *. epsilon)
				max_weight)
       and vv = Random.float (remaining_value -.
				((float_of_int count) *. epsilon)) in
	 { weight = wv;
	   value = vv;
	   id = count;}::(fn (remaining_weight -. wv) (remaining_value -. vv)
			    (count - 1))) in
  {items = fn total_weight total_value (count - 1)}


let rec make_random_max_vals ?(seed = 314159) max_weight total_weight
    total_value count =
  Random.set_state !(Math.random_state_from seed);
  let rec fn count =
    if (count = 0)
    then [{weight = Math.fmax (Random.float max_weight) epsilon;
	  value = Math.fmax (Random.float total_value) epsilon;
	  id = count;}]
    else
      {weight = Math.fmax (Random.float max_weight) epsilon;
	value = Math.fmax (Random.float total_value) epsilon;
	id = count;}::(fn (count - 1)) in
    {items = fn (count - 1)}



(********************************** I\O **************************************)
let write_columns t ch =
  Datafile.write_colnames ch ["id"; "value"; "weight";];
  List.iter (fun a ->
		Verb.pf Verb.always ch "%i\t%f\t%f\n" a.id a.value a.weight)
    t.items

let read_columns df =
  let ids = Datafile.get_col df "id"
  and values = Datafile.get_col df "value"
  and weights = Datafile.get_col df "weight" in
  let trips = Array.init (Array.length ids)
    (fun i -> { weight = weights.(i);
		value = values.(i);
		id = int_of_float ids.(i);}) in
    {items = Array.to_list trips}

let read_columns_ch ch =
  read_columns (Datafile.read "foo" ch)

let read_columns_str str =
  read_columns (Datafile.load str)

(* EOF *)
