(** Representation of a set of items and some functions for
    generating them *)


type item =
    { weight : float;
      value  : float;
      id : int;}


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

let get_min_weight t =
  (** returns the minimum weight of all the items in [t] *)
  Array.fold_left (fun accum element -> Math.fmin accum element.weight)
    infinity t

let get_min_value t =
  (** returns the minimum value of all the items in [t] *)
  Array.fold_left (fun accum element -> Math.fmin accum element.value)
    infinity t

let rec get_max_weight t =
  (** returns the maximum weight of all the items in [t] *)
  Array.fold_left (fun accum element -> Math.fmax accum element.weight)
    ~-.infinity t

let rec get_max_value t =
  (** returns the maximum value of all the items in [t] *)
  Array.fold_left (fun accum element -> Math.fmax accum element.value)
    ~-.infinity t

let rec get_total_weight t =
  (** returns the sum of all weights of the items in [t] *)
  Array.fold_left (fun accum element -> element.weight +. accum) 0. t

let rec get_total_value t =
  (** returns the sum of all values of the items in [t] *)
  Array.fold_left (fun accum element -> element.value +. accum) 0. t

let rec get_total_pair t =
  (** returns the sum of all weights and values of the items in [t] *)
  Array.fold_left (fun (wt,v) element -> (element.weight +. wt),
		     (element.value +. v)) (0.,0.) t

let rec get_min_weight_tuple t =
  assert ((Array.length t) > 0);
  Array.fold_left (fun accum element ->
		     item_min_weight accum element)
    t.(0) t

let rec get_max_value_tuple t =
  assert ((Array.length t) > 0);
  Array.fold_left (fun accum element ->
		     item_max_value accum element)
    t.(0) t

(***************************** Search Functions *****************************)


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
  Array.of_list (fn total_weight total_value (count - 1))


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
    Array.of_list (fn (count - 1))



(********************************** I\O **************************************)
let write_columns t ch =
  Datafile.write_colnames ch ["id"; "value"; "weight";];
  Array.iter (fun a ->
		Verb.pf Verb.always ch "%i\t%f\t%f\n" a.id a.value a.weight)
    t

let read_columns df =
  let ids = Datafile.get_col df "id"
  and values = Datafile.get_col df "value"
  and weights = Datafile.get_col df "weight" in
    Array.init (Array.length ids)
      (fun i -> { weight = weights.(i);
		  value = values.(i);
		  id = int_of_float ids.(i);})

let read_columns_ch ch =
  read_columns (Datafile.read "foo" ch)

let read_columns_str str =
  read_columns (Datafile.load str)

(* EOF *)
