(** An alternate encoding of the zero one knapsack problem *)

type t =
    { left_behind : Item_set_list.t;
      taking : Item_set_list.t;
      value : float;
      over_weight : float;
      sack_size : float; }


let h t =
  let rec fn items remaining =
    match items with
	[] -> 0.
      | hd::tl ->
	  (if hd.Item_set_list.weight > remaining
	   then (hd.Item_set_list.value *. (hd.Item_set_list.weight /. remaining))
	   else hd.Item_set_list.value +. fn tl (remaining -. hd.Item_set_list.weight))
  and vd = Item_set_list.value_density in
    if t.over_weight > 0.
    then
      (fn (List.sort (fun a b ->
			let vd_a = vd a and vd_b = vd b in
			  if vd_a = vd_b then 0
			  else if vd_a > vd_b then 1 else -1)
	     t.taking.Item_set_list.items) t.over_weight)
    else 0.



let d_shortest t =
  let rec fn items remaining =
      match items with
	| [] -> 0.
	| hd::tl ->
	    (if hd.Item_set_list.weight > remaining
	     then 1.
	     else 1. +. (fn tl (remaining -. hd.Item_set_list.weight))) in
    fn (List.sort (fun a b ->
		     if a.Item_set_list.weight = b.Item_set_list.weight then 0
		     else if a.Item_set_list.weight > b.Item_set_list.weight then -1
		     else 1) t.taking.Item_set_list.items) t.over_weight


let d t =
  let rec fn items remaining =
    match items with
	[] -> 0.
      | hd::tl ->
	  (if hd.Item_set_list.weight > remaining
	   then 1.
	   else 1. +. fn tl (remaining -. hd.Item_set_list.weight))
  and vd = Item_set_list.value_density in
    if t.over_weight > 0.
    then
      (fn (List.sort (fun a b ->
			let vd_a = vd a and vd_b = vd b in
			  if vd_a = vd_b then 0
			  else if vd_a > vd_b then 1 else -1)
	     t.taking.Item_set_list.items) t.over_weight)
    else 0.


let expand t t_g =
    Verb.pe Verb.debug "Over %f\tRemaining %i\tvalue possible %f\n" t.over_weight (int_of_float (d t))
      (h t);
    List.fold_left (fun accum (to_leave,remaining) ->
		      (let rw = t.over_weight -. to_leave.Item_set_list.weight
		       and g = t_g +. to_leave.Item_set_list.value in
			 ({left_behind = Item_set_list.add t.left_behind to_leave;
			   taking = remaining;
			   value = g;
			   over_weight = rw;
			   sack_size = t.sack_size},g)::accum))
      [] (Item_set_list.expand t.taking)


let key t =
  (* will be better than t.in_sack for highly regular objects *)
  Item_set_list.key t.taking




let print_node t =
  let at_verb = Verb.debug in
  let print_ids items =
      List.iter (Verb.pe at_verb " %i")
	(List.map (fun i -> i.Item_set_list.id)
	   items.Item_set_list.items) in
    Verb.pe at_verb "Left_Behind:";
    print_ids t.left_behind;
    Verb.pe at_verb "\nTaking: ";
    print_ids t.taking;
    Verb.pe at_verb "\nValue: %f\nOver: %f\nSack: %f\n"
      t.value t.over_weight t.sack_size

let equal = (=)

let goal_p t =
  let b = t.over_weight <= 0. in
    if b then print_node t; b



let make_initial objects sack_size =
  { left_behind = Item_set_list.null_set;
    taking = objects;
    value = 0.;
    over_weight = (Item_set_list.get_total_weight objects.Item_set_list.items) *. (1. -. sack_size);
    sack_size = sack_size;}

let hd t =
  (h t), (d t)

let sol_length t =
  List.length t.left_behind.Item_set_list.items

(*************************** Search Interfaces *******************************)

let default_interface t limits =
  Search_interface.make
    ~h:h
    ~d:d
    ~hd:hd
    ~key:key
    ~key_print:string_of_int
    ~goal_p:goal_p
    ~halt_on:limits
    ~get_sol_length:sol_length
    ~equals:equal
    ~domain_expand:expand
    Search_interface.Rucksack
    t
    (fun _ _ -> false)
    (fun _ -> ())


(********************************** I\O **************************************)
let to_pairs t =
  (* Assumes t is inital node *)
  let t_weight,t_val = Item_set_list.get_total_pair t.taking.Item_set_list.items in
    [("value", string_of_float t_val);
     ("weight", string_of_float t_weight);
     ("sack size", string_of_float t.sack_size);
     ("item count", string_of_int (List.length t.taking.Item_set_list.items));]


let write_instance t ch =
  Datafile.write_header_pairs ch;
  Item_set_list.write_columns t.taking ch;
  Datafile.write_pairs ch (to_pairs t);
  Datafile.write_trailer_pairs ch


let read_instance file_name ch =
  let df = Datafile.read file_name ch in
(*  let total_value = float_of_string (Datafile.get_val df "total value")
    and total_weight = float_of_string (Datafile.get_val df "total weight")*)
  let sack_size = float_of_string (Datafile.get_val df "sack size")
  and remaining = Item_set_list.read_columns df in
    make_initial remaining sack_size


let make_random_fixed ?(seed = 314159) t_value t_weight count percent =
  make_initial (Item_set_list.make_random_fixed_totals
		  ~seed:seed (percent *. t_weight) t_weight t_value count)
    percent

let make_random_max ?(seed = 314159) t_value t_weight count percent =
  make_initial (Item_set_list.make_random_max_vals
		  ~seed:seed (percent *. t_weight) t_weight t_value count)
    percent

(* EOF *)
