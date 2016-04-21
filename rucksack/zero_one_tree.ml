(** The encodings we had before were still awkward.
    You can represent the problem as a binary tree of non-fixed depth
    one branch represents keeping an item, the other branch represents
    not keeping the item.  Most value-dense objects sit up front. *)


type t =
    { left_behind : bool array;
      over_weight : float;
      considering : int;}


let print_node t =
  let at_verb = Verb.debug in
  let print_ids fn =
    Array.iteri (fun i e -> if (fn e) then Verb.pe at_verb " %i" i) in
    Verb.pe at_verb "Left_Behind:";
    (print_ids (fun a -> a)) t.left_behind;
    Verb.pe at_verb "\nTaking: ";
    (print_ids (fun a -> not a)) t.left_behind;
    Verb.pe at_verb "\nOver: %f\n" t.over_weight

let key t =
  let (_,k) =
  Array.fold_left
    (fun (i,accum) e ->
       if e then (i+1, accum + Math.int_exp 2 i)
       else (i+1, accum)) (0,0) t.left_behind in
    t.considering, k


let print_key (a,b) =
  Verb.pe Verb.always "%i,%i" a b

let key_to_string (a,b) =
  Wrutils.str "%i,%i" a b

let equal (a,b) (c,d) =
  a = c && b = d

let goal_p t =
  t.over_weight <= 0.

let make_initial objects sack_size =
    (** Sack size is a percentage of the total weight *)
    {left_behind = Array.create (Array.length objects) false;
     over_weight = (Item_set_array.get_total_weight objects) *. (1. -. sack_size);
     considering = 0;}

let sol_length t =
  print_node t;
  Array.fold_left (fun accum element -> if element then accum else accum + 1)
    0 t.left_behind


let h items t =
  let rec fn ind items remaining =
    if ind < t.considering then 0.
    else (if items.(ind).Item_set_array.weight > remaining
	  then ((Item_set_array.value_density items.(ind)) *. remaining)
	  else items.(ind).Item_set_array.value +.
	    fn (ind - 1) items (remaining -. (items.(ind).Item_set_array.weight))) in
    if t.over_weight > 0.
    then
      (fn ((Array.length items) - 1) items t.over_weight)
    else 0.


let d items t =
  let rec fn ind items remaining =
    if ind < t.considering then 0.
    else (if items.(ind).Item_set_array.weight > remaining
	  then 1.
	  else 1. +.
	    fn (ind - 1) items (remaining -. (items.(ind).Item_set_array.weight))) in
    if t.over_weight > 0.
    then
      (fn ((Array.length items) - 1) items t.over_weight)
    else 0.

let shortest_d items t =
  let max_leng = Array.length items in
  let rec fn ind items remaining =
      if ind >= max_leng
      then 0.
      else (if items.(ind).Item_set_array.weight > remaining
	    then 1.
	    else 1. +.
	      (fn (ind + 1) items
		 (remaining -. (items.(ind).Item_set_array.weight)))) in
    if t.over_weight > 0.
    then (fn t.considering items t.over_weight)
    else 0.



let hd_cheap items t =
  let rec fn ind items remaining =
    if ind < t.considering then (0., 0.)
    else (if items.(ind).Item_set_array.weight > remaining
	  then (((Item_set_array.value_density items.(ind)) *. remaining), 1.)
	  else
	    (let h,d =
	       fn (ind - 1) items (remaining -. (items.(ind).Item_set_array.weight)) in
	       items.(ind).Item_set_array.value +. h, d +. 1.)) in
  let h,d =
    if t.over_weight > 0.
    then
      (fn ((Array.length items) - 1) items t.over_weight)
    else (0.,0.) in
    Verb.pe Verb.debug "%f\t%f\n" h d;
    h,d

let hd_shortest items t = h items t, shortest_d items t

let hd = hd_shortest

let expand items t t_g =
  assert (t.over_weight > 0.);
  let length = Array.length items in
  let keep = Array.copy t.left_behind
  and leave = Array.copy t.left_behind
  and next_ind = t.considering + 1 in
    (*keep.(t.considering) <- false;*)
    leave.(t.considering) <- true;
    List.filter
      (fun (state,_) ->
	 let remaining_items = Array.sub items (state.considering)
	   (length - state.considering) in
	   (Item_set_array.get_total_weight remaining_items) >= state.over_weight)
      [({left_behind = keep;
	 over_weight = t.over_weight;
	 considering = next_ind;}, t_g);
       ({left_behind = leave;
	 over_weight = t.over_weight -.
	    items.(t.considering).Item_set_array.weight;
	 considering = next_ind;},
	t_g +. items.(t.considering).Item_set_array.value)]

(*************************** Search Interfaces *******************************)

let default_interface (t,items) limits =
  Search_interface.make
    ~h:(h items)
    ~d:(d items)
    ~hd:(hd items)
    ~rev_hd:(fun _ -> 0.,0.)
    ~t:(fun _ -> 0)
    ~key:key
    ~key_print:key_to_string
    ~goal_p:goal_p
    ~halt_on:limits
    ~get_sol_length:sol_length
    ~equals:equal
    ~domain_expand:(expand items)
    Search_interface.Rucksack
    t
    (fun _ _ -> false)
    (fun _ -> ())



let to_pairs t items =
  (* Assumes t is inital node *)
  let t_weight,t_val = Item_set_array.get_total_pair items in
    [("value", string_of_float t_val);
     ("weight", string_of_float t_weight);
     ("item count", string_of_int (Array.length items));]


let write_instance t items ch =
  Datafile.write_header_pairs ch;
  Item_set_array.write_columns items ch;
  Datafile.write_pairs ch (to_pairs t items);
  Datafile.write_trailer_pairs ch


let read_instance file_name ch =
  let df = Datafile.read file_name ch in
  let sack_size = float_of_string (Datafile.get_val df "sack size")
  and remaining = Item_set_array.read_columns df in
  let vd = Item_set_array.value_density in
    Array.sort (fun a b ->
		  let vd_a = (vd a)
		  and vd_b = (vd b) in
		    if vd_a > vd_b  then -1 else if vd_a < vd_b then 1 else 0)
      remaining;
    Verb.pe Verb.debug "Make_initial: ";
    Array.iter (fun e -> Verb.pe Verb.debug "%i,%f\t"
		  e.Item_set_array.id (Item_set_array.value_density e))
      remaining;
    Verb.pe Verb.debug "\n%!";
    (make_initial remaining sack_size), remaining


let make_random_fixed ?(seed = 314159) t_value t_weight count percent =
  make_initial (Item_set_array.make_random_fixed_totals
		  ~seed:seed (percent *. t_weight) t_weight t_value count)
    percent

let make_random_max ?(seed = 314159) t_value t_weight count percent =
  make_initial (Item_set_array.make_random_max_vals
		  ~seed:seed (percent *. t_weight) t_weight t_value count)
    percent

(* EOF *)
