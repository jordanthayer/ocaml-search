(** A bounded depth *knapsack* (not rucksack :) ) solver.

    @author eaburns
    @since 2011-01-10
*)


type t = {
  allowance : float;
  total_vl : float;
  nitems : int;
  sorted : int array;
  (** Ids of the items sorted in decreasing order of value density. *)
  weights : float array;
  values : float array;
  densities : float array;
}

(************************************************************)
(** {1 Instances} *)

(** Reads the instance from the given channel.

    @param f is the filename used to open the channel.  It is just
    used for outputting error messages and may be a bogus name if the
    channel is not backed by a file.

    {b Note} The items are read in in reverse order of ID so
    everything needs tobe reversed.
*)
let read f ch =
  let df = Datafile.read f ch in
  let nitems = int_of_string (Datafile.get_val df "item count") in
  let sack_size = float_of_string (Datafile.get_val df "sack size") in
  let total_wt = float_of_string (Datafile.get_val df "weight") in
  let allowance = sack_size *. total_wt in
  let ids = Array.map truncate (Datafile.get_col df "id") in
    Wrarray.rev ids;
    Array.iteri (fun i id -> assert (i = id)) ids;
    let values = Datafile.get_col df "value" in
    let total_vl = Array.fold_left ( +. ) 0. values in
    let weights = Datafile.get_col df "weight" in
    let densities = Wrarray.map2 (fun v w -> v /. w) values weights in
    let sorted = Array.copy ids in
      Wrarray.rev values;
      Wrarray.rev weights;
      Wrarray.rev densities;
      Array.sort
	(fun a b -> ~- (Math.fcompare densities.(a) densities.(b)))
	sorted;
      { allowance = allowance;
	total_vl = total_vl;
	nitems = nitems;
	sorted = sorted;
	weights = weights;
	values = values;
	densities = densities; }


(** Loads an instance from the given file. *)
let load f =
  Wrio.with_infile f (read f)


(************************************************************)
(** {1 Search} *)

type inclusion = Take | Leave

type node = {
  items : inclusion list;
  left : int;
  (** Number of remaining items. *)
  wt : float;
  vl : float;
  cost : float;
}


(** Gets the next item and whether or not it fits. *)
let make_next_item inst =
  let sorted = inst.sorted in
  let weights = inst.weights in
  let allowance = inst.allowance in
    (fun node ->
       let item_num = sorted.(inst.nitems - node.left) in
       let weight = weights.(item_num) in
	 item_num, (node.wt +. weight <= allowance))


(** Makes a function that propogates, leaving out items that don't fit
    in the sack until it gets to an item that does fit and must branch. *)
let make_propogate inst =
  let next = make_next_item inst in
  let values = inst.values in
  let rec prop node =
    if node.left = 0 then
      node
    else
      let item_num, fits = next node in
	if fits then
	  node
	else
	  prop { node with
		   items = Leave :: node.items;
		   left = node.left - 1;
		   cost = node.cost +. values.(item_num); }
  in prop


(** Gets the number of the next fitting item, the result is ~-1 if
    there is no next fit. *)
let make_next_fit inst =
  let next = make_next_item inst in
  let weights = inst.weights in
  let nitems = inst.nitems in
  let allowance = inst.allowance in
  let rec next_fit wt i =
    if i < nitems then
      if weights.(i) +. wt <= allowance then
	i
      else
	next_fit wt (i + 1)
    else
      ~-1
  in
    (fun node ->
       let item_num, _ = next node in
	 next_fit node.wt item_num)


(** Make the initial search node. *)
let make_init inst =
  make_propogate inst { items = [];
			left = inst.nitems;
			wt = 0.;
			vl = 0.;
			cost = 0.; }


(** Makes a function that tests if the current node is a leaf. *)
let make_is_leaf inst node =
  node.left = 0


(** There are always two children if this is not a leaf. *)
let make_num_children inst =
  let is_leaf = make_is_leaf inst in
    (fun n ->
       assert (not (is_leaf n));
       2)


(** Makes a function that gets the nth child of a node. *)
let make_nth_child inst =
  let weights = inst.weights in
  let values = inst.values in
  let propogate = make_propogate inst in
  let next = make_next_item inst in
    (fun node n ->
       let item_num, fit = next node in
	 (* At this point, the next item must fit. *)
	 assert (fit);
	 if n = 0 then
	   propogate { node with
			 items = Take :: node.items;
			 wt = node.wt +. weights.(item_num);
			 vl = node.vl +. values.(item_num);
			 left = node.left - 1; }
	 else if n = 1 then
	   propogate { node with
			 items = Leave :: node.items;
			 left = node.left - 1;
			 cost = node.cost +. values.(item_num); }
	 else
	   invalid_arg "nth_child called with invalid n")


(** Tests if the node is better than an incumbent. *)
let is_better node incumbent =
  node.cost < incumbent.cost


(** Copy a node into an incumbent.  An incumbent is represented using
    the same structure as a node: do nothing. *)
let copy_state node =
  node


(** Gets the maximum depth for an instance. *)
let max_depth inst =
  inst.nitems


(** Gets the maximum number of children for an instance. *)
let max_children _ =
  2


(** Tests if a node should be pruned.  Cannot decide to prune
    early. *)
let should_prune incumbent node =
  false


(** Tests if an incumbent is the optimal incumbent.  In this domain,
    we don't know if it is optimal until the tree is exhausted. *)
let is_optimal _ =
  false


(** Gets the cost of a leaf. *)
let leaf_cost node =
  node.cost


(** Makes a function that gets the cost of the children of a node. *)
let make_child_costs inst =
  let next_fit = make_next_fit inst in
  let next = make_next_item inst in
  let densities = inst.densities in
    (fun node ->
       let item_num, fit = next node in
	 (* The next item must fit because of the way that nodes are
	    generated. *)
	 assert (fit);
	 let next_item = next_fit node in
	   if next_item < 0 then
	     (* if there is not another item that fits, then we
		certainly don't want to leave this item out. *)
	     [ 0.; infinity ]
	   else
	     [ 0.; densities.(item_num) -. densities.(next_item) ])
