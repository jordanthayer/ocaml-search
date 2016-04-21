(** A search-based solver for the 'Son of darts' problem.

    @author eaburns
    @since 2010-03-11
*)

type inst = {
  n_regions : int;
  n_darts : int;
}

type state = {
  (* A partial solution: a state in the search space. *)

  regions : int list;
  (* The current set of regions. *)

  assigned_regions : int;
  (* The number of assigned regions. *)

  min_unattainable : int;
  (* The minimum unattainable value in this state. *)

  attainable : Int_tree.t array;
  (* Values attainable by each dart. *)

  num_children : int;
  (* The number of children. *)

  mutable child_regions : int array;
  (* The ordered children region numbers. *)

  mutable child_cache : state Weak.t;
  (* Cache of children *)

  mutable child_costs : float list;
  (* Cost of each child. *)
}


let min_unattainable attainable =
  (** [min_unattainable attainable] gets the minimum unattainable
      value from the given array of dart attainabilities. *)
  let n = Array.length attainable in
    match Int_tree.min_range (attainable.(n - 1)) with
      | None -> invalid_arg "min_unattainable: nothing attainable"
      | Some (_, max) -> max + 1


let make_add_region inst =
  (** [make_add_region inst] makes a function that adds a region to
      the given state, recomputing tha attainable values. *)
  let n_darts = inst.n_darts in
  let latest = Array.make n_darts Int_tree.empty in
    (fun state r ->
       let attainable = state.attainable in
       let regions = state.regions in
       let regions' = r :: state.regions in
       let attainable' = Array.make n_darts Int_tree.empty in
	 Array.fill latest 0 (n_darts - 1) Int_tree.empty;
	 attainable'.(0) <- Int_tree.insert r attainable.(0);
	 latest.(0) <- Int_tree.insert r Int_tree.empty;
	 for d = 1 to n_darts - 1 do
	   let new_in_last_dart =
	     (* New regions attained by adding all old region values
		to all new regions attainable by the previous dart. *)
	     Int_tree.fold
	       (fun (min, max) ints ->
		  List.fold_left
		    (fun ints r ->
		       let range = min + r, max + r in
			 latest.(d) <- Int_tree.insert_range range latest.(d);
			 Int_tree.insert_range range ints)
		    ints regions)
	       latest.(d - 1) attainable.(d)
	   in
	   let r =
	     (* New attainable values by using region [r]. *)
	     Int_tree.fold
	       (fun (min, max) ints ->
		  let range = min + r, max + r in
		    latest.(d) <- Int_tree.insert_range range latest.(d);
		    Int_tree.insert_range range ints)
	       attainable'.(d - 1) new_in_last_dart
	   in attainable'.(d) <- r
	 done;
	 let min_unattainable = min_unattainable attainable' in
	   { regions = regions';
	     assigned_regions = state.assigned_regions + 1;
	     min_unattainable = min_unattainable;
	     attainable = attainable';
	     num_children = min_unattainable - r;
	     child_regions = [||];
	     child_cache = Weak.create 0;
	     child_costs = [];
	   })


let make_initial inst =
  (** [make_initial inst] makes the initial state for the given
      instance. *)
  let i =
    {
      regions = [0];
      assigned_regions = 0;
      min_unattainable = 0;
      attainable = Array.make inst.n_darts (Int_tree.insert 0 Int_tree.empty);
      num_children = 1;
      child_regions = [||];
      child_cache = Weak.create 0;
      child_costs = [];
    }
  in make_add_region inst i 1


let num_children s = s.num_children
  (** [num_children s] gets the number of children for the state
      [s]. *)


let populate_child_regions add_region state =
  (** [populate_child_regions add_region state] fills in the
      child_regions array. *)
  if (Array.length state.child_regions) <> state.num_children
  then begin
    let children =
      Array.init state.num_children (fun n ->
				       let r = state.min_unattainable - n in
					 add_region state r)
    in
      Array.sort (fun a b -> compare b.min_unattainable a.min_unattainable)
	children;
      let cache = Weak.create state.num_children in
	state.child_cache <- cache;
	let regions = (Array.mapi (fun i ch ->
				     Weak.set cache i (Some ch);
				     List.hd ch.regions) children) in
	  state.child_regions <- regions;
	  for i = (Array.length regions) - 1 downto 0 do
	    state.child_costs <-
	      ~-.(float children.(i).min_unattainable) :: state.child_costs
	  done;
  end


let make_nth_child add_region s n =
  (** [make_nth_child add_region s n] makes a function that gets the
      [n]th child of state [s]. *)
  populate_child_regions add_region s;
  match Weak.get s.child_cache n with
    | Some ch -> ch
    | None -> add_region s s.child_regions.(n)


let make_child_costs add_region n =
  (** [make_child_costs add_region n] makes a function that gets the
      children costs. *)
  if n.child_costs = [] then populate_child_regions add_region n;
  n.child_costs


let make_is_leaf inst s =
  (** [make_is_leaf inst s] makes a function that tests if [s] is a
      leaf node. *)
  s.assigned_regions = inst.n_regions


let is_better node saved =
  (** [is_better node saved] tests if a state is better than an
      incumbent solution. *)
  node.min_unattainable > saved.min_unattainable


let copy_state node = node
  (** [copy_state node] copies a state to make an incumbent
      solution. *)


let leaf_cost node = float node.min_unattainable


let max_depth inst = inst.n_regions
  (** [max_depth inst] gets the maximum depth of an instance. *)


let get_regions state =
  (** [get_regions state] gets the regions of the state as a string. *)
  let module P = Printf in
    List.fold_left
      (fun s r -> if r > 0 then P.sprintf "%d, %s" r s else s)
      (string_of_int (List.hd state.regions)) (List.tl state.regions)


let make_interface inst =
  (** [make_interface inst] makes the interface for bounded-depth
      search. *)
  let module I = Bounded_depth_interface in
  let add_region = make_add_region inst in
  let nth_child = make_nth_child add_region in
  let is_leaf = make_is_leaf inst in
    { (I.default num_children nth_child is_leaf is_better copy_state)
      with
	I.max_depth = max_depth inst;
	I.child_costs = make_child_costs add_region;
	I.leaf_cost = leaf_cost;
    }

