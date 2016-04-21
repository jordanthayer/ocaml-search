(**

    @author jtd7
    @since 2010-09-13
    A collection of interfaces to the sliding tiles problem
*)

module ST = Sliding_tiles
module STI = Sliding_tiles_inst
module STH = Sliding_heuristics

let default_interface ?(cost_ratio = 0) cost_name inst limit =
  (** [default_interface cost_name inst limit] makes the default
      search interface. *)
  let info = Packed_ints.info inst.STI.nelms in
  let create = Packed_ints.make_create info in
  let get = Packed_ints.make_get info in
  let set = Packed_ints.make_set info in
  let tile_cost =
    match cost_ratio with
	0 -> ST.Tile_cost.by_name cost_name
      | n -> ST.Tile_cost.by_cost cost_ratio
  in
  let mt = STH.Manhattan_distance.table tile_cost inst in
  let mt_unit = STH.Manhattan_distance.table ST.Tile_cost.unit inst in
  let inc_h = STH.Manhattan_distance.make_incremental_h inst mt in
  let inc_d = STH.Manhattan_distance.make_incremental_d inst mt_unit in
  let blank_pos = ST.make_blank_position_type inst
  and root = (ST.make_initial inst create set
		(STH.Manhattan_distance.from_scratch inst mt get)
		(fun c -> truncate
		   (STH.Manhattan_distance.from_scratch inst mt_unit get c)))
  and rev_inst = STI.reverse_inst inst in
  let mt_rev = STH.Manhattan_distance.table tile_cost rev_inst in
  let mt_rev_unit = STH.Manhattan_distance.table ST.Tile_cost.unit rev_inst in
  let rev_h_base = STH.Manhattan_distance.from_scratch rev_inst mt_rev get
  and rev_d_base = (STH.Manhattan_distance.from_scratch
		      rev_inst mt_rev_unit get) in
    Search_interface.make
      ~h:(fun s -> s.ST.h)
      ~d:(fun s -> float s.ST.d)
      ~hd:(fun s -> s.ST.h, float s.ST.d)
      ~rev_h:(fun s -> rev_h_base s.ST.contents)
      ~rev_d:(fun s -> rev_d_base s.ST.contents)
      ~rev_hd:(fun s -> rev_h_base s.ST.contents, rev_d_base s.ST.contents)
      ~domain_expand:(ST.domain_expand inst tile_cost get set inc_h inc_d)
      ~t:(fun n -> blank_pos n.ST.parent.ST.blank n.ST.blank)
      ~key:ST.Key.of_state
      ~key_print:(ST.Key.make_to_string inst get)
      ~equals:(ST.Key.equals)
      ~hash:(ST.Key.make_hash inst)
      ~goal_p:(ST.make_is_goal inst get)
      ~halt_on:limit
      ~p_update:(fun n p -> n.ST.parent <- p)
      ~predecessor:(ST.predecessor inst tile_cost get set inc_h inc_d)
      (match cost_name with
	 | "inverse" -> Search_interface.Inv_tiles
	 | _ -> Search_interface.Tiles)
      root
      (fun _ _ -> false)
      (fun _ -> ())


let linear_conflicts_interface cost_name inst limit =
  (** [default_interface cost_name inst limit] makes the default
      search interface. *)
  let info = Packed_ints.info inst.STI.nelms in
  let create = Packed_ints.make_create info in
  let get = Packed_ints.make_get info in
  let set = Packed_ints.make_set info in
  let tile_cost = ST.Tile_cost.by_name cost_name in
  let mt = STH.Manhattan_distance.table tile_cost inst
  and mt_unit = STH.Manhattan_distance.table ST.Tile_cost.unit inst in
  let minc_h = STH.Manhattan_distance.make_incremental_h inst mt
  and minc_d = STH.Manhattan_distance.make_incremental_d inst mt_unit in
  let lc_unit = STH.Linear_conflicts.from_scratch (fun _ _ -> 1.) inst
  and lc_cost = STH.Linear_conflicts.from_scratch tile_cost inst in
  let blank_pos = ST.make_blank_position_type inst
  and root = (ST.make_initial inst create set
		(fun n ->
		   (STH.Manhattan_distance.from_scratch inst mt get n) +.
		     (lc_cost n))
		(fun c ->
		   (truncate
		      ((STH.Manhattan_distance.from_scratch
			  inst mt_unit get c) +. (lc_unit c)))))
  and rev_inst = STI.reverse_inst inst in
  let mt_rev = STH.Manhattan_distance.table tile_cost rev_inst in
  let mt_rev_unit = STH.Manhattan_distance.table ST.Tile_cost.unit rev_inst in
  let rev_h_base = STH.Manhattan_distance.from_scratch rev_inst mt_rev get
  and rev_d_base = (STH.Manhattan_distance.from_scratch
		      rev_inst mt_rev_unit get) in
    Search_interface.make
      ~h:(fun s -> s.ST.h +. (lc_cost s.ST.contents))
      ~d:(fun s -> (float s.ST.d) +. (lc_unit s.ST.contents))
      ~hd:(fun s -> s.ST.h +. (lc_cost s.ST.contents),
	     (float s.ST.d) +. (lc_unit s.ST.contents))
      ~rev_h:(fun s -> rev_h_base s.ST.contents)
      ~rev_d:(fun s -> rev_d_base s.ST.contents)
      ~rev_hd:(fun s -> rev_h_base s.ST.contents, rev_d_base s.ST.contents)
      ~domain_expand:(ST.domain_expand inst tile_cost get set
			minc_h minc_d)
      ~t:(fun n -> blank_pos n.ST.parent.ST.blank n.ST.blank)
      ~key:ST.Key.of_state
      ~key_print:(ST.Key.make_to_string inst get)
      ~equals:(ST.Key.equals)
      ~hash:(ST.Key.make_hash inst)
      ~goal_p:(ST.make_is_goal inst get)
      ~halt_on:limit
      ~p_update:(fun n p -> n.ST.parent <- p)
      Search_interface.Tiles
      root
      (fun _ _ -> false)
      (fun _ -> ())



let misplaced cost_name inst limit =
  (** [default_interface cost_name inst limit] makes the default
      search interface. *)
  let info = Packed_ints.info inst.STI.nelms in
  let create = Packed_ints.make_create info in
  let get = Packed_ints.make_get info in
  let set = Packed_ints.make_set info in
  let tile_cost = ST.Tile_cost.by_name cost_name in
  let mt = STH.Misplaced_tiles.table tile_cost inst in
  let mt_unit = STH.Misplaced_tiles.table ST.Tile_cost.unit inst in
  let inc_h = STH.Misplaced_tiles.make_incremental_h inst mt in
  let inc_d = STH.Misplaced_tiles.make_incremental_d inst mt_unit in
  let blank_pos = ST.make_blank_position_type inst
  and root = (ST.make_initial inst create set
		(STH.Misplaced_tiles.from_scratch tile_cost inst get)
		(fun c -> truncate
		   (STH.Misplaced_tiles.from_scratch (fun _ _ -> 1.)
		      inst get c))) in
    Search_interface.make
      ~h:(fun s -> s.ST.h)
      ~d:(fun s -> float s.ST.d)
      ~hd:(fun s -> s.ST.h, float s.ST.d)
      ~domain_expand:(ST.domain_expand inst tile_cost get set inc_h inc_d)
      ~t:(fun n -> blank_pos n.ST.parent.ST.blank n.ST.blank)
      ~key:ST.Key.of_state
      ~key_print:(ST.Key.make_to_string inst get)
      ~equals:(ST.Key.equals)
      ~hash:(ST.Key.make_hash inst)
      ~goal_p:(ST.make_is_goal inst get)
      ~halt_on:limit
      ~p_update:(fun n p -> n.ST.parent <- p)
      Search_interface.Tiles
      root
      (fun _ _ -> false)
      (fun _ -> ())


let reverse_interface cost_name inst limit =
  default_interface cost_name (STI.reverse_inst inst) limit


let recording_interface cost_name inst limit =
  (** [default_interface cost_name inst limit] makes the default
      search interface. *)
  let info = Packed_ints.info inst.STI.nelms in
  let create = Packed_ints.make_create info in
  let get = Packed_ints.make_get info in
  let set = Packed_ints.make_set info in
  let tile_cost = ST.Tile_cost.by_name cost_name in
  let mt = STH.Manhattan_distance.table tile_cost inst in
  let mt_unit = STH.Manhattan_distance.table ST.Tile_cost.unit inst in
  let inc_h = STH.Manhattan_distance.make_incremental_h inst mt in
  let inc_d = STH.Manhattan_distance.make_incremental_d inst mt_unit in
  let blank_pos = ST.make_blank_position_type inst
  and root = (ST.make_initial inst create set
		(STH.Manhattan_distance.from_scratch inst mt get)
		(fun c -> truncate
		   (STH.Manhattan_distance.from_scratch inst mt_unit get c)))
  and rev_inst = STI.reverse_inst inst in
  let rev_h_base = STH.Manhattan_distance.from_scratch rev_inst mt get
  and rev_d_base = STH.Manhattan_distance.from_scratch rev_inst mt_unit get in
    Search_interface.make
      ~h:(fun s -> s.ST.h)
      ~d:(fun s -> float s.ST.d)
      ~hd:(fun s -> s.ST.h, float s.ST.d)
      ~rev_h:(fun s -> rev_h_base s.ST.contents)
      ~rev_d:(fun s -> rev_d_base s.ST.contents)
      ~rev_hd:(fun s -> rev_h_base s.ST.contents, rev_d_base s.ST.contents)
      ~domain_expand:(ST.domain_expand inst tile_cost get set inc_h inc_d)
      ~t:(fun n -> blank_pos n.ST.parent.ST.blank n.ST.blank)
      ~key:ST.Key.of_state
      ~key_print:(ST.Key.make_dump_key inst get)
      ~equals:(ST.Key.equals)
      ~hash:(ST.Key.make_hash inst)
      ~goal_p:(ST.make_is_goal inst get)
      ~halt_on:limit
      ~p_update:(fun n p -> n.ST.parent <- p)
      Search_interface.Tiles
      root
      (fun _ _ -> false)
      (fun _ -> ())


let exhaustive_recording_interface cost_name inst limit =
  (** [default_interface cost_name inst limit] makes the default
      search interface. *)
  let info = Packed_ints.info inst.STI.nelms in
  let create = Packed_ints.make_create info in
  let get = Packed_ints.make_get info in
  let set = Packed_ints.make_set info in
  let tile_cost = ST.Tile_cost.by_name cost_name in
  let mt = STH.Manhattan_distance.table tile_cost inst in
  let mt_unit = STH.Manhattan_distance.table ST.Tile_cost.unit inst in
  let inc_h = STH.Manhattan_distance.make_incremental_h inst mt in
  let inc_d = STH.Manhattan_distance.make_incremental_d inst mt_unit in
  let blank_pos = ST.make_blank_position_type inst
  and root = (ST.make_initial inst create set
		(STH.Manhattan_distance.from_scratch inst mt get)
		(fun c -> truncate
		   (STH.Manhattan_distance.from_scratch inst mt_unit get c)))
  and rev_inst = STI.reverse_inst inst in
  let rev_h_base = STH.Manhattan_distance.from_scratch rev_inst mt get
  and rev_d_base = STH.Manhattan_distance.from_scratch rev_inst mt_unit get in
    Search_interface.make
      ~h:(fun s -> s.ST.h)
      ~d:(fun s -> float s.ST.d)
      ~hd:(fun s -> s.ST.h, float s.ST.d)
      ~rev_h:(fun s -> rev_h_base s.ST.contents)
      ~rev_d:(fun s -> rev_d_base s.ST.contents)
      ~rev_hd:(fun s -> rev_h_base s.ST.contents, rev_d_base s.ST.contents)
      ~domain_expand:(ST.domain_expand inst tile_cost get set inc_h inc_d)
      ~t:(fun n -> blank_pos n.ST.parent.ST.blank n.ST.blank)
      ~key:ST.Key.of_state
      ~key_print:(ST.Key.make_dump_key inst get)
      ~equals:(ST.Key.equals)
      ~hash:(ST.Key.make_hash inst)
      ~goal_p:(fun _ -> false)
      ~halt_on:limit
      ~p_update:(fun n p -> n.ST.parent <- p)
      Search_interface.Tiles
      root
      (fun _ _ -> false)
      (fun _ -> ())


let record_truth_interface cost_name inst limit =
  let o_inst = inst in
  let inst = STI.reverse_inst inst in
  let info = Packed_ints.info inst.STI.nelms in
  let create = Packed_ints.make_create info in
  let get = Packed_ints.make_get info in
  let set = Packed_ints.make_set info in
  let tile_cost = ST.Tile_cost.by_name cost_name in
  let mt = STH.Manhattan_distance.table tile_cost inst in
  let mt_unit = STH.Manhattan_distance.table ST.Tile_cost.unit inst in
  let inc_h = STH.Manhattan_distance.make_incremental_h inst mt in
  let inc_d = STH.Manhattan_distance.make_incremental_d inst mt_unit in
  let blank_pos = ST.make_blank_position_type inst
  and root = (ST.make_initial inst create set
		(STH.Manhattan_distance.from_scratch inst mt get)
		(fun c -> truncate
		   (STH.Manhattan_distance.from_scratch inst mt_unit get c))) in
  let rmt = STH.Manhattan_distance.table tile_cost o_inst in
  let rmt_unit = STH.Manhattan_distance.table ST.Tile_cost.unit o_inst in
  let rev_h_base = STH.Manhattan_distance.from_scratch o_inst rmt get
  and rev_d_base = STH.Manhattan_distance.from_scratch o_inst rmt_unit get in
    Search_interface.make
      ~rev_h:(fun s -> 0.)
      ~rev_d:(fun s -> 0.)
      ~rev_hd:(fun s -> 0.,0.)
      ~h:(fun s -> rev_h_base s.ST.contents)
      ~d:(fun s -> rev_d_base s.ST.contents)
      ~hd:(fun s -> rev_h_base s.ST.contents, rev_d_base s.ST.contents)
      ~domain_expand:(ST.domain_expand inst tile_cost get set inc_h inc_d)
      ~t:(fun n -> blank_pos n.ST.parent.ST.blank n.ST.blank)
      ~key:ST.Key.of_state
      ~key_print:(ST.Key.make_dump_key inst get)
      ~equals:(ST.Key.equals)
      ~hash:(ST.Key.make_hash inst)
      ~goal_p:(fun _ -> false)
      ~halt_on:limit
      ~p_update:(fun n p -> n.ST.parent <- p)
      Search_interface.Tiles
      root
      (fun _ _ -> false)
      (fun _ -> ())



(* EOF *)
