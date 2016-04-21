(**

   Sliding Tiles Interface where one or more of the tiles have been
   glued in place.

   Christopher Wilt

   January 22, 2011

*)

module ST = Sliding_tiles
module STI = Sliding_tiles_inst
module STH = Sliding_heuristics

let default_interface cost_name inst limit =
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
      ~domain_expand:(Glued_functions.domain_expand 
			inst tile_cost get set inc_h inc_d)
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


