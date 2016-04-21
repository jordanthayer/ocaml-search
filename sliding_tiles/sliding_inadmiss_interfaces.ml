(**

    @author jtd7
    @since 2010-09-13

   A few inadmissible heuristic interfaces for the sliding tiles problem.
   At current, the search code assumes h & d are inadmissible and that hd
   contains the admissible information.  Eventually we will want to clean this
   up, but right now I'm worried about getting results faster.
*)

module ST = Sliding_tiles
module STI = Sliding_tiles_inst
module STH = Sliding_heuristics
module STPDB = Sliding_tiles_pdb
module STUPDB = Sliding_unit_pdb
module MST = Macro_sliding_tiles

let load_pdb ?(add = false) cost moves problem patterns =
  Verb.pe Verb.debug "Loading pdb\n";
  let instance_root = User_paths.instance_root ^ "tiles_instances"
  and attrs = List.map (fun p ->
			  Verb.pe Verb.debug "pattern: |%s|\n" p;
			  ["model", "pdb";
			   "rows", string_of_int problem.STI.nrows;
			   "cols", string_of_int problem.STI.ncols;
			   "cost", cost;
			   "move", moves;
			   "additive", string_of_bool add;
			   "pattern", p;]) patterns in
  Verb.pe Verb.debug "Built attrs sets\n";
    let paths = List.map (Rdb.path_for instance_root) attrs in
    let lfun = match cost with
      |	"unit" -> STUPDB.pdb_from_path
      | _ -> failwith "Using Wrong Interface File" in
      List.iter (Verb.pe Verb.debug "Pdb @ %s\n") paths;
      List.map lfun paths

let sum_pdb_iface patterns pdbs szs cost moves inst limit =
  let info = Packed_ints.info inst.STI.nelms in
  let create = Packed_ints.make_create info in
  let get = Packed_ints.make_get info in
  let set = Packed_ints.make_set info in
  let tile_cost = ST.Tile_cost.by_name cost in
  let mt = STH.Manhattan_distance.table tile_cost inst in
  let mt_unit = STH.Manhattan_distance.table ST.Tile_cost.unit inst in
  let inc_h = STH.Manhattan_distance.make_incremental_h inst mt in
  let inc_d = STH.Manhattan_distance.make_incremental_d inst mt_unit in
  let blank_pos = ST.make_blank_position_type inst in
  let keys = (List.map (fun p -> STPDB.make_pdb_key p inst) patterns) in
  let h_funs = List.map2 STUPDB.make_h pdbs (List.combine keys szs) in
  let h = (fun n -> List.fold_left (fun accum h ->
				      accum +. (h n)) 0. h_funs) in
  let d = h
  and root = (ST.make_initial inst create set
		(STH.Manhattan_distance.from_scratch inst mt get)
		(fun c -> truncate
		   (STH.Manhattan_distance.from_scratch inst mt get c))) in
  let domain_expand = match moves with
    | "standard" -> ST.domain_expand inst tile_cost get set inc_h inc_d
    | "macro" -> MST.domain_expand inst get set inc_h inc_d
    | _ -> failwith "Moves not recognized" in
    (Search_interface.make
       ~h
       ~d
       ~hd:(fun s -> s.ST.h, float_of_int s.ST.d)
       ~domain_expand
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
       (fun _ -> ()))


let mult_lc_iface ?(w = 3.) cost_name inst limit =
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
      ~h:(fun s -> s.ST.h +. w *. (lc_cost s.ST.contents))
      ~d:(fun s -> (float s.ST.d) +.  w *. (lc_unit s.ST.contents))
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


(* EOF *)
