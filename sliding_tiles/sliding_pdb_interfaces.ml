(**

    @author jtd7
    @since 2010-09-08

   Interface calls to the sliding tiles problem *)

module ST = Sliding_tiles
module STI = Sliding_tiles_inst
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
	"unit" -> failwith "Using Wrong interface file"
      | _ -> (fun p -> let ar = STPDB.pdb_from_path p in
	      ar, Array.length ar) in
      List.iter (Verb.pe Verb.debug "Pdb @ %s\n") paths;
      List.map lfun paths


let single_pdb_iface pattern pdb sz cost moves inst limit =
  let info = Packed_ints.info inst.STI.nelms in
  let create = Packed_ints.make_create info in
  let get = Packed_ints.make_get info in
  let set = Packed_ints.make_set info in
  let tile_cost = ST.Tile_cost.by_name cost in
  let pdb_key = STPDB.make_pdb_key pattern inst
  and blank_pos = ST.make_blank_position_type inst
  and inc_h _ _ _ = 0.
  and inc_d _ _ _ = 0 in
  let h = STPDB.make_h pdb pdb_key
  and d = STPDB.make_d pdb pdb_key
  and hd = STPDB.make_hd pdb pdb_key
  and root = ST.make_initial inst create set (fun _ -> 0.) (fun _ -> 0) in
  let domain_expand = match moves with
    | "standard" -> ST.domain_expand inst tile_cost get set inc_h inc_d
    | "macro" -> MST.domain_expand inst get set inc_h inc_d
    | _ -> failwith "Moves not recognized" in
    (Search_interface.make
       ~h
       ~d
       ~hd
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


let max_pdb_iface patterns pdbs sz cost moves inst limit =
  let info = Packed_ints.info inst.STI.nelms in
  let create = Packed_ints.make_create info in
  let get = Packed_ints.make_get info in
  let set = Packed_ints.make_set info in
  let tile_cost = ST.Tile_cost.by_name cost in
  let blank_pos = ST.make_blank_position_type inst in
  let keys = (List.map (fun p -> Sliding_tiles_pdb.make_pdb_key p inst)
		patterns) in
  let h_funs = List.map2 STPDB.make_h pdbs keys
  and d_funs = List.map2 STPDB.make_d pdbs keys
  and hd_funs= List.map2 STPDB.make_hd pdbs keys
  and inc_h _ _ _ = 0.
  and inc_d _ _ _ = 0 in
  let h = (fun n -> List.fold_left (fun accum h ->
				      Math.fmax accum (h n)) 0. h_funs)
  and d = (fun n -> List.fold_left (fun accum d ->
				      Math.fmax accum (d n)) 0. d_funs)
  and hd = (fun n -> List.fold_left
	      (fun (accum_h,accum_d) hd ->
		 let h,d = hd n in
		   Math.fmax accum_h h, Math.fmax accum_d d)(0.,0.) hd_funs)
  and root = ST.make_initial inst create set (fun _ -> 0.) (fun _ -> 0) in
  let domain_expand = match moves with
    | "standard" -> ST.domain_expand inst tile_cost get set inc_h inc_d
    | "macro" -> MST.domain_expand inst get set inc_h inc_d
    | _ -> failwith "Moves not recognized" in
    (Search_interface.make
       ~h
       ~d
       ~hd
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


let sum_pdb_iface patterns pdbs sz cost moves inst limit =
  let info = Packed_ints.info inst.STI.nelms in
  let create = Packed_ints.make_create info in
  let get = Packed_ints.make_get info in
  let set = Packed_ints.make_set info in
  let tile_cost = ST.Tile_cost.by_name cost in
  let blank_pos = ST.make_blank_position_type inst in
  let keys = (List.map (fun p -> Sliding_tiles_pdb.make_pdb_key p inst)
		patterns) in
  let h_funs = List.map2 STPDB.make_h pdbs keys
  and d_funs = List.map2 STPDB.make_d pdbs keys
  and hd_funs= List.map2 STPDB.make_hd pdbs keys
  and inc_h _ _ _ = 0.
  and inc_d _ _ _ = 0 in
  let h = (fun n -> List.fold_left (fun accum h ->
				      accum +. (h n)) 0. h_funs)
  and d = (fun n -> List.fold_left (fun accum d ->
				      accum +. (d n)) 0. d_funs)
  and hd = (fun n -> List.fold_left (fun (accum_h,accum_d) hd ->
				       let h,d = hd n in
					  accum_h +. h, accum_d +. d)
	      (0.,0.) hd_funs)
  and root = ST.make_initial inst create set (fun _ -> 0.) (fun _ -> 0) in
  let domain_expand = match moves with
    | "standard" -> ST.domain_expand inst tile_cost get set inc_h inc_d
    | "macro" -> MST.domain_expand inst get set inc_h inc_d
    | _ -> failwith "Moves not recognized" in
    (Search_interface.make
       ~h
       ~d
       ~hd
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


let prototype_iface = Sliding_inadmiss_interfaces.sum_pdb_iface


let get_pdb_iface add cost moves pat problem =
  match cost with
    | "unit" -> (Sliding_unit_pdb_interfaces.get_pdb_iface
		   add cost moves pat problem)
    | _ ->
	let sum = String.contains pat '+'
	and max = String.contains pat '-'
	and prot = String.contains pat 'p' in
	  if sum & max then failwith "Cannot sum and max, pick one";
	  let rx = (if sum then Str.regexp "+" else
		      (if max then Str.regexp "-" else Str.regexp "p")) in
	  let pat = if sum || max || prot then Str.split rx pat else [pat] in
	  let pdb_pairs = load_pdb ~add:add cost moves problem pat in
	    match pdb_pairs with
	      | [(pattern,pdb),sz] ->
		  single_pdb_iface pattern pdb sz cost moves problem
	      | _ -> (let (patternsxpdbs),szs = (List.split pdb_pairs) in
		      let patterns,pdbs = List.split patternsxpdbs in
			if max
			then max_pdb_iface patterns pdbs szs cost moves problem
			else
			  (if sum
			   then (if add
				 then (sum_pdb_iface
					 patterns pdbs szs cost moves problem)
				 else (Sliding_inadmiss_interfaces.sum_pdb_iface
					 patterns pdbs szs cost moves problem))
			   else (prototype_iface
				   patterns pdbs szs cost moves problem)))


(* EOF *)
