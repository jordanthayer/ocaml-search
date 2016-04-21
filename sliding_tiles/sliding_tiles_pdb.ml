(**

    @author jtd7
    @since 2010-09-08

    Calculating, Writing, Loading Pattern Databases for Sliding Tiles
*)

module STI = Sliding_tiles_inst
module ST = Sliding_tiles
module MST = Macro_sliding_tiles

type pattern = int list

type pdb_state = Packed_ints.t


(** Parsing command line arguments associated with tiles pdbs **)

let get_pattern string =
  (** Takes a string, getting the desired pattern back out *)
  Verb.pe Verb.debug "Parsing pattern %s\n" string;
  let vals = Str.split (Str.regexp "-") string in
    List.map int_of_string vals


let rec pattern_to_string = function
    (** converts a pattern into a string representing that pattern *)
  | [] -> ""
  | hd::tl -> (string_of_int hd) ^ "-" ^ (pattern_to_string tl)


let make_h (cost,_) key = (fun n -> cost.(key n))


let make_d (_,dist) key = (fun n -> float dist.(key n))


let make_hd (cost,dist) key =
  (fun n ->
     let kv = key n in
       cost.(kv), float_of_int dist.(kv))

(************************ PDB Tools ******************************************)
let make_pdb_key pattern inst =
  (** Generates a key function for the pdb. *)
  let info = Packed_ints.info inst.STI.nelms
  and tile_to_index = Wrlist.mapi (fun ind a -> a , ind) pattern
  and my_compare (a,_) (b,_) = Fn.int_compare a b
  and is_mem = Array.init inst.STI.nelms (fun ele -> List.mem ele pattern) in
  let assoc_index = Array.init inst.STI.nelms (fun ele ->
					     if is_mem.(ele)
					     then List.assoc ele tile_to_index
					     else -1) in
  let build_vals content =
    Array.of_list
      (List.map snd
	 (List.sort my_compare
	    (Packed_ints.make_fold_lefti info inst.STI.nelms
	       (fun accum i ele ->
		  if is_mem.(ele)
		  then (assoc_index.(ele),i)::accum
		  else accum) [] content))) in
    (fun n -> Array_hasher.lex_rank_abst inst.STI.nelms (build_vals
							   n.ST.contents))

let make_gen_pdb_key pattern inst =
  (** Generates a key function for the pdb. *)
  let info = Packed_ints.info inst.STI.nelms
  and pleng = List.length pattern
  and tile_to_index = Wrlist.mapi (fun ind a -> a , ind) pattern
  and index_to_tile = Wrlist.mapi (fun ind a -> ind , a) pattern
  and my_compare (a,_) (b,_) = Fn.int_compare a b
  and is_mem = Array.init inst.STI.nelms (fun ele -> List.mem ele pattern) in
  let non_mem = (Wrarray.fold_lefti (fun accum index ele ->
					if not ele then index else accum) 0
		   is_mem) in
  let assoc_index = Array.init inst.STI.nelms (fun ele ->
					     if is_mem.(ele)
					     then List.assoc ele tile_to_index
					     else -1) in
  let build_vals content =
    (* convert the location list into a location array *)
    Array.of_list
      (* strip off the pattern index, be left with just the location *)
      (List.map snd
	 (* sort the pattern index by location list by pattern index *)
	 (List.sort my_compare
	    (Packed_ints.make_fold_lefti info inst.STI.nelms
	       (fun accum i ele ->
		  (* if this is a member of the pattern *)
		  if is_mem.(ele)
		    (* return it's pattern index,
		       and the current index it has in the search *)
		  then (assoc_index.(ele),i)::accum
		  else accum) [] content))) in

    ((fun n -> let to_lex = build_vals n in
	(*Verb.pe Verb.always "Lexing: ";
	for i = 0 to (Array.length to_lex) - 1
	do (Verb.pe Verb.always "%i\t" to_lex.(i)) done;
	Verb.pe Verb.always "\n%!";*)
	Array_hasher.lex_rank_abst inst.STI.nelms to_lex),

     (fun key ->
	(* this is the permutation array from build_vals*)
	let iar = Array_hasher.lex_unrank_abst pleng key in
	  Verb.pe Verb.always "unlexing: ";
	  Array.iter (fun e -> Verb.pe Verb.always "%i\t" e) iar;
	  Verb.pe Verb.always "\n%!";
	let piar = Packed_ints.make_create info inst.STI.nelms in
	let set = Packed_ints.make_set info piar
	and get = Packed_ints.make_get info piar in
	  for i = 0 to (inst.STI.nelms - 1) do set i non_mem done;
	  Array.iteri (fun index ele ->
			 let tile = List.assoc index index_to_tile in
			   Verb.pe Verb.always "%i in %i\n%!" tile ele;
			   set ele tile) iar;
	  for i = 0 to (inst.STI.nelms - 1)
	  do (Verb.pe Verb.always "%i\t" (get i)) done;
	  Verb.pe Verb.always "\n%!";
	  piar))
(*

    (fun n -> Array_hasher.rank (build_vals n)),
  (fun r -> let perm = Array_hasher.derank pleng r in
   let par = Packed_ints.make_create info inst.STI.nelms in
   let set = Packed_ints.make_set info par in
     for i = 0 to (inst.STI.nelms - 1)
     do set i ~-1 done;
     Array.iteri (fun index elm ->
		    let tile = List.assoc index index_to_tile in
		      set elm tile) perm;
     par)
*)

let has_cost pattern inst tile =
  (** Determins if the element just moved has cost *)
  List.mem tile pattern


let path ?(add = false) rows cols cost move pattern =
  (* this may need to be in a different place? *)
  let instance_root = User_paths.instance_root ^ "tiles_instances"
  and pdb_name = (List.fold_left (fun accum ele ->
				   if accum = "" then string_of_int ele
				   else accum ^ "_" ^ string_of_int ele) ""
		    pattern) in
    Rdb.path_for instance_root
      ["model", "pdb";
       "rows", string_of_int rows;
       "cols", string_of_int cols;
       "cost", cost;
       "move", move;
       "additive", string_of_bool add;
       "pattern", pdb_name;]


(************************ Expand Functions ***********************************)

let make_gen_pdb_child inst get set tile_cost =
  (** [make_gen_child inst get set h d tile_cost]
      makes a function that generates children from a parent's
      contents. *)
  let tile_cost = tile_cost inst in
    (fun contents b b' cost ->
       let tile = get contents b' in
       let contents' = Packed_ints.copy contents in
       let cost' = (tile_cost tile) +. cost in
	 set contents' b' STI.blank_tile;
	 set contents' b tile;
	 contents', cost')


let pdb_expand add pattern inst tile_cost info get set =
  let nrows = inst.STI.nrows
  and ncols = inst.STI.ncols
  and has_cost = has_cost pattern inst in
    (* this needs updated with a min_add value sometime in the future *)
  let has_cost thc  = has_cost thc || (not add) in
  let my_tile_cost inst tile = (if has_cost tile
				then tile_cost inst tile
				else 0.) in
  let find_blank = (let foldi = Packed_ints.make_fold_lefti info (ncols * nrows)
		    in (fun state ->
			  foldi (fun accum index ele ->
				   if ele = 0 then index else accum) ~-1
			    state)) in
  let gen_child = make_gen_pdb_child inst get set my_tile_cost in
    (fun state cost ->
       let pb = -1 in
       let b = find_blank state in
       let row, col = ST.row_and_col ncols b in
       let contents = state in
       let left =
	 let b' = b - 1 in
	   if b' <> pb && col > 0
	   then [ gen_child contents b b' cost ]
	   else [] in
       let right =
	 let b' = b + 1 in
	   if b' <> pb && col < ncols - 1
	   then gen_child contents b b' cost :: left
	   else left in
       let up =
	 let b' = b - ncols in
	   if b' <> pb && row > 0
	   then gen_child contents b b' cost :: right
	   else right in
       let down =
	 let b' = b + ncols in
	   if b' <> pb && row < nrows - 1
	   then gen_child contents b b' cost :: up
	   else up in
	 down)


let macro_pdb_expand add pattern inst tile_cost get set h d =
  let ncols = inst.STI.ncols
  and has_cost = has_cost pattern inst in
    (* this needs updated with a min_add value sometime in the future *)
  let has_cost thc  = has_cost thc || (not add) in
  let gen_child = MST.make_gen_child_noparent inst get set h d in

  let rec make_horiz succ accum ~row parent state ~pb ~b cost had_cost =
    let b' = succ b in
      if (b' / ncols) <> row || b' >= inst.STI.nelms || b' < 0
      then accum
      else
	(let had_cost = had_cost || has_cost b' in
	 let child,child_cost = gen_child parent state b b' cost in
	 let cc = child, if had_cost then child_cost else cost  in
	 let accum' = if b' <> pb then cc :: accum else accum in
	   make_horiz succ accum' ~row parent child ~pb ~b:b' cost had_cost) in

  let rec make_vert succ accum ~col parent state ~pb ~b cost had_cost =
    let b' = succ b in
      if (b' mod ncols) <> col || b' >= inst.STI.nelms || b' < 0
      then accum
      else
	(let had_cost = had_cost || has_cost b' in
	 let child,child_cost = gen_child parent state b b' cost in
	 let cc = child, if had_cost then child_cost else cost in
	 let accum' = if b' <> pb then cc :: accum else accum in
	   make_vert succ accum' ~col parent child ~pb ~b:b' cost had_cost) in

    (fun ?parent state cost ->
       let pb = ~-1 in
       let b = state.ST.blank in
       let row, col = ST.row_and_col ncols b in
       let left = make_horiz pred [] ~row state state ~pb ~b cost false in
       let right = make_horiz succ left ~row state state ~pb ~b cost false in
       let up = make_vert (fun n -> n - ncols) right ~col state state
	 ~pb ~b cost false in
       let down = make_vert (fun n -> n + ncols) up ~col state state
	 ~pb ~b cost false in
	 down)


(************************ Generation Functions********************************)
let make_pdb max add pattern tile_cost inst move =
  (** solves the problem backwards, writing down the pdb *)
  let inst = STI.reverse_inst inst in
  let info = Packed_ints.info inst.STI.nelms in
  let create = Packed_ints.make_create info in
  let get = Packed_ints.make_get info
  and set = Packed_ints.make_set info
  and current = ref 0
  and last_p = ref 0 in
  let exp = match move with
    | "standard" -> pdb_expand add pattern inst tile_cost info get set
    (*| "macro" -> macro_pdb_expand add pattern inst tile_cost get set h d*)
    | _ -> failwith "Movement not found" in
  let initial = ST.make_initial inst create set (fun _ -> 0.) (fun _ -> 0) in
  let initial = initial.ST.contents in
  let expand state cost =
    current := !current + 1;
    let p = !current * 100 / max in
      if p > !last_p && (p mod 5) = 0
      then (last_p := p;
	    Verb.pe Verb.always "%i%% Complete\n" p); exp state cost in
  let key,unrank = make_gen_pdb_key pattern inst in
    Gen_pdb.dups max expand key initial (=) Fn.identity



let generate_pdb add pattern cost move problem =
  (** pattern is a list of tiles to be made *)
  Verb.pe Verb.debug "%b " add;
  List.iter (Verb.pe Verb.debug "%i ") pattern;
  Verb.pe Verb.debug "\n";
  assert (List.mem 0 pattern); (* always pay attention to the blank*)
  let cols = problem.STI.ncols
  and rows = problem.STI.nrows
  and pkey = make_pdb_key pattern in
  let size = rows * cols in
    Verb.pe Verb.debug "Board Size: %i\n" size;
  let pdb_size = fst (List.fold_left
			(fun (accum,sz) ele ->
			   (accum * sz), sz - 1) (1,size) pattern) in
    Verb.pe Verb.debug "Pdb size: %i\n" pdb_size;
  let pdb = make_pdb pdb_size add pattern (snd cost) problem move in
    Verb.pe Verb.debug "Pdb generated\n";
    (path ~add:add rows cols (fst cost) move pattern), (pattern,pkey,pdb)


(******************************* I/O ******************************************)

let write_pdb ch pdb_pair =
  Marshal.to_channel ch pdb_pair []


let read_pdb ch =
  Marshal.from_channel ch


let pdb_to_path path (pattern,pkey,pdb) =
  let pdb_pair = pattern,pdb in
    Wrfname.ensure_path path;
    if not (Sys.file_exists path)
    then Wrio.with_outfile path (fun ch -> write_pdb ch pdb_pair)


let pdb_from_path path =
  (** Returns a tuple of the pattern, pdb_key and the pdb, in that order
      from the file.  The heuristics must be generated after the pdb is
      lodade *)
  if Sys.file_exists path
  then Wrio.with_infile path read_pdb
  else failwith (Wrutils.str "No PDB @ %s" path)


let make_and_save prob add pattern cost movement =
  let path, pdb_trip = generate_pdb add pattern cost movement prob in
    Wrfname.ensure_path path;
    pdb_to_path path pdb_trip


(* EOF *)
