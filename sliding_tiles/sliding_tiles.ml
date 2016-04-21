(** A sliding tiles solver.

    All arrays are index ed on tile location and give the tile number
    at the given location.

    @author eaburns
    @since 2010-08-31
*)

open Printf
open Sliding_tiles_inst

type state = {
  contents : Packed_ints.t;
  (* index = row * ncols + col *)

  blank : int;
  (* Location of the blank. *)

  h : float;
  (* Cached heuristic value. *)

  d : int;
  (* Cached distance value. *)

  mutable parent : state;
  (* Do not touch this field unless you have to.  Maybe we will get
     rid of it some day. *)
}


module Tile_cost = struct
  (* Different cost functions on a single movement of a single
     tile. *)

  let unit inst tile = 1.
  let sqrt inst tile = sqrt (float tile)
  let inverse inst tile = 1. /. (float tile)
  let pow_1 inst tile = (float tile)
  let pow_2 inst tile = (float tile) ** 2.0
  let pow_3 inst tile = (float tile) ** 3.0
  let pow_4 inst tile = (float tile) ** 4.0
  let pow_5 inst tile = (float tile) ** 5.0
  let pow_6 inst tile = (float tile) ** 6.0
  let pow_7 inst tile = (float tile) ** 7.0
  let pow_8 inst tile = (float tile) ** 8.0
  let pow_9 inst tile = (float tile) ** 9.0
  let pow_10 inst tile = (float tile) ** 10.0
  let pow_100 inst tile = (float tile) ** 100.0


  let by_name = function
    | "unit" -> unit
    | "sqrt" -> sqrt
    | "inverse" -> inverse
    | "pow_1" -> pow_1
    | "pow_2" -> pow_2
    | "pow_3" -> pow_3
    | "pow_4" -> pow_4
    | "pow_5" -> pow_5
    | "pow_6" -> pow_6
    | "pow_7" -> pow_7
    | "pow_8" -> pow_8
    | "pow_9" -> pow_9
    | "pow_10" -> pow_10
    | "pow_100" -> pow_100
    | s ->
	(fun inst tile ->
	   let mcf = Gen_cost_function.make_special s in
	     mcf tile)

  let by_cost max_range =
    fun inst tile ->
      let mcf = Gen_cost_function.make_cost_function
	~max_cost_out:(float_of_int max_range)
	~min_cost_out:1.0
	~max_cost_in:(float_of_int inst.Sliding_tiles_inst.nelms)
	~min_cost_in:1.0 in
	mcf (float tile)


end


let make_initial inst create set h d =
  (** [make_initial inst create set h d] makes the initial state. *)
  let nelms = inst.nelms in
  let init = inst.initial in
  let contents = create nelms in
  let sinit = Array.length inst.initial in
    if nelms <> sinit
    then failwith (Printf.sprintf "Expected: %i Actual: %i" nelms sinit);
  let b = ref ~-1 in
    for i = 0 to nelms - 1 do
      let t = init.(i) in
	if t = blank_tile then b := i;
	set contents i t
    done;
    let h = h contents and d = d contents in
      Verb.pr Verb.debug "Initial h=%f, d=%d\n" h d;
    let rec s = { contents = contents;
		  blank = !b;
		  h = h;
		  d = d;
		  parent = s;
		}
    in s


let row_and_col ncols index =
  (** [row_and_col ncols index] gets the row and column number from an
      index. *)
  index / ncols, index mod ncols


let tile_goal_location goal tile =
  (** [tile_goal_location goal tile] finds the index of the goal
      location of the given tile. *)
  let ntiles = Array.length goal in
  let rec find_goal_loc goal tile i =
    if i < ntiles
    then if goal.(i) = tile then i else find_goal_loc goal tile (i + 1)
    else raise Not_found
  in find_goal_loc goal tile 0


(** {1 Search functions} ****************************************)


module Key = struct
  (* A keys are used for the hash table. *)

  let of_state state =
    (** [of_state state] gets a unique key value for the given state. *)
    state.contents


  let make_to_string inst get =
    (** [make_to_string inst get] makes a function that converts keys
	into strings. *)
    let b = Buffer.create 100 in
    let nrows = inst.nrows and ncols = inst.ncols in
    let size = (truncate (log10 (float (nrows * ncols)))) + 1 in
      (* number of decimal digits in the maximum tile value. *)
      (fun key ->
	 Buffer.clear b;
	 for r = 0 to nrows - 1 do
	   let base = r * ncols in
	     for c = 0 to ncols - 1 do
	       Buffer.add_string b (sprintf "%*d" size (get key (base + c)));
	       Buffer.add_char b ' '
	     done;
	     if r < nrows - 1 then Buffer.add_char b '\n'
	 done;
	 Buffer.add_char b '\n';
	 Buffer.add_char b '\n';
	 Buffer.contents b)


  let make_dump_key inst get =
    (** [make_to_string inst get] makes a function that converts keys
	into strings. *)
    let b = Buffer.create 100 in
    let nrows = inst.nrows and ncols = inst.ncols in
    (*let size = (truncate (log10 (float (nrows * ncols)))) + 1 in*)
      (* number of decimal digits in the maximum tile value. *)
      (fun key ->
	 Buffer.clear b;
	 for r = 0 to nrows - 1 do
	   let base = r * ncols in
	     for c = 0 to ncols - 1 do
	       Buffer.add_string b (sprintf "%d" (get key (base + c)));
	       Buffer.add_char b ';'
	     done;
	 done;
	 Buffer.contents b)



  let make_hash inst =
    (** [make_hash inst] makes a hash function that looks at enough
	pieces of data of the key for this tiles instance. *)
    let hash = Hashtbl.hash_param inst.nelms 100 in
      (* inst.nelms is an upper bound on the number of meaningful
	 elements in the (packed) contents array. *)
      (fun key -> hash key)


  let equals =
    (** [equals] compares two keys for equality. *)
    (* This is faster than walking through the packed_ints. *)
    (=)

end


let make_is_goal inst get =
  (** [make_is_goal inst get] makes a function that test if the
      current state is a goal state. *)
  let nelms = inst.nelms in
  let goal = inst.goal in
  let rec check_tile ary i =
    if i < nelms
    then if (get ary i) <> goal.(i) then false else check_tile ary (i + 1)
    else true
  in (fun state -> check_tile state.contents 0)


let rec unroll_path inst ?(path=[]) state =
  (** [unroll_path inst ?path ?cost state] walks back up the
      chain of parent pointers building the path. *)
  let p = state.parent in
    if p == state then path else unroll_path inst ~path:(state :: path) p


let make_gen_child inst get set h d tile_cost =
  (** [make_gen_child inst get set h d tile_cost]
      makes a function that generates children from a parent's
      contents. *)
  let tile_cost = tile_cost inst in
    (fun parent contents b b' cost ->
       let tile = get contents b' in
       let contents' = Packed_ints.copy contents in
       let cost' = (tile_cost tile) +. cost in
	 set contents' b' blank_tile;
	 set contents' b tile;
	 { contents = contents';
	   blank = b';
	   h = h parent b' tile;
	   d = d parent b' tile;
	   parent = parent; }, cost')


let make_gen_child_noparent inst get set h d tile_cost =
  (** [make_gen_child inst get set h d tile_cost]
      makes a function that generates children from a parent's
      contents. *)
  let tile_cost = tile_cost inst in
    (fun parent contents b b' cost ->
       let tile = get contents b' in
       let contents' = Packed_ints.copy contents in
       let cost' = (tile_cost tile) +. cost in
	 set contents' b' blank_tile;
	 set contents' b tile;
	 let rec s = { contents = contents';
		       blank = b';
		       h = h parent b' tile;
		       d = d parent b' tile;
		       parent = s; } in
	   s, cost')



let make_expand inst tile_cost get set h d =
  (** [make_expand inst tile_cost get set h d] makes an expansion
      function. *)
  let nrows = inst.nrows and ncols = inst.ncols in
  let gen_child = make_gen_child inst get set h d tile_cost in
    (fun ?parent state cost ->
       let pb = match parent with Some p -> p.blank | None -> ~-1 in
       let b = state.blank in
       let row, col = row_and_col ncols b in
       let contents = state.contents in
       let left =
	 let b' = b - 1 in
	   if b' <> pb && col > 0
	   then [ gen_child state contents b b' cost ]
	   else []
       in
       let right =
	 let b' = b + 1 in
	   if b' <> pb && col < ncols - 1
	   then gen_child state contents b b' cost :: left
	   else left
       in
       let up =
	 let b' = b - ncols in
	   if b' <> pb && row > 0
	   then gen_child state contents b b' cost :: right
	   else right
       in
       let down =
	 let b' = b + ncols in
	   if b' <> pb && row < nrows - 1
	   then gen_child state contents b b' cost :: up
	   else up
       in down)

let domain_expand inst tile_cost get set h d =
  (** [domain_expand inst tile_cost get set h d] makes the domain
      expand function.  This just converts [expand] to match the
      signature that the search interface is looking for. *)
  let exp = make_expand inst tile_cost get set h d
  in (fun state cost -> exp ~parent:state.parent state cost)

let predecessor inst tile_cost get set h d =
  let exp = make_expand inst tile_cost get set h d
  in (fun state cost -> exp state cost)

let make_blank_position_type inst =
  (** [make_blank_position_type inst pb cb] makes a function that gets
      a unique number that represents the combination of parent and
      child blank positions. *)
  let ncols = inst.ncols and nrows = inst.nrows in
  let corner = 0 and side = 1 and middle = 2 and nvals = 3 in
  let sides b =
    let r, c = row_and_col ncols b in
      c = 0, c = ncols - 1, r = 0, r = nrows - 1
  in
  let value b =
    let t, b, l, r = sides b in
      if (t && l) || (t && r) || (b && l) || (b && r)
      then corner
      else if t || l || b || r then side else middle
  in
    (fun pb cb ->
       let pval = value pb and cval = value cb in
	 pval * nvals + cval)


let rwalk ~nrows ~ncols step =
  Verb.pe Verb.always "Making instance";
  let i =  (inst ~nrows ~ncols (canonical_goal ~nrows ~ncols)
	      (canonical_goal ~nrows ~ncols)) in
  let info = Packed_ints.info i.nelms in
  let create = Packed_ints.make_create info in
  let get = Packed_ints.make_get info in
  let set = Packed_ints.make_set info in
  let root = make_initial i create set (fun _ -> 0.) (fun _ -> 0) in
  let exp = (domain_expand i Tile_cost.unit get set
	       (fun _ _ _ -> 0.) (fun _ _ _ -> 0)) in
  let rec do_step remaining kid =
    if remaining > 0
    then do_step (remaining - 1) (fst (Wrlist.random_elt (exp kid 0.)))
    else kid in
  let rand_state = do_step step root in
  let i' = { i with initial = Packed_ints.unpack rand_state.contents
	   i.nelms} in
    assert ((Array.length i'.initial) = (Array.length i'.goal));
    Verb.pe Verb.always "Done\n%!";
    i'

let make_rwalk_set ?(overwrite=false) ~nrows ~ncols ~count =
  let inst_root = User_paths.instance_root ^ "tiles_instances/" in
  let set_attrs =
    [ "model", "random_walk";
      "rows", string_of_int nrows;
      "cols", string_of_int ncols; ]
  in
    for i = 1 to count do
      let inst_attrs = set_attrs @ [("num", string_of_int i)] in
      let file = Rdb.path_for inst_root inst_attrs in
	if overwrite || not (Sys.file_exists file)
	then (let inst = rwalk ~nrows ~ncols 1_000_000 in
		Verb.pr Verb.always "Saving %s\n" file;
		save inst file)
	else Verb.pr Verb.always "Skipping %s\n" file
    done


(* EOF *)
