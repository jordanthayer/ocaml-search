(** Sliding tiles with macro moves.

    @author eaburns
    @since 2010-09-07
*)

open Sliding_tiles_inst
open Sliding_tiles
open Sliding_heuristics

let make_gen_child inst get set h d =
  (** [make_gen_child inst get set h d] makes a function that
      generates children from a parent's contents. *)
  (fun parent state b b' cost ->
     (* parent is used to set the parent pointer, state is used for
	the incremental heuristic computation.  state should be the
	state with the previous blank position (1 slide over from the
	child being generated). *)
     let contents = state.contents in
     let tile = get contents b' in
     let contents' = Packed_ints.copy contents in
     let cost' = 1. +. cost in
       set contents' b' blank_tile;
       set contents' b tile;
       { contents = contents';
	 blank = b';
	 h = h state b' tile;
	 d = d state b' tile;
	 parent = parent; }, cost')


let make_gen_child_noparent inst get set h d =
  (** [make_gen_child inst get set h d] makes a function that
      generates children from a parent's contents. *)
  (fun parent state b b' cost ->
     (* parent is used to set the parent pointer, state is used for
	the incremental heuristic computation.  state should be the
	state with the previous blank position (1 slide over from the
	child being generated). *)
     let contents = state.contents in
     let tile = get contents b' in
     let contents' = Packed_ints.copy contents in
     let cost' = 1. +. cost in
       set contents' b' blank_tile;
       set contents' b tile;
       let rec s = { contents = contents';
		     blank = b';
		     h = h state b' tile;
		     d = d state b' tile;
		     parent = s; } in s, cost')



let make_macro_expand inst get set h d =
  (** [make_macro_expand inst get set h d] make an expand function for
      macro tiles. *)
  let nelms = inst.nelms and ncols = inst.ncols in
  let gen_child = make_gen_child inst get set h d in
  let rec make_horiz succ accum ~row parent state ~pb ~b cost =
    let b' = succ b in
      if (b' / ncols) <> row || b' >= nelms || b' < 0
      then accum
      else begin
	let cc = gen_child parent state b b' cost in
	let child, _ = cc in
	let accum' = if b' <> pb then cc :: accum else accum in
	  make_horiz succ accum' ~row parent child ~pb ~b:b' cost
      end
  in
  let rec make_vert succ accum ~col parent state ~pb ~b cost =
    let b' = succ b in
      if (b' mod ncols) <> col || b' >= nelms || b' < 0
      then accum
      else begin
	let cc = gen_child parent state b b' cost in
	let child, _ = cc in
	let accum' = if b' <> pb then cc :: accum else accum in
	  make_vert succ accum' ~col parent child ~pb ~b:b' cost
      end
  in
    (fun ?parent state cost ->
       let pb = match parent with Some p -> p.blank | None -> ~-1 in
       let b = state.blank in
       let row, col = row_and_col ncols b in
       let left =
	 make_horiz pred [] ~row state state ~pb ~b cost
       in
       let right =
	 make_horiz succ left ~row state state ~pb ~b cost
       in
       let up =
	 make_vert (fun n -> n - ncols) right ~col state state ~pb ~b cost
       in
       let down =
	 make_vert (fun n -> n + ncols) up ~col state state ~pb ~b cost
       in
	 down)


let domain_expand inst get set h d =
  let exp = make_macro_expand inst get set h d in
    (fun state cost ->
       let kids = exp ~parent:state.parent state cost in
(*
	 Printf.printf "expanding\n%s h=%f g=%f\n\n--------\n"
	   (Key.make_to_string inst get state.contents)
	   (state.h /. (float (inst.ncols - 1))) cost;
	 List.iter (fun (k, c) ->
		      Printf.printf "\n%s h=%f g=%f\n"
			(Key.make_to_string inst get k.contents)
			(k.h /. (float (inst.ncols - 1))) c)
	   kids;
*)
	 kids)


let default_interface inst limit =
  (** [default_interface inst limit] makes the default search
      interface. *)
  if inst.ncols <> inst.nrows then failwith "Must be square (macro)";
  let info = Packed_ints.info inst.nelms in
  let create = Packed_ints.make_create info in
  let get = Packed_ints.make_get info in
  let set = Packed_ints.make_set info in
  let mt = Manhattan_distance.table Tile_cost.unit inst in
  let inc_h = Manhattan_distance.make_incremental_h inst mt in
  let inc_d = Manhattan_distance.make_incremental_d inst mt in
  let blank_pos = make_blank_position_type inst in
  let denom = float (inst.ncols - 1) in
  let rev_inst = reverse_inst inst in
  let mt_rev = Manhattan_distance.table Tile_cost.unit rev_inst in
  let rev_h_base = Manhattan_distance.from_scratch rev_inst mt_rev get
  and rev_d_base = Manhattan_distance.from_scratch rev_inst mt_rev get in
    Search_interface.make
      ~h:(fun s -> s.h /. denom)
      ~d:(fun s -> (float s.d) /. denom)
      ~hd:(fun s -> s.h /. denom, (float s.d) /. denom)
      ~rev_h:(fun s -> (rev_h_base s.contents) /. denom)
      ~rev_d:(fun s ->  (rev_d_base s.contents) /. denom)
      ~rev_hd:(fun s -> (rev_h_base s.contents) /. denom,
		 (rev_d_base s.contents) /. denom)
      ~domain_expand:(domain_expand inst get set inc_h inc_d)
      ~t:(fun n -> blank_pos n.parent.blank n.blank)
      ~key:Key.of_state
      ~key_print:(Key.make_to_string inst get)
      ~equals:(Key.equals)
      ~hash:(Key.make_hash inst)
      ~goal_p:(make_is_goal inst get)
      ~halt_on:limit
      ~p_update:(fun n p -> n.parent <- p)
      Search_interface.Tiles
      (make_initial inst create set
	 (Manhattan_distance.from_scratch inst mt get)
	 (fun c -> truncate (Manhattan_distance.from_scratch inst mt get c)))
      (fun _ _ -> false)
      (fun _ -> ())

