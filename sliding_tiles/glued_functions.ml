(**

   Functions to support the Sliding Tiles Interface where one or more
   of the tiles have been glued in place.

   Christopher Wilt

   January 22, 2011


*)

module ST = Sliding_tiles
module STI = Sliding_tiles_inst
let make_expand inst tile_cost get set h d =
  (** [make_expand inst tile_cost get set h d] makes an expansion
      function. *)
  let nrows = inst.STI.nrows and ncols = inst.STI.ncols in
  let gen_child = ST.make_gen_child inst get set h d tile_cost in
    (fun ?parent state cost ->
       assert(inst.STI.glued != []);
       let do_not_move = 
	 match parent with
	     Some p -> p.ST.blank :: inst.STI.glued
	   | None -> inst.STI.glued in
       let b = state.ST.blank in
       let row, col = ST.row_and_col ncols b in
       let contents = state.ST.contents in
       let left =
	 let b' = b - 1 in
	   if (not (List.mem b' do_not_move)) && col > 0
	   then [ gen_child state contents b b' cost ]
	   else []
       in
       let right =
	 let b' = b + 1 in
	   if (not (List.mem b' do_not_move)) && col < ncols - 1
	   then gen_child state contents b b' cost :: left
	   else left
       in
       let up =
	 let b' = b - ncols in
	   if (not (List.mem b' do_not_move)) && row > 0
	   then gen_child state contents b b' cost :: right
	   else right
       in
       let down =
	 let b' = b + ncols in
	   if (not (List.mem b' do_not_move)) && row < nrows - 1
	   then gen_child state contents b b' cost :: up
	   else up
       in down)


let domain_expand inst tile_cost get set h d =
  (** [domain_expand inst tile_cost get set h d] makes the domain
      expand function.  This just converts [expand] to match the
      signature that the search interface is looking for. *)
  let exp = make_expand inst tile_cost get set h d
  in (fun state cost -> exp ~parent:state.ST.parent state cost)



let inst ~nrows ~ncols initial goal glued =
  (** [inst ~nrows ~ncols initial goal] makes a new instance. *)
  let nelms = nrows * ncols in
    if (Array.length initial) <> nelms
    then invalid_arg "Initial configuration has incorrect number of tiles";
    if (Array.length goal) <> nelms
    then invalid_arg "Goal configuration has incorrect number of tiles";
    { STI.nrows = nrows; 
      STI.ncols = ncols; 
      STI.nelms = nelms;
      STI.glued =glued;
      STI.initial = initial; 
      STI.goal = goal }


let read inch =
  (** [read inch] reads an instance from the given channel. *)
  let nrows = Wrio.input_int inch and ncols = Wrio.input_int inch in
  let nelms = nrows * ncols in
    Wrio.skip_through_str inch "tile:";
    let init = Wrarray.read_ints inch nelms in
      Wrio.skip_through_str inch "ns:";
      let goal = Wrarray.read_ints inch nelms in
	Wrio.skip_through_str inch "glued:";
	let glued = Wrio.read_ints inch in

	    { 
	      STI.nrows = nrows; 
	      STI.ncols = ncols; 
	      STI.nelms = nelms;
	      STI.glued = glued;
	      STI.initial = STI.contents_of_positions init;
	      STI.goal = STI.contents_of_positions goal;
	    }

