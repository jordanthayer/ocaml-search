(** A different vacuum world representation where hallways are
    condensed where possible into a single node.

    @author eaburns
    @since 2009-11-29
*)

open Printf

type maze = {
  w : int;
  (* Width of the original grid. *)

  h : int;
  (* Height of the original grid. *)

  max_cost : float;
  (* The cost of the longest hallway. (for d, the search distance
     estimate). *)

  start : node;
  (* The start node. *)

  dirt : (int * int) array;
  (* Where the dirt is. *)

  instance : int;
  (* The instance number? *)
}

and node = {
  (* A node in the condensed maze.  NOTE: This is not a search node,
     see 'type state' below. *)

  x : int;
  y : int;
  (* The coordinates in the original grid (for the heuristic). *)

  adj : edge option array;
  (* Adjacent nodes. *)
}

and edge = {
  node : node;
  (* The adjacent node. *)

  cost : float;
  (* The hallway length. *)

  path : (int * int) list;
  (* The path of a hallway... only used for converting back to a
     standard vacuum board (for debugging.) *)
}

(************************************************************)
(* Directions.                                              *)
(************************************************************)

type dir = Up | Down | Left | Right

let directions = [ Up; Down; Left; Right ]

let reverse = function
    (** [reverse dir] gets the reverse direction. *)
  | Up -> Down
  | Down -> Up
  | Left -> Right
  | Right -> Left


let nadj = 4
  (** [nadj] size of the adj array. *)


let index_of_dir = function
    (** [index_of_dir dir] gets the index into the adj array for a node
	for a given direction. *)
  | Up -> 0
  | Down -> 1
  | Left -> 2
  | Right -> 3


let vacuum_op = 4
  (** The number of the vacuum operation. *)


let dir_of_index = function
    (** [dir_of_index i] gets the direction associated with [i], the
	nidex into the adj array for a node.  *)
  | 0 -> Up
  | 1 -> Down
  | 2 -> Left
  | 3 -> Right
  | i -> invalid_arg (Wrutils.str "%d is an invalid index\n%!" i)


let string_of_dir = function
  | Up -> "Up"
  | Down -> "Down"
  | Left -> "Left"
  | Right -> "Right"


(************************************************************)
(* Misc. helper functions.                                  *)
(************************************************************)

let string_of_dirt lst =
  (** [string_of_dirt lst] gets a string representation of a dirt list. *)
  List.fold_left (fun s (x,y) -> Wrutils.str "%s; %d,%d" s x y) "" lst


let dist x0 y0 x1 y1 =
  (** [dist x0 y0 x1 y1] is the Manhattan distance. *)
  let xmax, xmin = Math.imax x0 x1, Math.imin x0 x1
  and ymax, ymin = Math.imax y0 y1, Math.imin y0 y1
  in (xmax - xmin) + (ymax - ymin)


let quadrent maze n =
  (** [quadrent maze n] gets the quadrent number for a maze location. *)
  let x, y = float n.x, float n.y in
  let w, h = maze.w, maze.h in
  let x_split = (float w) /. 2.
  and y_split = (float h) /. 2. in
    if x > x_split && y > y_split then 0
    else if x <= x_split && y > y_split then 1
    else if x <= x_split && y <= y_split then 1
    else 4


let dirt_index maze loc =
  (** [dirt_index maze (x, y)] tests if the current location is dirty.
      The result is <0 if the current location is not dirty, otherwise
      it is the index into the dirt array. *)
  let rec check_mem ary loc i =
    if i >= (Array.length ary)
    then ~-1
    else if ary.(i) = loc then i else check_mem ary loc (i + 1)
  in check_mem maze.dirt loc 0


let initially_dirty maze loc = (dirt_index maze loc) >= 0
  (** [initially_dirty maze (x, y)] tests if the given location has
      dirt in the initial maze. *)


(************************************************************)
(* Building a condensed maze.                               *)
(************************************************************)

let make_node x y =
  (** [make_node x y ] makes a new node with the given x and y
      location. *)
  { x = x;
    y = y;
    adj = Array.make nadj None}


let is_hallway inst x y =
  (** [is_hallway inst x y] tests if a given [x],[y] location is for a
      hallway (one entrance and one exit.). *)
  let neighbors =
    List.fold_left (fun s (x, y) ->
		      if Vacuum.is_passable inst x y
		      then s + 1
		      else s)
      0
      [ x + 1, y; x - 1, y; x, y + 1; x, y - 1; ]
  in (Vacuum.is_passable inst x y) && neighbors = 2


let location_in_dir inst x y dir =
  (** [location_in_dir inst x y dir] gets the location in the given
      direction from loc [x],[y].  The result is an option type since
      there may not be any adjacent room in the given direction. *)
  let x, y = match dir with
    | Up -> x, y - 1
    | Down -> x, y + 1
    | Left -> x - 1, y
    | Right -> x + 1, y
  in
    if Vacuum.is_passable inst x y
    then Some (x, y)
    else None


let hallway_exit_dir inst x y dir =
  (** [hallway_exit_dir inst x y dir] gets the exit direction for a
      hallway room [x],[y] that is entered by moving in direction
      [dir] (entered from entry [reverse dir]). *)
  assert (is_hallway inst x y);
  let entry_side = reverse dir in
  let xit = (List.filter (fun (o, _) -> o <> None)
	       (List.filter (fun (loc, d) -> d <> entry_side)
		  (List.map (fun d -> location_in_dir inst x y d, d)
		     directions)))
  in assert ((List.length xit) = 1);
    snd (List.hd xit)


let get_node (tbl, queue) x y =
  (** [get_node (tbl, queue) x y] gets a node from the [tbl] table if
      it exists.  If there is no matching node in the table then one
      is created and added to [tbl] and [queue]. *)
  try Hashtbl.find tbl (x, y)
  with Not_found ->
    let n = make_node x y in
      Hashtbl.add tbl (x, y) n;
      Queue.push n queue;
      n

let rec condense inst nodes node
    ?(path=[]) ?(cost=0.) ?(x=node.x) ?(y=node.y) dir =
  (** [condense inst nodes node ?path ?cost ?x ?y dir] condenses all
      of the nodes that are hallways in the given direction from
      [node]. *)
  Verb.pr Verb.debug "\tcondensing x=%d, y=%d dir=%s\n%!"
    x y (string_of_dir dir);
  match location_in_dir inst x y dir with
    | None ->
	Verb.pr Verb.debug "\t\tdeadend\n%!";
	None
    | Some (x, y) ->
	if not (is_hallway inst x y) || Vacuum.has_dirt inst x y
	then begin
	  (* Not a hallway or there is dirt so get a node to connect
	     with. *)
	  Verb.force Verb.debug
	    (lazy (if not (is_hallway inst x y)
		   then
		     Verb.pr Verb.debug"\t\tx=%d, y=%d is not a hall\n%!" x y
		   else
		     Verb.pr Verb.debug "\t\tx=%d, y=%d has dirt\n%!" x y));
	  let n = get_node nodes x y in
	  let c = cost +. 1. in
	  let ind = index_of_dir (reverse dir) in
	    assert (n.adj.(ind) = None);
	    assert (c = ((float (List.length path)) +. 1.));
	    n.adj.(ind) <- Some { node = node; cost = c; path = path };
	    Some { node = n; cost = c; path = path }
	end else
	  if x = node.x && y = node.y
	  then begin
	    (* Found a cycle *)
	    Verb.pr Verb.debug "\tfound a cycle in dir=%s\n%!"
	      (string_of_dir dir);
	    assert (cost = (float (List.length path)));
	    Some { node = node; cost = cost +. 1.; path = path }
	  end else
	    (* Condense more nodes in the new direction. *)
	    let new_dir = hallway_exit_dir inst x y dir
	    in condense inst nodes node ~path:((x,y)::path)
		 ~cost:(cost +. 1.) ~x:x ~y:y new_dir


let to_instance maze =
  let board = Array.make_matrix maze.w maze.h true in
  let o = Stack.create ()
  and c = Hashtbl.create 100 in
    Stack.push maze.start o;
    while not (Stack.is_empty o) do
      let n = Stack.pop o in
	if not (Hashtbl.mem c n)
	then begin
	  Hashtbl.add c n true;
	  board.(n.x).(n.y) <- false;
	  Verb.pr Verb.debug "clearing x=%d,y=%d\n%!" n.x n.y;
	  for i = 0 to nadj - 1 do
	    match n.adj.(i) with
	      | Some e ->
		  List.iter (fun (x, y) ->
			       Verb.pr Verb.debug
				 "clearing x=%d,y=%d\n%!" x y;
			       board.(x).(y) <- false)
		    e.path;
		  assert (e.cost = ((float (List.length e.path)) +. 1.));
		  Stack.push e.node o;
	      | _ -> ()
	  done
	end
    done;
    { Vacuum.blocked = board;
      Vacuum.dirt = Array.to_list maze.dirt;
      Vacuum.start = maze.start.x, maze.start.y;
      Vacuum.instance = maze.instance; }


let condense_node inst nodes n =
  (** [condense_node inst nodes n] condenses a node in all directions
      where it currently doesn't have a neighbor. *)
  let max_cost = ref 0. in
    Verb.pr Verb.debug "x=%d, y=%d\n%!" n.x n.y;
    for i = 0 to nadj - 1 do
      match n.adj.(i) with
	| None ->
	    begin match condense inst nodes n (dir_of_index i) with
	      | None -> ()
	      | Some e as edge ->
		  max_cost := Math.fmax !max_cost e.cost;
		  Verb.pr Verb.debug
		    "\t\tcondensed %f in dir=%s\n%!"
		    e.cost (string_of_dir (dir_of_index i));
		  assert (e.cost = ((float (List.length e.path)) +. 1.));
		  n.adj.(i) <- edge;
	    end
	| Some e ->
	    assert (e.cost = ((float (List.length e.path)) +. 1.));
    done;
    !max_cost


let compute_avg_branching maze tbl =
  (** [compute_avg_branching tbl] gets the average branching factor
      given the table of all nodes. *)
  let sum = Hashtbl.fold (fun _ n s ->
			    let neighbors =
			      Array.fold_left (fun s -> function
						 | None -> s
						 | Some _ -> s + 1)
				0 n.adj
			    and dirt =
			      if initially_dirty maze (n.x, n.y)
			      then 1
			      else 0
			    in s + neighbors + dirt) tbl 0
  in (float sum) /. (float (Hashtbl.length tbl))


let of_instance inst =
  (** [of_instance inst] create a condensed maze representation from a
      vacuum world instance. *)
  let t = Hashtbl.create 100
  and q = Queue.create () in
  let nodes = t, q in
  let x, y = inst.Vacuum.start in
  let start = make_node x y in
  let max_cost = ref 0. in
    Queue.push start q;
    Hashtbl.add t (start.x, start.y) start;
    while not (Queue.is_empty q) do
      let n = Queue.take q in
	max_cost := Math.fmax !max_cost (condense_node inst nodes n)
    done;
    let maze = {
      w = Vacuum.width inst;
      h = Vacuum.height inst;
      max_cost = !max_cost;
      start = start;
      dirt = Array.of_list inst.Vacuum.dirt;
      instance = inst.Vacuum.instance; }
    in
      (* Print a bunch of debug info but eventually just result in
	 [maze]. *)
    let new_nodes = Hashtbl.length t
    and old_nodes = (Vacuum.width inst) * (Vacuum.height inst) in
      Datafile.write_pairs stdout
	[ ("condensed nodes", string_of_int new_nodes);
	  ("original nodes", string_of_int old_nodes);
	  ("condense factor", string_of_float
	     ((float new_nodes) /. (float old_nodes)));
	  ("max edge cost", string_of_float !max_cost);
	];
      let avg_branching = compute_avg_branching maze t in
	Datafile.write_pairs stdout [("average branching",
				      string_of_float avg_branching)];
	if Verb.level Verb.debug
	then begin
	  let inst = to_instance maze in
	    Vacuum_instance.fprint stdout inst
	end;
	maze


(************************************************************)
(* Information Gathering.                                   *)
(************************************************************)

let iter f maze =
  (** [iter f maze] calls [f] at each unique node in the maze. *)
  let c = Hashtbl.create 100 in
  let o = Queue.create () in
    Queue.push maze.start o;
    while not (Queue.is_empty o) do
      let n = Queue.take o in
	if not (Hashtbl.mem c n)
	then begin
	  Hashtbl.add c n true;
	  Array.iter (function
			| Some edge -> Queue.push edge.node o;
			| None -> ())
	    n.adj;
	  f n;
	end
    done


let output_stats maze outfile =
  (** [output_stats maze outfile] outputs the given staticsics to a
      file in a format that is readable by spt-it-out. *)
  let nnodes = ref 0 in
  let nadjs = ref [] in
  let costs = ref [] in
  let accum_stats n =
    let nadj = ref 0 in
      incr nnodes;
      Array.iter (function
		    | Some edge ->
			costs := edge.cost :: !costs;
			incr nadj;
		    | None -> ())
	n.adj;
      nadjs := !nadj :: !nadjs
  in
    iter accum_stats maze;
    let outchan = open_out outfile in
      Printf.fprintf outchan "(adjs (";
      List.iter (Printf.fprintf outchan "%d ") !nadjs;
      Printf.fprintf outchan "))\n";
      Printf.fprintf outchan "(costs (";
      List.iter (Printf.fprintf outchan "%f ") !costs;
      Printf.fprintf outchan "))\n";
      Printf.fprintf outchan "(nnodes %d)" !nnodes;
      close_out outchan


(************************************************************)
(* Search interface.                                        *)
(************************************************************)

type state = {
  (* a search state *)

  mutable parent : state;
  g : float;
  position : node;
  cleaned : Bitset.t;
  nremaining : int;
}


let is_dirty maze st =
  (** [is_dirty maze st] tests if the current state has the vacuum at
      a location with dirt. *)
  let pos = st.position in
  let i = dirt_index maze (pos.x, pos.y) in
    i >= 0 && (not (Bitset.mem st.cleaned i))


let dirt_array maze to_clean =
  let lst = ref [] in
  let dirt = maze.dirt in
    for i = 0 to (Array.length dirt) - 1 do
      if Bitset.mem to_clean i then lst := dirt.(i) :: !lst
    done;
    Array.of_list !lst


let node_type maze n = if is_dirty maze n then 1 else 0
  (** [node_type maze n] two types of nodes, on dirt and not. *)


let make_initial maze =
  (** [make_initial maze] make the initial search node. *)
  let ndirt = Array.length maze.dirt in
  let cleaned = Bitset.create ndirt in
  let rec init = {
    parent = init;
    g = 0.;
    position = maze.start;
    cleaned = cleaned;
    nremaining = ndirt;
  } in init


let max_displacements maze n =
  (** [max_displacements maze n] computes the maximum displacement in
      each direction. *)
  let pos = n.position in
  let x = pos.x and y = pos.y and cleaned = n.cleaned in
  let y_disp (_, j) = abs (y - j)
  and x_disp (i, _) = abs (x - i) in
  let max_up = ref 0 and max_down = ref 0 in
  let max_left = ref 0 and max_right = ref 0 in
    Array.iteri
      (fun i ((u, v) as loc) ->
	 if not (Bitset.mem cleaned i)
	 then begin
	   let dy = y_disp loc and dx = x_disp loc in
	     if v < y (* above *)
	     then (if dy > !max_up then max_up := dy)
	     else (if dy > !max_down then max_down := dy);
	     if u < x (* left *)
	     then (if dx > !max_left then max_left := dx)
	     else (if dx > !max_right then max_right := dx);
	 end)
      maze.dirt;
    !max_up, !max_down, !max_left, !max_right


let max_displacement_sum maze n =
  (** [max_displacement_sum maze n] computes the sum of the max x and
      y displacements. *)
  let above, below, left, right = max_displacements maze n in
    above + below + left + right


let make_displacement_heuristic maze n =
  (** [make_displacement_heuristic maze n] computes the sum of the max
      x and y displacements. *)
  float ((max_displacement_sum maze n) + n.nremaining)


let make_displacement_distance maze n =
  (** [make_displacement_distance maze n] computes the sum of the max
      x and y displacements divided by the max hallway cost.  This
      *should* be an admissible distance estimate. *)
  (ceil ((float (max_displacement_sum maze n)) /. maze.max_cost))
  +. (float n.nremaining)


let make_displacement_distance2 maze n =
  (** [make_displacement_distance2 maze n] an alternative displacement
      distance estimate. *)
  let above, below, left, right = max_displacements maze n in
  let delta_x = (min left right) * 2 + (max left right)
  and delta_y = (min above below) * 2 + (max above below)
  and ndirt = float n.nremaining
  in (ceil ((float (delta_x + delta_y)) /. maze.max_cost)) +. ndirt

let update_parent n p = n.parent <- p
    (** [update_parent n p] updates the parent pointer. *)


let rec sol_length n =
  (** [sol_length n] get the length of a solution. *)
  if n.parent != n then 1 + (sol_length n.parent) else 1


let is_goal n = n.nremaining = 0
  (** [is_goal n] tests if state [n] is a goal state. *)


let key n =
  (** [make_key maze] makes a hash key function. *)
  let pos = n.position in (n.cleaned, pos.x, pos.y)

let rec find_adj_index adj target i =
  (** [find_adj_index adj target i] Find the index of [target] in
      the array [adj]. *)
  if i >= (Array.length adj)
  then raise Not_found
  else match adj.(i) with
    | Some edge when edge.node == target -> i
    | _ -> find_adj_index adj target (i + 1)


let get_neighbor_operators maze st =
  (** [get_neighbor_operators maze st] gets the key of each
      prececessor state along with the operator that will transform
      the predecessor into this state. *)
  let pos = st.position in
  let adj = pos.adj in
  let dirt_ind = dirt_index maze (pos.x, pos.y) in
  let parents = ref [] in
    for i = 0 to (Array.length adj) - 1 do
      match adj.(i) with
	| Some edge ->
	    let n = edge.node in
	    let nadj = n.adj in
	    let rev_op = find_adj_index nadj pos 0 in
	      begin match nadj.(rev_op) with
		| None -> assert false;
		| Some edge -> assert (edge.node == pos);
	      end;
	      parents := ((st.cleaned, n.x, n.y), rev_op) :: !parents;
	| None -> ()
    done;
    if not (is_dirty maze st) && (dirt_ind >= 0)
    then
      let cl = Bitset.copy st.cleaned in
	Bitset.remove cl dirt_ind;
	((cl, pos.x, pos.y), vacuum_op) :: !parents
    else !parents


let get_generating_operator maze st =
  (** [get_generating_operator st] get the operator that generated the
      current state.  *)
  let pos = st.position and parent = st.parent in
  let parent_pos = parent.position in
    if parent_pos == pos
    then vacuum_op
    else find_adj_index parent_pos.adj pos 0



let make_expand maze =
  (** [make_expand cost maze] creates an expansion function. *)
  (fun n g ->
     let pos = n.position in
     let ppos = n.parent.position in
     let px = ppos.x and py = ppos.y in
     let moves =
       List.filter
	 (fun (child, _) -> child.position.x <> px || child.position.y <> py)
	 (Array.fold_left (fun l -> function
			     | None -> l
			     | Some edge ->
				 ({ n with
				      g = g +. edge.cost;
				      parent = n;
				      position = edge.node;
				  }, g +. edge.cost) :: l)
	    [] n.position.adj)
     in
     let dirt_ind = dirt_index maze (pos.x, pos.y) in
       if dirt_ind >= 0 && not (Bitset.mem n.cleaned dirt_ind)
       then
	 let cl = Bitset.copy n.cleaned in
	   Bitset.insert cl dirt_ind;
	   let vacuum =
	     { n with
		 parent = n;
		 g = g +. 1.;
		 cleaned = cl;
		 nremaining = n.nremaining - 1;
	     }, g +. 1.
	   in vacuum :: moves
       else moves)


let goal_location x y =
  (fun s ->
     let pos = s.position in
       s.nremaining = 0 && pos.x = x && pos.y = y)


let dirt_and_manhattan maze x y =
  (fun n ->
     let disp = make_displacement_heuristic maze n in
     let pos = n.position in
     let dx = abs (x - pos.x) and dy = abs (y - pos.y) in
     let md = (float (dx + dy)) /. maze.max_cost in
       max md disp)


let make_new_subproblem make_sface maze lim info ~init ~target =
  let total_to_clean =
      (Bitset.complement (Bitset.create (Array.length maze.dirt)))
  in
  let init_to_clean = Bitset.subtract total_to_clean init.cleaned in
  let target_to_clean = Bitset.subtract total_to_clean target.cleaned in
  let left_to_clean = Bitset.subtract init_to_clean target_to_clean in
  let tpos = target.position in
  let init_dirt = dirt_array maze left_to_clean in
  let maze' = { maze with start = init.position; dirt = init_dirt; } in
    { (make_sface maze' []) with
	Search_interface.info = info;
	Search_interface.halt_on = lim;
	Search_interface.goal_p = (goal_location tpos.x tpos.y);
	Search_interface.h = (dirt_and_manhattan maze' tpos.x tpos.y);
    }


let key_printer maze (cleaned, x, y) =
  Wrarray.fold_lefti
    (fun s i (x, y) ->
       if i = 0
       then sprintf "%s(%d,%d)" s x y
       else sprintf "%s,(%d,%d)" s x y)
    (sprintf "(%d,%d);" x y)
    (dirt_array maze (Bitset.complement cleaned))


let rec default_interface maze lim =
  let make_d = make_displacement_distance in
  let hd n = make_displacement_heuristic maze n, make_d maze n in
  let new_subproblem = make_new_subproblem default_interface maze lim in
    (Search_interface.make
       ~h:(make_displacement_heuristic maze)
       (* Could use spanning tree, but this is consistent and spanning
	  tree isn't. *)
       ~d:(make_d maze)
       ~hd:hd
       ~t:(node_type maze)
       ~domain_expand:(make_expand maze)
       ~key:key
       ~key_print:(key_printer maze)
       ~equals:(=)
       ~goal_p:is_goal
       ~halt_on:lim
       ~get_sol_length:sol_length
       ~p_update:update_parent
       ~new_subproblem
       ~get_neighbor_operators:(get_neighbor_operators maze)
       ~get_generating_operator:(get_generating_operator maze)
       Search_interface.Vacuum_maze
       (make_initial maze)
       (fun _ _ -> false)
       (fun _ -> ()))
