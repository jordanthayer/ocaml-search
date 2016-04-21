(** Instance Representation and loadings *)

type direction =
  | Left
  | Right
  | Up
  | Down

type move =
  | Push of direction
  | Walk of direction

type cell =
  | Floor
  | Wall
  | Target
  | Player
  | Box
  | Box_on_target
  | Player_on_target

type state = {
  location : int * int;
  boxes : (int * int) array;
  parent : state;
}


let possible_moves =
  [( 1, 0); (* east *)
   (-1, 0); (* west *)
   ( 0, 1); (* north *)
   ( 0,-1);](* south *)

let char_to_element c =
  match c with
    | '$' -> Box
    | ' ' -> Floor
    | '#' -> Wall
    | '.' -> Target
    | '@' -> Player
    | '*' -> Box_on_target
    | '+' -> Player_on_target
    |  _ -> failwith (Wrutils.str "Unrecognized character |%c|" c)


let element_to_str e =
  match e with
    | Floor -> " "
    | Box -> "$"
    | Wall -> "#"
    | Player -> "@"
    | Target -> "."
    | Box_on_target -> "*"
    | Player_on_target -> "+"


let direction_to_string d =
  match d with
    | Left -> "l"
    | Right -> "r"
    | Up -> "u"
    | Down -> "d"

let move_to_string m =
    match m with
      | Push d -> String.uppercase (direction_to_string d)
      | Walk d -> String.lowercase (direction_to_string d)


let make_state_to_str world =
  (** Makes a functionf for Converting states into a strings for printing. *)
  (fun s ->
     let world = Array.init (Array.length world)
       (fun i -> Array.copy world.(i))
     and mx,my = s.location
     and str = ref "" in
       world.(mx).(my) <- Player;
       Array.iter
	 (fun (bx,by) ->
	    match world.(bx).(by) with
	      | Floor -> world.(bx).(by) <- Box;
	      | Target -> world.(bx).(by) <- Box_on_target
	      | _ -> failwith "Shouldn't be possible")
	 s.boxes;
       for y = 0 to (Array.length world.(0) - 1)
       do
	 (for x = 0 to (Array.length world -1)
	  do
	    str := !str ^ (element_to_str world.(x).(y))
	  done;
	  str := !str ^ "\n")
       done;
       !str)

(***************************************************************************)
let height b =
  (** How tall is the [b]oard *)
  Array.length b.(0)

let width b =
  (** How wide is the [b]oard *)
  Array.length b

(*
let update_parent node new_parent =
  (** Replaces the parent of [node] with [new_parent] *)
  node.parent <- new_parent
*)

let get_parent node =
  (** returns the parent of [node] *)
  node.parent


let make_root boxes start =
  (** Generates the root of a search problem for a given problem
      [boxes] - a list of the crates, possibly unsorted
      [start] - starting location of the man *)
  let rec n = {location = start;
	       boxes = Array.of_list (List.sort compare boxes);
	       parent = n; } in
    n


let make_goal world =
  (** Creates a goal test for the given [world]*)
  (fun state ->
     Array.fold_left (fun accum (x,y) ->
			if accum
			then world.(x).(y) = Target
			else accum) true state.boxes)


let make_explicit_target_goal target =
  (** Creates a goal test for the given [world]*)
  (fun state -> Wrarray.mem target state.boxes)

let key state =
  (** Uniquely identifies a world configuration *)
  state.location, state.boxes


let move_box old_world old_boxes (ox,oy) (dx,dy) =
  (** Returns a new sorted array of boxes which is the result of moving the
      man and pushing a box *)
  let nx,ny = (ox + dx, oy + dy) in
  let nbx,nby = nx + dx, ny + dy in
    if nbx >= 0 && nbx < (width old_world) &&
      nby >=0 && nby < (height old_world)
    then
      (match old_world.(nbx).(nby) with
	 | Wall -> None
	 | _ ->
	     (let box_ind = Wrarray.fold_lefti
		(fun accum ind (bx,by) ->
		   if nbx = bx && nby = by then ~-2
		   else if bx = nx && by = ny && accum <> ~-2
		   then ind
		   else accum) ~-1 old_boxes in
		if box_ind < 0
		then None
		else (let nbar = Array.copy old_boxes in
			nbar.(box_ind) <- (nbx,nby);
			Array.sort compare nbar;
			Some nbar)))
    else None


let make_expand world walk_cost push_cost =
  (** Generates the expand function based on the game board [world]
      and the cost of movement with [push_cost] and without [walk_cost]
      shifting a box -- Generates the parents by default *)
  let width = width world
  and height = height world in
    (fun state cost ->
       let man_x, man_y = state.location
       and children = ref [] in
	 List.iter
	   (fun (dx,dy) ->
	      let x = man_x + dx
	      and y = man_y + dy in
		if ((x >= 0) && (x < width) &&    (*in bounds x*)
		      (y >= 0) && (y < height))   (*in bounds y*)
		then
		  (match world.(x).(y) with
		     | Wall -> ()
		     | _ ->
			 if (Wrarray.mem (x,y) state.boxes)
			 then (match (move_box world state.boxes
					(man_x,man_y) (dx,dy)) with
				   None -> ()
				 | Some box_array ->
				     (Wrutils.push
					({location = (x,y);
					  boxes = box_array;
					  parent = state;},
					 cost +. push_cost) children))
			 else (Wrutils.push
				 ({location = (x,y);
				   boxes = state.boxes;
				   parent = state;},
				  cost +. walk_cost) children)))
	   possible_moves;
	 !children)


let find_move start result =
  (** What move was taken between [start] and [result] *)
  let sx,sy = start.location
  and rx,ry = result.location
    (* this could probably be sped up *)
  and box_changed = start.boxes <> result.boxes in
  let dx = sx - rx
  and dy = sy - ry in
    match (dx,dy) with
      | (0,0)  -> failwith "Sokoban_instance.find_move: No move?"
      | (1,0)  -> if box_changed then Push Right else Walk Right
      | (-1,0) -> if box_changed then Push Left else Walk Left
      | (0,1)  -> if box_changed then Push Up else Walk Up
      | (0,-1) -> if box_changed then Push Down else Walk Down
      | _      -> failwith "Sokoban_instance.find_move: Illegal move"

let rec find_path solution_state =
  (** Returns a sequence of steps to solve the problem in reverse *)
  if solution_state.parent == solution_state
  then []
  else ((find_move solution_state.parent solution_state)::
	  (find_path solution_state.parent))

let get_solution state =
  (* God help us if the solution is too long to reverse w/o tail recursion *)
  List.rev (find_path state)


let solution_to_string sol =
  (** Converts the in order move list [sol] into a string *)
  List.fold_left (fun accum next ->
		    accum ^ (move_to_string next)) "" sol


let revsolution_to_string sol =
  (** Converts the reversed move list [sol] into a string *)
  List.fold_left (fun accum next ->
		    (move_to_string next) ^ accum) "" sol


(********************************* I/O ************************************)
let read ch =
  (** Reads a problem from [ch], returining a board, start location, and box
      list *)
  let as_lines = Wrio.input_lines ch in
  let rec get_board lines =
    match lines with
	[] -> []
      | hd::tl ->
	  (let hd_list = Str.split (Str.regexp ":") hd in
	     match hd_list with
		 [singleton] -> singleton::(get_board tl)
	       | _ -> []) in
  let board_as_lines = get_board as_lines in
  let height = List.length board_as_lines
  and width = (List.fold_left (fun accum line ->
				 max accum (String.length line))
		 0 board_as_lines) in
  let empty_board = Array.create_matrix width height Floor
  and box_list = ref []
  and start = ref (-1,-1) in
    Wrlist.iteri (fun y row ->
		    Wrstr.iteri
		      (fun x c ->
			 match char_to_element c with
			   | Floor -> empty_board.(x).(y) <- Floor
			   | Wall -> empty_board.(x).(y) <- Wall
			   | Target -> empty_board.(x).(y) <- Target
			   | Box -> (empty_board.(x).(y) <- Floor;
				     box_list := (x,y)::!box_list)
			   | Player -> (empty_board.(x).(y) <- Floor;
					start := (x,y))
			   | Box_on_target -> (empty_board.(x).(y) <- Target;
					       box_list := (x,y)::!box_list)
			   | Player_on_target ->
			       (empty_board.(x).(y) <- Target;
				start := (x,y))) row)
      board_as_lines;
    (make_root !box_list !start), empty_board


let read_filename str =
  (** read a problem from a file with path [str], same as above *)
  Wrio.with_infile str read

(* EOF *)
