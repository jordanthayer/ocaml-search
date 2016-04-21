(**

   Solitaire Chess

   Christopher Wilt

   January 30, 2011

   In the solitaire chess domain, pieces move exactly as they do in
   normal chess, with the exception that the goal is to make all the
   pieces kill one another.  In the usual variation of this game, each
   move a piece must die, so the solutions were all at the same
   depth, and all solutions were equivalent.  In this version, pieces
   do not have to die each turn, and the goal is to find the shortest
   combination of moves that will kill all the pieces.

*)


type cost = Unit | Value


(*different things that could be in a square*)
type square = 
    Pawn | Knight | Bishop | Rook | Queen | King
  | Empty


let get_value p = 
  match p with 
      Pawn -> 1.0
    | Knight -> 3.0
    | Bishop -> 3.25
    | Rook -> 5.0
    | Queen -> 9.0
    | King -> 2.5
    | Empty -> failwith "empty squares do not have a cost"


let from_string s = 
  let p = match s with 
      "P" -> Pawn
    | "N" -> Knight
    | "B" -> Bishop
    | "R" -> Rook
    | "Q" -> Queen
    | "K" -> King
    | bad_string -> 
	failwith (Printf.sprintf "invalid input: \"%s\"" bad_string)
  in
    p

let piece_string p = 
  (*string representation of the chess piece*)
  match p with
      Pawn -> "P"
    | Knight -> "N"
    | Bishop -> "B"
    | Rook -> "R"
    | Queen -> "Q"
    | King -> "K"
    | Empty -> "_"




let square_string s = 
  (*string for the contents of a square*)
  piece_string s

type board = 
    {
      chess_board: square array array;
      mutable hash: int;
    }


type state = 
    {
      board: board;
      remaining_pieces: int;
      moves_taken: int;
    }


let fix_hash s = 
  let k = s.board.chess_board in 
  let n_nodes = (Array.length k) * ((Array.length k.(0))+1) in
    s.board.hash <- Hashtbl.hash_param n_nodes n_nodes k 


let check_hash s = 
  let k = s.board.chess_board in 
  let n_nodes = (Array.length k) * ((Array.length k.(0))+1) in
    assert(s.board.hash = Hashtbl.hash_param n_nodes n_nodes k)



let make_empty rows cols total_pieces = 
{
  board = 
    {
      chess_board = Array.make_matrix rows cols Empty;
      hash = 0;
    };
  remaining_pieces = total_pieces;
  moves_taken = 0;
}


let get_board_value square = 
  get_value square


let width s = 
  Array.length (s.board.chess_board.(0))


let height s = 
  Array.length s.board.chess_board


let is_valid s row col = 
  if(row < 0) then false
  else if (row >= height s) then false
  else if (col < 0) then false
  else if (col >= width s) then false
  else true


let copy_board b =
  {
    chess_board = Wrarray.copy_matrix b.chess_board;
    hash = b.hash;
  }

let copy_state ?(capture = false) s = 
  let new_remaining_pieces = 
    if(capture) then s.remaining_pieces - 1
    else s.remaining_pieces in
    {
      board = copy_board s.board;
      remaining_pieces = new_remaining_pieces;
      moves_taken = s.moves_taken + 1;
    }



let print_board b os= 
  (**prints a string representation of the board to the specified
     stream.*)
  Array.iter (
    fun a -> 
      Array.iter 
	(fun s -> Printf.fprintf os "%s" (square_string s)) a;
      Printf.fprintf os "\n";
  ) b


let to_string_board b = 
  (**turns a board into a string*)
  let b = b.chess_board in 
  let buf = Buffer.create 100 in
    Array.iter (
      fun a -> 
	Array.iter 
	  (fun s -> Buffer.add_string buf (square_string s)) a;
	Buffer.add_string buf "\n";
    ) b;
    Buffer.contents buf


let print_state s os = 
  print_board s.board.chess_board os;
  Printf.fprintf os "Remaining Pieces: %d\n" s.remaining_pieces;
  Printf.fprintf os "Moves Taken: %d\n" s.moves_taken


let to_string s = 
  let buf = Buffer.create 100 in
    Buffer.add_string buf (to_string_board s.board);
    Buffer.add_string buf 
      (Printf.sprintf "Remaining Pieces: %d\n" s.remaining_pieces);
    Buffer.add_string buf 
      (Printf.sprintf "Moves Taken: %d\n" s.moves_taken);
    Buffer.contents buf


exception BadPromotion


let promote_pawn s row col = 
  (*checks*)
  (*piece should be a pawn*)
  assert (row = 0);
  if(not (s.board.chess_board.(row).(col) = Pawn)) then
    (Printf.printf "%d %d\n" row col;
     print_state s stdout;
     flush stdout;
     raise BadPromotion;
    );
  let piece_choices = [Queen;Rook;Bishop;Knight] in
  let children = List.map (
    fun new_piece -> 
      let new_state = copy_state s in
	new_state.board.chess_board.(row).(col) <- new_piece;
	fix_hash new_state;
	new_state
  ) piece_choices in
    children


let pawn_children s row col = 
  (*make sure the piece is a pawn*)
  assert (s.board.chess_board.(row).(col) = Pawn);
  (*pawns can't reside in the back row, they get promoted*)
  assert (row != 0);
  let children = ref [] in
    if(row = 1) then
      (
	if(s.board.chess_board.(row-1).(col) = Empty) then
	  (
	    let new_state = copy_state s in
	      new_state.board.chess_board.(row).(col) <- Empty;
	      new_state.board.chess_board.(row-1).(col) <- Pawn;
	      fix_hash new_state;
	      children := List.rev_append 
		(promote_pawn new_state (row-1) col) !children;
	  );
	if((col != (width s) - 1) && s.board.chess_board.(row-1).(col+1) != Empty) then
	  (
	    let new_state = copy_state s in
	      new_state.board.chess_board.(row).(col) <- Empty;
	      new_state.board.chess_board.(row-1).(col+1) <- Pawn;
	      fix_hash new_state;
	      children := List.rev_append 
		(promote_pawn new_state (row-1) (col+1)) !children;
	  );
	if((col != 0) && s.board.chess_board.(row-1).(col-1) != Empty) then
	  (
	    let new_state = copy_state s in
	      new_state.board.chess_board.(row).(col) <- Empty;
	      new_state.board.chess_board.(row-1).(col-1) <- Pawn;
	      fix_hash new_state;
	      children := List.rev_append 
		(promote_pawn new_state (row-1) (col-1)) !children;
	  );
      )
    else
      (
	if(s.board.chess_board.(row-1).(col) = Empty) then
	  (
	    let new_state = copy_state s in
	      new_state.board.chess_board.(row).(col) <- Empty;
	      new_state.board.chess_board.(row).(col) <- Pawn;
	      fix_hash new_state;
	      children := new_state :: !children;
	  );
	if((col != (width s) - 1) && s.board.chess_board.(row-1).(col+1) != Empty) then
	  (
	    let new_state = copy_state s in
	      new_state.board.chess_board.(row).(col) <- Empty;
	      new_state.board.chess_board.(row).(col) <- Pawn;
	      fix_hash new_state;
	      children := new_state :: !children;
	  );
	if((col != 0) && s.board.chess_board.(row-1).(col-1) != Empty) then
	  (
	    let new_state = copy_state s in
	      new_state.board.chess_board.(row).(col) <- Empty;
	      new_state.board.chess_board.(row).(col) <- Pawn;
	      fix_hash new_state;
	      children := new_state :: !children;
	  );
      );
    !children


let knight_children s row col = 
  assert (s.board.chess_board.(row).(col) = Knight);
  let children = ref [] in
  let move_to d_row d_col = 
    assert(is_valid s d_row d_col);
    let next_state = copy_state ~capture:(s.board.chess_board.(d_row).(d_col) != Empty) s in
      next_state.board.chess_board.(row).(col) <- Empty;
      next_state.board.chess_board.(d_row).(d_col) <- Knight;
      fix_hash next_state;
      next_state in
    (*- -*)
    if(is_valid s (row-1) (col-2)) then
      children := (move_to (row-1) (col-2)) :: !children;
    if(is_valid s (row-2) (col-1)) then
      children := (move_to (row-2) (col-1)) :: !children;
    (*+ +*)
    if(is_valid s (row+1) (col+2)) then
      children := (move_to (row+1) (col+2)) :: !children;
    if(is_valid s (row+2) (col+1)) then
      children := (move_to (row+2) (col+1)) :: !children;
    (*- +*)
    if(is_valid s (row-1) (col+2)) then
      children := (move_to (row-1) (col+2)) :: !children;
    if(is_valid s (row-2) (col+1)) then
      children := (move_to (row-2) (col+1)) :: !children;
    (*+ -*)
    if(is_valid s (row+1) (col-2)) then
      children := (move_to (row+1) (col-2)) :: !children;
    if(is_valid s (row+2) (col-1)) then
      children := (move_to (row+2) (col-1)) :: !children;
    !children


type direction = 
    N | E | S | W | NW | NE | SW | SE


let dir_to_ints d = 
  match d with 
      N -> (-1,0)
    | E -> (0,-1)
    | S -> (1,0)
    | W -> (0,1)
    | NW -> (-1,1)
    | NE -> (-1,-1)
    | SW -> (1,1)
    | SE -> (1,-1)


let rec move_piece dir s s_row s_col c_row c_col accum = 
  (*says where to move the piece*)
  let (del_row, del_col) = dir_to_ints dir in
  let new_row = c_row + del_row in
  let new_col = c_col + del_col in
    (*taking a piece*)
    if(is_valid s new_row new_col && s.board.chess_board.(new_row).(new_col) != Empty) then
      let new_state = copy_state ~capture:true s in
	new_state.board.chess_board.(new_row).(new_col) <- new_state.board.chess_board.(s_row).(s_col);
	new_state.board.chess_board.(s_row).(s_col) <- Empty;
	fix_hash new_state;
	new_state :: accum
	  (*just moving*)
    else if (is_valid s new_row new_col) then 
      let new_state = copy_state s in
	new_state.board.chess_board.(new_row).(new_col) <- new_state.board.chess_board.(s_row).(s_col);
	new_state.board.chess_board.(s_row).(s_col) <- Empty;
	fix_hash new_state;
	move_piece dir s s_row s_col new_row new_col (new_state :: accum)
    else
      []


let bishop_children s row col = 
  let bishop_directions = [NW;NE;SW;SE] in 
  let cll = List.map 
    (fun dir -> move_piece dir s row col row col []) 
    bishop_directions in 
    List.flatten cll


let rook_children s row col = 
  let rook_directions = [N;E;S;W;] in 
  let cll = List.map 
    (fun dir -> move_piece dir s row col row col []) 
    rook_directions in 
    List.flatten cll


let queen_children s row col = 
  let queen_directions = [NW;NE;SW;SE;N;E;W;S;] in 
  let cll = List.map 
    (fun dir -> move_piece dir s row col row col []) 
    queen_directions in 
    List.flatten cll

      
let king_children s row col = 
  assert (s.board.chess_board.(row).(col) = King);
  let king_directions = [NW;NE;SW;SE;N;E;W;S;] in 
  let children = ref [] in
  let move_king dir = 
    let (del_row, del_col) = dir_to_ints dir in
    let new_row = row + del_row in
    let new_col = col + del_col in
      (*taking a piece*)
      if(is_valid s new_row new_col && 
	   s.board.chess_board.(new_row).(new_col) != Empty) then
	let new_state = copy_state ~capture:true s in
	  new_state.board.chess_board.(new_row).(new_col) <- King;
	  new_state.board.chess_board.(row).(col) <- Empty;
	  fix_hash new_state;
	  children := new_state :: !children;
	  (*just moving*)
      else if (is_valid s new_row new_col) then 
	let new_state = copy_state s in
	  new_state.board.chess_board.(new_row).(new_col) <- King;
	  new_state.board.chess_board.(row).(col) <- Empty;
	  fix_hash new_state;
	  children := new_state :: !children
  in
    List.iter move_king king_directions;
    !children


let gen_children s row col p = 
  let children = match p with
      Pawn -> (pawn_children s row col)
    | Knight -> (knight_children s row col)
    | Bishop -> (bishop_children s row col)
    | Rook -> (rook_children s row col)
    | Queen -> (queen_children s row col)
    | King -> (king_children s row col) 
    | Empty -> failwith "can't move an empty square"
  in
    children


let expand_location s row col = 
  match s.board.chess_board.(row).(col) with
      Empty -> []
    | p -> gen_children s row col p


let expand ?(cost=Unit) s g_value = 
  let children = ref [] in 
    for i = 0 to (height s) - 1 
    do
      for j = 0 to (width s) - 1
      do
	let c = expand_location s i j in
	  (*iterate through this list and attach costs to it*)
	  List.iter (
	    fun child -> 
	      let child_cost = match cost with
		  Unit -> 1.0
		| Value -> get_board_value s.board.chess_board.(i).(j) in 
		children := (child, child_cost +. g_value) :: !children;
	  ) c;
      done
    done;
(*
    List.iter (fun (s,g) -> check_hash s) !children;
*)
    !children


let is_goal s = 
  s.remaining_pieces <= 1



let h_fun s = float_of_int s.remaining_pieces


let d_fun s = float_of_int s.remaining_pieces


let hd_fun s = (float_of_int s.remaining_pieces), 
  (float_of_int s.remaining_pieces)


let hash (k: board) = 
  k.hash
(*
  let n_nodes = (Array.length k) * ((Array.length k.(0))+1) in
    Hashtbl.hash_param n_nodes n_nodes k
*)

let sol_length s = s.moves_taken


let hash_key s = s.board


exception Flag


let hash_compare (s1: board) (s2) = 
  if(s1.hash = s2.hash) then (
    try
      for i = 0 to (Array.length s1.chess_board) - 1
      do
	for j = 0 to (Array.length s1.chess_board.(0)) - 1
	do
	  if(not (s1.chess_board.(i).(j) = s2.chess_board.(i).(j)))
	  then raise Flag
	done;
      done;
      true
    with Flag -> false)
  else false

