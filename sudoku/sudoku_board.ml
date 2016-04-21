(**

    @author jtd7
    @since 2011-03-07
*)

type board_element = {
  contents : int array;
  mutable remaining : int list;
}

type board = {
  column : board_element array;
  row : board_element array;
  block : board_element array;
}

type location =
  | Column of int
  | Row of int

let board_size = 9
and empty = 0
let init_list = Wrlist.range board_size


(* Initialization *)
let init_board_element  _ =
  (** initialize a board element for use in play *)
  {contents = Array.create board_size empty;
   remaining = init_list; }


let init_board () =
  (** initializes the entire board for play *)
  { column = Array.init board_size init_board_element;
    row = Array.init board_size init_board_element;
    block = Array.init board_size init_board_element; }


(* Board manipulation *)
let get_block_id column row =
  (* converts column and row into a block id *)
  (row / 3) * 3 + (column / 3)


let insert board column row element =
  (* inserts element into board at column x row.
     raises an exception if the move was illegal *)
  (assert (element > 0  && element <= board_size);
   let block = get_block_id column row in
     (* assert that the insert is legal *)
     assert (List.mem element board.column.(column).remaining);
     assert (List.mem element board.row.(row).remaining);
     assert (List.mem element board.block.(block).remaining);
     (* update the board *)
     board.column.(column).contents.(row) <- element;
     board.row.(row).contents.(column) <- element;
     board.block.(block).contents.(row * 3 + column) <- element;
     (* filter the remaining lists *)
     board.column.(column).remaining <- List.filter (fun e -> e <> element)
       board.column.(column).remaining;
     board.row.(row).remaining <- List.filter (fun e -> e <> element)
       board.row.(row).remaining;
     board.block.(block).remaining <- List.filter (fun e -> e <> element)
       board.block.(block).remaining)

let remove board column row = failwith "Not yet implemented"
(* Other board functions *)
let completed board =
  Array.fold_left (fun accum ele -> accum && ele.remaining = []) true
    board.row


let most_constrained board =
  failwith "Not Implemented"


let least_constraining board location =
  failwith "Not Implemented"


(* I/O *)
let insert_by_index board index element =
  (** ideally, this is only called from I/O.  Everything else will be
      handled by the column / row insert. This is why it skips the insert
      if the element is 0 or less, as this is the blank character for input*)
  if element > 0
  then (let row = index / 9
	and column = index mod 9 in
	  insert board column row element)


let board_from_string str =
  (* str must be a space delimited string *)
  let elements = Str.split Wrstr.whitespace_regexp str in
    if (List.length elements) <> (board_size * board_size)
    then failwith "Input does not match board size"
    else (let board = init_board () in
	    ignore
	      (List.fold_left
		 (fun accum ele ->
		    insert_by_index board accum (int_of_string ele);
		    accum + 1) 0 elements);
	    board)


let print_board board =
  (* prints a visual representation of the board *)
  for i = 0 to (board_size - 1) do
    (Array.iter (fun e -> if e > 0
		 then Printf.printf " %i" e
		 else Printf.printf " _") board.row.(i).contents;
     Printf.printf "\n")
  done


(* EOF *)
