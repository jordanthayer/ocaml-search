(**

    @author jtd7
    @since 2011-03-07
*)

let rec solve b =
  if Sudoku_board.completed b
  then Sudoku_board.print_board b
  else
    (let next_location = Sudoku_board.most_constrained b in
     let next_insert = (match next_location with
			  | Sudoku_board.Row index -> -1
			  | Sudoku_board.Column index -> -1) in
       ())

(* EOF *)
