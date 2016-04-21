(**

   Utility functions for doing I/O with solitaire chess instances

*)


let read_instance ch = 
  let rows = Wrio.input_int ch in
  let cols = Wrio.input_int ch in
  let n_pieces = Wrio.input_int ch in
  let initial = Solitaire_chess.make_empty rows cols n_pieces in
    for i = 0 to n_pieces - 1 
    do
      let piece = Wrio.read_token ch in
      let np_row = Wrio.input_int ch in
      let np_col = Wrio.input_int ch in 
      let piece_type = Solitaire_chess.from_string piece in
	assert(Solitaire_chess.is_valid initial np_row np_col);
	initial.Solitaire_chess.board.Solitaire_chess.chess_board.(np_row).(np_col) <-
	  piece_type;
    done;
    initial


let read_from_file f = 
  let inch = open_in f in
  let new_instance = read_instance inch in
    close_in inch;
    new_instance


let write_instance ch inst = 
  Wrio.output_int ch (Solitaire_chess.height inst);
  Printf.fprintf ch " ";
  Wrio.output_int ch (Solitaire_chess.width inst);
  Printf.fprintf ch " ";
  Wrio.output_int ch (inst.Solitaire_chess.remaining_pieces);
  Printf.fprintf ch "\n";
  for i = 0 to (Array.length inst.Solitaire_chess.board.Solitaire_chess.chess_board) - 1
  do
    for j = 0 to (Array.length inst.Solitaire_chess.board.Solitaire_chess.chess_board.(0)) - 1
    do
      match inst.Solitaire_chess.board.Solitaire_chess.chess_board.(i).(j) with 
	  Solitaire_chess.Empty -> ()
	| p -> 
	    Printf.fprintf ch "%s %d %d\n" (Solitaire_chess.piece_string p) i j
    done;
  done
