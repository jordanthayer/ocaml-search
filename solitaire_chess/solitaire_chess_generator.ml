(**

   Makes solitaire chess instances

*)

module SC = Solitaire_chess

let chess_nature = 
  [|
    SC.Pawn; SC.Pawn; SC.Pawn; SC.Pawn; SC.Pawn; SC.Pawn; SC.Pawn; SC.Pawn;
    SC.King; SC.Queen; SC.Bishop; SC.Knight; SC.Rook; SC.Bishop; SC.Knight; 
    SC.Rook;
  |], "chess"

let uniform = 
  [|
    SC.Pawn; SC.King; SC.Queen; SC.Bishop; SC.Knight; SC.Rook;
  |], "uniform"


let expand_assoc ass_lst = 
  let new_list = ref [] in
    List.iter (
      fun (item, count) ->
	for i = 0 to count - 1 
	do
	  new_list := item :: !new_list;
	done;

    ) ass_lst; Array.of_list (!new_list);;

let weak = 
  (expand_assoc
     [
       SC.Pawn, 50; 
       SC.King, 20;     
       SC.Bishop, 20; 
       SC.Knight, 20; 
       SC.Queen, 1;     
       SC.Rook, 1;
     ]), "weak"


let slow = 
  (expand_assoc
     [
       SC.Pawn, 3; 
       SC.King, 20;     
       SC.Bishop, 20; 
       SC.Knight, 20; 
       SC.Queen, 3;     
       SC.Rook, 3;
     ]), "slow"


let make_instance rows cols total_pieces piece_distribution= 
  let new_instance = SC.make_empty rows cols total_pieces in

  let rec get_random_empty () = 
    let first_int = Random.int (SC.height new_instance) in
    let second_int = Random.int (SC.width new_instance) in
      match new_instance.SC.board.SC.chess_board.(first_int).(second_int)
      with SC.Empty -> (first_int, second_int)
	| _ -> get_random_empty () in

  let rec get_random_piece row col = 
    let next_piece = Wrarray.random_elt piece_distribution in
      if(row = 0) then
	match next_piece with
	    SC.Pawn -> get_random_piece row col
	  | p -> p 
      else next_piece in 
    

    for i = 0 to total_pieces - 1
    do
      let (this_row, this_col) = get_random_empty () in
      let this_piece_type = get_random_piece this_row this_col in
	assert (new_instance.SC.board.SC.chess_board.(this_row).(this_col) = SC.Empty);
	new_instance.SC.board.SC.chess_board.(this_row).(this_col) <-
	  this_piece_type;
    done;
    new_instance


let path_root = User_paths.instance_root


let path domain_name domain_attrs instance_name =
  (** [path domain_name domain_attrs pdb_name pdb_size] gets the path
      to the pattern database. *)
  Rdb.path_for (path_root ^ domain_name)
    ((domain_attrs
      @ ["num", instance_name;]))


let make_instances rows cols total_pieces piece_distribution
    n_instances = 
  for i = 0 to n_instances - 1
  do

    let attrs = [
      "piece_distribution",snd piece_distribution;
      "rows",string_of_int rows;
      "cols",string_of_int cols;
      "n_pieces",string_of_int total_pieces;
    ] in 
    let instance_name = path "solitaire_chess" attrs (string_of_int i) in
    let new_instance = make_instance rows cols total_pieces 
      (fst piece_distribution) in
    let outch = open_out instance_name in
      Solitaire_chess_io.write_instance outch new_instance;
      close_out outch;
  done


(**

Solitaire_chess_generator.make_instances
4 4 4 
Solitaire_chess_generator.chess_nature 10;;

*)
