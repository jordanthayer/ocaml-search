(** Makes an image of the grid board and optionally "lights up" a set
    of cells based on a simple input file.

    @author eaburns
    @since 2010-05-24
*)

open Eps


let rec color_cells img = function
    (** [color_cells img cell_files]  *)
  | cells_file :: tl ->
      let inch = open_in cells_file in
      let points = ref [] in
	begin try
	  while true do
	    let p = Scanf.fscanf inch " %d, %d, %d: (%d, %d)"
	      (fun r g b x y  -> ((r, g, b), x, y)) in
	      points := p :: !points
	  done;
	  failwith "Impossible"
	with End_of_file ->
	  List.iter (fun (c, x, y) -> Image.draw_point img x y c) !points;
	  close_in inch;
	  color_cells img tl
	end
  | [] -> ()


let board_image board =
  (** [board_image board] gets an image containing the board. *)
  let w = Array.length board.Grid.blocked
  and h = Array.length board.Grid.blocked.(0) in
  let image = Image.create w h in
    for i = 0 to w - 1 do
      for j = 0 to h - 1 do
	if board.Grid.blocked.(i).(j)
	then Image.draw_point image i j (192, 192, 192)
      done
    done;
    image


let output_board ?(cell_files=[]) board_file output_file =
  (** [output_board ?cell_files board_file output_file] outputs the
      board to the given file. *)
  let board = Grid_instance.load board_file in
  let img = board_image board in
    color_cells img cell_files;
    Image.export "eps" img output_file


let main () =
  let board_file = ref ""
  and output_file = ref ""
  and cell_files = ref [] in
    Arg.parse [("--cells",
		Arg.String (fun s -> cell_files := s :: !cell_files),
		"file containing x,y pairs of cells to color";);
	      ]
      (fun s ->
	 if !board_file = ""
	 then board_file := s
	 else
	   if !output_file = ""
	   then output_file := s
	   else failwith "Too many arguments")
      "grid_image [--cells <cell file>] <board file> <output file>";
    if !board_file = "" then failwith "Board file unspecified";
    if !output_file = "" then failwith "Output file unspecified";
    output_board ~cell_files:!cell_files !board_file !output_file

let _ = main ()
