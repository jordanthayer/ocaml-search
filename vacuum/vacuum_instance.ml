(** Instance of a vacuum problem *)

open Vacuum


let block_unreachable board =
  Verb.pr Verb.always "Doing Reachability Test\n";
  let q = Dpq.create_with (fun (g1,d1) (g2,d2) -> g1 <= g2) (-1,(0,0))
  and closed = Hashtbl.create  500
  and min_x = 0
  and min_y = 0
  and max_x = (width board)
  and max_y = (height board) in
  let touched = Array.make_matrix max_x max_y false in
  let valid (_,(x,y)) =
    x >= min_x && x < max_x &&
      y >= min_y && y < max_y &&
      not board.blocked.(x).(y) in
  let make_kids (g,(x,y)) =
    let ng = g + 1 in
      List.iter (Dpq.insert q) (List.filter valid
				  [(ng,(x+1,y)); (ng,(x-1,y));
				   (ng,(x,y+1)); (ng,(x,y-1))]) in
    Dpq.insert q (0,board.start);
    while not (Dpq.empty_p q)
    do
      let (g,(x,y)) = Dpq.extract_first q in
	if not (Hashtbl.mem closed (x,y))
	then (touched.(x).(y) <- true;
	      Hashtbl.add closed (x,y) g;
	      make_kids (g,(x,y)))
    done;
    Verb.pr Verb.always "Fililng In Unreachable Cells";
    for x = 0 to (max_x - 1)
    do
      for y = 0 to (max_y - 1)
      do
	if not touched.(x).(y) then board.blocked.(x).(y) <- true
      done
    done


let uniform_random_blocked p_blocked width height =
  Array.init width
    (fun _ ->
       Array.init height (fun _ ->
			    Math.true_with_prob p_blocked))


let maze_blocked cycle_prob width height =
  (** [maze_blocked cycle_prob width height] creates an matrix of the
      obstacles which should form a maze.*)
  let maze = Mazes.make_maze_dfs cycle_prob width height in
    maze.Grid.blocked


let find_start blocked =
  Verb.pr Verb.always "Finding Starting Location\n";
  let max_x = Array.length blocked
  and max_y = Array.length blocked.(0) in
  let rec go () =
    let sx = Random.int max_x
    and sy = Random.int max_y in
      if not blocked.(sx).(sy) then (sx,sy) else go()
  in
    go ()



let rec place_dirt b count =
  if count = 0 then b
  else
    (let pos = find_start b.blocked in
       if not (List.mem pos b.dirt) && pos != b.start
       then b.dirt <- pos::b.dirt;
       place_dirt b (count - 1))



let make ?(dirts = 2) blocked_fun width height =
  Verb.pr Verb.always "Setting up obstacles\n";
  let blcked = blocked_fun width height in
  let b =
    { blocked = blcked;
      dirt = [];
      start = find_start blcked;
      instance = -1} in
    block_unreachable b;
    place_dirt b dirts


let uniform_random_board ?(dirts = 2) width height p_blocked =
  make ~dirts:dirts (uniform_random_blocked p_blocked) width height


let maze_board ?(dirts = 2) width height cycle_prob =
  make ~dirts:dirts (maze_blocked cycle_prob) width height


(********* IO **********)

let print_cell (x,y) board =
  if board.blocked.(x).(y)
  then '#'
  else (if board.start = (x,y)
	then 'V'
	else (if (List.mem (x,y) board.dirt)
	      then '*'
	      else ' '))

let fprint ch b =
  Wrutils.pf ch "%d %d\nBoard:\n" (height b) (width b);
  for y = (height b) - 1 downto 0 do
    for x = 0 to (width b) - 1 do
      Wrutils.pf ch "%c" (print_cell (x,y) b)
    done;
    Wrutils.newline ch
  done;
  Wrutils.pf ch "\n"



let save board location =
  Wrio.with_outfile location
    (fun ch -> fprint ch board)


let read instance ch =
  let h = Wrio.input_int ch
  and w = Wrio.input_int ch
  and dirts = ref []
  and start = ref (-1,-1) in
    Wrio.skip_through_str ch "Board:\n";
    let b = Array.init w (fun _ -> Array.make h false) in
      for y = h - 1 downto 0 do
	for x = 0 to w - 1
	do
	  (let c = input_char ch in
	     b.(x).(y) <- c = '#';
	     if c = 'V' then start := (x,y);
	     if c = '*' then dirts := (x,y)::!dirts)
	done;
	Wrio.skip_line ch;
      done;
      { blocked = b;
	start = !start;
	dirt = !dirts;
	instance = instance}


let prob_path_to_instance pp =
  let parts = Str.split (Str.regexp "/") pp in
    try
      int_of_string (List.hd (List.rev parts))
    with Failure _ -> -1


let load path =
  let instance = prob_path_to_instance path in
    Wrio.with_infile path (read instance)

(************************************************************)

let make_maze_insts width height cycle_prob dirt num =
  for num = 1 to num do
    let i = maze_board ~dirts:dirt width height cycle_prob in
    let attrs = [ "obstacles", "maze";
		  "width", string_of_int width;
		  "height", string_of_int height;
		  "cycle_prob", string_of_float cycle_prob;
		  "dirt", string_of_int dirt;
		  "num", string_of_int num;
		]
    in
    let path = Rdb.path_for "./data/vacuum_instances" attrs in
      save i path
  done


let make_uniform_insts width height obst_prob dirt num =
  for num = 1 to num do
    let i = uniform_random_board ~dirts:dirt width height obst_prob in
    let attrs = [ "obstacles", "uniform";
		  "width", string_of_int width;
		  "height", string_of_int height;
		  "obst_prob", string_of_float obst_prob;
		  "dirt", string_of_int dirt;
		  "num", string_of_int num;
		]
    in
    let path = Rdb.path_for "./group/data/vacuum_instances" attrs in
      save i path
  done

(* EOF *)
