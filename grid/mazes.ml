(** For generating maze-like grid instances
    Jordan - October 09 *)
open Grid
open Grid_instance

(* Note that all of the generated maze problems are by definition solvable *)

let make_gridded_board width height =
  (** Generates a gridded off board for use in maze creation*)
  let sx,sy = (width / 2), (height / 2)
  and g = 1,1 in
  let s = (if sx mod 2 = 0 then sx + 1 else sx),
	   (if sy mod 2 = 0 then sy + 1 else sy) in
  let b = Array.create_matrix width height false in
    for x = 0 to (width - 1)
    do
	for y = 0 to (height - 1)
	do
	  (if (x mod 2) = 0 then b.(x).(y) <- true;
	   if (y mod 2) = 0 then b.(x).(y) <- true;
	   if x = 0 then b.(x).(y) <- true;
	   if x = (width - 1) then b.(x).(y) <- true;
	   if y = 0 then b.(x).(y) <- true;
	   if y = (height - 1) then b.(x).(y) <- true;)
	done
    done;
    let sx,sy = s
    and gx,gy = g in
      assert (not (b.(sx).(sy)));
      assert (not (b.(gx).(gy)));
    { blocked = b;
      costs = Grid.Unit;
      moves = Grid.Fourway;
      goal = [g];
      start = s;
      instance = -1}


let legal max_x max_y (x,y) =
  x >= 0 && y >=0 && x < max_x && y < max_y


let permute lst =
  let ar = Array.of_list lst in
    Wrarray.permute ar;
    Array.to_list ar


let get_children max_x max_y =
  let l = legal max_x max_y in
    (fun (x,y) ->
       let kids = [(x,y+2); (x,y-2);(x+2,y);(x-2,y)] in
	 permute (List.filter l kids))


let remove_wall board (px,py) (cx,cy) =
  let dx = cx - px
  and dy = cy - py in
  let x = px + dx / 2
  and y = py + dy / 2 in
    try
      if (not board.(px).(py)) && (not board.(cx).(cy))
      then board.(x).(y) <- false
       with Invalid_argument v ->
	 failwith (Wrutils.str "Parent: %i , %i Child:%i , %i Wall: %i , %i"
		     px py cx cy x y)


(*** Method 1: Depth First Search *)
let make_maze_dfs ?(i = 1) cycle_prob width height =
  let width = if width mod 2 = 0 then width + 1 else width
  and height = if height mod 2 = 0 then height + 1 else height in
  let board = make_gridded_board width height
  and closed_list = Hashtbl.create 100
  and stack = Stack.create ()
  and get_kids = get_children width height in
  let rec consider_children n kids = (* giggity *)
    match kids with
	[] -> ()
      | c::tl ->
	  if not (Hashtbl.mem closed_list c)
	  then (remove_wall board.blocked n c;
		Hashtbl.add closed_list c c;
		Stack.push c stack)
	  else (if (Random.float 1.) < cycle_prob then
	       remove_wall board.blocked n c);
	  consider_children n tl in
    Stack.push board.start stack;
    Verb.pe Verb.always "Starting maze generation\n";
    while not (Stack.is_empty stack)
    do
      (let n = Stack.pop stack in
	 consider_children n (get_kids n))
    done;
    { blocked = board.blocked;
      costs = board.costs;
      moves = board.moves;
      goal = [ ((width - 1) - 1, (height -1) - 1)];
      start = 1,1;
      instance = i;}



(*** Method 2: Kruskal *)

(*** Method 3: Prim *)

(*** Recursive Division *)

let scale_maze m w =
(** Scaling - Take an already created maze, and scale it up so that halways
     have a width [w] *)
  let blocked = Array.create_matrix ((width m) * w) ((height m) * w) false in
    for x = 0 to ((width m) - 1)
    do
      for y = 0 to ((height m - 1))
      do
	(for i = 0 to (w - 1)
	 do
	   for j = 0 to (w - 1)
	   do
	     blocked.(x*w+i).(y*w+j) <- m.blocked.(x).(y)
	   done
	 done)
      done
    done;
    { blocked = blocked;
      costs = m.costs;
      moves = m.moves;
      goal = (match m.goal with [x,y] -> [x * w, y * w]
		| _ -> failwith "Maze with multiple goals?");
      start = w,w;
      instance = m.instance}

(* EOF *)
