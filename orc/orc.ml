(**

   Christopher Wilt

   April 19, 2011

   Version of grid path planning where the objective is to get from
   one side of the graph to the other, but the transitions get more
   expensive the closer to the goal you get, but everything is
   somewhat random, making for very complicated problems.

   The basic domain just has blocked and unblocked cells and floating
   point transition costs. In order to get the above advetisement, you
   have to run with an interseting file.

*)

type board =
{
  cells: float array array;
  x_size: int;
  y_size: int;
  min_cost: float;
  max_cost: float;
  p_blocked: float;
}


type move_type = 
    Up 
  | Left 
  | Right
  | Down
  | Initial


let mt_str m = 
  match m with
      Up -> "Up"
    | Left -> "Left"
    | Right -> "Right"
    | Down -> "Down"
    | Initial -> "Initial"


let get_children lmt = 
  match lmt with
      Up -> [Left;Right;Up;]
    | Down -> [Left;Right;Down;]
    | Left -> [Left;Up;Down;]
    | Right -> [Right;Up;Down;]
    | Initial -> failwith "not good code 1xxke"

type state = 
{
  xLoc : int;
  yLoc : int;
  last_move : move_type;
  total_moves : int;
}

let width world = 
  Array.length (world.cells.(0))


let height s = 
  Array.length s.cells

let copy_state s = 
{
  xLoc = s.xLoc;
  yLoc = s.yLoc;
  last_move = s.last_move;
  total_moves = s.total_moves
}

let print_board b os = 
  Printf.fprintf os "size: %d %d\n" b.x_size b.y_size;
  Printf.fprintf os "cost: %f %f\n" b.min_cost b.max_cost;
  Printf.fprintf os "blocked: %f\n" b.p_blocked;
  Array.iter
    (
      fun ar ->
	Array.iter (fun v->
	  Printf.fprintf os "%10.10f " v
	) ar;
	Printf.fprintf os "\n";
    )
    b.cells

let is_goal b s = 
  s.xLoc = b.x_size - 1

let can_move b x y = 
  (*move off the edge of the world*)
  if(x < 0) then false
  else if (y < 0) then false
  else if (x >= b.x_size) then false
  else if (y >= b.y_size) then false
    (*didn't move off the edge of the world*)
  else 
    match classify_float b.cells.(y).(x)
    with 
	FP_normal -> (true)
      | FP_nan -> false
      | _ -> failwith "strange floating point"


let can_move_d b s d = 
  match d with
      Left -> can_move b (s.xLoc - 1) s.yLoc
    | Right -> can_move b (s.xLoc + 1) s.yLoc
    | Up -> can_move b (s.xLoc) (s.yLoc - 1)
    | Down -> can_move b (s.xLoc) (s.yLoc + 1)
    | _ -> failwith "bad lllkdbd"


let make_child b s d = 
  assert(can_move_d b s d);
  match d with 
      Left -> 
	(
	  {xLoc = s.xLoc - 1;yLoc = s.yLoc; last_move = Left;
	   total_moves = s.total_moves + 1}
	)
    | Right -> 
	(
	  {xLoc = s.xLoc + 1;yLoc = s.yLoc; last_move = Right;
	   total_moves = s.total_moves + 1}
	)
    | Up -> 
	(
	  {xLoc = s.xLoc;yLoc = s.yLoc - 1; last_move = Up;
	   total_moves = s.total_moves + 1}
	)
    | Down -> 
	(
	  {xLoc = s.xLoc;yLoc = s.yLoc + 1; last_move = Down;
	   total_moves = s.total_moves + 1}
	)
    | _ -> failwith "bad aljkbhf"


let initial_expand b = 
  let flag = ref true in
  let children = ref [] in
    for i = 0 to b.y_size - 1 do
      if(can_move b 0 i) then 
	(
	  children := ({
			 xLoc = 0;
			 yLoc = i;
			 last_move = Right;
			 total_moves = 1
		       }, 0.0) :: !children;
	  flag := true;
	)
    done;
    !children
      
    


let expand b s g_value = 
  assert(can_move b s.xLoc s.yLoc || s.last_move = Initial);
  match s.last_move with
      Initial -> initial_expand b
    | lmd -> 
	(let children = ref [] in
	 let child_types = get_children lmd in
	   List.iter (fun ct ->
			if can_move_d b s ct then
			  (
			    children := 
			      (make_child b s ct,(g_value +. b.cells.(s.yLoc).(s.xLoc)))
			    :: !children;
			  )
		     ) child_types;
	   !children
	     
	)
	  
let h_fun b s = 
  (float_of_int (b.x_size - s.xLoc)) *. b.min_cost


let d_fun b s = 
  (float_of_int (b.x_size - s.xLoc))

let hash_key s = 
  s.xLoc,s.yLoc

let hash_compare (s1:int*int) s2 = 
  s1 = s2


let to_string k = 
  let x,y = k in
    Printf.sprintf "%d %d" x y

let hd_fun b s = 
  (h_fun b s),(d_fun b s)

let sol_length s = s.total_moves
