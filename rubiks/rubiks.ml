(** Puzzle representation for the Rubik's Cube *)

(********************************* Types **********************************)

type color =
  | Blue
  | Green
  | Red
  | Orange
  | White
  | Yellow

type face =
  | Top
  | Bottom
  | Left
  | Right
  | Front
  | Back
  | No_Face

type cubie =
  | Dummy
  | Edge of (color * color)
  | Corner of (color * color * color)

type state = {
  corners : cubie array;
  edges : (cubie array) array;
  last_moved : face;
}

type problem = {
  size : int;      (* a single dimension of the cube, not # cubies *)
  initial : state;
}

(* corner cubie indices *)
let top_front_left = 0
and top_front_right = 1
and top_back_right = 2
and top_back_left = 3
and bottom_front_left = 4
and bottom_front_right = 5
and bottom_back_right = 6
and bottom_back_left = 7
(* edge cubie indecies *)
and top_front = 0
and top_right = 1
and top_back = 2
and top_left = 3
and left_front = 4
and front_right = 5
and right_back = 6
and back_left = 7
and bottom_front = 8
and bottom_right = 9
and bottom_back = 10
and bottom_left = 11
(* inside of an edge array, cubies are enumarated
   left to right, front to back *)

(******************************* Creation Functions ***********************)

let face_to_color = function
  | Top -> Blue
  | Bottom -> Green
  | Left -> Red
  | Right -> Orange
  | Front -> White
  | Back -> Yellow
  | No_Face -> failwith "Can't map non-existant face to color"

let make_edge f1 f2 =
  Edge (face_to_color f1, face_to_color f2)

let make_corner f1 f2 f3 =
  Corner (face_to_color f1, face_to_color f2, face_to_color f3)


let make_blank dim =
  (* Constructs the blank Rubik's cube *)
  let num_cubies = (Math.int_exp dim 3) - 1 - 6 in
    {size = num_cubies;
     initial = {corners= Array.create 8 Dummy;
		edges = Array.init 12 (fun i -> Array.create (dim - 2) Dummy);
		last_moved = No_Face;};}

let make_edge_array edge_length ind =
  if ind = top_front then Array.create edge_length (make_edge Top Front)
  else if ind = top_right then Array.create edge_length (make_edge Top Right)
  else if ind = top_back then Array.create edge_length (make_edge Top Back)
  else if ind = top_left then Array.create edge_length (make_edge Top Left)
  else if ind = left_front then Array.create edge_length (make_edge Left Front)
  else if ind = front_right then Array.create edge_length (make_edge Front Right)
  else if ind = right_back then Array.create edge_length (make_edge Right Back)
  else if ind = back_left then Array.create edge_length (make_edge Back Left)
  else if ind = bottom_front then Array.create edge_length (make_edge Bottom Front)
  else if ind = bottom_right then Array.create edge_length (make_edge Bottom Right)
  else if ind = bottom_back then Array.create edge_length (make_edge Bottom Back)
  else if ind = bottom_left then Array.create edge_length (make_edge Bottom Left)
  else failwith "Unrecognized edge!"


let make_solved dim =
  (* Constructs a solved rubiks cube *)
  let num_cubies = (Math.int_exp dim 3) - 1 - 6
  and edge_length = dim - 2 in
    { size = num_cubies;
      initial =
	{ corners = [| make_corner Top Front Left;
		       make_corner Top Front Right;
		       make_corner Top Back Right;
		       make_corner Top Back Left;
		       make_corner Bottom Front Left;
		       make_corner Bottom Front Right;
		       make_corner Bottom Back Right;
		       make_corner Bottom Back Left;|];
	  edges = Array.init 12 (make_edge_array edge_length) ;
	  last_moved = No_Face;};}

(**************************************************************************)

let rec test_array ?(ind = 0) ar1 ar2 =
  (* helper function for goal test *)
  if ind >= (Array.length ar1)
  then true
  else (if ar1 = ar2
	then (test_array ~ind:(ind + 1) ar1 ar2)
	else false)


let make_goal_test dim =
  let goal_cube = (make_solved dim).initial in
    (fun s -> test_array s.corners goal_cube.corners &&
       Wrarray.fold_left2
       (fun acc ed_p ed_g ->
	  (test_array ed_p ed_g) && acc) true s.edges goal_cube.edges)


(* EOF *)
