(* $Id: instance.ml,v 1.1 2005/01/30 17:00:59 ruml Exp ruml $

   path planning with obstacles as occupancy grid

   inspired by ARA* robot problem.


   KNOWN BUGS:

   ramifications of cell_offset: max pos is cell_width * (dimension - 1)
   simulation hacks
*)


(****** problem instance *******)


(* cm per cell side *)
let cell_size = 40
(* cells are offset from position grid by this much *)
let cell_offset = cell_size / 2

type problem = {
  (* o[x][y] with x increasing eastward and y increasing northward.  note
     that position in cm must be divided by cell_size before indexing into the
     array! *)
  obstacles : bool array array;
  (* x, y (in cm), heading (degrees, 0 is east) (assume stopped) *)
  start : int * int * int;
  (* must get within cell_size/2 (assume stopped) *)
  goal : int * int * int;
}


let reverse_problem p =
  (** Reverses a specific problem instance *)
  {obstacles = p.obstacles;
   start = p.goal;
   goal = p.start}


let max_x w =
  Array.length w.obstacles

let max_y w =
  Array.length w.obstacles.(0)


let matx a =
  Array.length a

let maty a =
  Array.length a.(0)


let legal_pos obstacles x y =
  (x >= 0) &&
  (y >= 0) &&
  (* first cell goes from 0 to cell_size, so truncate *)
  let x = (x + cell_offset) / cell_size
  and y = (y + cell_offset) / cell_size in
    (x < (matx obstacles)) &&
    (y < (maty obstacles)) &&
    (not obstacles.(x).(y))


(************ solution is a list of states *********)


type state = {
  (* cm *)
  x : int;
  y : int;
  (* degrees, 0 = east, goes counter-clockwise.  always positive *)
  heading : int;
  (* cm/sec *)
  speed : int;
}

let print_state s =
  Verb.pe Verb.always "(%i,%i)"
    (s.x /  cell_size) (s.y / cell_size)


(******** parameters of search space ********)


(* robot parameters *)
(* cm/sec *)
let max_speed = 300.
let min_speed = 0.
(* cm/sec^2 *)
let max_accel = 150.
(* degrees/sec.  don't bother modeling rotational inertia. *)
let max_turn = 90.


(* in cm *)
let position_delta = 40.
(* in degrees *)
let heading_delta = 360. /. 32.
(* cm/sec *)
let speed_delta = (max_speed -. min_speed) /. 15.


(* heading res of 16 and speed res of 15 = 240 states per position
   Likhachev et al use ~370 states per position (eg, 33 x 11)

   their world is 228 x 236


   Runs.test2 ~seed:6553 25 25 0.2 3.0;;
*)


let fround a =
  (** rounds the given floating point number to the nearest integral value,
    returning a floating point number *)
  if a >= 0. then
    snd (modf (a +. 0.5))
  else
    snd (modf (a -. 0.5))


let int_at_res x res =
  Math.round ((fround (x /. res)) *. res)


let int_of_pos x =
  int_at_res x position_delta

let int_of_heading x =
  let x = int_at_res x heading_delta in
    (x + 360) mod 360


let int_of_speed x =
  int_at_res x speed_delta


let check_parameters () =
  Verb.pe 4 "Speeds %.2f to %.2f in increments of %.2f, accel %.2f.\n"
    min_speed max_speed speed_delta max_accel;
  Verb.pe 4 "Max turn is %.2f, heading increments of %.2f.\n"
    max_turn heading_delta;
  Verb.pe 4 "Position in increments of %.2f.\n" position_delta;
  (* can we jump over obstacles? *)
  Verb.pe 4 "position delta %.2f, cell_size %d\n" position_delta cell_size;
  if position_delta > (float cell_size) then
    failwith "can jump over obstacles";
  (* can we turn at high speed? *)
  let min_time = position_delta /. max_speed in
  let max_turn_at_max_speed = max_turn *. min_time in
    Verb.pe 4 "min time %.3f, max_turn_at_speed %.2f, heading_delta %.2f.\n"
      min_time max_turn_at_max_speed heading_delta;
    if max_turn_at_max_speed <= heading_delta then
      (* decrease heading_delta, decrease max_speed, increase position_delta *)
      failwith "can't change heading at full speed";
    (* can we speed up at high speed? *)
    let almost_min_time = position_delta /.
			    (max_speed -. (speed_delta /. 2.)) in
    let max_accel_at_speed = max_accel *. almost_min_time in
      Verb.pe 4 "almost min time %.2f, max_accel_at_speed %.2f, speed_delta %.2f\n"
	almost_min_time max_accel_at_speed speed_delta;
      if max_accel_at_speed < speed_delta then
	(* decrease speed_delta, increase max_speed,
	   increase max_accel, increase position_delta *)
	failwith "can't reach full speed";
      (* print parameters *)
      Verb.pe 4 "Checked robot parameters.\n%!"


let _ =
  Verb.with_level 3 check_parameters


let distance x1 y1 x2 y2 =
  (** takes ints to a float *)
  let dx = x1 - x2
  and dy = y1 - y2 in
    sqrt (float ((dx * dx) + (dy * dy)))


let near =
  let delta = truncate (ceil (position_delta /. 2.)) in
    (fun a b ->
       (abs (a - b)) <= delta)


(************* drawing instances *********)


let offset = 20.


let make_scale maxx maxy =
  (** returns function mapping to PS position *)
  (* want max dim to fit page *)
  (fun _ ->
     failwith "yapp is fail")
    (*
  let perx = ((Psout.in2pt 8.5) -. (2. *. offset)) /. (float maxx)
  and pery = ((Psout.in2pt 11.) -. (2. *. offset)) /. (float maxy) in
  let per = min perx pery in
    (fun x ->
       (float x) *. per)*)

let make_scale_cell w =
  make_scale (max_x w) (max_y w)

let make_scale_pos w =
  make_scale ((max_x w) * cell_size) ((max_y w) * cell_size)


let world_coords w =
  let s = make_scale_cell w in
    offset, offset,
    offset +. (s (max_x w)),
    offset +. (s (max_y w))


let world_box w =
  failwith "yapp is busted"(*
  let x1,y1,x2,y2 = world_coords w in
    Box.of_coords (x1 -. 5.) (y1 -. 5.) (x2 +. 5.) (y2 +. 5.)*)


let mark_state pso w (x, y, h) color =
  failwith "psout is busted"
    (*
  let s = make_scale_pos w in
  let x = offset +. (s x) +. (s cell_offset)
  and y = offset +. (s y) +. (s cell_offset)
  and r = 7. in
    Psout.circle pso ~color:color x y r;
    let rads = Math.to_radians h in
    let fx = x +. (r *. (cos rads))
    and fy = y +. (r *. (sin rads)) in
      Psout.line pso x y fx fy
    *)

let draw_world pso w =
  failwith "psout is broken"
    (*
  (let x1,y1,x2,y2 = world_coords w in
     Psout.polyline pso [x1, y1;
			 x2, y1;
			 x2, y2;
			 x1, y2;
			 x1, y1]);
  (let s = make_scale_cell w in
     Array.iteri (fun x row ->
		    Array.iteri (fun y filled ->
				   if filled then
				     Psout.frect pso
				       (offset +. (s x))
				       (offset +. (s y))
				       (s 1)
				       (s 1))
		    row)
       w.obstacles);
  mark_state pso w w.start Psout.light_gray;
  mark_state pso w w.goal Psout.black
    *)

let with_ps_world path w f =
  failwith "Temporarily hosed"(*
  (** write PS of [w] to [path], then calls [f] on pso and scaler *)
  Psout.to_file path (world_box w) "Robot Navigation" Psout.Portrait false
    (fun pso ->
       draw_world pso w;
       f pso)*)


let draw_instance path w =
  (** generates a postscript file at [path] *)
  with_ps_world path w Fn.no_op1


(**************** making instances **************)


(**** uniform ****)


let make_uniform x y p =
  Array.init x (fun _ -> Array.init y (fun _ -> Math.true_with_prob p))


(**** clumpy ****)

let pair2i b (x, y) =
  (x * (maty b)) + y

let i2pair b i =
  let ymax = maty b in
  let y = i mod ymax
  and x = i / ymax in
    x,y


let choose_free_cell board =
  let maxx = Array.length board
  and maxy = maty board in
    Wrutils.eval_until (fun () -> Random.int maxx, Random.int maxy)
      (fun (x,y) -> not board.(x).(y))


let rec add_neighbor cells board =
  (** adds an obstacle to board in a free position next to one of the
    [cells] (which will be modified to add the obstacle and remove any cells
    without free neighbors *)
  let loc = Uset.random cells in
  let x,y = i2pair board loc
  and xmax = matx board
  and ymax = maty board in
  let rec test_neighbor = function
      [] ->
	Uset.remove cells loc;
	add_neighbor cells board
    | (dx,dy)::rest ->
	let nx = x + dx
	and ny = y + dy in
	  if (nx < 0) || (ny < 0) ||
	    (nx = xmax) || (ny = ymax) ||
	    board.(nx).(ny) then
	      test_neighbor rest
	  else
	    (board.(nx).(ny) <- true;
	     Uset.insert cells (pair2i board (nx, ny)))
  in
    test_neighbor (Wrlist.shuffle [ 0,1; 0,-1; 1,0; -1,0;])


let make_clumpy x y p =
  let board = Array.make_matrix x y false
  and locs = Uset.make (x * y)
  and n = Math.round ((float (x * y)) *. p) in
  let seeds = min n (min (max 10 (n / 20))
		       80) in
    Wrutils.ntimes (fun () ->
		    let x, y = choose_free_cell board in
		      board.(x).(y) <- true;
		      Uset.insert locs (pair2i board (x,y)))
      seeds;
    Wrutils.ntimes (fun () -> add_neighbor locs board)
      (n - seeds);
    board


(**** liney ****)


let draw_line max_dist b =
  let x1,y1 = choose_free_cell b in
  let x2,y2 = Wrutils.eval_until (fun () -> choose_free_cell b)
		(fun (x3,y3) ->
		   (distance x1 y1 x3 y3) < ((float (min (matx b) (maty b))) *.
					       max_dist))
  in
    (let x1,y1,x2,y2 = (if x1 <= x2 then x1,y1,x2,y2 else x2,y2,x1,y1) in
     let slope = Math.div (y2 - y1) (x2 - x1) in
       for x = x1 to x2 do
	 let y = y1 + (Math.round ((float (x - x1)) *. slope)) in
	   b.(x).(y) <- true
       done);
    (let x1,y1,x2,y2 = (if y1 <= y2 then x1,y1,x2,y2 else x2,y2,x1,y1) in
     let slope = Math.div (x2 - x1) (y2 - y1) in
       for y = y1 to y2 do
	 let x = x1 + (Math.round ((float (y - y1)) *. slope)) in
	   b.(x).(y) <- true
       done)


let make_liney line_length x y n =
  if not (Math.integral_p n) then failwith "non-integral number of lines";
  let board = Array.make_matrix x y false in
    Wrutils.ntimes (fun () -> draw_line line_length board) (Math.round n);
    board


(**** complete problem (w/ start and goal) ****)


let choose_free_pos board minx maxx =
  let maxy = ((maty board) - 1) * cell_size in
    Wrutils.eval_until (fun () ->
			int_of_pos (float (minx + (Random.int (maxx-minx)))),
			int_of_pos (float (Random.int maxy)))
      (fun (x,y) ->
	 legal_pos board x y)


let near_obs b x y =
  let maxx = matx b
  and maxy = maty b in
  let check_y x =
    (let y = y - 1 in
       (y >= 0) && b.(x).(y)) ||
    b.(x).(y) ||
    (let y = y + 1 in
       (y < maxy) && b.(x).(y))
  in
    (let x = x - 1 in
       (x >= 0) && check_y x) ||
    (check_y x) ||
    (let x = x + 1 in
       (x < maxx) && check_y x)


let expand_obstacles problem =
  let b = problem.obstacles in
  let b2 = Array.map Array.copy b in
    Array.iteri (fun x col ->
		   Array.iteri (fun y _ ->
				  if near_obs b x y then
				    b2.(x).(y) <- true)
		   col) b;
    Verb.pe Verb.toplvl "Obstacles Expanded\n%!";
    { problem with obstacles = b2; }


let make_instance_opposite obs_func x y p =
  let obs = obs_func x y p
  and maxx = (x - 1) * cell_size in
  let sx, sy = choose_free_pos obs 0 (maxx / 10)
  and gx, gy = choose_free_pos obs ((maxx * 9)/10) maxx in
    { obstacles = obs;
      start = sx, sy, 90;
      goal = gx, gy, 90; }


let make_instance obs_func x y p =
  let obs = obs_func x y p
  and maxx = x * cell_size in
  let sx, sy = choose_free_pos obs 0 maxx
  and gx, gy = choose_free_pos obs 0 maxx
  and sh = Random.int 360
  and gh = Random.int 360 in
    { obstacles = obs;
      start = sx, sy, sh;
      goal = gx, gy, gh; }


let show_instance i =
  failwith "psout busted"
    (*
  Wrfname.with_temp_file ~suffix:".ps"
    (fun path ->
       (let x,y,h = i.start
	and gx,gy,gh = i.goal in
	  Wrutils.pr "Start is at %d,%d@%d.\n" x y h;
	  Wrutils.pr "Goal is at %d,%d@%d.\n%!" gx gy gh);
       draw_instance path i;
       Psout.view path)*)


let test_instance ob_f x y p =
  show_instance (make_instance ob_f x y p)


(**************** instance I/O ***************)


let fprint ch p =
  (let b = p.obstacles in
     Wrutils.pf ch "%d %d\n" (matx b) (maty b);
     Wrutils.pf ch "Obstacles:\n";
     for y = (maty b) - 1 downto 0 do
       for x = 0 to (matx b) - 1 do
	 Wrutils.pf ch "%c" (if b.(x).(y) then '#' else ' ')
       done;
       Wrutils.newline ch
     done);
  (let x,y,h = p.start in
     Wrutils.pf ch "%d %d %d\n" x y h);
  (let x,y,h = p.goal in
     Wrutils.pf ch "%d %d %d\n" x y h)


let save path p =
  Wrio.with_outfile path (fun ch -> fprint ch p)


let read ch =
  let x = Wrio.input_int ch in
  let y = Wrio.input_int ch in
    Wrio.skip_through_str ch "Obstacles:\n";
    let b = Array.init x (fun _ -> Array.make y false) in
      for y = y - 1 downto 0 do
	for x = 0 to x - 1 do
	  b.(x).(y) <- (input_char ch) = '#'
	done;
	Wrio.skip_line ch;
      done;
      let start = Scanf.fscanf ch " %d %d %d" Fn.gather3 in
      let goal = Scanf.fscanf ch " %d %d %d" Fn.gather3 in
	{ obstacles = b;
	  start = start;
	  goal = goal; }


let load path =
  Wrio.with_infile path read



(********* check solution **********)


let check_sol w s t =
  (* speed required is within accel *)
  (* direction required is within turn *)
  (* no obstacles *)
  (* time is accurate *)
  ()


(************* draw solution *********)


let draw_path pso w p mark_states =
  failwith "psout busted"(*
  let s = make_scale_pos w in
    Psout.polyline pso ~color:Psout.black ~dash:([5.;3.],0.)
      (List.map (fun state ->
		   offset +. (s state.x) +. (s cell_offset),
		   offset +. (s state.y) +. (s cell_offset))
	 p);
    if mark_states then
      let n = List.length p in
	Wrlist.iteri (fun i state ->
			if ((Math.divisible i 10) &&
			    (i > 5) &&
			    (i < (n-5))) then
			  mark_state pso w (state.x, state.y, state.heading)
			    Psout.med_gray)
	  p*)


let draw_sol path w sol mark_states =
  (** generates a postscript file *)
  with_ps_world path w
    (fun pso ->
       draw_path pso w sol mark_states)


let show_sol w s mark_states =
  failwith "psout is busted, yapp is busted"
    (*
  (* generate PS to temp file *)
  (* call viewer *)
  (* remove file *)
  Wrfname.with_temp_file ~suffix:".ps"
    (fun path ->
       draw_sol path w s mark_states;
       Psout.view path)*)


let print_sol states =
  Wrlist.iteri (fun i s ->
		  Wrutils.pr "%d: %d,%d heading %d speed %d\n"
		  i s.x s.y s.heading s.speed)
    states;
  flush stdout


(* EOF *)
