(* $Id: instance.ml,v 1.1 2004/12/22 20:18:19 ruml Exp ruml $

   code helpful for testing (mostly informal)
*)


open Grid


(******** making boards **********)


let uniform_random_blocked width height p_blocked =
  Array.init width
    (fun _ ->
       Array.init height (fun _ ->
			    Math.true_with_prob p_blocked))


let make_board b c m s g =
  (** ensures start and goal are not blocked! *)
  (let (x, y) = s in
     b.(x).(y) <- false);
  (let (x, y) = g in
     b.(x).(y) <- false);
  { blocked = b;
    costs = c;
    moves = m;
    goal = [g];
    start = s;
    instance = not_known;
  }


let make_uniform_board ?(start = None) ?(goal = None) width height
    p_blocked c m =
  let ss = match start with
    | None -> (0,0)
    | Some (x,y) -> x,y
  and gs = match goal with
    | None -> (width - 1,0)
    | Some (x,y) -> x,y in
  (** not necessarily feasible! *)
  let blocked = uniform_random_blocked width height p_blocked in
    make_board blocked c m ss gs


let feasible_p solver b =
  match ((solver { b with costs = Unit; } ())) with
      (None,_,_,_,_,_) -> false
    | _ -> true


let feasible_board ?(start = None) ?(goal = None) solver w h p cost_model
    move_model =
  Wrutils.eval_until (fun () ->
		      (*Wrutils.pr "trying...%!";*)
		      make_uniform_board ~start:start ~goal:goal
			w h p cost_model move_model)
    (feasible_p solver)



let life_four_board solver w h p =
  feasible_board solver w h p Life Fourway

let unit_four_board solver w h p =
  feasible_board solver w h p Unit Fourway

let unit_eight_board solver w h p =
  feasible_board solver w h p Unit Eightway

(**
  * Makes a board from the map provided by player stage.
  *
  *   map = int array where:
  *     -1 = free
  *      0 = unknown
  *      1 = blocked
  *   map_width = the width of the map in cells
  *   start = (int * int), the x,y location of robot
  *   goal = (int * int), the x,y location of the goal
  *)
let make_playerstage_board map map_width start goal =
  let board = Array.make_matrix map_width ((Array.length map)/map_width) false in
    Array.iteri (fun i a ->
                   match a with
                       -1 -> board.(i mod map_width).(i / map_width) <- false
                     |  _ -> board.(i mod map_width).(i / map_width) <- true)
      map;
    make_board board Unit Eightway start goal


(*
 * Makes a board for a player/stage domain.
 *
 * params:
 *  expanded_map : (bool array) array : map of obstacles expanded by the robot's
 *                                      radius. True = Blocked.
 *  start : int * int : the starting location of the robot (x,y)
 *  goal  : int * int : the goal location of the robot (x,y)
 *)
let make_playerstage_board2 expanded_map start goal =
  make_board expanded_map Unit Eightway start goal


(********* I/O ********)


let unit_cost_tag = "Unit"
let life_cost_tag = "Life"
let four_tag = "Four-way"
let eight_tag = "Eight-way"
 (* obsolete *)
let specified_cost_tag = "These specified costs:"


let tag_of_costs c =
  match c with
    Unit -> unit_cost_tag
  | Life -> life_cost_tag

let tag_of_moves m =
  match m with
    Fourway -> four_tag
  | Eightway -> eight_tag


let costs_of_tag tag =
  if tag = unit_cost_tag then Unit
  else if tag = life_cost_tag then Life
  else failwith ("unrecognized cost tag: " ^ tag)

let moves_of_tag tag =
  if tag = four_tag then Fourway
  else if tag = eight_tag then Eightway
  else failwith ("unrecognized moves tag: " ^ tag)


let fprint ch b =
  Wrutils.pf ch "%d %d\nBoard:\n" (height b) (width b);
  for y = (height b) - 1 downto 0 do
    for x = 0 to (width b) - 1 do
      Wrutils.pf ch "%c" (if b.blocked.(x).(y) then '#' else ' ')
    done;
    Wrutils.newline ch
  done;
  Wrutils.pf ch "%s\n" (tag_of_costs b.costs);
  Wrutils.pf ch "%s\n" (tag_of_moves b.moves);
  Wrutils.pf ch "%d %d\t" (fst b.start) (snd b.start) ;
  List.iter (fun (x,y) -> Wrutils.pf ch "%d %d\t" x y) b.goal;
  Wrutils.pf ch "\n"


let save path b =
  Wrio.with_outfile path (fun ch -> fprint ch b)


let rec get_goals x lst accu =
  match lst with
      h::tl ->
	(match x with
	     None -> get_goals (Some h) tl accu
    | Some v -> get_goals None tl ((v,h)::accu))
    | [] -> accu


let read seedinst instance ch =
  if seedinst then begin
    Verb.pe Verb.debug "Reading a seed instance\n%!";
    Grid_seedinst.read ch
  end else begin
    Verb.pe Verb.debug "Reading a normal instance\n%!";
    let h = Wrio.input_int ch in
    let w = Wrio.input_int ch in
      Wrio.skip_through_str ch "Board:\n";
      let b = Array.init w (fun _ -> Array.make h false) in
	for y = h - 1 downto 0 do
	  for x = 0 to w - 1 do
	    b.(x).(y) <- (input_char ch) = '#'
	  done;
	  Wrio.skip_line ch;
	done;
	let c = costs_of_tag (input_line ch) in
	let m = moves_of_tag (input_line ch) in
	  match Wrio.read_ints ch with
	      sx::sy::tl ->
		{ blocked = b;
		  costs = c;
		  moves = m;
		  start = (sx,sy);
		  goal = (get_goals None tl []);
		  instance = instance; }
	    | _ -> failwith "oops"
  end


let read_old_format move_model ch =
  let h = Wrio.input_int ch in
  let w = Wrio.input_int ch in
    Wrio.skip_through_str ch "Board:\n";
    let b = Array.init w (fun _ -> Array.make h false) in
      for y = h - 1 downto 0 do
	for x = 0 to w - 1 do
	  b.(x).(y) <- (input_char ch) = '#'
	done;
	Wrio.skip_line ch;
      done;
      let tag = input_line ch in
      let c = (if tag = unit_cost_tag then
		 Unit
	       else if tag = life_cost_tag then
		 Life
	       else if tag = specified_cost_tag then
		 failwith ("custom costs no longer supported")
	       else
		 failwith ("unrecognized tag: " ^ tag)) in
      let start = Scanf.fscanf ch " %d %d" Fn.gather2 in
      let goal = Scanf.fscanf ch " %d %d" Fn.gather2 in
	{ blocked = b;
	  costs = c;
	  moves = move_model;
	  start = start;
	  goal = [goal];
	  instance = not_known;
	}


let prob_path_to_instance pp =
  let parts = Str.split (Str.regexp "/") pp in
    try
      int_of_string (List.hd (List.rev parts))
    with Failure _ -> -1


let prob_path_to_prob pp =
  let parts = Str.split (Str.regexp "/") pp in
    try
      (List.nth (List.rev parts) 3)
    with Failure _ -> ""


let load path =
  let instance = prob_path_to_instance path in
  let seedinst =
    Str.string_match (Str.regexp ".*seedinst.*") path 0 in
  Wrio.with_infile path (read seedinst instance)


let io_test () =
  let p = "/tilde/ruml/foo.board"
  and b1 = make_uniform_board 30 10 0.3 Unit Fourway in
    save p b1;
    let b2 = load p in
      b1 = b2


(* EOF *)
