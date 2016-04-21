(* $Id: grid.ml,v 1.4 2004/12/22 20:18:08 ruml Exp ruml $

   basic data structures and algorithms for gridworld path planning
*)


(******** instance data structure *******)


type cost_model = Unit | Life
type move_model = Fourway | Eightway

let not_known = -1

let cost_to_string cm =
  match cm with Unit -> "unit" | Life -> "life"

let move_to_string m =
  match m with Fourway -> "Fourway" | Eightway -> "Eightway"

type board = {
  (* a.(x).(y) with x going to the right and y going upscreen *)
  blocked : bool array array;
  costs : cost_model;
  moves : move_model;
  goal : (int * int) list;
  start : int * int;
  instance: int;
}

type node = {
  pos : int * int;
  (* root points to self *)
  mutable parent : node;
}

let rt2 = sqrt 2.


let height b =
  Array.length b.blocked.(0)

let width b =
  Array.length b.blocked


let eight_moves = [ ( 1,  0);  (* right *)
		    (-1,  0);  (* left *)
		    ( 0,  1);  (* up *)
		    ( 0, -1);  (* down *)
		    ( 1,  1);  (* rightup *)
		    ( 1, -1);  (* rightdown *)
		    (-1,  1);  (* leftup *)
		    (-1, -1);  (* leftdown *)
		  ]

let four_moves = Wrlist.firstn eight_moves 4


(********** search space *********)

let update_parent node new_parent =
  node.parent <- new_parent


let get_parent node =
  node.parent

let clone_node n =
    { pos = n.pos; parent = n.parent; }

let make_root w =
  let rec n = { pos = w.start;
		parent = n; } in
    n


let make_goal_p w =
  (fun n ->
     List.mem n.pos w.goal)

let no_goal_p w =
  (fun _ -> false)


let cost_array w =
  let h = height w in
    Array.init h (match w.costs with
		    Unit -> (Fn.constantly1 1.)
		      (* y=(h-1) is free *)
		  | Life -> (fun row -> float ((h - row) - 1)))


let block_unreachable board =
  let moves = (match board.moves with
		 Fourway -> four_moves
	       | Eightway -> eight_moves) in
  Verb.pe Verb.toplvl "Doing Reachability Test\n";
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
      List.iter (Dpq.insert q)
	(List.filter valid
	   (List.map (fun (dx,dy) -> (ng, (x+dx, y+dy))) moves)) in
    Dpq.insert q (0,board.start);
    while not (Dpq.empty_p q)
    do
      let (g,(x,y)) = Dpq.extract_first q in
	if not (Hashtbl.mem closed (x,y))
	then (touched.(x).(y) <- true;
	      Hashtbl.add closed (x,y) g;
	      make_kids (g,(x,y)))
    done;
    Verb.pe Verb.toplvl "Fililng In Unreachable Cells\n";
    let i = ref 0 in
    for x = 0 to (max_x - 1)
    do
      for y = 0 to (max_y - 1)
      do
	if not touched.(x).(y) then
	  (i:= !i + 1;
	   board.blocked.(x).(y) <- true)
      done
    done;
      Verb.pe Verb.toplvl "%i spaces filled in\n" !i



(* This expand function does not generate the parent *)
let expand_func w =
  let costs = cost_array w
  and moves = (match w.moves with
		 Fourway -> four_moves
	       | Eightway -> eight_moves)
  and width = width w
  and height = height w in
    (fun n g ->
       (** doesn't generate [n]'s parent *)
       let nx, ny = n.pos
       and px, py = n.parent.pos in
       let cost = costs.(ny)
       and children = ref [] in
	 List.iter (fun (dx, dy) ->
		      let x = nx + dx
		      and y = ny + dy in
			if (((x != px) || (y != py)) &&
			    (x >= 0) && (x < width) &&
			    (y >= 0) && (y < height) &&
			    (* only check after ensuring legality! *)
			    (not w.blocked.(x).(y))) then
			  Wrutils.push ({ pos = (x,y);
					parent = n; },
				      if (not (dx = 0)) && (not (dy = 0))
				      then g +. (cost *. rt2 )
				      else (g +. cost))
			    children)
	   moves;
	 !children)


(* This one does *)
let expand_func_wparent w =
  let costs = cost_array w
  and moves = (match w.moves with
		 Fourway -> four_moves
	       | Eightway -> eight_moves)
  and width = width w
  and height = height w in
    (fun n g ->
       (** doesn't generate [n]'s parent *)
       let nx, ny = n.pos in
       let cost = costs.(ny)
       and children = ref [] in
	 List.iter (fun (dx, dy) ->
		      let x = nx + dx
		      and y = ny + dy in
			if ((x >= 0) && (x < width) &&
			    (y >= 0) && (y < height) &&
			    (* only check after ensuring legality! *)
			    (not w.blocked.(x).(y))) then
			  Wrutils.push ({ pos = (x,y);
					parent = n; },
				      if (not (dx = 0)) && (not (dy = 0))
				      then g +. (cost *. rt2 )
				      else (g +. cost))
			    children)
	   moves;
	 !children)


let reverse_expand_func w =
  let costs = cost_array w
  and moves = (match w.moves with
		 Fourway -> four_moves
	       | Eightway -> eight_moves)
  and width = width w
  and height = height w in
    (fun n g ->
       (** doesn't generate [n]'s parent ([n]'s child, in this case) *)
       let nx, ny = n.pos
       and px, py = n.parent.pos
       and children = ref [] in
	 List.iter (fun (dx, dy) ->
		      let x = nx + dx
		      and y = ny + dy in
			if (((x != px) || (y != py)) &&
			    (x >= 0) && (x < width) &&
			    (y >= 0) && (y < height) &&
			    (* only check after ensuring legality! *)
			    (not w.blocked.(x).(y))) then
                        let cost = costs.(y) in
                          (*let rec child = {pos = (x,y); parent = child} in*)
			  Wrutils.push ({ pos = (x,y);
					parent = n; },
                                      (* child, *)
				      if (not (dx = 0)) && (not (dy = 0))
				      then g +. (cost *. rt2 )
				      else (g +. cost))
			    children)
	   moves;
	 !children)


let make_verbose e =
  (fun n g ->
     Wrutils.pr "Expanding %d,%d(%d):" (fst n.pos) (snd n.pos) (Math.round g);
     let ns = e n g in
       List.iter (fun (n,_) ->
		    Wrutils.pr " %d,%d(%d)"
		    (fst n.pos) (snd n.pos) (Math.round g))
	 ns;
       Wrutils.pr "\n";
       ns)


let enable_cache, disable_cache, get_cache, clear_cache, make_caching =
  (** storing expanded positions for debugging *)
  let use_cache = ref false
  and cache = ref [] in
    (fun () -> use_cache := true),
    (fun () -> use_cache := false),
    (fun () -> List.rev !cache),
    (fun () -> cache := []),
    (fun e ->
       if !use_cache then
	 (fun n g ->
	    Wrutils.push n.pos cache;
	    e n g)
       else e)


let make_expand w =
  (* add verbosity and caching according to the dynamic environment *)
  let e = ref (expand_func w) in
    Verb.force 5 (lazy (e := make_verbose !e));
    make_caching !e


let make_expand_wp w =
  let e = ref (expand_func_wparent w) in
    Verb.force 5 (lazy (e := make_verbose !e));
    make_caching !e


(*** utilities ***)
let sum_between a1 a2 b =
  (** returns the sum of [b] values linearly interpolated between [a1]
    and [a2] inclusive *)
  ((a1 + a2) * b) / 2


let int_costs w =
  Array.map (fun x ->
	       assert (Math.integral_p x);
	       truncate x)
    (cost_array w)


let cost_from costs y1 y2 =
  (** cost to move from y1 to y2 *)
  (* won't pay y2's cost *)
  let last = (if y1 > y2 then
		y2 + 1
	      else if y1 < y2 then
		y2 - 1
	      else
		y2) in
    sum_between costs.(y1) costs.(last) (abs (y1 - y2))



let key n =
  n.pos


let key_to_int grid (x,y) =
  (** Converts an x,y position into a unique integer, allowing
      for more tightly packed hashing / storing position informatio in
      alt col data *)
  (* Does the counting from left to right, top to bottom *)
  let width = width grid in
    x + width * y


let int_to_key grid i =
  let width = width grid in
  let y = i / width in
    (i - y * width),y


let print_key k =
  match k with
      (a,b) -> Verb.pe Verb.always "(%i,%i)" a b


let key_to_string k =
  match k with
      (a,b) -> Wrutils.str "(%i,%i)" a b


let check_cost board =
  let costs = cost_array board in
  let rec check_cost accum path =
    match path with
      | [] -> failwith "Empty solution?"
      | [(x2,y2)] -> accum
      | (x1,y1)::(x2,y2)::rest ->
	  let cost = costs.(y1) in
	    check_cost (if x1 <> x2 && y1 <> y2
			then (accum +. rt2 *. cost)
			else (accum +. cost))
	      ((x2,y2)::rest) in
    check_cost



let check_path b path cost =
  let rec check_obst path =
    match path with
	[] -> ()
      | (x,y)::rest ->
	  if b.blocked.(x).(y) then
	    failwith "path uses blocked cell!"
	  else
	    check_obst rest in
  let check_cost = check_cost b in
    check_obst path;
    let c = check_cost 0. path in
      if (abs_float (c -. cost)) > 0.01 then
	(Verb.pe Verb.toplvl "Real: %f <> %f :Reported\n%!" c cost;
	 (*failwith "Path costs do not match reported cost."*));
      if (List.length path) = 0 then
	failwith "path has no elements - it doesn't even start!";
      if (List.hd path) <> b.start then
	failwith "path doesn't start at start";
      if not (List.mem (Wrlist.last path) b.goal) then
	failwith "path doesn't end at goal";
      c


let path n =
  (** returns an ordered list of (x,y) pairs *)
  let rec get_rest tail n =
    let tail = n.pos::tail in
      if n.parent == n then tail
      else get_rest tail n.parent
  in
    get_rest [] n



let print_path cost node =
  let p = path node in
    Verb.pe Verb.toplvl "Cost: %f" cost;
    List.iter (fun k ->
		 Verb.pe Verb.toplvl "\t";
		 print_key k) p;
    Verb.pe Verb.toplvl "\n%!"


let make_diffs board =
  let check_cost = check_cost board in
    (fun node parent_a parent_b ->
       (check_cost 0. (path { pos = node.pos;
			      parent = parent_a;})) -.
	 (check_cost 0. (path { pos = node.pos;
				parent = parent_b;})))


(************************ lower bounds ***********************)


(*** life costs, 8-way.  cheapest goal is one of the nearest! ***)
let life_eight_cost_dist w =
  let costs = int_costs w
  and h = height w in
  let get_cost_dist =
    (fun n (gx,gy) ->
       (** admissible for 8-way actions (and therefore also 4-way) *)
       let x, y = n.pos in
       let dx = abs (gx - x)
       and dy = abs (gy -y) in
	 if dx > dy then
	   (* path might go above higher point *)
	   let max_y = h - 1 in
	   let max_up = Math.imin (max_y - y) (max_y - gy)
	   and extra = dx - dy in
	     (* amount by which we will go above *)
	   let up = Math.imin max_up (extra / 2) in
	   let high_y = (Math.imax y gy) + up
	   and across = extra - (2 * up) in
	   let cost = (cost_from costs y high_y) +
		      (across * costs.(high_y)) +
		      (cost_from costs high_y gy) in
	     (float cost), (float dx)
	 else
	   (* path stays between points *)
	   (float (cost_from costs y gy)), (float dy))
  in
    (fun node ->
       List.fold_right (fun (a,b) (c,d) -> min a c, min b d)
	 (List.map (get_cost_dist node) w.goal) (infinity,infinity))


let life_nearest_eight_cost_dist w =
  let costs = int_costs w
  and h = height w in
  let get_cost_dist =
    (fun n (gx,gy) ->
       (** admissible for 8-way actions (and therefore also 4-way) *)
       let x, y = n.pos in
       let dx = abs (gx - x)
       and dy = abs (gy -y) in
	 if dx > dy then
	   (* path might go above higher point *)
	   let max_y = h - 1 in
	   let max_up = Math.imin (max_y - y) (max_y - gy)
	   and extra = dx - dy in
	     (* amount by which we will go above *)
	   let up = Math.imin max_up (extra / 2) in
	   let high_y = (Math.imax y gy) + up
	   and across = extra - (2 * up) in
	   let cost = (cost_from costs y high_y) +
		      (across * costs.(high_y)) +
		      (cost_from costs high_y gy) in
	     (float cost), (float dx)
	 else
	   (* path stays between points *)
	   (float (cost_from costs y gy)), (float (max dx dy)))
  in
    (fun node ->
       List.fold_right (fun (a,b) (c,d) -> min a c, min b d)
	 (List.map (get_cost_dist node) w.goal) (infinity,infinity))


let life_eight_cost w =
  let dist = life_eight_cost_dist w in
    (fun n ->
       fst (dist n))


let life_eight_dist w =
  let dist_node =
    (fun n (gx, gy) ->
       let (x, y) = n.pos in
	 float (Math.imax (abs (gx - x))
		  (abs (gy - y))))
  in
    (fun node ->
       List.fold_right min
	 (List.map (dist_node node) w.goal) infinity)

(*** life costs, 4-way, cheapest goal ***)


let life_cheapest_four_cost_dist w =
  let costs = int_costs w
  and h = height w in
  let get_cost_dist =
    (fun n (gx,gy) ->
       (** admissible for 4-way board but not 8-way *)
       let x, y = n.pos in
	 (* must equalize y, either at start or end *)
       let cost_equal_y = cost_from costs y gy
	 (* can either go straight over or up and over *)
       and over_y = Math.imax y gy
       and dx = abs (gx - x) in
       let cost_of_straight = dx * (h - over_y - 1)
       and cost_of_up_and_over = ((over_y * over_y) +
				    (over_y * (2 - (2 * h))) +
				    (h * h) - (2 * h) + 1) in
	 if cost_of_straight <= cost_of_up_and_over then
	    ((float (cost_equal_y + cost_of_straight)),
	    (float ((abs (x - gx)) + (abs (y - gy)))))
	 else
	   let over = abs (x - gx)
	   and max_y = h - 1 in
	   let up_down = (max_y - y) + (max_y - gy) in
	     (float (cost_equal_y + cost_of_up_and_over)),
       (float (up_down + over))) in
    (fun node ->
       List.fold_right (fun (a,b) (c,d) -> min a c, min b d)
	 (List.map (get_cost_dist node) w.goal) (infinity,infinity))


let life_nearest_four_cost_dist w =
  let h = height w in
  let get_cost_dist n (gx, gy) =
    let x, y = n.pos in
    let dx = abs (gx - x) and dy = abs (gy - y) in
    let up = float ((h - gy) + (h - y)) *. float dy /. 2. in
    let over = if gy < y then dx * (h - gy) else dx * (h - y) in
    up +. float over, float (dx + dy) in
  (fun node ->
    List.fold_right (fun (a,b) (c,d) -> min a c, min b d)
      (List.map (get_cost_dist node) w.goal) (infinity,infinity))


let life_cheapest_four_cost w =
  let cost = life_cheapest_four_cost_dist w in
    (fun n ->
       fst (cost n))


let life_cheapest_four_dist w =
  let cost = life_cheapest_four_cost_dist w in
    (fun n ->
       snd (cost n))


(*** life costs, 4-way nearest goal.  same goal as for unit costs! ***)

(*
let life_nearest_four_cost_dist w =
  (** admissible for 4-way.  handles life or unit costs. *)
  (* based on manhattan distance.  never closer to go up, over, and down! *)
  let costs = int_costs w
  and (gx, gy) = w.goal in
    (fun n ->
       let x, y = n.pos in
       let dx = abs (gx - x) in
       let hor_cost = dx * costs.(Math.imax y gy) in
       let ver_cost = cost_from costs y gy in
	 (float (hor_cost + ver_cost)), (float (dx + (abs (gy - y)))))
*)


let life_nearest_four_cost w =
  let costs = int_costs w in
  let get_cost =
    (fun n (gx,gy) ->
       let x, y = n.pos in
       let dx = abs (gx - x) in
       let hor_cost = dx * costs.(Math.imax y gy) in
       let ver_cost = cost_from costs y gy in
	 float (hor_cost + ver_cost)) in
    (fun node ->
       List.fold_right min
	 (List.map (get_cost node) w.goal) infinity)


let life_nearest_four_dist w =
  let get_dist =
    (fun n (gx,gy) ->
       (** admissible in the 4-move board but not the 8-move board *)
       (* based on manhattan *)
       let (x, y) = n.pos in
	 float ((abs (gx - x)) + (abs (gy - y)))) in
    (fun node ->
       List.fold_right min
	 (List.map (get_dist node) w.goal) infinity)


(******************** cost and dist under unit costs *******************

  distance = cost, nearest = cheapest
*)


let unit_eight_cost_dist w =
  let get_cost_dist =
    (fun n (gx,gy) ->
       (** admissible for 8-way actions (and therefore also 4-way) *)
       (* based on max dim *)
       let x, y = n.pos in
       let total = float (Math.imax (abs (gx - x)) (abs (gy - y)))
       and diag =  float (Math.imin (abs (gx - x)) (abs (gy - y))) in
	 diag *. rt2 +. total -. diag , total) in
    (fun node ->
       List.fold_right (fun (a,b) (c,d) -> min a c, min b d)
	 (List.map (get_cost_dist node) w.goal) (infinity,infinity))


let get_unit_four_cost_dist n (gx, gy) =
  (** admissible for 4-way but not 8-way *)
  (* manhattan *)
  let x, y = n.pos in
  let total = float ((abs (gx - x)) + (abs (gy - y))) in
    total, total


let unit_four_cost_dist w =
  (fun node ->
     List.fold_right (fun (a,b) (c,d) -> min a c, min b d)
       (List.map (get_unit_four_cost_dist node) w.goal) (infinity,infinity))


let unit_four_cost_dist_between w =
  (fun src dst -> get_unit_four_cost_dist src dst.pos)


let unit_four_cost_between w =
  (fun src dst -> fst (unit_four_cost_dist_between w src dst))


let unit_four_dist_between w =
  (fun src dst -> snd (unit_four_cost_dist_between w src dst))



let unit_eight w =
  let get_cost =
    (fun n (gx,gy) ->
       (** admissible for 8-way actions (and therefore also 4-way) *)
       (* based on max dim *)
       let x, y = n.pos in
       let total = float (Math.imax (abs (gx - x)) (abs (gy - y)))
       and diag =  float (Math.imin (abs (gx - x)) (abs (gy - y))) in
	 diag *. rt2 +. total -. diag) in
    (fun node ->
       List.fold_right (fun a c -> min a c)
	 (List.map (get_cost node) w.goal) (infinity))


let unit_four w =
  life_nearest_four_dist w


(******************* selectors *****************)

let random_h =
  (fun _ -> Random.float 1.)
and random_d =
  (fun _ -> Random.float 1.)


let get_cheapest_h w =
  (** cost of cheapest sol under a node *)
  (match w.costs with
     Unit -> (match w.moves with
		Fourway -> unit_four
	      | Eightway -> unit_eight)
   | Life -> (match w.moves with
		Fourway -> life_cheapest_four_cost
	      | Eightway -> life_eight_cost)) w


let get_cheapest_d w =
  (** distance to cheapest sol under a node *)
  (match w.costs with
     Unit -> (match w.moves with
		Fourway -> unit_four
	      | Eightway -> unit_eight)
   | Life -> (match w.moves with
		Fourway -> life_cheapest_four_dist
	      | Eightway -> life_eight_dist)) w


let get_nearest_h w =
  (** cost of nearest sol under a node *)
  (match w.costs with
     Unit -> (match w.moves with
		Fourway -> unit_four
	      | Eightway -> unit_eight)
   | Life -> (match w.moves with
		Fourway -> life_nearest_four_cost
	      | Eightway -> life_eight_cost)) w


let get_nearest_d w =
  (** distance to nearest sol under a node *)
  (match w.costs with
     Unit -> (match w.moves with
		Fourway -> unit_four
	      | Eightway -> unit_eight)
   | Life -> (match w.moves with
		Fourway -> life_nearest_four_dist
	      | Eightway -> life_eight_dist)) w


let get_cheapest_hd w =
  (match w.costs with
     Unit -> (match w.moves with
		Fourway -> unit_four_cost_dist
	      | Eightway -> unit_eight_cost_dist)
   | Life -> (match w.moves with
		Fourway -> life_cheapest_four_cost_dist
	      | Eightway -> life_eight_cost_dist)) w


let get_nearest_hd w =
  (match w.costs with
     Unit -> (match w.moves with
		Fourway -> unit_four_cost_dist
	      | Eightway -> unit_eight_cost_dist)
   | Life -> (match w.moves with
		Fourway -> life_nearest_four_cost_dist
	      | Eightway -> life_nearest_eight_cost_dist)) w


let get_between_heuristics w = match w.costs with
  | Unit -> begin match w.moves with
      | Fourway ->
	  Some (unit_four_cost_between w),
	  Some (unit_four_dist_between w)
      | _ -> None, None
    end
  | _ -> None, None


let get_hd_0 w =
  (fun n -> 0., 0.)

let get_hd_1 w =
  let gp = make_goal_p w in
  (fun n -> if gp n then 0., 0. else 1., 1.)


let sol_length n =
  List.length (path n)


let equals (x1,y1) (x2,y2) =
  (x1:int) = x2 && (y1:int) = y2

(*
let equals a b =
  a = b
*)

let get_avg_vector model board =
  let domain =  (match board.moves with
		   | Fourway -> (match board.costs with
				   | Unit -> Offline_vectors.Unit4
				   | Life -> Offline_vectors.Life4)
		   | Eightway -> (match board.costs with
				    | Unit -> Offline_vectors.Unit8
				    | Life -> Offline_vectors.Life8))
  and width = Array.length board.blocked
  and height = Array.length board.blocked.(0) in
    Offline_vectors.get_avg_vect width height board.instance domain model


let reverse_board b =
  { blocked = b.blocked;
    costs = b.costs;
    moves = b.moves;
    goal =  [b.start];
    start = List.hd b.goal;
    instance = b.instance}


let get_type b =
  match b.costs with
    | Life -> Search_interface.LGrid
    | Unit -> Search_interface.UGrid

(* EOF *)
