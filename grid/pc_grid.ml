(** Basically a copy of grids, but used for handling nodes with both
    parent and child pointers  -- Jordan Thayer *)

open Grid


type pc_node = {
  pc_pos : int * int;
  (* root points to self *)
  mutable pc_parent : pc_node;
  (* empty list indicates no children *)
  mutable pc_children : pc_node list;
}

let update_parent node np =
  node.pc_parent <- np

let get_parent node =
  node.pc_parent

let make_pc_root w =
  let rec n = { pc_pos = w.start;
		pc_parent = n;
		pc_children = [];} in n

let make_goal_p w =
  (fun n ->
     List.mem n.pc_pos w.goal)

(* no goal p stays the same *)

(* cost array stays the same *)

let expand_func w =
  let costs = cost_array w
  and moves = (match w.moves with
		 Fourway -> four_moves
	       | Eightway -> eight_moves)
  and width = width w
  and height = height w in
    (fun n g ->
       (** doesn't generate [n]'s parent *)
       let nx, ny = n.pc_pos
       and px, py = n.pc_parent.pc_pos in
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
			  Wrutils.push ({ pc_pos = (x,y);
					pc_parent = n;
					pc_children = [];},
				      if (not (dx = 0)) && (not (dy = 0))
				      then g +. (cost *. rt2 )
				      else (g +. cost))
			    children)
	   moves;
	 n.pc_children <- List.map fst !children;
	 !children)


let expand_func_wparent w =
  let costs = cost_array w
  and moves = (match w.moves with
		 Fourway -> four_moves
	       | Eightway -> eight_moves)
  and width = width w
  and height = height w in
    (fun n g ->
       (** doesn't generate [n]'s parent *)
       let nx, ny = n.pc_pos in
       let cost = costs.(ny)
       and children = ref [] in
	 List.iter (fun (dx, dy) ->
		      let x = nx + dx
		      and y = ny + dy in
			if ((x >= 0) && (x < width) &&
			    (y >= 0) && (y < height) &&
			    (* only check after ensuring legality! *)
			    (not w.blocked.(x).(y))) then
			  Wrutils.push ({ pc_pos = (x,y);
					pc_parent = n;
					pc_children = [];},
				      if (not (dx = 0)) && (not (dy = 0))
				      then g +. (cost *. rt2 )
				      else (g +. cost))
			    children)
	   moves;
	 n.pc_children <- List.map fst !children;
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
       let nx, ny = n.pc_pos
       and px, py = n.pc_parent.pc_pos
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
			  Wrutils.push ({ pc_pos = (x,y);
					pc_parent = n;
					pc_children = [];},
                                      (* child, *)
				      if (not (dx = 0)) && (not (dy = 0))
				      then g +. (cost *. rt2 )
				      else (g +. cost))
			    children)
	   moves;
	 n.pc_children <- List.map fst !children;
	 !children)


let make_verbose e =
  (fun n g ->
     Wrutils.pr "Expanding %d,%d(%d):" (fst n.pc_pos) (snd n.pc_pos) (Math.round g);
     let ns = e n g in
       List.iter (fun (n,_) ->
		    Wrutils.pr " %d,%d(%d)"
		    (fst n.pc_pos) (snd n.pc_pos) (Math.round g))
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
	    Wrutils.push n.pc_pos cache;
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

(* sum_between remains the same *)

(* int_costs remains the same *)

(* cost_from remains the same *)


let key n =
  n.pc_pos


(* print_key remains the same *)

(* check_cost remains the same *)

(* check_path remains the same *)


let path n =
  (** returns an ordered list of (x,y) pairs *)
  let rec get_rest tail n =
    let tail = n.pc_pos::tail in
      if n.pc_parent == n then tail
      else get_rest tail n.pc_parent
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
  (** This may need to have the children passed in at a later date *)
  let check_cost = check_cost board in
    (fun node parent_a parent_b ->
       (check_cost 0. (path { pc_pos = node.pc_pos;
			      pc_parent = parent_a;
			      pc_children = []; })) -.
	 (check_cost 0. (path { pc_pos = node.pc_pos;
				pc_parent = parent_b;
				pc_children = [];})))


(************************ lower bounds ***********************)


(*** life costs, 8-way.  cheapest goal is one of the nearest! ***)
let life_eight_cost_dist w =
  let costs = int_costs w
  and h = height w in
  let get_cost_dist =
    (fun n (gx,gy) ->
       (** admissible for 8-way actions (and therefore also 4-way) *)
       let x, y = n.pc_pos in
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
	   (float (cost_from costs y gy)),
           (float dy))
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
       let x, y = n.pc_pos in
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
       let (x, y) = n.pc_pos in
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
       let x, y = n.pc_pos in
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
  let costs = int_costs w
  and h = height w in
  let get_cost_dist =
    (fun n (gx,gy) ->
       (** admissible for 4-way board but not 8-way *)
       let x, y = n.pc_pos in
	 (* must equalize y, either at start or end *)
       let cost_equal_y = cost_from costs y gy
	 (* can either go straight over or up and over *)
       and over_y = Math.imax y gy
       and dx = abs (gx - x)
       and dy = abs (gy - y) in
       let cost_of_straight = dx * (h - over_y - 1)
       and cost_of_up_and_over = ((over_y * over_y) +
				    (over_y * (2 - (2 * h))) +
				    (h * h) - (2 * h) + 1) in
	 if cost_of_straight <= cost_of_up_and_over then
	   ((float (cost_equal_y + cost_of_straight)),
	    (float ((abs (x - gx)) + (abs (y - gy)))))
	 else
	     (float (cost_equal_y + cost_of_up_and_over)),
       float (dx + dy))
  in
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
       let x, y = n.pc_pos in
       let dx = abs (gx - x) in
       let hor_cost = dx * costs.(Math.imax y gy) in
       let ver_cost = cost_from costs y gy in
	 (float (hor_cost + ver_cost)), (float (dx + (abs (gy - y)))))
*)


let life_nearest_four_cost w =
  let costs = int_costs w in
  let get_cost =
    (fun n (gx,gy) ->
       let x, y = n.pc_pos in
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
       let (x, y) = n.pc_pos in
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
       let x, y = n.pc_pos in
       let total = float (Math.imax (abs (gx - x)) (abs (gy - y)))
       and diag =  float (Math.imin (abs (gx - x)) (abs (gy - y))) in
	 diag *. rt2 +. total -. diag , total) in
    (fun node ->
       List.fold_right (fun (a,b) (c,d) -> min a c, min b d)
	 (List.map (get_cost_dist node) w.goal) (infinity,infinity))


let unit_four_cost_dist w =
  let get_cost_dist =
    (fun n (gx,gy) ->
       (** admissible for 4-way but not 8-way *)
       (* manhattan *)
       let x, y = n.pc_pos in
       let total = float ((abs (gx - x)) + (abs (gy - y))) in
	 total, total) in
    (fun node ->
       List.fold_right (fun (a,b) (c,d) -> min a c, min b d)
	 (List.map (get_cost_dist node) w.goal) (infinity,infinity))


let unit_eight w =
  let get_cost =
    (fun n (gx,gy) ->
       (** admissible for 8-way actions (and therefore also 4-way) *)
       (* based on max dim *)
       let x, y = n.pc_pos in
       let total = float (Math.imax (abs (gx - x)) (abs (gy - y)))
       and diag =  float (Math.imin (abs (gx - x)) (abs (gy - y))) in
	 diag *. rt2 +. total -. diag) in
    (fun node ->
       List.fold_right (fun a c -> min a c)
	 (List.map (get_cost node) w.goal) (infinity))


let unit_four w =
  life_nearest_four_dist w


(******************* selectors *****************)


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


(* get_hd_0 remains the same *)

let sol_length n =
  List.length (path n)

(* equals remains the same *)

(* get_avg_vector remains the same *)

(* reverse_board remains the same *)


let default_interface w lim =
  let rev_board = reverse_board w in
    (Search_interface.make
       ~h:(get_cheapest_h w)
       ~d:(get_cheapest_d w)
       ~hd:(get_cheapest_hd w)
       ~rev_h:(get_cheapest_h rev_board)
       ~rev_d:(get_cheapest_d rev_board)
       ~rev_hd:(get_cheapest_hd rev_board)
       ~domain_expand:(make_expand w)
       ~key:key
       ~equals:equals
       ~goal_p:(make_goal_p w)
       ~halt_on:lim
       ~get_sol_length:sol_length
       (get_type w)
       (make_pc_root w)
       (fun _ _ -> false)
       (fun _ -> ())), [||]


(* EOF *)
