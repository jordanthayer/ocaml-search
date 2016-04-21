(* Contains all reversed heuristics for the grid domain 
   Jordan April 8
*)

open Grid

(* Life *)
let life_eight_cost_dist_rev w =
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
	   (float (cost_from costs y gy)), 
           (float dy))
  in
    (fun node ->
       List.fold_right (fun (a,b) (c,d) -> min a c, min b d)
	 (List.map (get_cost_dist node) [w.start]) (infinity,infinity))


let life_nearest_eight_cost_dist_rev w =
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
	 (List.map (get_cost_dist node) [w.start]) (infinity,infinity))


let life_cheapest_four_cost_dist_rev w =
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
	 (List.map (get_cost_dist node) [w.start]) (infinity,infinity))


let life_nearest_four_cost_dist_rev w = 
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
	 (List.map (get_cost_dist node) [w.start]) (infinity,infinity))


let life_nearest_four_cost_rev w =
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
	 (List.map (get_cost node) [w.start]) infinity)


let life_nearest_four_dist_rev w =
  let get_dist = 
    (fun n (gx,gy) ->
       (** admissible in the 4-move board but not the 8-move board *)
       (* based on manhattan *)
       let (x, y) = n.pos in
	 float ((abs (gx - x)) + (abs (gy - y)))) in
    (fun node ->
       List.fold_right min
	 (List.map (get_dist node) [w.start]) infinity)


let life_eight_cost_rev w =
  let dist = life_eight_cost_dist_rev w in
    (fun n ->
       fst (dist n))

let life_eight_dist_rev w =
  let dist_node =
    (fun n (gx, gy) ->
       let (x, y) = n.pos in
	 float (Math.imax (abs (gx - x))
		  (abs (gy - y))))
  in
    (fun node ->
       List.fold_right min
	 (List.map (dist_node node) [w.start]) infinity)


let life_cheapest_four_cost_rev w =
  let cost = life_cheapest_four_cost_dist_rev w in
    (fun n ->
       fst (cost n))


let life_cheapest_four_dist_rev w =
  let cost = life_cheapest_four_cost_dist_rev w in
    (fun n ->
       snd (cost n))


(***************************************************************************)

(* Unit *)
let unit_eight_cost_dist_rev w =
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
	 (List.map (get_cost_dist node) [w.start]) (infinity,infinity))


let unit_four_cost_dist_rev w =
  let get_cost_dist =
    (fun n (gx,gy) ->
       (** admissible for 4-way but not 8-way *)
       (* manhattan *)
       let x, y = n.pos in
       let total = float ((abs (gx - x)) + (abs (gy - y))) in
	 total, total) in
    (fun node ->
       List.fold_right (fun (a,b) (c,d) -> min a c, min b d)
	 (List.map (get_cost_dist node) [w.start]) (infinity,infinity))

(***************************************************************************)

let runit_eight w =
  life_eight_dist_rev w

    
let runit_four w =
  life_nearest_four_dist_rev w


(* Getters *)

let get_cheapest_rh w =
  (** cost of cheapest sol under a node *)
  (match w.costs with
     Unit -> (match w.moves with
		Fourway -> runit_four
	      | Eightway -> runit_eight)
   | Life -> (match w.moves with
		Fourway -> life_cheapest_four_cost_rev
	      | Eightway -> life_eight_cost_rev)) w
    
    
let get_cheapest_rd w =
  (** distance to cheapest sol under a node *)
  (match w.costs with
     Unit -> (match w.moves with
		Fourway -> runit_four
	      | Eightway -> runit_eight)
   | Life -> (match w.moves with
		Fourway -> life_cheapest_four_dist_rev
	      | Eightway -> life_eight_dist_rev)) w


let get_nearest_rh w =
  (** cost of nearest sol under a node *)
  (match w.costs with
     Unit -> (match w.moves with
		Fourway -> runit_four
	      | Eightway -> runit_eight)
   | Life -> (match w.moves with
		Fourway -> life_nearest_four_cost_rev
	      | Eightway -> life_eight_cost_rev)) w
    
    
let get_nearest_rd w =
  (** distance to nearest sol under a node *)
  (match w.costs with
     Unit -> (match w.moves with
		Fourway -> runit_four
	      | Eightway -> runit_eight)
   | Life -> (match w.moves with
		Fourway -> life_nearest_four_dist_rev
	      | Eightway -> life_eight_dist_rev)) w

    
let get_cheapest_rhd w =
  (match w.costs with
     Unit -> (match w.moves with
		Fourway -> unit_four_cost_dist_rev
	      | Eightway -> unit_eight_cost_dist_rev)
   | Life -> (match w.moves with
		Fourway -> life_cheapest_four_cost_dist_rev
	      | Eightway -> life_eight_cost_dist_rev)) w

    
let get_nearest_rhd w =
  (match w.costs with
     Unit -> (match w.moves with
		Fourway -> unit_four_cost_dist_rev
	      | Eightway -> unit_eight_cost_dist_rev)
   | Life -> (match w.moves with
		Fourway -> life_nearest_four_cost_dist_rev
	      | Eightway -> life_nearest_eight_cost_dist_rev)) w


(* EOF *)
