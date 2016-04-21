(** A collection of inadmissible heuristics for grid world problems
    And the  interfaces which supply them to searches. *)


let get_unit_4way_obstacle_cost w =
  (* Gets the manhattan distance between two points and adds in the number
     of blocked obstacles on a path between them *)
  (fun n ->
     let sx,sy = n.Grid.pos in
     let (gx,gy),admiss_d = List.fold_left
       (fun ((gx,gy),d) (ngx,ngy) ->
	  let nd  = (abs (ngx - sx)) + (abs (ngy - sy)) in
	    if nd < d
	    then (ngx,ngy),nd
	    else (gx,gy),d) ((-1,-1),max_int) w.Grid.goal in
     let blocked = ref 0 in
       for x = sx to gx
       do
	 for y = sy to gy
	 do
	   if w.Grid.blocked.(x).(y)
	   then blocked := !blocked + 1
	 done;
       done;
       float_of_int (admiss_d + !blocked))


let get_unit_4way_obstacle_costdist w =
  let h = get_unit_4way_obstacle_cost w in
    (fun n -> let v = h n in v,v)


let get_unit_8way_obstacle_cost w =
  (* Gets the octile distance between two points and adds in the number
     of blocked obstacles on a path between them *)
  (fun n ->
     let sx,sy = n.Grid.pos in
     let (gx,gy),admiss_d = List.fold_left
       (fun ((gx,gy),d) (ngx,ngy) ->
	  let dx = abs (ngx - sx)
	  and dy = abs (ngy - sy) in
	  let nd =(if dx > dy
		   then (Grid.rt2 *.
			   (float_of_int dx) +. (float_of_int (dx - dy)))
		   else (Grid.rt2 *. (float_of_int dy) +.
			   (float_of_int (dy - dx)))) in
	    if nd < d
	    then (ngx,ngy),nd
	    else (gx,gy),d) ((-1,-1),infinity) w.Grid.goal in
     let blocked = ref 0 in
       for x = sx to gx
       do
	 for y = sy to gy
	 do
	   if w.Grid.blocked.(x).(y)
	   then blocked := !blocked + 1
	 done;
       done;
       admiss_d +. (float_of_int !blocked))


let get_unit_8way_obstacle_costdist w =
  (* Gets the octile distance between two points and adds in the number
     of blocked obstacles on a path between them *)
  (fun n ->
     let sx,sy = n.Grid.pos in
     let (gx,gy),(admiss_h,admiss_d) = List.fold_left
       (fun ((gx,gy),(h,d)) (ngx,ngy) ->
	  let dx = abs (ngx - sx)
	  and dy = abs (ngy - sy) in
	  let nd =(if dx > dy
		   then (Grid.rt2 *. (float_of_int dx) +.
			   (float_of_int (dx - dy)))
		   else (Grid.rt2 *. (float_of_int dy) +.
			   (float_of_int (dy - dx)))) in
	    if nd < h
	    then (ngx,ngy),(nd, dx + dy)
	    else (gx,gy),(h,d)) ((-1,-1),(infinity,max_int)) w.Grid.goal in
     let blocked = ref 0 in
       for x = sx to gx
       do
	 for y = sy to gy
	 do
	   if w.Grid.blocked.(x).(y)
	   then blocked := !blocked + 1
	 done;
       done;
       admiss_h +. (float_of_int !blocked),
     float_of_int (admiss_d + !blocked))


let get_unit_8way_obstacle_dist w =
  let h = get_unit_8way_obstacle_costdist w in
    (fun n -> snd (h n))


let life_cheapest_four_costdist w =
  let costs = Grid.int_costs w
  and h = Grid.height w in
  let blocked = ref 0 in
  let get_cost_dist =
    (fun n (gx,gy) ->
       blocked := 0;
       (** admissible for 4-way board but not 8-way *)
       let x, y = n.Grid.pos in
	 (* must equalize y, either at start or end *)
       let cost_equal_y = Grid.cost_from costs y gy
	 (* can either go straight over or up and over *)
       and over_y = Math.imax y gy
       and dx = abs (gx - x) in
       let cost_of_straight = dx * (h - over_y - 1)
       and cost_of_up_and_over = ((over_y * over_y) +
				    (over_y * (2 - (2 * h))) +
				    (h * h) - (2 * h) + 1) in
	 if cost_of_straight <= cost_of_up_and_over
	 then (* We go straight *)
	   (for x = x to gx
	    do
	      if w.Grid.blocked.(x).(y) then blocked := !blocked + 1
	    done;
	    (float (cost_equal_y + cost_of_straight + !blocked)),
	    (float ((abs (x - gx)) + (abs (y - gy)) + !blocked)))
	 else (* Up and over *)
	   let over = abs (x - gx)
	   and max_y = h - 1 in
	   let up_down = (max_y - y) + (max_y - gy) in
	     for x = x to gx do
	       if w.Grid.blocked.(x).(max_y) then blocked := !blocked + 1
	     done;
	     for y = y to max_y do
	       (if w.Grid.blocked.(x).(y) then blocked := !blocked + 1;
		if w.Grid.blocked.(gx).(y) then blocked := !blocked + 1)
	     done;
	     ((float (cost_equal_y + cost_of_up_and_over + !blocked)),
	      (float (up_down + over + !blocked)))) in
    (fun node ->
       List.fold_right (fun (a,b) (c,d) -> min a c, min b d)
	 (List.map (get_cost_dist node) w.Grid.goal) (infinity,infinity))


let life_cheapest_four_icost w =
  let hd = life_cheapest_four_costdist w in
    (fun n -> fst (hd n))

let life_cheapest_four_idist w =
  let hd = life_cheapest_four_costdist w in
    (fun n -> snd (hd n))

(*** life costs, 8-way.  cheapest goal is one of the nearest! ***)
let life_eight_costdist w =
  let costs = Grid.int_costs w
  and h = Grid.height w
  and blocked = ref 0 in
  let get_cost_dist =
    (fun n (gx,gy) ->
       blocked := 0;
       (** admissible for 8-way actions (and therefore also 4-way) *)
       let x, y = n.Grid.pos in
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
	   let cost = (Grid.cost_from costs y high_y) +
	     (across * costs.(high_y)) +
	     (Grid.cost_from costs high_y gy) in
	   let half_cross = across / 2 in
	     for i = 0 to half_cross
	     do
	       (if w.Grid.blocked.(x+i).(y+i) then blocked := !blocked + 1;
		if w.Grid.blocked.(x+half_cross + i).(max_y - i)
		then blocked := !blocked + 1)
	     done;
	     (float (cost + !blocked)), (float (dx + !blocked))
	 else
	   (* path stays between points *)
	   (for x = x to gx do
	      if w.Grid.blocked.(x).(y) then blocked := !blocked + 1
	    done;
	    (float ((Grid.cost_from costs y gy) + !blocked)),
            (float (dy + !blocked))))
  in
    (fun node ->
       List.fold_right (fun (a,b) (c,d) -> min a c, min b d)
	 (List.map (get_cost_dist node) w.Grid.goal) (infinity,infinity))


let life_cheapest_eight_cost w =
  let hd = life_eight_costdist w in
    (fun n -> fst (hd n))

let life_cheapest_eight_dist w =
  let hd = life_eight_costdist w in
    (fun n -> snd (hd n))

(* Selecting heuristics *)

let select_h w =
  match w.Grid.costs with
    | Grid.Unit -> (match w.Grid.moves with
		      | Grid.Fourway -> get_unit_4way_obstacle_cost w
		      | Grid.Eightway -> get_unit_8way_obstacle_cost w)
    | Grid.Life -> (match w.Grid.moves with
		      | Grid.Fourway -> life_cheapest_four_icost w
		      | Grid.Eightway -> life_cheapest_eight_cost w)

let select_d w =
  match w.Grid.costs with
    | Grid.Unit -> (match w.Grid.moves with
		      | Grid.Fourway -> get_unit_4way_obstacle_cost w
		      | Grid.Eightway -> get_unit_8way_obstacle_dist w)
    | Grid.Life -> (match w.Grid.moves with
		      | Grid.Fourway -> life_cheapest_four_idist w
		      | Grid.Eightway -> life_cheapest_eight_dist w)


let select_hd w =
  match w.Grid.costs with
    | Grid.Unit -> (match w.Grid.moves with
		      | Grid.Fourway -> get_unit_4way_obstacle_costdist w
		      | Grid.Eightway -> get_unit_8way_obstacle_costdist w)
    | Grid.Life -> (match w.Grid.moves with
		      | Grid.Fourway -> life_cheapest_four_costdist w
		      | Grid.Eightway -> life_eight_costdist w)


let scaled_h ?(s = 1.35) w =
  let h = Grid.get_cheapest_h w in
    (fun n -> s *. (h n))

let scaled_d ?(s = 1.35) w =
  let d = Grid.get_cheapest_d w in
    (fun n -> s *. (d n))

(* Interfaces *)
let inadmissible_heuristic_interface_countblocked w lim =
  (Search_interface.make
     ~h:(select_h w)
     ~d:(select_d w)
     ~hd:(Grid.get_cheapest_hd w)
     ~domain_expand:(Grid.make_expand w)
     ~key:Grid.key
     ~equals:Grid.equals
     ~goal_p:(Grid.make_goal_p w)
     ~halt_on:lim
     ~get_sol_length:Grid.sol_length
     (Grid.get_type w)
     (Grid.make_root w)
     (fun _ _ -> false)
     (fun _ -> ()))


let inadmissible_heuristic_interface_scaled w lim =
  (Search_interface.make
     ~h:(scaled_h w)
     ~d:(scaled_d w)
     ~hd:(Grid.get_cheapest_hd w)
     ~domain_expand:(Grid.make_expand w)
     ~key:Grid.key
     ~equals:Grid.equals
     ~goal_p:(Grid.make_goal_p w)
     ~halt_on:lim
     ~get_sol_length:Grid.sol_length
     (Grid.get_type w)
     (Grid.make_root w)
     (fun _ _ -> false)
     (fun _ -> ()))


(* EOF *)
