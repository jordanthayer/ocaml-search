(* $Id: dynamic.ml,v 1.3 2005/01/27 02:05:12 ruml Exp ruml $

   path planning with dynamics

   always move to an adjacent position (ie, on position_delta grid).  this
   means that multiple headings can produce the same position.  However, if
   the trajectory is used in a postprocess to recover commands, this is OK.

*)


open Drn_instance


type  node = {
  state : state;
  problem : problem;
  mutable parent : node;
}


let update_parent node new_parent =
  node.parent <- new_parent


let make_root problem =
  let x, y, h = problem.start in
  let x = int_of_pos (float x)
  and y = int_of_pos (float y)
  and h = int_of_heading (float h) in
  let rec r = { state = { x = x;
			  y = y;
			  heading = h;
			  speed = 0; };
		problem = problem;
		parent = r; } in
    r


(********* generating children **********)


let make_child x y h s p =
  { state = { x = x;
	      y = y;
	      heading = h;
	      speed = s; };
    problem = p.problem;
    parent = p; }


let get_dxy heading =
  (** given a heading, returns the dx dy to the next grid point.  also
    returns the distance to that point *)
  let cut = Math.to_degrees (atan 0.5)
  and d = Math.round position_delta
  and p = position_delta
  and r2 = position_delta *. (sqrt 2.) in
    if (heading < cut) || (heading > (360 - cut)) then
      d, 0, p
    else if heading < (90 - cut) then
      d, d, r2
    else if heading < (90 + cut) then
      0, d, p
    else if heading < (180 - cut) then
      -d, d, r2
    else if heading < (180 + cut) then
      -d, 0, p
    else if heading < (270 - cut) then
      -d, -d, r2
    else if heading < (270 + cut) then
      0, -d, p
    else
      d, -d, r2


let for_speeds f =
  Wrutils.iter_for_f min_speed (max_speed +. (speed_delta /. 4.)) speed_delta
    (fun s ->
       f (int_of_speed s))

let for_headings f =
  Wrutils.iter_for_f 0. 360. heading_delta
    (fun h ->
       f (int_of_heading h))


let h_diff h1 h2 =
  let diff = abs (h1 - h2) in
    if diff > 180 then
      360 - diff
    else diff


let h_diff_from_proper =
  let d = truncate (ceil (heading_delta /. 2.)) in
    (fun h ->
       let h = h mod 45 in
	 not ((h < d) ||
	      ((45 - h) < d)))


let make_possible_transitions verbose heading speed =
  (* actions are accel/deccel and turn. given current, what are possible
     future values (and position and time deltas).  very hacky:

     1. compute time until next position at current speed and heading
     2. compute turns possible in that time
     3. assume heading changes instantly, speed changes linearly?
     4. compute next position
     5. compute time to that position (+ time to adjust heading?)
  *)

  let _,_,dist_at_heading = get_dxy heading
  and next = ref [] in
    for_speeds
      (fun s ->
	 let mean_speed = (float (s + speed)) /. 2. in
	 let time_at_heading = dist_at_heading /. mean_speed
	 and accel = float (abs (s - speed)) in
	 let time_for_accel = accel /. max_accel in
	   if time_for_accel <= time_at_heading then
	     for_headings
	       (fun h ->
		  let dx,dy,dist_at_h = get_dxy h in
		  let time_at_h = dist_at_h /. mean_speed
		  and turn = float (h_diff h heading) in
		  let time_for_turn = turn /. max_turn in
		    if time_for_turn <= time_at_h then
		      if s = 0 then
			Wrutils.push (0, 0, h, s,
				    max time_for_accel time_for_turn)
			  next
		      else
			let dx = if s < 0 then -dx else dx
			and dy = if s < 0 then -dy else dy
			and penalty = (if h_diff_from_proper h then
					 time_at_h *. 0.01 else 0.) in
			  Wrutils.push (dx, dy, h, s, time_at_h +. penalty)
			    next)
	   else
	     if verbose then
	       Wrutils.pr "Can't accelerate %f to %d (taking %f) in %f.\n"
		 accel s time_for_accel time_at_heading);
    Wrlist.remove_duplicates !next



let reverse_possible_transitions verbose heading speed =
  (* actions are accel/deccel and turn. given current, what are possible
     future values (and position and time deltas).  very hacky:

     1. compute time until next position at current speed and heading
     2. compute turns possible in that time
     3. assume heading changes instantly, speed changes linearly?
     4. compute next position
     5. compute time to that position (+ time to adjust heading?)
  *)
  let heading = (if heading > 180
		 then (heading - 180)
		 else (heading + 180)) in
  let _,_,dist_at_heading = get_dxy heading
  and next = ref [] in
    for_speeds
      (fun s ->
	 let mean_speed = (float (s + speed)) /. 2. in
	 let time_at_heading = dist_at_heading /. mean_speed
	 and accel = float (abs (s - speed)) in
	 let time_for_accel = accel /. max_accel in
	   if time_for_accel <= time_at_heading then
	     for_headings
	       (fun h ->
		  let dx,dy,dist_at_h = get_dxy h in
		  let time_at_h = dist_at_h /. mean_speed
		  and turn = float (h_diff h heading) in
		  let time_for_turn = turn /. max_turn in
		    if time_for_turn <= time_at_h then
		      if s = 0 then
			Wrutils.push (0, 0, h, s,
				      max time_for_accel time_for_turn)
			  next
		      else
			let dx = if s < 0 then -dx else dx
			and dy = if s < 0 then -dy else dy
			and penalty = (if h_diff_from_proper h then
					 time_at_h *. 0.01 else 0.) in
			  Wrutils.push (dx, dy, h, s, time_at_h +. penalty)
			    next)
	   else
	     if verbose then
	       Wrutils.pr "Can't accelerate %f to %d (taking %f) in %f.\n"
		 accel s time_for_accel time_at_heading);
    Wrlist.remove_duplicates !next


let get_possible_transitions =
  (* Verb.pr 4 "Precomputing transitions...%!"; *)
  let t = Hashtbl.create 1000 in
    for_speeds
      (fun speed ->
	 for_headings
	 (fun heading ->
	    (* Wrutils.pr "%d,%d\n" heading speed; *)
	    Hashtbl.add t (heading, speed)
	    (make_possible_transitions false heading speed)));
    (* Verb.pr 4 "done.\n%!"; *)
    (fun h s ->
       try
	 Hashtbl.find t (h, s)
       with Not_found -> failwith (Wrutils.str "no transitions from %d,%d" h s))



let get_reverse_transitions =
  (* Verb.pr 4 "Precomputing transitions...%!"; *)
  let t = Hashtbl.create 1000 in
    for_speeds
      (fun speed ->
	 for_headings
	 (fun heading ->
	    (* Wrutils.pr "%d,%d\n" heading speed; *)
	    Hashtbl.add t (heading, speed)
	    (reverse_possible_transitions false heading speed)));
    (* Verb.pr 4 "done.\n%!"; *)
    (fun h s ->
       try
	 Hashtbl.find t (h, s)
       with Not_found -> failwith (Wrutils.str "no transitions from %d,%d" h s))



let print_possible_transitions h s =
  let next = make_possible_transitions true h s in
    Wrutils.pr "From heading %d and speed %d, %d states:\n"
      h s (List.length next);
    Wrlist.iteri (fun i (dx,dy,h,s,dt) ->
		    Wrutils.pr "  %d: %d,%d heading %d speed %d dt %.3f\n"
		    i dx dy h s dt)
      next


let expand node time =
  let state = node.state
  and obstacles = node.problem.obstacles
  and children = ref [] in
    (* Wrutils.pr "Expanding %d,%d,%d,%d at %.3f.\n%!"  state.x state.y
       state.heading state.speed time; *)
    List.iter (fun (dx, dy, h, s, dt) ->
		 let x = state.x + dx
		 and y = state.y + dy in
		   if legal_pos obstacles x y then
		     Wrutils.push ((make_child x y h s node), time +. dt)
		       children)
      (get_possible_transitions state.heading state.speed);
    !children


let pred node time =
  let state = node.state
  and obstacles = node.problem.obstacles
  and children = ref [] in
    (* Wrutils.pr "Expanding %d,%d,%d,%d at %.3f.\n%!"  state.x state.y
       state.heading state.speed time; *)
    List.iter (fun (dx, dy, h, s, dt) ->
		 let x = state.x + dx
		 and y = state.y + dy in
		   if legal_pos obstacles x y then
		     Wrutils.push ((make_child x y h s node), time +. dt)
		       children)
      (get_possible_transitions state.heading state.speed);
    !children


(******** other search interface ********)


let goal_p node =
  let s = node.state in
    (s.speed = 0) &&
    let gx, gy, gh = node.problem.goal in
      (near s.x gx) &&
      (near s.y gy) &&
      ((h_diff s.heading gh) < (Math.round heading_delta))


(*
  all the algorithm has to do is get the robot on top of the goal, the
  actual approach vector is unimportant.
*)
let relaxed_goal_p node =
  let s = node.state in
    let gx, gy, gh = node.problem.goal in
      (near s.x gx) &&
      (near s.y gy)


let key node =
  node.state

let key_to_string state =
  Wrutils.str "(%i,%i,%i,%i)" state.x state.y state.heading state.speed


let extract_path sol =
  match sol with
    None -> failwith "no path found!"
  | Some (goal, t) ->
      let rec do_next tail node =
	if node.parent == node then
	  node.state::tail
	else
	  do_next (node.state::tail) node.parent
      in
	(do_next [] goal), t


(************ a simple heuristic *************)


let straight_line_distance node =
  let s = node.state
  and gx, gy, _ = node.problem.goal in
  let dist_to_goal = distance s.x s.y gx gy
  and braking_time = (float s.speed) /. max_accel in
  let braking_distance = ((float s.speed) *. braking_time) /. 2. in
    if braking_distance > dist_to_goal then
      braking_distance +. (braking_distance -. dist_to_goal)
    else dist_to_goal


let straight_line_time node =
  (straight_line_distance node) /. max_speed


(************ a complex heuristic *************)


let make_approx_time_h problem =
  let get_d = Non_dynamic.make_get_distance problem in
    (fun node ->
       let s = node.state in
       let d = get_d s.x s.y in
	 d /. max_speed)


let make_approx_d problem =
  let get_d = Non_dynamic.make_get_d problem in
    (fun node ->
       let s = node.state in
	 float (get_d s.x s.y))


let make_logger () =
  (** prevent anytime A* from writing thousands of lines - only write if
    improved significantly *)
  let start = Sys.time ()
  and threshold = ref infinity in
    (fun n f e g ->
       (** cost, length, expanded, gen, time *)
       if f <= !threshold then
	 let t = (Sys.time ()) -. start in
	   Wrutils.pr "%f\t%d\t%d\t%f\n%!" f e g t;
	   threshold := f *. 0.999)


(********** bugsy stores own parent pointers *********)


let make_broot problem =
  let x, y, h = problem.start in
    { x = int_of_pos (float x);
      y = int_of_pos (float y);
      heading = int_of_heading (float h);
      speed = 0; }


let make_bexpand problem =
  let obstacles = problem.obstacles in
    (fun state ->
       let children = ref [] in
	 List.iter (fun (dx, dy, h, s, dt) ->
		      let x = state.x + dx
		      and y = state.y + dy in
			if legal_pos obstacles x y then
			  Wrutils.push ({ x = x;
					y = y;
					heading = h;
					speed = s; }, dt)
			    children)
	   (get_possible_transitions state.heading state.speed);
	 !children)


let make_hd problem =
  let get_h = Non_dynamic.make_get_distance problem
  and get_d = Non_dynamic.make_get_d problem in
    (fun s ->
       let d = get_h s.x s.y in
	 (d /. max_speed), (float_of_int (get_d s.x s.y)))


let hd problem =
  let d = (make_approx_d problem)
  and h = (make_approx_time_h problem) in
    (fun n -> h n, d n)


(**************************** Interfaces *****************************)
let default_interface problem lim =
  let problem = expand_obstacles problem in
  let rp = reverse_problem problem in
    (Search_interface.make
       ~h:(make_approx_time_h problem)
       ~d:(make_approx_d problem)
       ~hd:(hd problem)
       ~rev_h:(make_approx_time_h rp)
       ~rev_d:(make_approx_d rp)
       ~rev_hd:(hd rp)
       ~domain_expand:expand
       ~predecessor:pred
       ~key:key
       ~key_print:key_to_string
       ~goal_p:goal_p
       ~halt_on:lim
       ~p_update:update_parent
       (*~get_sol_length: *)
       ~equals:(fun a b -> a = b)
       Search_interface.Robot
       (make_root problem)
       (fun _ _ -> false)
       (fun _ -> ()))

(* uses the relaxed goal predicate *)
let lazy_interface problem lim =
  let problem = expand_obstacles problem in
    (Search_interface.make
       ~h:(make_approx_time_h problem)
       ~d:(make_approx_d problem)
       ~hd:(hd problem)
       ~domain_expand:expand
       ~predecessor:pred
       ~key:key
       ~key_print:key_to_string
       ~goal_p:relaxed_goal_p
       ~halt_on:lim
       ~p_update:update_parent
       (*    ~rev_h:
	     ~rev_d:
	     ~rev_hd:
	     ~get_sol_length: *)
       ~equals:(fun a b -> a = b)
       Search_interface.Robot
       (make_root problem)
       (fun _ _ -> false)
       (fun _ -> ()))




let feasible_p problem =
  if not (Non_dynamic.feasible_p problem)
  then false
  else
    (let iface= (default_interface problem [Limit.Time 300.]) in
     let sol,_,_,_,_,_ =
       Speedy.dups iface [||] in
       match sol with
	   None -> false
	 | _ -> true)

(********** comparing against shortest path *********)


(*
let make_sp_root states problem =


let extract_shortest_path s =
  match s with
    None -> failwith "no path along shortest path?"
  | Some (states, time) ->
      states, time


let fastest_on_shortest problem =
  let (path, dist), e, g = Non_dynamic.shortest_path problem in
  let s, e2, g2 = Astar.a_star_dups foo in
    (extract_shortest_path s), e, g
*)

(* EOF *)
