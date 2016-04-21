(*
let accels robot =
  let dt = time_step robot in
    List.map (fun factor ->
		let accel = (float robot.max_accel) *. factor in
		let diff = Math.round (accel *. dt) in
		  if (factor <> 0.) && ((abs diff) < 1) then
		    failwith "possible acceleration under +/- 1cm/time-step";
		  diff)
      [-1.; -0.5; 0.; 0.5; 1.]


let turns robot =
  let dt = time_step robot in
    List.map (fun factor ->
		let diff = (float robot.max_turn) *. factor *. dt in
		  Wrutils.pr "Max turn %d * factor %.2f * dt %f = %f\n"
		    robot.max_turn factor dt diff;
		  let diff = Math.round diff in
		    if (factor <> 0.) && ((abs diff) < heading_res) then
		      failwith "possible turn under heading_res";
		    diff)
      [-1.; -0.5; 0.; 0.5; 1.]


let new_pos old new_speed new_heading dt =
  (** assumes instantaneous change in speed
    and heading! *)
  let distance = (float new_speed) *. dt
  and rads = Math.to_radians new_heading in
  let dx = Math.round (distance *. (cos rads))
  and dy = Math.round (distance *. (tan rads)) in
    (old.x + dx), (old.y + dy)
*)

let make_child x y h s p =
  { p with
      state = { x = x;
		y = y;
		heading = h;
		speed = s; };
      parent = Some p; }


let make_expand robot =
  let dt = time_step robot
  and accels = accels robot
  and turns = turns robot in
    (fun node time ->
       let state = node.state
       and obstacles = node.world.obstacles
       and new_time = time +. dt
       and children = ref [] in
	 List.iter
	   (fun accel ->
	      let speed = state.speed + accel in
		if (speed <= robot.max_speed) &&
		  (speed >= robot.min_speed) then
		    List.iter (fun turn ->
				 if not ((state.speed = 0) &&
					 (accel = 0) && (turn = 0)) then
				   let heading = normalized_heading
						   state.heading turn in
				   let x,y = new_pos state speed heading dt in
				     if legal_pos obstacles x y then
				       Wrutils.push ((make_child x y
						      speed heading node),
						   new_time)
					 children)
		      turns)
	   accels;
	 !children)




let closer_to_diag heading =
  (abs ((heading mod 90) - 45)) < 23



let make_possible_transitions heading speed =
  (* actions are accel/deccel and turn. given current, what are possible
     future values (and position deltas).  very hacky:

     1. compute time until next position at current speed and heading
     2. compute turns possible in that time
     3. assume heading changes instantly, speed changes linearly?
     4. compute next position
     5. compute time to that position (+ time to adjust heading?)
  *)
  let _,_,dist_until_pos = get_dxy heading in
  let time_until_pos = dist_until_pos /. (float speed) in
  let max_turn = min 180. (time_until_pos *. max_turn) in
  let num_h_incrs = Math.round (max_turn /. heading_delta) in
  let max_accel = min max_speed (time_until_pos *. max_accel) in
  let num_s_incrs = Math.round (max_accel /. speed_delta)
  and next = ref [] in
    for i = - num_h_incrs to num_h_incrs do
      let dh = (float i) *. heading_delta in
      let h = int_of_heading ((float heading) +. dh) in
	for j = - num_s_incrs to num_s_incrs do
	  let ds = (float j) *. speed_delta in
	  let s = int_of_speed ((float speed) +. ds) in
	    if s = 0 then
	      (if speed <> 0 then
		 (* stopping *)
		 Wrutils.push (0, 0, h, s, (float speed) /. max_accel)
		   next
	       else if h <> heading then
		 (* just turning *)
		 Wrutils.push (0, 0, h, s,
			     (float (abs (h - heading))) /. max_turn)
		   next)
	    else
	      let dx, dy, d = get_dxy h in
	      let dt = d /. ((float (speed + s)) /. 2.) in
		Wrutils.push (dx, dy, h, s, dt) next
	done
    done;
    Wrlist.remove_duplicates !next


    for hi = 0 to truncate (360. /. heading_delta) do
      let heading = int_of_heading ((float hi) *. heading_delta)
      and s = ref min_speed in
	while !s <= max_speed do
	  let speed = int_of_speed !s in


	    s := (float speed) +. speed_delta
	done;
    done;



  let max_turn = time_until_pos *. max_turn
  and max_accel = time_until_pos *. max_accel
  and next = ref [] in
    for hi = 0 to truncate (ceil (360. /. heading_delta)) do
      let h = int_of_heading ((float hi) *. heading_delta) in
	if ((float (abs (h - heading))) /. max_turn) <= time_until_pos then
	  for si = 0 to truncate (ceil ((max_speed -. min_speed) /.
					  speed_delta)) do
	    let s = int_of_speed (min_speed +.
				    ((float si) *. speed_delta)) in
	      if ((float (abs (s - speed))) /. max_accel) <= time_until_pos then
		if s = 0 then
		  (if speed <> 0 then
		     (* stopping *)
		     Wrutils.push (0, 0, h, s, (float speed) /. max_accel)
		       next
		   else if h <> heading then
		     (* just turning *)
		     Wrutils.push (0, 0, h, s,
				 (float (abs (h - heading))) /. max_turn)
		       next)
		else
		  let dx, dy, d = get_dxy h in
		  let dt = d /. ((float (abs (speed + s))) /. 2.) in
		  let dx = if s < 0 then -dx else dx
		  and dy = if s < 0 then -dy else dy in
		    Wrutils.push (dx, dy, h, s, dt) next
	  done
    done;
    Wrlist.remove_duplicates !next



(* EOF *)
