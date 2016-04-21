(** Locations, velocity and collision detection for objects.

    Name functions well so that this module can be 'open'ed.

    @author eaburns
    @since 2010-03-05
*)

open Printf

type obj = {
  mutable x : float;
  mutable y : float;
  mutable vx : float;
  mutable vy : float;
  mutable facing : float;
  mutable half_width : float;
  mutable width : float;
  mutable height : float;
}

let facing_right = 1.

let facing_left = ~-. 1.

let ground_inertia = 0.89

let air_inertia = 0.89

let copy_object t =
  (** [copy_object t] copies the object. *)
  {
    x = t.x;
    y = t.y;
    vx = t.vx;
    vy = t.vy;
    facing = t.facing;
    half_width = t.half_width;
    width = t.width;
    height = t.height;
  }

let make_object x y facing half_width height =
  (** [make_object x y facing half_width height] makes a new
      object. *)
  {
    x = x;
    y = y;
    vx = 0.;
    vy = 0.;
    facing = facing;
    half_width = half_width;
    width = half_width *. 2.;
    height = height;
  }


let output_object ch ?(prefix="") o =
  (** [output_object ch ?prefix o] outputs the object to the given
      channel. *)
  fprintf ch "%slocation: %f, %f\n" prefix o.x o.y;
  fprintf ch "%svelocity: %f, %f\n" prefix o.vx o.vy;
  fprintf ch "%sfacing: %s\n" prefix (if o.facing = facing_left
				      then "left"
				      else "right");
  fprintf ch "%shalf width: %f\n%!" prefix o.half_width;
  fprintf ch "%sheight: %f\n%!" prefix o.height


(** {6 Moving within a level} ****************************************)

exception Collide

let move_object_x
    ?(sliding=ref true) ?(avoid_cliff=false) collision level o vx =
  (** [move_object_x ?sliding ?avoid_cliff collision level o vx] moves
      an object through the level in the x direction. *)
  let is_blocked = Level.is_blocked level in
  let tw = Level.tile_widthf in
  let rec move level o vx =
    let vx = ref vx and vy = 0. in
      while !vx > 8. do move level o 8.; vx := !vx -. 8.; done;
      while !vx < ~-.8. do move level o ~-.8.; vx := !vx +. 8.; done;
      let collide = ref false in
      let vx = !vx in
      let x = o.x and y = o.y and w = o.half_width and h = o.height in
      let x_low = x +. vx -. w and x_high = x +. vx +. w in
      let y_high = y +. vy -. h in
	if vx > 0. then begin
	  sliding := true;
	  if is_blocked x_high y_high vx vy
	  then collide := true else sliding := false;
	  if is_blocked x_high (y +. vy -. h /. 2.) vx vy
	  then collide := true else sliding := false;
	  if is_blocked x_high (y +. vy) vx vy
	  then collide := true else sliding := false;
	  if is_blocked x_high (y +. tw) vx 1. && avoid_cliff
	  then collide := true;
	end;
	if vx < 0. then begin
	  sliding := true;
	  if is_blocked x_low y_high vx vy
	  then collide := true else sliding := false;
	  if is_blocked x_low (y +. vy -. h /. 2.) vx vy
	  then collide := true else sliding := false;
	  if is_blocked x_low (y +. vy) vx vy
	  then collide := true else sliding := false;
	  if is_blocked x_low (y +. tw) vx 1. && avoid_cliff
	  then collide := true;
	end;
	if !collide
	then begin
	  if vx < 0. then begin
	    o.x <- (floor ((x -. w) /. tw)) *. tw +. w;
	    o.vx <- 0.;
	  end;
	  if vx > 0. then begin
	    o.x <- (floor ((x +. w) /. tw +. 1.)) *. tw -. w -. 1.;
	    o.vx <- 0.;
	  end;
	  collision vx;
	  raise Collide
	end else o.x <- o.x +. vx;
  in
    begin try move level o vx with Collide -> () end


let move_object_y collision level o vy =
  (** [move_object_y collision level o vy] moves an object through the
      level in the y direction. *)
  let is_blocked = Level.is_blocked level in
  let th = Level.tile_heightf in
  let rec move level o vy =
    let vy = ref vy and vx = 0. in
      while !vy > 8. do move level o 8.; vy := !vy -. 8.; done;
      while !vy < ~-.8. do move level o ~-.8.; vy := !vy +. 8.; done;
      let collide = ref false in
      let vy = !vy in
      let x = o.x and y = o.y and w = o.half_width and h = o.height in
      let x_low = x +. vx -. w and x_high = x +. vx +. w in
      let y_low = y +. vy and y_high = y +. vy -. h in
	if vy > 0. then begin
	  let y_lower = y_low +. 1. in
	    if is_blocked x_low y_low vx 0.then collide := true
	    else if is_blocked x_high y_low vx 0. then collide := true
	    else if is_blocked x_low y_lower vx vy then collide := true
	    else if is_blocked x_high y_lower vx vy then collide := true
	end;
	if vy < 0. then begin
	  if is_blocked (x +. vx) y_high vx vy then collide := true
	  else if is_blocked x_low y_high vx vy then collide := true
	  else if is_blocked x_high y_high vx vy then collide := true
	end;
	if !collide
	then begin
	  if vy < 0. then begin
	    o.y <- (floor ((y -. h) /. th)) *. th +. h;
	    o.vy <- 0.;
	  end;
	  if vy > 0. then o.y <- (floor ((y -. 1.) /. th +. 1.)) *. th -. 1.;
	  collision vy;
	  raise Collide
	end else begin
	  o.x <- o.x +. vx;
	  o.y <- o.y +. vy;
	end
  in
    begin try move level o vy with Collide -> () end

(** {6 Object on object collision} ****************************************)

let objects_collide a b =
  (** [objects_collide a b] tests if two objects collide. *)
  let dx = a.x -. b.x and dy = a.y -. b.y in
    (dx > ~-.(b.width) -. a.half_width && dx < b.width +. a.half_width)
    && (dy > ~-.(b.height) && dy < a.height)

