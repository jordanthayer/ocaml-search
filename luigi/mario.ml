(** The man him self.

    @author eaburns
    @since 2010-03-05
*)

open Object

type t = {
  obj : obj;
  mutable may_jump : bool;
  mutable on_ground: bool;
  mutable was_on_ground : bool;
  mutable sliding : bool;
  mutable ducking : bool;
  mutable jump_time : float;
  mutable xjump_speed : float;
  mutable yjump_speed : float;
  mutable invulnerable_time : int;
  mutable mode : mode;
}

and mode =
  | Small
  | Large
  | Fire


and action = {
  left : bool;
  right : bool;
  down : bool;
  jump : bool;
  speed : bool;
}

let no_action =
  (** [no_action] don't do anything. *)
  {
    left = false;
    right = false;
    down = false;
    jump = false;
    speed = false;
  }


let width = 8.

let half_width = width /. 2.

let height duck mode = match duck, mode with
  | false, Small -> 12.
  | false, _ -> 24.
  | true, _ -> 12.


exception Dead
  (** [Dead] is raised when Mario dies. *)


let get_hurt m =
  (** [get_hurt m] called when Mario gets hurt by a MOB.  Raises
      [Dead] if Mario is killed. *)
  if m.invulnerable_time = 0
  then begin
    let o = m.obj in
    let orig_height = o.height in
    m.mode <- (match m.mode with
		 | Fire -> Large
		 | Large -> Small
		 | Small -> raise Dead);
    m.invulnerable_time <- 32;
  end


let do_move_x level m vx =
  (** [do_move_x level m vx] moves Mario in the level in the x
      direction. *)
  let level_collision_x vx = () in
  let sliding = ref m.sliding in
    move_object_x ~sliding level_collision_x level m.obj vx;
    m.sliding <- !sliding


let do_move_y level m vy =
  (** [do_move_y level m vy] moves Mario in the level in the y
      direction. *)
  let level_collision_y vy =
    if vy < 0. then m.jump_time <- 0.;
    if vy > 0. then m.on_ground <- true
  in move_object_y level_collision_y level m.obj vy


let stomp level m obj =
  (** [stomp level m obj] stomps an object. *)
  let ty = obj.y -. obj.height /. 2. in
  let mo = m.obj in
    do_move_y level m (ty -. mo.y);
    m.xjump_speed <- 0.;
    m.yjump_speed <- ~-. 1.9;
    m.jump_time <- 8.;
    mo.vy <- m.jump_time *. m.yjump_speed;
    m.on_ground <- false;
    m.sliding <- false;
    m.invulnerable_time <- 1


let do_action level mario a =
  (** [do_action level mario a] handle Mario's movement.  This is
      mostly a copy of the code in Mario.java. *)
  let m = { mario with obj = copy_object mario.obj } in
  let o = m.obj in
  let side_ways_speed = if a.speed then 1.2 else 0.6 in
    if m.invulnerable_time > 0
    then m.invulnerable_time <- m.invulnerable_time - 1;
    m.was_on_ground <- m.on_ground;
    if m.on_ground then begin
      if a.down && m.mode <> Small
      then m.ducking <- true
      else m.ducking <- false
    end;
    if o.vx > 2. then o.facing <- facing_right;
    if o.vx < ~-.2. then o.facing <- facing_left;
    if a.jump || (m.jump_time < 0. && not m.on_ground && not m.sliding)
    then begin
      if m.jump_time < 0. then begin
	o.vx <- m.xjump_speed;
	o.vy <- ~-.(m.jump_time) *. m.yjump_speed;
	m.jump_time <- m.jump_time +. 1.;
      end else if m.on_ground && m.may_jump then begin
	m.xjump_speed <- 0.;
	m.yjump_speed <- ~-. 1.9;
	m.jump_time <- 7.;
	o.vy <- m.jump_time *. m.yjump_speed;
	m.on_ground <- false;
	m.sliding <- false;
      end else if m.sliding && m.may_jump then begin
	m.xjump_speed <- ~-.(o.facing) *. 6.;
	m.yjump_speed <- ~-. 2.;
	m.jump_time <- ~-. 6.;
	o.vx <- m.xjump_speed;
	o.vy <- ~-. (m.jump_time) *. m.yjump_speed;
	m.on_ground <- false;
	m.sliding <- false;
	o.facing <- ~-.(o.facing);
      end else if m.jump_time > 0. then begin
	o.vx <- o.vx +. m.xjump_speed;
	o.vy <- m.jump_time *. m.yjump_speed;
	m.jump_time <- m.jump_time -. 1.;
      end
    end else m.jump_time <- 0.;
    if a.left && not m.ducking then begin
      if o.facing = facing_right then m.sliding <- false;
      o.vx <- o.vx -. side_ways_speed;
      if m.jump_time >= 0. then o.facing <- facing_left
    end;
    if a.right && not m.ducking then begin
      if o.facing = facing_left then m.sliding <- false;
      o.vx <- o.vx +. side_ways_speed;
      if m.jump_time >= 0. then o.facing <- facing_right
    end;
    if (not a.left && not a.right) || m.ducking || o.vy < 0. || m.on_ground
    then m.sliding <- false;
    (*
      if (keys[KEY_SPEED] && canShoot && Mario.fire && world.fireballsOnScreen<2)
    *)
    if abs_float o.vx < 0.5 then o.vx <- 0.;
    if m.sliding then o.vy <- o.vy *. 0.5;
    m.may_jump <- (m.on_ground || m.sliding) && not a.jump;
    m.on_ground <- false;
    do_move_x level m m.obj.vx;
    do_move_y level m m.obj.vy;
    if o.x < 0. then begin o.x <- 0.; o.vx <- 0. end;
    o.vy <- o.vy *. 0.85;
    if m.on_ground
    then o.vx <- o.vx *. ground_inertia
    else begin
      o.vx <- o.vx *. air_inertia;
      o.vy <- o.vy +. 3.;
    end;
    m


let relocate mario x y =
  (** [relocate mario x y] relocates mario. *)
  mario.obj.x <- x;
  mario.obj.y <- y


let make ?(mode=Fire) x y may_jump on_ground =
  (** [make ?mode x y may_jump on_ground] makes a Mario
      record. *)
  { obj = make_object x y facing_right half_width (height false mode);
    mode = mode;
    may_jump = may_jump;
    on_ground = on_ground;
    was_on_ground = false;
    sliding = false;
    ducking = false;
    jump_time = 0.;
    xjump_speed = 0.;
    yjump_speed = 0.;
    invulnerable_time = 0;
  }



let output ch mario =
  (** [output ch mario] outputs Mario's information to the given
      channel. *)
  Printf.fprintf ch "Mario:\n";
  output_object ch ~prefix:"\t" mario.obj;
  Printf.fprintf ch "\ton ground: %s\n" (string_of_bool mario.on_ground);
  Printf.fprintf ch "\tmay jump: %s\n" (string_of_bool mario.may_jump);
  Printf.fprintf ch "\tducking: %s\n" (string_of_bool mario.ducking);
  Printf.fprintf ch "\tsliding: %s\n" (string_of_bool mario.sliding);
  Printf.fprintf ch "\tjump_time: %f\n" mario.jump_time;
  Printf.fprintf ch "\txjump_speed: %f\n" mario.xjump_speed;
  Printf.fprintf ch "\tyjump_speed: %f\n" mario.yjump_speed;
  Printf.fprintf ch "\tinvlunerable_time: %d\n" mario.invulnerable_time;
  Printf.fprintf ch "\tmode: %s\n" (match mario.mode with
				      | Small -> "Small"
				      | Large -> "Large"
				      | Fire -> "Fire")
