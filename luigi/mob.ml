(** Handling of the motion for the different types of mobile objects
    in the game.

    @author eaburns
    @since 2010-03-03
*)

open Object
open Printf

type t = {
  mutable kind : kind;
  obj : obj;
  mutable on_ground : bool;
  mutable may_jump : bool;
  mutable dead : bool;

  mutable jump_time : float;
  mutable y_start : float;
  (* Used for the enemy flower. *)
}

and kind =
    (* Not [Blah of Object.t] because it may be convenient to refer to
       a mob kind without knowing its object yet. *)
  | Goomba
  | Winged_goomba
  | Red_koopa
  | Winged_red_koopa
  | Green_koopa
  | Winged_green_koopa
  | Bullet_bill
  | Spiky
  | Winged_spiky
  | Enemy_flower
  | Shell
  | Mushroom
  | Fire_flower
  | Fire_ball


let classify = function
    (** [classify value] converts from a value (sent by the
	server) into a [t]. *)
  | 2 -> Goomba
  | 3 -> Winged_goomba
  | 4 -> Red_koopa
  | 5 -> Winged_red_koopa
  | 6 -> Green_koopa
  | 7 -> Winged_green_koopa
  | 8 -> Bullet_bill
  | 9 -> Spiky
  | 10 -> Winged_spiky
  | 12 -> Enemy_flower
  | 13 -> Shell
  | 14 -> Mushroom
  | 15 -> Fire_flower
  | 25 -> Fire_ball
  | x -> invalid_arg (sprintf "Unknown MOB %d\n" x)


let to_string = function
  (** [to_string t] gets a human-readable string representation. *)
  | Goomba -> "Goomba"
  | Winged_goomba -> "Winged Goomba"
  | Red_koopa -> "Red Koopa"
  | Winged_red_koopa -> "Winged Red Koopa"
  | Green_koopa -> "Green Koopa"
  | Winged_green_koopa -> "Winged Green Koopa"
  | Bullet_bill -> "Bullet Bill"
  | Spiky -> "Spiky"
  | Winged_spiky -> "Winged Spiky"
  | Enemy_flower -> "Enemy Flower"
  | Shell -> "Shell"
      (* Power ups *)
  | Mushroom -> "Mushroom"
  | Fire_flower -> "Fire Flower"
      (* Fireball *)
  | Fire_ball -> "Fireball"


let to_char = function
  (** [to_char t] gets a character representation. *)
  | Goomba -> 'g'
  | Winged_goomba -> 'G'
  | Red_koopa -> 'r'
  | Winged_red_koopa -> 'R'
  | Green_koopa -> 'k'
  | Winged_green_koopa -> 'K'
  | Bullet_bill -> 'B'
  | Spiky -> 's'
  | Winged_spiky -> 'S'
  | Enemy_flower -> 'V'
  | Shell -> 'h'
      (* Power ups *)
  | Mushroom -> 'M'
  | Fire_flower -> 'F'
      (* Fireball *)
  | Fire_ball -> 'f'


let output ch m =
  (** [output ch m] outputs the MOB to the given channel. *)
  fprintf ch "%s:\n" (to_string m.kind);
  output_object ch ~prefix:"\t" m.obj


(** {6 Properties} ****************************************)

let width = function
    (** [width m] width from the center of the MOB. *)
  | Goomba
  | Winged_goomba
  | Red_koopa
  | Winged_red_koopa
  | Green_koopa
  | Winged_green_koopa
  | Bullet_bill
  | Spiky
  | Winged_spiky
  | Shell -> 4.
  | Enemy_flower -> 2.
  | _ -> invalid_arg "Mob.width: No idea what the width of this MOB is."


let height = function
    (** [height m] height of the MOB. *)
  | Goomba
  | Winged_goomba
  | Red_koopa
  | Winged_red_koopa
  | Green_koopa
  | Winged_green_koopa
  | Spiky
  | Winged_spiky -> 24.
  | Shell
  | Bullet_bill
  | Enemy_flower -> 12.
  | _ -> invalid_arg "Mob.width: No idea what the width of this MOB is."


let winged = function
    (** [winged m] tests if a MOB has wings. *)
  | Winged_goomba
  | Winged_red_koopa
  | Winged_green_koopa
  | Winged_spiky -> true
  | Bullet_bill -> false
  | _ -> false


let unwing m =
  (** [unwing m] removes the wings from a winged MOB. *)
  m.kind <- (match m.kind with
	       | Winged_goomba -> Goomba
	       | Winged_red_koopa -> Red_koopa
	       | Winged_green_koopa -> Green_koopa
	       | Winged_spiky -> Spiky
	       | k ->
		   invalid_arg (sprintf "unwing: %s not a winged MOB"
				  (to_string k)));
  m.obj.vy <- 0.


let avoid_cliff m =
  (** [avoid_cliff m] test if the given enemy will avoid cliffs. *)
  m.kind = Red_koopa && m.on_ground

(** {6 Motion Models} ****************************************)

let side_ways_speed = 1.75


let move_enemy level m =
  (** [move_enemy level m] move the enemy throught the level handling
      collisions between it and the tiles of the level. *)
  let o = m.obj in
  let winged = winged m.kind in
  let level_collision_x vx =
    m.on_ground <- false;
    m.obj.facing <- ~-. (m.obj.facing)
  and level_collision_y vy = if vy > 0. then m.on_ground <- true in
    if o.vx > 2. then o.facing <- facing_right;
    if o.vx < ~-.2. then o.facing <- facing_left;
    o.vx <- o.facing *. side_ways_speed;
    m.may_jump <- m.on_ground;
    m.on_ground <- false;
    move_object_x ~avoid_cliff:(avoid_cliff m)
      level_collision_x level m.obj m.obj.vx;
    move_object_y level_collision_y level m.obj m.obj.vy;
    o.vy <- o.vy *. (if winged then 0.95 else 0.85);
    if m.on_ground then begin
      o.vx <- o.vx *. ground_inertia;
      if winged then o.vy <- ~-. 10.;
    end else begin
      o.vx <- o.vx *. air_inertia;
      o.vy <- o.vy +. (if winged then 0.6 else 2.);
    end


let move_flower level mario m =
  (** [move_flower level mario m] moves an enemy flower in the
      level. *)
  let o = m.obj in
    if o.vy >= m.y_start
    then begin
      let dx = floor (abs_float (mario.Mario.obj.x -. o.x)) in
	o.y <- m.y_start;
	m.jump_time <- m.jump_time +. 1.;
	if m.jump_time > 40. && dx > 24. then o.vy <- ~-.8. else o.vy <- 0.
    end else m.jump_time <- 0.;
    o.y <- o.y +. o.vy;
    o.vy <- o.vy *. 0.9;
    o.vy <- o.vy +. 0.1


let move_bullet level m =
  (** [move_bullet level m] moves a bullet bill in the level. *)
  let o = m.obj in
    o.vx <- o.facing *. 4.;
    o.x <- o.x +. o.vx


(** {6 Creation} ****************************************)

let init_mob level mario m =
  (** [init_mob level mario m] initializes a MOB.  Needs [level] and
      [mario] to initialize some of the MOBs. *)
  let o = m.obj in
    begin match m.kind with
      | Enemy_flower ->
	  let y = o.y in
	    o.vy <- ~-. 8.;
	    o.y <- o.y -. 1.;
	    for i = 1 to 5 do move_flower level mario m done;
	    m.y_start <- m.y_start +. (y -. o.y) +. 1.;
	    o.y <- y;
      | Bullet_bill -> o.vy <- ~-. 5.;
      | _ -> o.vx <- ~-. 2.
    end; m


let make level mario kind x y =
  (** [make kind mario x y] makes a new MOB. *)
  let m = { kind = kind;
	    obj = make_object x y facing_left (width kind) (height kind);
	    on_ground = false;
	    may_jump = false;
	    dead = false;
	    jump_time = 0.;
	    y_start = y;
	  }
  in init_mob level mario m


(** {6 Collision with Mario} ****************************************)


let check_collide level mario mob =
  (** [check_collide level mario mob] checks if the MOB collides with
      Mario.  Returns [true] if Mario got hurt. *)
  let mario_obj = mario.Mario.obj
  and mob_obj = mob.obj in
    if objects_collide mario_obj mob.obj
    then begin
      if mob.kind <> Spiky && mob.kind <> Winged_spiky
	&& mario_obj.vy > 0. && (mario_obj.y -. mob_obj.y) <= 0.
	&& (not mario.Mario.on_ground || not mario.Mario.was_on_ground)
      then begin
	Mario.stomp level mario mob_obj;
	if winged mob.kind then unwing mob;
	mob.dead <- true;
	false
      end else begin
	Mario.get_hurt mario;
	true
      end
    end else false


(** {6 Sets of MOBs} ****************************************)

let position_and_chars mobs get_x get_y =
  (** [position_and_chars mobs get_x get_y] gets a list of position
      and characters of the MOBs for printing in the level display. *)
  List.map (fun m -> get_x m.obj.x, get_y m.obj.y, to_char m.kind) mobs


let detect_collisions level mario mobs =
  (** [detect_collisions level mario mobs] handles collisions with
      Mario and returns [true] if Mario was hurt. *)
  List.fold_left (fun c m -> c || (check_collide level mario m)) false mobs


let update level mario mobs info =
  (** [update level mario mobs info] updates a list of MOBs given the
      info from the server.

      For now, just discard the initial list and give a new list that
      exactly reflects the new info from the server. *)
  let dist x y obj = sqrt (((x -. obj.x) ** 2.) +. ((y -. obj.y) ** 2.)) in
  let close a b = (abs_float (a -. b)) < 0.0001 in
  let new_mobs = ref [] in
    List.iter
      (fun (k, x, y) ->
	 let kinds = List.filter (fun m -> m.kind = k) mobs in
	   if kinds = []
	   then begin
	     printf "Creating a new %s\n" (to_string k);
	     new_mobs := (make level mario k x y) :: !new_mobs;
	   end else begin
	     let dist, mob =
	       List.fold_left (fun ((c, _) as last) m ->
				 let d = dist x y m.obj in
				   if d < c then d, m else last)
		 (infinity, List.hd mobs) mobs
	     in
	       if dist < 0.01
	       then begin
	       (try
		 assert (close x mob.obj.x);
		 assert (close y mob.obj.y);
	       with _ ->
		 printf "expected %f, %f\ngot %f, %f\n%!" mob.obj.x mob.obj.y x y;
		 assert false
	       );
		 printf "%s is where expected\n" (to_string k);
		 mob.obj.x <- x;
		 mob.obj.y <- y;
	       end else begin
		 printf "Creating a new %s\n" (to_string k);
		 new_mobs := (make level mario k x y) :: !new_mobs;
	       end
	   end
      )
    info;
    (List.filter (fun m -> not m.dead) mobs) @ !new_mobs


let move level mario mobs =
  (** [move level mario mobs] moves the MOBs within the level. *)
  List.iter (fun m -> match m.kind with
	      | Enemy_flower -> move_flower level mario m
	      | Bullet_bill -> move_bullet level m
	      | _ -> move_enemy level m)
    mobs

