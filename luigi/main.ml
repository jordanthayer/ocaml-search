(** Things to evaluate by default when loading a top level.

    @author eaburns
    @since 2010-03-03
*)

open Object
open Printf

let timeout sec =
  (** [timeout sec] cause a timeout for a given number of seconds. *)
  let stop = (Unix.gettimeofday ()) +. sec in
    while (Unix.gettimeofday ()) < stop do () done


let forward_agent () =
  (** [forward_agent ()] a simple agent that merely moves forward and
      jumps when it can. *)
  let jump_time = ref 0 in
  let speed_right = { Mario.no_action with
			Mario.right = true;
			Mario.speed = true; }
  in
    (fun mario ->
       if !jump_time > 0
       then begin
	 decr jump_time;
	 { speed_right with Mario.jump = true }
       end else begin
	 if not mario.Mario.may_jump
	 then speed_right
	 else begin
	   jump_time := 7;
	   { speed_right with Mario.jump = true }
	 end
       end)


let close a b = (abs_float (a -. b)) < 0.0001


let output_mario mario mario_x mario_y may_jump on_ground =
  (** [output_mario mario mario_x mario_y may_jump on_ground] prints
      where we expected Mario to be versus where he ended up... asserts
      that they are very similar locations. *)
  printf "Got: (%f, %f); %s; %s\n"
    mario_x mario_y (if may_jump then "  can jump" else "can't jump")
    (if on_ground then "on ground" else "in air");
  printf "Expected:\n"; Mario.output stdout mario;
  assert (close mario.Mario.obj.x mario_x);
  assert (close mario.Mario.obj.y mario_y)


let rec tick client level agent pause mobs mario screen =
  (** [tick client level agent pause mobs mario screen] performs a
      single game 'tick'. *)
  (* Do something. *)
  let act = agent mario in
  let mx = mario.Mario.obj.x and my = mario.Mario.obj.y in
  let mario' = Mario.do_action level mario act in
    Client.do_action client act;
    if not pause then Mob.move level mario' mobs;
    let pause' = Mob.detect_collisions level mario' mobs in

    (* Get the next frame from the server. *)
    let jmp', ground', screen', mx', my', mob_info = Client.next client in
      Level.add_screen level screen mx my;
      (* Add the screen at Mario's previous position. *)
      let mob_chars = Mob.position_and_chars mobs Level.get_x Level.get_y in
      let mobs' = Mob.update level mario mobs mob_info in
	List.iter (Mob.output stdout) mobs';
	output_mario mario' mx' my' jmp' ground';
	Mario.relocate mario' mx' my';
	Level.output_screen stdout level ~mobs:mob_chars mx' my';
	(*
	  Level.output stdout level;
	*)
	printf "-------------------------------------------------------\n%!";
	timeout 0.01;

	tick client level agent pause' mobs' mario' screen'


let main () =
  let client = Client.create "Luigi: he finally gets to play" in
  let level = Level.make () in
  let agent = forward_agent () in

    Client.connect client;
    Client.reset_game client;

    try
      let jmp, ground, screen, mx, my, mob_info = Client.next client in
      let mario = Mario.make mx my jmp ground in
      let mario = Mario.do_action level mario Mario.no_action in
      let mobs = Mob.update level mario [] mob_info in
      let mx = mario.Mario.obj.x and my = mario.Mario.obj.y in
      let mob_chars = Mob.position_and_chars mobs Level.get_x Level.get_y in
	Level.add_screen level screen mx my;
	(* Level.output stdout level; *)
	Level.output_screen stdout level ~mobs:mob_chars mx my;
	printf "-------------------------------------------------------\n%!";
	tick client level agent false mobs mario screen
    with Client.Finished _ -> Level.output stdout level

let _ = main ()
