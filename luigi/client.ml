(** This module should contain handling for all data values sent by
    the server.  In addition it handles some basic connection managemet

    @author eaburns
    @since 2010-02-25
*)

(** {6 Basic communication} ****************************************)

type t = {
  mutable con : Connection.t option;
  name : string;
}


exception Disconnected
  (** [Disconnected] is raised if the client is disconnected when an
      operation is attempted. *)



let create name =
  (** [create name] makes a new client with the given name. *)
  {
    con = None;
    name = name
  }


let bye_string = "ciao"
  (** [bye_string] sent to the server to notify that the connection
      will be closing. *)


let connect ?(address="localhost") ?(port=4242) t =
  (** [connect ?address ?port t] connects the client to the server
      (default is localhost on the default port 4242). *)
  let c = Connection.create address port in
  let _ = Connection.receive_string c in (* hello from server *)
    Connection.send_string c (Printf.sprintf "Client: I am %s\n" t.name);
    t.con <- Some c


let disconnect t =
  (** [disconnect t] disconnects from the server. *)
  match t.con with
    | None -> raise Disconnected
    | Some c ->
	Connection.send_string c (Printf.sprintf "%s\n" bye_string);
	Connection.close c;
	t.con <- None


let receive t =
  (** [receive t] receive data from the server.

      This setup seems a bit clumbsy becasue there are no delimiters between
      'packets' of data from the server.  We just need to hope that
      they will arrive in the same chunks that the server intended. *)
  match t.con with
    | None -> raise Disconnected
    | Some c ->
	let str = Connection.receive_string c in
	  if str = bye_string
	  then raise Disconnected
	  else str


let send t str =
  (** [send t str] sends a string to the server.  The server seems
      to perform a 'readLine' on its input stream so these strings
      should probably end with newlines. *)
  match t.con with
    | None -> raise Disconnected
    | Some c -> Connection.send_string c str


let reset_game ?(opts="-ld 5 -lt 1 -zm 1 -ze 0") t =
  (** [reset_game ?opts t] sends a reset string to the server to reset
      the game. *)
  send t (Printf.sprintf "reset %s\n" opts)


(** {6 The Screen} ****************************************)

let output_screen ch screen =
  (** [output_screen ch screen mario_x mario_y] outputs the screen to
      the given channel. *)
  for i = 0 to Level.screen_width - 1 do
    Printf.fprintf ch "-"
  done;
  Printf.fprintf ch "\n";
  for j = 0 to Level.screen_height - 1 do
    Printf.fprintf ch "|";
    for i = 0 to Level.screen_width - 1 do
      Printf.fprintf ch "%c" (Level.char_of_tile screen.(i).(j));
    done;
    Printf.fprintf ch "|\n"
  done;
  for i = 0 to Level.screen_width - 1 do
    Printf.fprintf ch "-"
  done;
  Printf.fprintf ch "\n"



(** {6 Simple TCP Data Parsing} ****************************************)

module Simple_tcp = struct
  (** [Simple_tcp] parsing functions for the simple TCP data
      format. *)

  let get_bools = function
      (** [get_bools strs] gets the can_jump and on_ground
	  booleans. *)
    | can_jmp_str :: on_ground_str :: tl ->
	let can_jump = bool_of_string can_jmp_str
	and on_ground = bool_of_string on_ground_str in
	  tl, can_jump, on_ground
    | _ -> invalid_arg "Simple_tcp.get_bools: Not enough data"


  let classify_level_tile = function
      (** [classify_level_tile value] classifies a number sent from the
	  server as a type of map tile. *)
    | 0 -> Level.Free_space
    | 1 -> Level.Hard_blocked
    | -10 -> Level.Hard_blocked
    | -11 -> Level.Soft_blocked
    | 20 -> Level.Flower_pot
    | 16 -> Level.Brick
    | 21 -> Level.Question_brick
    | _ -> Level.Unknown


  let rec get_row screen row col = function
      (** [get_row screen row col strs] gets a row of the screen. *)
    | hd :: tl ->
	screen.(col).(row) <- classify_level_tile (int_of_string hd);
	if col < Level.screen_width - 1
	then get_row screen row (col + 1) tl
	else tl
    | _ -> invalid_arg "Simple_tcp.get_row: Not enough data"


  let rec get_screen screen ?(row=0) strs =
    (** [get_screen screen row strs] gets the screen array from the
	string vector. *)
    if row < Level.screen_height
    then
      let strs = get_row screen row 0 strs in
	get_screen screen ~row:(row + 1) strs
    else strs


  let get_mario_pos = function
      (** [get_mario_pos offs vect] gets Mario's position from the data
	  vector. *)
    | x :: y :: tl ->
	tl, float_of_string x, float_of_string y;
    | _ -> invalid_arg "Simple_tcp.get_mario_pos: Not enough data"


  let classify_mob = function
      (** [classify_mob value] converts from a value (sent by the
	  server) into a mob kind. *)
    | 2 -> Mob.Goomba
    | 3 -> Mob.Winged_goomba
    | 4 -> Mob.Red_koopa
    | 5 -> Mob.Winged_red_koopa
    | 6 -> Mob.Green_koopa
    | 7 -> Mob.Winged_green_koopa
    | 8 -> Mob.Bullet_bill
    | 9 -> Mob.Spiky
    | 10 -> Mob.Winged_spiky
    | 12 -> Mob.Enemy_flower
    | 13 -> Mob.Shell
    | 14 -> Mob.Mushroom
    | 15 -> Mob.Fire_flower
    | 25 -> Mob.Fire_ball
    | x -> invalid_arg (Printf.sprintf "Unknown MOB %d\n" x)


  let rec get_mobs ?(accum=[]) = function
      (** [get_mobs ?accum strs] gets an MOB list. *)
    | [] -> accum
    | kind_str :: x_str :: y_str :: tl ->
	let kind = truncate (float_of_string kind_str)
	and x, y = float_of_string x_str, float_of_string y_str
	in get_mobs ~accum:((classify_mob kind, x, y)::accum) tl
    | _ -> invalid_arg "Simple_tcp.get_mobs: Not enough data"


  let parse data =
    (** [parse_simple_tcp_data data] takes the simple TCP data vector
	from the server and results in tuple (can_jump, on_ground,
	screen, mario position, mobs). *)
    let screen =
      Array.make_matrix Level.screen_width Level.screen_height Level.Free_space
    in
    let strs = Str.split (Str.regexp_string " ") data in
    let strs, can_jump, on_ground = get_bools (List.tl strs) in
    let strs = get_screen screen strs in
    let strs, mario_x, mario_y = get_mario_pos strs in
    let mobs = get_mobs strs in
(*
      output_screen stdout screen;
*)
      Printf.printf "mario @ (%f, %f)\n%!" mario_x mario_y;
      List.iter (fun (m, mx, my) ->
		   Printf.printf "%s @ (%f, %f)\n%!" (Mob.to_string m) mx my)
	mobs;
      can_jump, on_ground, screen, mario_x, mario_y, mobs

end

(** {6 Status} ****************************************)

type status =
  | Win
  | Dead

exception Finished of status * float * int * Mario.mode * int
  (** [Finished (status, distance, time left, Mario mode, coins).  *)


let parse_status data =
  (** [parse_status data] parses a status message from the server then
      raises a [Finished] exception with the given status. *)
  let strings = Array.of_list (Str.split (Str.regexp_string " ") data) in
  let status = match strings.(1) with
    | "0" -> Dead
    | "1" -> Win
    | x -> invalid_arg (Printf.sprintf "Unknown status: %s" x)
  and distance = float_of_string strings.(2)
  and time_left = int_of_string strings.(3)
  and mario_mode = match strings.(4) with
    | "0" -> Mario.Small
    | "1" -> Mario.Large
    | "2" -> Mario.Fire
    | x -> invalid_arg (Printf.sprintf "Unknown mode: %s" x)
  and coins = int_of_string strings.(5)
  in raise (Finished (status, distance, time_left, mario_mode, coins))

(** {6 Actions} ****************************************)

let string_of_actions action =
  (** [string_of_actions action] gets a string from an action *)
  let str = "00000\n" in
    String.fill str 0 ((String.length str) - 1) '0';
    if action.Mario.left then str.[0] <- '1';
    if action.Mario.right then str.[1] <- '1';
    if action.Mario.down then str.[2] <- '1';
    if action.Mario.jump then str.[3] <- '1';
    if action.Mario.speed then str.[4] <- '1';
    str


let do_action client actions =
  (** [do_action actions] sends the action to the server. *)
  let str = string_of_actions actions in
    Printf.printf "Sending string %s\n%!" str;
    send client str


(** {6 Ticks} ****************************************)


let next client =
  (** [next client] get the next reading from the client. *)
  let data = receive client in
    match data.[0] with
      | 'O' -> Simple_tcp.parse data
      | 'F' -> parse_status data
      | _ -> invalid_arg "Only Simple TCP mode is supported"
