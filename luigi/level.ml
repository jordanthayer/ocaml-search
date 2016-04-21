(** The known portion of the current level.

    TODO:

    * Instead of copying the screen into the level each tick, just
    track the updates (probably one column at a time).  May need a
    set to keep track of which colums in the level have an 'unknown'
    value in it so that it can be fixed when we finally see what is
    there.

    @author eaburns
    @since 2010-03-03
*)

open Printf

type t = {
  tiles : tile array Garray.t;
}


and tile =
  | Free_space
      (* Completely unblocked. *)
  | Soft_blocked
      (* Blocked going down, but you can jump thru from the bottom. *)
  | Hard_blocked
      (* Blocked from all sides. *)
  | Flower_pot
      (* An enemy flower pot or a cannon. *)
  | Brick
  | Question_brick
  | Unknown


let char_of_tile = function
    (** [char_of_tile elm] gets a character representation of a
	screen tile for printing. *)
  | Free_space -> ' '
  | Hard_blocked -> 'O'
  | Soft_blocked -> 'o'
  | Flower_pot -> 'P'
  | Brick -> '#'
  | Question_brick -> '?'
  | Unknown -> 'U'

(** {6 Sizes} ****************************************)

let tile_width = 16
  (** [tile_width] the width of a tile (in the same units as a MOB's
      location). *)
let tile_widthf = float tile_width
  (** [tile_widthf] the width of a tile as a float. *)
let tile_height = 16
  (** [tile_height] the height of a tile (in the same units as a MOB's
      location). *)
let tile_heightf = float tile_height
  (** [tile_heightf] the height of a tile as a float. *)

let screen_width = 22
  (** [screen_width] the width (in blocks) of the visible screen. *)
and screen_height = 22
  (** [screen_height] the height (in blocks) of the visible screen. *)
let half_screen_width = screen_width / 2
  (** [half_screen_width] half of the screen width. *)
and half_screen_height = screen_height / 2
  (** [half_screen_height] half of the screen height. *)
and quarter_screen_height = screen_height / 4
  (** [quarter_screen_height] a quarter of the screen height. *)

let imax a b = if (a:int) > b then a else b

let imin a b = if (a:int) < b then a else b

(** {6 Creation} ****************************************)

let make () =
  (** [make ()] makes a new level. *)
  let h = screen_height in
    {
      tiles = Garray.init (fun _ -> (Array.create h Free_space));
    }


(** {6 Accessing} ****************************************)

let get_x x =
  (** [get_x pos] gets the x index of a block given a position as a
      float. *)
  truncate (x /. tile_widthf)

let get_y y =
  (** [get_y pos] gets the y index of a block given a position as a
      float. *)
  truncate (y /. tile_heightf)


let can_pass vx vy = function
    (** [can_pass vy vy] test if a block is passable in the given
	direction. *)
  | Free_space -> true
  | Soft_blocked -> if vy < 0. then true else false
  | _ -> false


let is_blocked level x y vx vy =
  (** [is_blocked level x y vx vy] tests if the given move is blocked
      in the level. *)
  let x = get_x x and y = get_y y in
    if y < 0
    then false
    else not (can_pass vx vy (Garray.get level.tiles x).(y))


(** {6 I/O} ****************************************)

let output ch l =
  (** [output ch l] outputs the entire level (vertically) to the given
      channel. *)
  let tiles = l.tiles in
  let width = Garray.get_fill tiles in
  let height = Array.length (Garray.get tiles 0) in
    for i = width - 1 downto 0 do
      let col = Garray.get tiles i in
	for j = 0 to height - 1 do
	  let tile = col.(j) in
	    fprintf ch "%c" (char_of_tile tile)
	done;
	fprintf ch "\n";
    done


let output_screen ch l ?(mobs=[]) mario_x mario_y =
  (** [output_screen ch l ?mobs mario_x mario_y] outputs the current
      screen to the given channel. *)
  let mx = get_x mario_x and my = get_y mario_y in
  let tiles = l.tiles in
  let x_offs = imax (mx - half_screen_width) 0
  and y_offs = my - half_screen_height in
    fprintf ch "+";
    for i = 0 to screen_width - 1 do fprintf ch "-"; done;
    fprintf ch "+\n";
    for j = y_offs to y_offs + screen_height - 1 do
      fprintf ch "|";
      for i = x_offs to x_offs + screen_width - 1 do
	let tile =
	  if i >= 0 && j >= 0 && j < screen_height
	  then (Garray.get tiles i).(j)
	  else Free_space
	in
	  if i = mx && j = my
	  then fprintf ch "M"
	  else begin
	    try
	      let _, _, c =
		List.find (fun (x, y, _) -> x = i && y = j) mobs
	      in fprintf ch "%c" c;
	    with Not_found -> fprintf ch "%c" (char_of_tile tile)
	  end
      done;
      fprintf ch "|\n";
    done;
    fprintf ch "+";
    for i = 0 to screen_width - 1 do fprintf ch "-"; done;
    fprintf ch "+\n"


(** {6 Modification.} ****************************************)

let add_screen t screen mario_x mario_y =
  (** [add_screen t screen mario_x mario_y] adds the screen to the
      level map given [mario_pos].  This assumes that the screen is
      centered on Mario. *)
  let w = Array.length screen
  and h = Array.length screen.(0) in
  let mx = get_x mario_x and my = get_y mario_y in
  let x_offs = mx - half_screen_width
  and y_offs = my - half_screen_height in
    for i = 0 to w - 1 do
      let col = screen.(i) in
	for j = 0 to h - 1 do
	  let x = i + x_offs and y = j + y_offs in
	    if y < h && y >= 0 && x >= 0
	    then begin
	      let level_col = Garray.get t.tiles x in
	      let tile = col.(j) in
		if tile <> Unknown
		then level_col.(y) <- tile
	    end
	done
    done
