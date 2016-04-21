(** Tests the Playerc module.

    @author eaburns
    @since 2010-01-28
*)

open Printf
open Playerc
open Playerc.Common

type dev =
  | Position2d of Position2d.t
  | Laser of Laser.t


let connect_client () =
  (** [connect_client ()] connects the client and returns it. *)
  let client = Client.create None "localhost" 6665 in
    Client.set_transport client Tcp;
    Client.connect client;
    client

let disconnect_client client =
  (** [disconnect_client client] disconnect from the player server. *)
  Client.disconnect client

let init_pos2d client ind =
  (** [init_pos2d client ind] initializes the position2d proxy. *)
  let pos = Position2d.create client ind in
    Position2d.subscribe pos Common.Open;
    Position2d.enable pos true;
    pos

let deinit_pos2d pos2d =
  (** [deinit_pos2d pos2d] cleans up a position2d. *)
  Position2d.enable pos2d false;
  Position2d.unsubscribe pos2d


let init_laser client ind =
  (** [init_laser client ind] initialaizes the laser range-finder. *)
  let laser = Laser.create client ind in
    Laser.subscribe laser Common.Open;
    laser

let deinit_laser laser = Laser.unsubscribe laser
  (** [deinit_laser laser] *)


let display_pos2d x y pos =
  (** [display_pos2d x y pos] draws the pose and velosity from the
      odometry. *)
  let char_height = 10 in
  let p = Position2d.position pos in
    (* Get the pose and velocity of the robot. *)
    Graphics.moveto x (y + (char_height * 5));
    Graphics.draw_string (sprintf "px=%f" p.Position2d.px);
    Graphics.moveto x (y + (char_height * 4));
    Graphics.draw_string (sprintf "py=%f" p.Position2d.py);
    Graphics.moveto x (y + (char_height * 3));
    Graphics.draw_string (sprintf "pa=%f" p.Position2d.pa);
    Graphics.moveto x (y + (char_height * 2));
    Graphics.draw_string (sprintf "vx=%f" p.Position2d.vx);
    Graphics.moveto x (y + (char_height * 1));
    Graphics.draw_string (sprintf "vx=%f" p.Position2d.vy);
    Graphics.moveto x (y + (char_height * 0));
    Graphics.draw_string (sprintf "va=%f" p.Position2d.va)


let display_laser x y width laser =
  (** [display_laser x y width laser] draws the laser range finder
      lines. *)
  let pi = 3.1415926535 in
  let border_color = Graphics.black in
  let scan_color = 0x99CCFF in
  let max_rangef = Laser.max_range laser in
    (* Get the maximum range of a scan line. *)
  let meters_per_pt = truncate ((float width) /. (max_rangef *. 2.)) in
  let originx = x + (width / 2) and originy = y in
  let delta = (truncate max_rangef) * meters_per_pt in
  let mags, thetas = Laser.scan laser in
    (* Get the magnitudes (meters) and angles (radians) of each ray
       from the current scan. *)
  let n = Array.length mags in
    assert ((Array.length thetas) = n);
    Graphics.set_color scan_color;
    for i = 0 to n - 1 do
      let mag = mags.(i) *. (float meters_per_pt) in
      let angle = thetas.(i) +. (pi /. 2.) in
      let x = truncate (mag *. (cos angle)) in
      let y = truncate (mag *. (sin angle)) in
	Graphics.draw_poly_line [| originx, originy;
				   originx + x, originy + y |]
    done;
    Graphics.set_color border_color;
    Graphics.draw_poly_line
      [|
	x, y;
	x + delta * 2, y;
	x + delta * 2, y + delta;
	x, y + delta;
	x, y
      |];
    Graphics.set_color Graphics.foreground


let update_display w h devs =
  (** [update_display w h devs] display an update for all devices. *)
  let border_width = 5 in
    List.iter (function
		 | Position2d pos -> display_pos2d border_width border_width pos
		 | Laser laser ->
		     let x = 100 + border_width in
		       display_laser x border_width (w - x) laser)
      devs


let main () =
  begin try
    let client = connect_client () in
    let pos = init_pos2d client 0 in
    let laser = init_laser client 0 in
    let w = 512 and h = 210 in

      Graphics.open_graph "";
      Graphics.set_window_title Sys.argv.(0);
      Graphics.resize_window w h;
      (* Create a new window with the *simple* OCaml graphics
	 package. *)

      Graphics.auto_synchronize false;
      (* Use double buffering. *)

      while true do
	Client.read client;
	(* Block, waiting for the robot to send updated data. *)

	Graphics.clear_graph ();
	(* Clear the back buffer. *)

	update_display
	  (Graphics.size_x ())
	  (Graphics.size_y ())
	  [Position2d pos; Laser laser];
	(* Draw data for each device onto the back buffer. *)

	Graphics.synchronize ();
	(* Blit the back buffer to the display. *)
      done;

      Graphics.close_graph ();
      (* Close the window. *)

      deinit_laser laser;
      deinit_pos2d pos;
      disconnect_client client
  with Playerc_error err_str ->
    Printf.printf "got a playerc error: %s\n" err_str
  end

let _ = main ()
