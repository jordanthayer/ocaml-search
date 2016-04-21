(*
 * This is a program that allows a user to trace a path around a map and have it
 * output as a list of motion primitives
 *)


(*
 *
 *)
let rec safe_delay dur =
  let start = Unix.gettimeofday () in
  try
    Thread.delay dur
  with _ -> (* we were interrupted... keep sleeping *)
    let dt = Unix.gettimeofday () -. start in
    if dt < dur then begin
      safe_delay dt
    end

let line_feed = 10 (* decimal representation of linefeed *)
let comment   = 35 (* decimal representation of comment *)
let space     = 32 (* decimal representation of comment *)
let byte_size = 8  (* bits per byte *)
let blocked = true
let white = 0x00FFFFFF

(**
  * eats a one line comment in the file starting at the current seek
  * position in the file. After eating the comment the read 
  * position will be on the beginning of the next line.
  *)
let eat_comment file =
  let rec eat_c file =
    let c = input_byte file in
    if c = line_feed then
      ()
    else
      eat_c file
  in
  let c = input_byte file in
  if c = comment then
    eat_c file
  else begin
    let pos = pos_in file - 1 in (* wasn't a comment, backtrack *)
    seek_in file pos;
    ()
  end


(**
  * reads from file until a space is found.
  *
  * @return String. everything read up until the space was
  * read.
  *)
let read_until_space file =
  let rec until_white buf file =
    let c = input_byte file in
      if c = space || c = line_feed then
        Buffer.contents buf
      else
        (Buffer.add_char buf (Char.chr c);
        until_white buf file)
  in
  let buf = Buffer.create 5 in
    until_white buf file

(**
  * read the world representation from [file]. Any non 0 value
  * is considered a blocked cell.
  *
  * @param [width] int. the number x dimension cells
  * @param [height] int. the number of y dimension cells
  * @param [file] in_channel. pointed at the beginning of the
  *   world representation.
  *
  * @return bool array array. a bool matrix representing the world.
  *)
let read_world width height file = 
  eat_comment file;
  let world = Array.make_matrix height width (not blocked) in
  for i = 0 to ( height - 1 ) do
    for j = 0 to ( width - 1 ) do
      let color = ref 0 in
      for b = 2 downto 0 do
        let c = input_byte file in
        let shifted = c lsl (b*byte_size) in
        color :=  !color lor shifted
      done;
      if !color <> white then world.(i).(j) <- blocked;
    done;
  done;
  let new_world = Array.make_matrix height width (not blocked) in
  for x = 0 to ( height - 1 ) do
    for y = 0 to ( width - 1 ) do
      new_world.(x).(y) <- world.(height-(1+y)).(x)
    done;
  done;
  new_world

(**
  * takes a pnm filename and reads it
  * to determine the width x height of
  * it
  *
  * @param [pnm] String. Filename of the pnm to read in
  * @return (float * float * bool array array). returns the width and height of
  *  the pnm as well as a bool matrix representing the blocked and unblocked
  *  cells in that order of [pnm]
  *)
let process_pnm pnm =
  let file = open_in pnm in
  let b1 = input_char file in
  let b2 = input_char file in
  (* ensure that this is a raw pnm file *)
  if b1 != 'P' || b2 != '6' then 
    failwith (pnm ^ " is not a raw PNM file format");
  ignore(input_byte file); (* throw away the line feed *)
  (* possibly eat up the comment following the format type *)
  eat_comment file;
  (* read in the dimensions *)
  let width = int_of_string (read_until_space file) in
  let height = int_of_string (read_until_space file) in
  (* eat up the max intensity value *)
  ignore(read_until_space file);
  (* read in the world *)
  let world = read_world width height file in
  close_in file;
  (float width), (float height), world

(*
 *
 *)
let load_bitmap file =
  let width, height, world = process_pnm file in
    world


(*****************************************************************************)
(*                            Drawing Functions                              *)
(*****************************************************************************)

(**
 * Fills in a grid square with a specified color.
 * @param [px_res] the resolution in pixels per meter (float).
 * @param [cell_res] the cell resolution in meters per cell (float).
 * @param [cell_x] the x index of the cell (int).
 * @param [cell_y] the y index of the cell (int).
 * @param [color] the fill color (Graphics.color).
 *)
let fill_cell px_res cell_res cell_x cell_y color =
  let w = truncate (cell_res *. px_res) in
  let x = Math.round (((float cell_x) *. cell_res) *. px_res)
  and y = Math.round (((float cell_y) *. cell_res) *. px_res) in
    Graphics.set_color color;
    Graphics.fill_rect x y w w

(**
 * Fills in the static obstacles as black grid squares.
 * @param [px_res] the resolution in pixels per meter (float).
 * @param [instance] the problem instance (Robot_plan_instance.instance).
 *)
let fill_static_obs px_res cell_res obstacles =
  Graphics.set_color Graphics.black;
  Array.iteri
    (fun j arr ->
       Array.iteri
         (fun k obs ->
            if obs then fill_cell px_res cell_res j k Graphics.black)
         arr)
    obstacles


let display_screen px_res cell_res obstacles =
  let cell_w = Array.length obstacles
  and cell_h = Array.length obstacles.(0) in
  let width = Math.round ((float cell_w) *. cell_res *. px_res)
  and height = Math.round ((float cell_h) *. cell_res *. px_res) in
    Graphics.open_graph "";
    Graphics.resize_window width height;
    fill_static_obs px_res cell_res obstacles


(*****************************************************************************)
(*                                   Main                                    *)
(*****************************************************************************)

(*
 *
 *)
let px_to_motion_prim px_res px_arr action_time time_points =
  let len = Array.length px_arr in
  let dt = action_time /. (float (time_points - 1)) in
    assert (len > 1);
  (* convert pixels to meters, calculate headings *)
  let mp_arr = Array.make len (0.0, 0.0, 0.0) in
    for i = 0 to len - 2 do
      let x0,y0 = px_arr.(i) in
      let x1,y1 = px_arr.(i+1) in
      let h = atan2 (float (y1-y0)) (float (x1-x0)) in
      let x = (float x0) /. px_res
      and y = (float y0) /. px_res in
        mp_arr.(i) <- (x, y, h)
    done;
    let _,_,h = mp_arr.(len-2) in
    let px,py = px_arr.(len-1) in
      mp_arr.(len-1) <- (((float px) /. px_res), ((float py) /. px_res), h);
    (* convert to a list of motion primitives *)
    let rec f i mps =
      if len - i >= time_points then
        (* get subset of points to make a single motion primitive *)
        let a = Array.sub mp_arr i time_points in
        let mp = { Motion_prim.points = Motion_prim.Trace a;
                   Motion_prim.dt = dt; } in
          f (i + time_points - 1) (mp::mps)
      else
        (* fill in rest of motion primitive with last state *)
        let a = Array.init time_points
                  (fun n -> if i + n < len then mp_arr.(i+n)
                            else mp_arr.(len - 1)) in
        let mp = { Motion_prim.points = Motion_prim.Trace a;
                   Motion_prim.dt = dt; } in
          (mp::mps)
    in
      f 0 []


(*
 *
 *)
let record_pixels px_res cell_res obstacles action_time time_points = 
  let mouse_pressed = ref false in
  let mouse_released = ref false in
  let wait = action_time /. (float (time_points - 1)) in
  let start_time = ref 0.0 in
  let last_time = ref 0.0 in
  let px_positions = ref [] in
    Graphics.set_color Graphics.red;
  (* wait for user to press mouse *)
  while not !mouse_pressed do
    let s = Graphics.wait_next_event [Graphics.Button_down] in
      (* check that it is in the window *)
    let mx = s.Graphics.mouse_x
    and my = s.Graphics.mouse_y in
      if (mx >= 0 && mx < (Graphics.size_x ())) 
      && (my >= 0 && my < (Graphics.size_y ())) then 
        ( 
          mouse_pressed := true;
          px_positions := (mx,my)::!px_positions;
          Graphics.plot mx my;
          Graphics.moveto mx my;
          Graphics.fill_ellipse mx my 3 3;
          last_time := Unix.gettimeofday ();
          start_time := !last_time
        )
  done;
  (* record mouse positions until user releases mouse *)
  while not !mouse_released do
    let t = (Unix.gettimeofday ()) -. !last_time in
    (if t < wait then safe_delay wait
     else failwith (Printf.sprintf "couldn't meet time constraints!\n  wait = %f, t = %f\n" wait t)
    );
    last_time := Unix.gettimeofday ();
    let mx, my = Graphics.mouse_pos () in
      assert (mx >= 0 && mx < (Graphics.size_x ()) 
              && my >= 0 && my < (Graphics.size_y ()));
      Graphics.lineto mx my;
      Graphics.fill_ellipse mx my 3 3;
      px_positions := (mx,my)::!px_positions;
      if not (Graphics.button_down ()) then
        mouse_released := true
  done;
  let tt = (Unix.gettimeofday ()) -. !start_time in
  Printf.fprintf stderr "total time elapsed: %f seconds\n" tt;
  Printf.fprintf stderr "motion prims expected = %d\n" (Math.round (ceil (tt /. action_time)));
  Array.of_list (List.rev !px_positions)


(*
 *
 *)
let process px_res cell_res obstacles action_time time_points = 
  (* draw the screen and the map *)
  display_screen px_res cell_res obstacles;
  (* record action *)
  let arr = record_pixels px_res cell_res obstacles action_time time_points in
  (* convert to motion primitives *)
  let mps = px_to_motion_prim px_res arr action_time time_points in
    Printf.fprintf stderr "number of motion prims = %d\n" (List.length mps);
    mps

(*
 *
 *)
let main () = 
  let map_file = ref ""
  and cell_res = ref 0.0 
  and px_res = ref 0.0
  and action_time = ref 0.0 
  and time_points = ref 0 in
  let ksd_list, anon, usage = 
    [],
    (fun s -> match !Arg.current with
         0 -> ()
       | 1 -> map_file := s
       | 2 -> cell_res := float_of_string s
       | 3 -> px_res := float_of_string s
       | 4 -> action_time := float_of_string s
       | 5 -> time_points := int_of_string s
       | _ -> failwith "Error - unknown argument"
    ),
    ("[usage]  tracer bitmap cell_res px_res action_time\n"
     ^ "  bitmap      - the map file to use (.pnm)\n"
     ^ "  cell_res    - the map cell resolution in meters per cell\n"
     ^ "  px_res      - the screen resolution in pixels per meter\n"
     ^ "  action_time - the duration of each motion primitive in seconds\n"
     ^ "  time_points - number of data points for each motion primitive")
  in
    (if (Array.length Sys.argv) <> 6 then  
       (Arg.usage ksd_list usage;
        exit 1));
    Arg.parse ksd_list anon usage;
    (if !action_time = 0.0 then
       (Arg.usage ksd_list usage;
        exit 1));
    (if !map_file <> "" && not (Sys.file_exists !map_file) then
       failwith ("Error - bitmap file doesn't exist: " ^ !map_file));
    (* read bitmap to get obstacle matrix *)
    let obstacles = load_bitmap !map_file in
    (* get motion primitives *)
    let mps = process !px_res !cell_res obstacles !action_time !time_points in
    (* draw motion primitives *)
    List.iter (fun mp -> Motion_prim_vis.draw_motion_prim !px_res (0,0) mp) mps;
    (* output motion primitives to stdout *)
    List.iter (fun mp -> Motion_prim.write stdout mp) mps;
    (* wait for user to click again to exit *)
    Printf.fprintf stderr "click mouse to exit\n%!";
    Graphics.wait_next_event [Graphics.Button_down]




let _ = main ()


(* EOF *)
