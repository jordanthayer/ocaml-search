(*
 * File defining a motion primitive type and different operations on it.
 *
 * TODO
 *)

(*****************************************************************************)
(*                               Types                                       *)
(*****************************************************************************)

(*
 * The different types of motion primitives.
 * [x] is in meters.
 * [y] is in meters.
 * [theta] is in radians, 0 is east.
 * [v] is in meters per second.
 * [w] is in radians per second.
 * [dx] is in meters per second.
 * [dy] is in meters per second.
 * [dtheta] is in radians per second.
 *)
type t =
    (* x, y, theta, v, w *)
    Diff_drive of (float * float * float * float * float) array
    (* x, y, theta, dx, dy, dtheta *)
  | Hovercraft of (float * float * float * float * float * float) array
    (* x, y, theta *)
  | Trace of (float * float * float) array

type motion_primitive = { points : t;   (* array of data points *)
                          dt : float; } (* the number of seconds in between each data point *)

type state =
    (* x, y, theta, v, w *)
    Diff_drive_state of (float * float * float * float * float)
    (* x, y, theta, dx, dy, dtheta *)
  | Hovercraft_state of (float * float * float * float * float * float)
    (* x, y, theta *)
  | Trace_state of (float * float * float)


(*****************************************************************************)
(*                    Functions on Motion Primitives                         *)
(*****************************************************************************)

(*
 * Get the number of points in the motion primitive.
 *
 * @param [mp] the motion primitive (motion_primitive).
 *)
let get_length mp = 
  match mp.points with
      Diff_drive arr -> Array.length arr
    | Hovercraft arr -> Array.length arr
    | Trace arr -> Array.length arr

(*
 * Get the duration of the motion primitive in seconds.
 *
 * @param [mp] the motion primitive (motion_primitive).
 *)
let get_duration mp =
  match mp.points with
      Diff_drive arr -> mp.dt *. (float ((Array.length arr) - 1)) 
    | Hovercraft arr -> mp.dt *. (float ((Array.length arr) - 1)) 
    | Trace arr -> mp.dt *. (float ((Array.length arr) - 1)) 

(*
 * Returns a mapping of the points of the motion primitive where each point only
 * contains the x, y, and heading info.
 *
 * @param [mp] the motion primitive (motion_primitive).
 *)
let get_xyh_array mp =
  match mp.points with
      Diff_drive arr -> Array.map (fun (x,y,h,_,_) -> (x,y,h)) arr
    | Hovercraft arr -> Array.map (fun (x,y,h,_,_,_) -> (x,y,h)) arr
    | Trace arr -> Array.map (fun (x,y,h) -> (x,y,h)) arr

(*
 * Returns the x, y, and heading of the robot at some index into the motion
 * primitive points array.
 *
 * @param [mp] the motion primitive (motion_primitive).
 * @param [index] the index to get the x, y, and heading for (int).
 *)
let get_xyh_index mp index =
  match mp.points with
      Diff_drive arr -> let x,y,h,_,_ = arr.(index) in (x,y,h)
    | Hovercraft arr -> let x,y,h,_,_,_ = arr.(index) in (x,y,h)
    | Trace arr -> let x,y,h = arr.(index) in (x,y,h)

(*
 * Returns the x, y, and heading of the robot at some time during the motion
 * primitive. If [time] doesn't fall on a data point, the two points that it
 * falls in between are interpolated to get the x, y, and heading. All motion
 * primitives implicitely start at time 0.
 *
 * @param [mp] the motion primitive (motion_primitive).
 * @param [time] the time to get the x, y, and heading for in seconds (float).
 *)
let get_xyh_time mp time =
  let dur = get_duration mp in
    if time < 0. || time > dur then
      failwith "error: can't access motion primitive - time out of bounds\n"
    else begin
      let f = (time /. mp.dt) in
      let fst = truncate f in
      let snd = int_of_float (ceil f) in
        if fst = snd || snd >= (get_length mp) then
          get_xyh_index mp fst
        else
          let x1,y1,h1 = get_xyh_index mp fst
          and x2,y2,h2 = get_xyh_index mp snd in
          let frac,_ = modf f in
          let x = x1 +. frac *. (x2 -. x1)
          and y = y1 +. frac *. (y2 -. y1)
          and h = h1 +. frac *. (h2 -. h1) in
            (x, y, h)
    end

(*
 * Returns the x, y, and heading of the robot at time 0.
 *
 * @param [mp] the motion primitive (motion_primitive).
 *)
let get_xyh_start mp =
  get_xyh_index mp 0

(*
 * Returns the x, y, and heading of the robot at the end of the motion
 * primitive.
 *
 * @param [mp] the motion primitive (motion_primitive).
 *)
let get_xyh_end mp =
  let i = (get_length mp) - 1 in
    get_xyh_index mp i

(*
 * Returns the complete state of the robot at some index in the motion primitive
 * as a state type.
 *
 * @param [mp] the motion primitive (motion_primitive).
 * @param [index] the index to get the state for (int).
 *)
let get_state_index mp index =
  match mp.points with
      Diff_drive arr -> Diff_drive_state arr.(index) 
    | Hovercraft arr -> Hovercraft_state arr.(index)
    | Trace arr -> Trace_state arr.(index)

(*
 * Returns the start state of the robot.
 *
 * @param [mp] the motion primitive (motion_primitive).
 *)
let get_state_start mp =
  get_state_index mp 0


(*
 * Returns the end state of the robot.
 *
 * @param [mp] the motion primitive (motion_primitive).
 *)
let get_state_end mp =
  let i = (get_length mp) in
    get_state_index mp i

(*
 * rotates a point (x,y) by an angle theta.
 *
 * @param [x] the x location (float).
 * @param [y] the y location (float).
 * @param [theta] the angle to rotate by in radians (float).
 *)
let rotate_point x y theta =
  let cos_t = cos theta
  and sin_t = sin theta in
    (((x *. cos_t) -. (y *. sin_t)), ((x *. sin_t) +. (y *. cos_t)))


(*
 * rotates the x, y, and heading parts of each point in the motion primitive by
 * theta.
 *
 * @param [mp] the motion primitive (t).
 * @param [theta] the angle to rotate by in radians (float).
 *)
let rotate_mp mp theta =
    match mp.points with
        Diff_drive arr ->
          let rot_arr = 
            Array.map
              (fun (x,y,h,v,w) ->
                 let x', y' = rotate_point x y theta in
                 let h' = h +. theta in (* FIXME - normalize *)
                   (x', y', h', v, w))
              arr in
            { points = Diff_drive rot_arr;
              dt = mp.dt; }
      | Hovercraft arr ->
          let rot_arr = 
            Array.map
              (fun (x,y,h,dx,dy,dh) ->
                 let x', y' = rotate_point x y theta in
                 let h' = h +. theta in (* FIXME - normalize *)
                 let dx' = dx (* FIXME - need to rotate *)
                 and dy' = dy in (* FIXME - need to rotate *)
                   (x', y', h', dx', dy', dh))
              arr in
            { points = Hovercraft rot_arr;
              dt = mp.dt; }
      | Trace arr -> 
          let rot_arr = 
            Array.map
              (fun (x,y,h) ->
                 let x', y' = rotate_point x y theta in
                 let h' = h +. theta in (* FIXME - normalize *)
                   (x', y', h'))
              arr in
            { points = Trace rot_arr;
              dt = mp.dt; }


(*****************************************************************************)
(*                             Planning Stuff                                *)
(*****************************************************************************)

let is_applicable start mp = 
  match start with
      Diff_drive_state (_,_,h,v,w) ->
        (match mp.points with
             Diff_drive arr ->
               let _,_,h0,v0,w0 = arr.(0) in
                 (Math.within h h0 epsilon_float)
                 && (Math.within v v0 epsilon_float)
                 && (Math.within w w0 epsilon_float)
           | _ -> invalid_arg "Diff_drive_state expects Diff_drive motion prim")
    | Hovercraft_state (x,y,h,dx,dy,dh) -> 
        (match mp.points with
             Hovercraft arr ->
               let _,_,_,dx0,dy0,dh0 = arr.(0) in
                 (Math.within dx dx0 epsilon_float)
                 && (Math.within dy dy0 epsilon_float)
                 && (Math.within dh dh0 epsilon_float)
           | _ -> invalid_arg "Hover_craft_state expects Hovercraft motion prim")
    | Trace_state s -> 
        (match mp.points with
             Trace arr -> true
           | _ -> invalid_arg "Trace_state expects Trace motion prim")


(*****************************************************************************)
(*                           Motion Primitive I/O                            *)
(*****************************************************************************)

(*
 * For the given motion primitive tag, reads in [num] data points and returns
 * them as an array.
 *
 * @param [ch] the input channel to read from (in_channel).
 * @param [s] the tag for the type of motion primitive (string).
 * @param [num] the number of data points to read in (int).
 *)
let read_type ch s num =
  match s with
      "diff_drive" ->
        let arr = Array.init num (fun _ ->
                                    let x = Wrio.input_float ch in
                                    let y = Wrio.input_float ch in
                                    let h = Wrio.input_float ch in
                                    let v = Wrio.input_float ch in
                                    let w = Wrio.input_float ch in
                                      (x,y,h,v,w)) in
          (Diff_drive arr)
    | "hovercraft" -> 
        let arr = Array.init num (fun _ ->
                                    let x = Wrio.input_float ch in
                                    let y = Wrio.input_float ch in
                                    let h = Wrio.input_float ch in
                                    let dx = Wrio.input_float ch in
                                    let dy = Wrio.input_float ch in
                                    let dh = Wrio.input_float ch in
                                      (x,y,h,dx,dy,dh)) in
          (Hovercraft arr)
    | "trace" ->
        let arr = Array.init num (fun _ ->
                                    let x = Wrio.input_float ch in
                                    let y = Wrio.input_float ch in
                                    let h = Wrio.input_float ch in
                                      (x,y,h)) in
          (Trace arr)
    | s -> failwith (Printf.sprintf "unknown motion primitive type: %s\n" s)

(*
 * Writes the points of a motion primitive to an output channel.
 *
 * @param [ch] the output channel to write to (out_channel).
 * @param [pts] the array of points for the motion primitive (t array).
 *)
let write_points ch pts =
  match pts with
      Diff_drive arr ->
        Printf.fprintf ch "%d\n" (Array.length arr);
        Array.iter
          (fun (x,y,h,v,w) ->
             Printf.fprintf ch "%f %f %f %f %f\n" x y h v w)
          arr
    | Hovercraft arr ->
        Printf.fprintf ch "%d\n" (Array.length arr);
        Array.iter
          (fun (x,y,h,dx,dy,dh) ->
             Printf.fprintf ch "%f %f %f %f %f %f\n" x y h dx dy dh)
          arr
    | Trace arr ->
        Printf.fprintf ch "%d\n" (Array.length arr);
        Array.iter
          (fun (x,y,h) ->
             Printf.fprintf ch "%f %f %f\n" x y h)
          arr

(*
 * Reads a motion primitive from a given input channel.
 *
 * @param [ch] the input channel to read from (in_channel).
 *)
let read ch = 
  let t = Wrio.read_token ch in
    (if t = "" then raise End_of_file);
  let dt = Wrio.input_float ch in
  let num = Wrio.input_int ch in
  let pts = read_type ch t num in
    { points = pts;
      dt = dt; }

(*
 * Writes the motion primitive to the given output channel.
 *
 * @param [ch] the output channel to write to (out_channel).
 * @param [mp] the motion primitive to write (motion_primitive).
 *)
let write ch mp = 
  (match mp.points with 
       Diff_drive _ -> Printf.fprintf ch "diff_drive\n"
     | Hovercraft _ -> Printf.fprintf ch "hovercraft\n"
     | Trace _ -> Printf.fprintf ch "trace\n");
  Printf.fprintf ch "%f\n" mp.dt;
  write_points ch mp.points

(*
 * Loads the motion primitive from the given file.
 *
 * @param [path] the filepath to load the motion prim from (string)
 *)
let load path = 
  Wrio.with_infile path read

(*
 * Saves the motion primitive to the given file.
 *
 * @param [path] the filepath to save the motion prim to (string)
 * @param [mp] the motion primitive to write (motion_primitive)
 *)
let save_mp path mp =
  Wrio.with_outfile path (fun s -> write s mp)

(* EOF *)
