(* Displays grid world problems *)

let grid_window = GWindow.window ~title:"Grid Display" ~width:1200 ~height:1200
  ~border_width:0 ()
(*let vb = GPack.vbox ~packing:grid_window#add ()
let scroll_win = GBin.scrolled_window ~border_width:50 ~hpolicy:`AUTOMATIC
  ~vpolicy:`AUTOMATIC ~packing:vb#add ()
let vp = GBin.viewport ~width:800 ~height:800 ~shadow_type:`NONE ~packing:scroll_win#add ()
*)

let q = GMisc.drawing_area ~width:8000 ~height:8000 ~packing:grid_window#add()
let w = q#misc#realize (); q#misc#window
let draw_area = new GDraw.drawable w
let pmap = ref (GDraw.pixmap ~width:8000 ~height:8000 ())

type 'a t = {
  blocked : bool array array;
  size : int * int;
  start : int * int;
  goal : (int * int) list;
  mutable lower_left : int * int;
}

let do_cross_display = ref true

(**************************************************************************)
(* Drawing Code *)

let set_lower_left t lx ly =
  (** Sets the grid displays lower lefthand corner to be [lx], [ly] *)
  t.lower_left = lx, ly

let new_pmap () =
  !pmap#set_foreground `BLACK;
  !pmap#rectangle ~x:0 ~y:0 ~width:8000 ~height:8000 ()

let redraw t =
  draw_area#put_pixmap ~x:0 ~y:0 ~xsrc:0 ~ysrc:0 !pmap#pixmap;
  new_pmap ()


let getColor hd min max =
  (** Returns the color to use at the current square given hd, min, max *)
  Render.scale hd min max


let drect t (x,y) =
  (** macro for drawing a grid square at a given x,y location, assumes the
      color to be used has already been set. *)
  let ox, oy = t.lower_left in
    !pmap#rectangle ~x:((x* !Constants.grid_squareSize) + ox)
      ~y:((y* !Constants.grid_squareSize) + oy)
      ~width:!Constants.grid_squareSize ~height:!Constants.grid_squareSize
      ~filled:true ()


let dcross t (x,y) =
  (** Draws the little crosses that you see in expanded nodes *)
  if !Constants.grid_squareSize >= 3 && !do_cross_display
  then
    (let ox, oy = t.lower_left in
       !pmap#set_line_attributes ~width:1 ();
       !pmap#set_foreground `BLACK;
       !pmap#segments
	 [ (x * !Constants.grid_squareSize + ox,
	    y * !Constants.grid_squareSize + oy),
	    ((x+1) * !Constants.grid_squareSize + ox,
	     (y+1) * !Constants.grid_squareSize + oy);
	   (x * !Constants.grid_squareSize + ox,
	    (y+1) * !Constants.grid_squareSize + oy),
	    ((x+1) * !Constants.grid_squareSize + ox,
	     y * !Constants.grid_squareSize + oy); ])


let rec draw_goals gd lst =
  match lst with
      [] -> ()
    | hd::tl -> (drect gd hd;
		 draw_goals gd tl)


let clean gd =
  (** Clears board for next draw *)
  !pmap#set_foreground `BLACK;
  !pmap#rectangle  ~x:0 ~y:0 ~width:880 ~height:800 ~filled:true ();
  let mx,my = gd.size in
    for x = 0 to mx - 1
    do
      for y = 0 to mx - 1
      do
	(if not (gd.blocked.(x).(y))
	 then !pmap#set_foreground `WHITE
	 else !pmap#set_foreground `BLACK;
	 drect gd (x,y))
      done
    done



let overlay_path t gc step min max xy_list =
  (** overlays a line relating the xypoints onto teh grid *)
  (* set the color to green for the overlay *)
  let offset = !Constants.grid_squareSize / 2 in
  let ox, oy = t.lower_left in
  let point_to_center v = v * !Constants.grid_squareSize + offset in
  let rec build_segments list =
    match list with
      | [] -> []
      | [singleton] -> []
      | [(x1,y1);(x2,y2)] -> [(((point_to_center x1) + ox,
				(point_to_center y1) + oy),
			       ((point_to_center x2) + ox,
				(point_to_center y2) + oy))]
      | (x1,y1)::(x2,y2)::tl -> (((point_to_center x1) + ox,
				  (point_to_center y1) + oy),
				 ((point_to_center x2) + ox,
				  (point_to_center y2) + oy))::
	  (build_segments ((x2,y2)::tl))
  in
  let segments = build_segments xy_list in
    List.iter
      (fun key ->
	 !pmap#set_foreground (`RGB (getColor (gc key step) min max));
	 drect t key) xy_list;
    !pmap#set_line_attributes ~width:(!Constants.grid_squareSize-3) ();
    !pmap#set_foreground (`RGB (0,40000,0));
    !pmap#segments segments;
  !pmap#set_foreground (`RGB (0, 40000, 0));
  drect t t.start;
  draw_goals t t.goal



let unlay_path gd gc step min max xy_list =
  List.iter
    (fun key ->
       !pmap#set_foreground (`RGB (getColor (gc key step) min max));
       drect gd key;
       dcross gd key;) xy_list



let overlay_struct t qs gc step min max =
  (** Draws a dpq (and only a dpq at the moment) over top of the grid *)
  List.iter
    (fun q ->
       (Array.iter
	  (fun e ->
	     !pmap#set_foreground (`RGB (getColor (gc e step) min max));
	     drect t e)
	  (Structure.get_entry q.Dpq_display.queue step).Structure.keys)) qs;
  !pmap#set_foreground (`RGB (0, 40000, 0));
  drect t t.start;
  draw_goals t t.goal


let unlay_struct t qs gc step min max =
  if step > 0
  then
    (let last_q = List.map
       (fun q -> (Structure.get_entry
		    q.Dpq_display.queue (step - 1)).Structure.keys) qs
     and this_q = List.map
       (fun q -> (Structure.get_entry
		    q.Dpq_display.queue step).Structure.keys) qs in
     let diffs = List.map2
       (fun lq tq ->
	  let rec filt torm lst =
	    match torm with
	      | _ -> lst
	      (*| hd::tl -> filt tl (List.filter (fun v -> v <> hd) lst)*) in
	    filt (Array.to_list lq) (Array.to_list tq)) last_q this_q in
       List.iter (fun ls -> List.iter
		    (fun v ->
		       !pmap#set_foreground
			 (`RGB (getColor (gc v 0) min max));
		       drect t v) ls) diffs)


let rec drawStep gd gc (run,step) ?(color_step = step) min max =
  (** Draws the grid
      [gd] the grid dispaly
      [gc] returns a float of the current context of the node
      [run] The recorded run
      [step] current step
      [min] minimum value for the context
      [max] maximum value for the context *)
  !pmap#set_foreground
    (`RGB (getColor (gc run.Recorded_run.sequence.(step) color_step) min max));
  drect gd run.Recorded_run.sequence.(step);
  dcross gd run.Recorded_run.sequence.(step);
  !pmap#set_foreground (`RGB (0, 40000, 0));
  drect gd gd.start;
  draw_goals gd gd.goal


let drawGrid gd gc (run,step) min max =
  (** Draws the grid
      [gd] the grid dispaly
      [gc] returns a float of the current context of the node
      [run] The recorded run
      [step] current step
      [min] minimum value for the context
      [max] maximum value for the context *)
  Verb.pe Verb.debug "Drawing step %i\n" step;
      (* Draws nodes which have been expanded *)
      for i = 0 to step
      do
	drawStep gd gc (run,i) ~color_step:step  min max
      done;
      (* Overlays start and goal state *)
      !pmap#set_foreground (`RGB (0, 40000, 0));
      drect gd gd.start;
      draw_goals gd gd.goal


let removeStep gd gc (run,step) min max =
  if step = 1 then ((clean gd);
		    (drawGrid gd gc (run,0) min max))
  else (!pmap#set_foreground `WHITE;
	drect gd run.Recorded_run.sequence.(step))


let rec resize ss gd =
  (** Resizes the grid to use squares of size [ss] *)
  Constants.grid_squareSize := ss;
  let xdim = fst gd.size * ss
  and ydim = snd gd.size * ss in
    q#set_size ~width:xdim ~height:ydim;
    (*vp#set_size ~width:xdim ~height:ydim;*)
    drawGrid gd


(**************************************************************************)
(* Loading code lifted wholesale from grids*)

let rec get_goals x lst accu =
  match lst with
      h::tl ->
	(match x with
	     None -> get_goals (Some h) tl accu
    | Some v -> get_goals None tl ((v,h)::accu))
    | [] -> accu


let read ch =
  let h = Wrio.input_int ch in
  let w = Wrio.input_int ch in
    Wrio.skip_through_str ch "Board:\n";
    let b = Array.init w (fun _ -> Array.make h false) in
      for y = h - 1 downto 0 do
	for x = 0 to w - 1 do
	  b.(x).(y) <- (input_char ch) = '#'
	done;
	Wrio.skip_line ch;
      done;
      let _ = (input_line ch) in
      let _ = (input_line ch) in
	match Wrio.read_ints ch with
	    sx::sy::tl ->
	      (Verb.pe Verb.always "Start @ %d,%d\n" sx sy;
	       { blocked = b;
		 size = w,h;
		 start = (sx,sy);
		 goal = (get_goals None tl []);
		 lower_left = Constants.offset , Constants.offset;
	       })
	  | _ -> failwith "oops"



let mouse_event t =
  let x_pos = GdkEvent.Motion.x t
  and y_pos = GdkEvent.Motion.y t in
    Verb.pe Verb.always "Mouse @ %f,%f\n%!" x_pos y_pos;
    true


let load path =
  Verb.pe Verb.always "Loading %s\n" path;
  let b = Wrio.with_infile path read in
  let mx,my = b.size in
    Verb.pe Verb.always "%i x %i\n" mx my;
    for x = 0 to (mx - 1)
    do
      for y = 0 to (my - 1)
      do
	try
	  ((if b.blocked.(x).(y)
	    then (!pmap#set_foreground `BLACK;
		  drect b (x,y))))
	with Invalid_argument str -> (Verb.pe Verb.always "%i %i oob\n" x y)
      done
    done;
    Verb.pe Verb.always "Loaded\n";
    b



(* EOF *)
