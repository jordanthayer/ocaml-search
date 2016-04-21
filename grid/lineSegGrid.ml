(* Builds Line Segment based grid maps.  Meant to look maze-ish, be more
   entertaining than your basic grid *)

open Grid_instance


type vector = {
  direction: float * float;
  magnitude: float;
}


type segment = {
  endpoint : int * int;
  vec : vector
}


(*finds the magnitude of a supplied vector *)
let magnitude vec =
  match vec.direction with
      (x,y) -> sqrt( x *. x +. y *. y)


(* checks to see if a given vector when located at a point is
   within bounds *)
let check_bounds (sx,sy) vec minX minY  maxX maxY =
  let dx,dy = vec.direction
  and sx = float_of_int sx
  and sy = float_of_int sy
  and minX = float_of_int minX
  and maxX = float_of_int maxX
  and minY = float_of_int minY
  and maxY = float_of_int maxY
  and mag = vec.magnitude in
  let ex = sx +. dx *. mag
  and ey = sy +. dy *. mag in
    ex >= minX & ex <= maxX & ey >= minY & ey <= maxY &
    sx >= minX & sx <= maxX & sy >= minY & sy <= maxY


(* returns a unit vector of random orientation*)
let rec random_vector mag =
  Random.self_init();
  let xdir = (Random.float 2.) -. 1.
  and ydir = (Random.float 2.) -. 1. in
  let raw_mag = magnitude
    { direction = (xdir,ydir);
      magnitude = mag } in
    (* if we got a stationary vector, pitch it.
       Otherwize, normalize it to a unit vector and return. *)
    if raw_mag = 0.
    then random_vector mag
    else     { direction = (xdir /. raw_mag ,ydir /. raw_mag);
	       magnitude = mag}


(* Returns a line segment wholly within the board. *)
let rec random_segment mag minX minY maxX maxY =
  let vec = random_vector mag
  and ep = ((Random.int maxX) - minX, (Random.int maxY) - minY)
  in
    if check_bounds ep vec minX minY maxX maxY
    then {endpoint =  ep;
	  vec = vec}
    else random_segment mag minX minY maxX maxY


(* builds a segment list of length n, each having length between
   min_mag and max_mag *)
let rec segmentList ?(min_mag = 0.) ?(max_mag = 10.) minX minY maxX maxY n =
  let mag = (Random.float (max_mag -. min_mag)) +. min_mag in
    if n = 0
    then []
    else (random_segment mag minX minY maxX maxY)::
      (segmentList ~min_mag:min_mag ~max_mag:max_mag
	 minX minY maxX maxY (n - 1))


(* blocks the line segments on the board *)
let drawSegment board segment =
  let delta = 0.0001 (* this is sort of a hack *)
  and (x,y) = segment.endpoint
  and (dx,dy) = segment.vec.direction in
  let xp = ref (float_of_int x)
  and yp = ref (float_of_int y)
  and stopx = (float_of_int x) +. dx *. segment.vec.magnitude
  and stopy = (float_of_int y) +. dy *. segment.vec.magnitude in
    while (abs_float (!xp -. stopx)) > delta &
      (abs_float (!yp -. stopy)) > delta
    do
      (let ix = int_of_float !xp
       and iy = int_of_float !yp in
	 board.(ix).(iy) <- true;
	 xp := !xp +. dx *. segment.vec.magnitude *. delta;
	 yp := !yp +. dy *. segment.vec.magnitude *. delta)
    done;
    ()


(* places all line segments on a board and returns it *)
let rec drawSegments board segments =
  match segments with
      [] -> ()
    | h::t ->
	(drawSegment board h);
	drawSegments board t


(* makes a board ready to be done *)
let init_ar maxX maxY =
  Array.make_matrix maxX maxY false


(* creates a board with the given segment parms *)
let doSegments maxX maxY segC =
  let b = init_ar maxX maxY
  and segs =
    segmentList
      ~min_mag:(sqrt(float_of_int (maxX * maxX + maxY * maxY)) *. 0.05)
      ~max_mag:(sqrt(float_of_int (maxX * maxX + maxY * maxY)) *. 0.25)
      0 0 (maxX - 1) (maxY -1) segC in
    drawSegments b segs;
    b


let make_segment_board width height count c m =
  (** not necessarily feasible! *)
  let blocked = doSegments width height count in
    Grid_instance.make_board blocked c m (0,(height / 2))
      ((width-1),(height / 2))


let feasible_board solver w h cost_model move_model lines =
  Wrutils.eval_until (fun () ->
		      Wrutils.pr "trying...%!";
		      make_segment_board w h lines cost_model move_model)
    (Grid_instance.feasible_p solver)
