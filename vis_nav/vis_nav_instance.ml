(** Instance Representation and loadings *)

type vertex = {
  x: float;
  y: float;
}

type state = {
  location : vertex;
  mutable parent : state;
}

type problem = {
  width : float;
  height : float;
  objects : vertex list list;
  connections : (vertex, (vertex list)) Hashtbl.t;
  start : vertex;
  goal : vertex;
}

let default_start = {x=0.;y=0.}
let default_goal = {x=100.;y=100.}

let dummy_v = {
  x = -1.;
  y = -1.;
}

let test_start = {x=0.;y=0.5}
let test_goal = {x=5.;y=0.5}
let test_obj1 = [{x=1.;y=0.1};
	       {x=2.;y=0.1};
	       {x=2.;y=1.};
	       {x=1.;y=1.}]
let test_obj2 = [{x=3.;y=0.};
	       {x=4.;y=0.};
	       {x=4.;y=1.1};
	       {x=3.;y=1.1}]
let horiz = {test_start with x = 999999.}

let make_vert vx vy =
  {x=vx; y=vy}


let make_state_to_str () =
  (** Makes a function for converting states into a strings for printing. *)
  (fun s ->
     let mx,my = s.location.x, s.location.y in
       Wrutils.str "location: %f, %f" mx my)

let state_to_str = make_state_to_str ()


(***************************** Printing *********************************)

let print_vert ?(before = "") ?(after = "") v =
  Printf.printf "%s(%f,%f)%s" before v.x v.y after

let print_obj ?(before = "") ?(after = "") obj =
  Printf.printf "%s" before;
  List.iter (fun v -> print_vert ~before:" " v) obj;
  Printf.printf "%s" after

let print_state ?(before = "") ?(after = "") state =
  Printf.printf "%s%s%s" before (state_to_str state) after


(***************************** Drawing *********************************)


let offset = 0.
let cell_size = 1.
let cell_offset = 0.

let make_scale maxx maxy =
  (** returns function mapping to PS position *)
  (* want max dim to fit page *)
  let perx = ((Psout.in2pt 8.5) -. (2. *. offset)) /. maxx
  and pery = ((Psout.in2pt 11.) -. (2. *. offset)) /. maxy in
  let per = min perx pery in
    (fun x ->
       x *. per)

let make_scale_cell w =
  make_scale w.width w.height

let make_scale_pos w =
  make_scale (w.width *. cell_size) (w.height *. cell_size)

let world_coords w =
  let s = make_scale_cell w in
    offset, offset,
    offset +. (s w.width),
    offset +. (s w.height)

let world_box w =
  let s = make_scale_cell w in
  let x1,y1,x2,y2 = 0.,0.,(s w.width),(s w.height) in
    Box.of_coords (x1 -. 5.) (y1 -. 5.) (x2 +. 5.) (y2 +. 5.)

let mark_state pso w v color =
  let s = make_scale_pos w in
  let x = offset +. (s v.x) +. (s cell_offset)
  and y = offset +. (s v.y) +. (s cell_offset)
  and r = 7. in
    Psout.circle pso ~color:color x y r

let draw_world pso w =
  (let x1,y1,x2,y2 = world_coords w in
     Psout.polyline pso [x1, y1;
			 x2, y1;
			 x2, y2;
			 x1, y2;
			 x1, y1]);
  (let s = make_scale_cell w in
     List.iter (fun o ->
		  let last = Wrlist.last o in
		    Psout.polyline pso
		      (((offset +. (s last.x)),
			(offset +. (s last.y)))::
			 (List.map (fun v -> (offset +. (s v.x)),
				      (offset +. (s v.y))) o)))
       w.objects);
  (* Draws the visibility graph, but it will cover objects. *)
  (*(let s = make_scale_cell w in
     Hashtbl.iter (fun v vis ->
		     List.iter (fun other -> Psout.line pso
				  ~color:Psout.light_gray
				  (offset +. (s v.x))
				  (offset +. (s v.y))
				  (offset +. (s other.x))
				  (offset +. (s other.y))) vis)
       w.connections);*)
  mark_state pso w w.start Psout.med_gray;
  mark_state pso w w.goal Psout.dark_gray

let with_ps_world path w f =
  (** write PS of [w] to [path], then calls [f] on pso and scaler *)
  Psout.to_file path (world_box w) "Robot Navigation" Psout.Portrait false
    (fun pso ->
       draw_world pso w;
       f pso)

let draw_instance path w =
  (** generates a postscript file at [path] *)
  with_ps_world path w Fn.no_op1


(***************************************************************************)
let update_parent node new_parent =
  (** Replaces the parent of [node] with [new_parent] *)
  node.parent <- new_parent

let get_parent node =
  (** returns the parent of [node] *)
  node.parent


let make_root start =
  (** Generates the root of a search problem for a given problem
      [start] - starting location of the agent *)
  let rec n = {location = start;
	       parent = n; } in
    n


let make_goal problem =
  (** Creates a goal test for the given [problem]*)
  (fun state -> state.location = problem.goal)


let make_explicit_target_goal target =
  (** Creates a goal test for the given [target]*)
  (fun state -> state.location = target)

let key state =
  (** Uniquely identifies a state *)
  state.location

let distance v1 v2 =
  (** Gets euclidian distance between two vertices *)
  let x1 = v1.x
  and y1 = v1.y
  and x2 = v2.x
  and y2 = v2.y in
    sqrt (((x1-.x2)**2.) +. ((y1-.y2)**2.))

let make_expand problem =
  (** Generates the expand function based on the [problem] -- Generates
      the parents by default *)
  (fun state g ->
     let children = ref []
     and con = Hashtbl.find problem.connections state.location in
       List.iter
	 (fun new_loc -> (Wrutils.push
			    ({location = new_loc; parent = state},
			     g+.(distance state.location new_loc)) children))
	 con;
       !children)


let find_move start result =
  (** What move was taken between [start] and [result] *)
  start.location, result.location

let rec find_path solution_state =
  (** Returns a sequence of steps to solve the problem in reverse *)
  if solution_state.parent == solution_state
  then []
  else ((*print_state solution_state ~after:"\n";*)
	((find_move solution_state.parent solution_state)::
	   (find_path solution_state.parent)))

let get_solution state =
  (* God help us if the solution is too long to reverse w/o tail recursion *)
  List.rev (find_path state)

let move_to_string mv =
  let start = fst mv
  and next = snd mv in
  Wrutils.str "from %f, %f to %f, %f\n" start.x start.y next.x next.y


let solution_to_string sol =
  (** Converts the in order move list [sol] into a string *)
  List.fold_left (fun accum next ->
		    accum ^ (move_to_string next)) "" sol


let revsolution_to_string sol =
  (** Converts the reversed move list [sol] into a string *)
  List.fold_left (fun accum next ->
		    (move_to_string next) ^ accum) "" sol


(***************************** Generating *********************************)

let pi = 3.14159265358979312

let slope v1 v2 =
  (v2.y -. v1.y)/.(v2.x -. v1.x)

let get_angle_orig v1 v2 v3 =
  (** Gets angle between three points, relative to v2. *)
  let ax = v1.x
  and ay = v1.y
  and bx = v2.x
  and by = v2.y
  and cx = v3.x
  and cy = v3.y in
  let abx = ax -. bx
  and aby = ay -. by
  and cbx = cx -. bx
  and cby = cy -. by in
  let result = (atan2 (abx*.cby -. aby*.cbx) (abx*.cbx +. aby*.cby))
    *. (180. /. pi) in
    ((*print_vert v1;
     print_vert ~before:" " v2;
     print_vert ~before:" " v3 ~after:"\n";*)
     if result < 0. then
       ((*Printf.printf "%f\n" (result +. 360.);*)
	result +. 360.)
     else
       ((*Printf.printf "%f\n" result;*)
	  result))

let get_angle v1 v2 v3 =
  (** Gets angle between three points, relative to direction. *)
  let ax = v1.x
  and ay = v1.y
  and bx = v2.x
  and by = v2.y
  and cx = v3.x
  and cy = v3.y in
  let abx = bx -. ax
  and aby = by -. ay
  and cbx = cx -. bx
  and cby = cy -. by in
  let result = (atan2 (abx*.cby -. aby*.cbx) (abx*.cbx +. aby*.cby))
    *. (180. /. pi) in
    ((*print_vert v1;
     print_vert ~before:" " v2;
     print_vert ~before:" " v3 ~after:"\n";*)
     if result < 0. then
       ((*Printf.printf "%f\n" (result +. 360.);*)
	result +. 360.)
     else
       ((*Printf.printf "%f\n" result;*)
	  result))

let intersection v1 v2 v3 v4 =
  (** Gets the intersection point of the lines formed by v1, v2 and
      v3, v4. *)
  let x1 = v1.x
  and y1 = v1.y
  and x2 = v2.x
  and y2 = v2.y
  and x3 = v3.x
  and y3 = v3.y
  and x4 = v4.x
  and y4 = v4.y in
  let xr = (((((x1*.y2)-.(y1*.x2)))*.(x3-.x4))-.
	      ((x1-.x2)*.((x3*.y4)-.(y3*.x4))))/.
    ((x1-.x2)*.(y3-.y4)-.(y1-.y2)*.(x3-.x4))
  and yr = (((((x1*.y2)-.(y1*.x2)))*.(y3-.y4))-.
	      ((y1-.y2)*.((x3*.y4)-.(y3*.x4))))/.
    ((x1-.x2)*.(y3-.y4)-.(y1-.y2)*.(x3-.x4)) in
    {x=xr; y=yr}

let intersects v1 v2 v3 v4 =
  (** Tests whether the line v1 v2 intersects with the line segment v3
      v4. *)
  let x1 = v1.x
  and y1 = v1.y
  and x2 = v2.x
  and y2 = v2.y
  and x3 = v3.x
  and y3 = v3.y
  and x4 = v4.x
  and y4 = v4.y in
  let xr = (((((x1*.y2)-.(y1*.x2)))*.(x3-.x4))-.
	      ((x1-.x2)*.((x3*.y4)-.(y3*.x4))))/.
    ((x1-.x2)*.(y3-.y4)-.(y1-.y2)*.(x3-.x4))
  and yr = (((((x1*.y2)-.(y1*.x2)))*.(y3-.y4))-.
	      ((y1-.y2)*.((x3*.y4)-.(y3*.x4))))/.
    ((x1-.x2)*.(y3-.y4)-.(y1-.y2)*.(x3-.x4)) in
  let vr = {x=xr; y=yr} in
    (((xr >= x1) && (xr <= x2)) || ((xr <= x1) && (xr >= x2))) &&
      (((yr >= y1) && (yr <= y2)) || ((yr <= y1) && (yr >= y2))) &&
      (((xr >= x3) && (xr <= x4)) || ((xr <= x3) && (xr >= x4))) &&
      (((yr >= y3) && (yr <= y4)) || ((yr <= y3) && (yr >= y4))) &&
      not ((vr = v1) || (vr = v2) || (vr = v3) || (vr = v4))

let intersects_line v1 v2 v3 v4 =
  (** Tests whether the line v1 v2 intersects with the line v3 v4. *)
  let x1 = v1.x
  and y1 = v1.y
  and x2 = v2.x
  and y2 = v2.y
  and x3 = v3.x
  and y3 = v3.y
  and x4 = v4.x
  and y4 = v4.y in
  let xr = (((((x1*.y2)-.(y1*.x2)))*.(x3-.x4))-.
	      ((x1-.x2)*.((x3*.y4)-.(y3*.x4))))/.
    ((x1-.x2)*.(y3-.y4)-.(y1-.y2)*.(x3-.x4))
  and yr = (((((x1*.y2)-.(y1*.x2)))*.(y3-.y4))-.
	      ((y1-.y2)*.((x3*.y4)-.(y3*.x4))))/.
    ((x1-.x2)*.(y3-.y4)-.(y1-.y2)*.(x3-.x4)) in
  let vr = {x=xr; y=yr} in
    (((xr >= x1) && (xr <= x2)) || ((xr <= x1) && (xr >= x2))) &&
      (((yr >= y1) && (yr <= y2)) || ((yr <= y1) && (yr >= y2))) &&
      not ((vr = v1) || (vr = v2) || (vr = v3) || (vr = v4))

let get_edges verts =
  (** Gets the set of edges belonging to an object. *)
  let rec edge_rest vert_rest acc =
    match vert_rest with
	[] -> acc
      | hd::tl -> match tl with
	    [] -> (hd, (List.hd verts))::acc
	  | tlh::tlt -> (hd, tlh)::(edge_rest tl acc) in
    edge_rest verts []

let get_centroid obj =
  let k = float (List.length obj) in
  let c = List.fold_left
    (fun acc v -> {x=acc.x+.v.x; y=acc.y+.v.y})
    {x=0.; y=0.} obj in
    {x=(c.x/.k); y=(c.y/.k)}

let is_ccw o v1 v2 =
  (** Determines whether v2 is counter-clockwise from v1, in relation
      to origin [o] (usually the center of the object to which v1 and v2
      belong.) *)
  let x1 = v1.x -. o.x
  and y1 = v1.y -. o.y
  and x2 = v2.x -. o.x
  and y2 = v2.y -. o.y in
  let areaTrapezoid = x1*.y2 -. y1*.x2 in
    if areaTrapezoid > 0. then
      1
    else if areaTrapezoid < 0. then
      -1
    else
      0

let in_object v verts =
  (** Detects whether a vertex falls within an existing object. *)
  let rec in_rest vert_rest acc =
    match vert_rest with
	[] -> acc
      | hd::tl -> match tl with
	    [] ->
	      if intersects v dummy_v hd (List.hd verts) then
		acc+1
	      else
		acc
	  | tlh::tlt ->
	      if intersects v dummy_v hd tlh then
		in_rest tl acc+1
	      else
		in_rest tl acc
  in
    if ((in_rest verts 0) mod 2) = 0 then
      false
    else
      true

let in_edge v e =
  ((fst e) = v) || ((snd e) = v)

let get_vis_verts_slow v objs incident =
  (** Generates the list of vertices in [others] which are visible
      from [v] using a naive O(n^2) algorithm. *)
  let others = List.flatten objs in
  let edges = List.flatten
    (List.map (fun o -> get_edges o) objs) in
  let vis_inc = (if (fst incident) = (snd incident) then
		   []
		 else
		   [(fst incident);(snd incident)]) in
    (List.filter
       (fun other ->
	  List.fold_left
	    (fun acc e -> acc &&
	       ((in_edge other e) ||
		  (not (intersects v other (fst e) (snd e)))))
	    (not (intersects v other (fst incident) (snd incident)))
	    edges) others)@vis_inc

let get_vis_verts v objs incident =
  (** Generates the list of vertices in [others] which are visible
      from [v] using the plane sweep algorithm. *)
  let others = List.flatten objs in
  let edges = List.flatten
    (List.map (fun o -> get_edges o) objs) in
  let horiz = {v with x = 999999.} in
  let closer c e1 e2 =
    compare
      (distance v
	 (intersection v c (fst e1) (snd e1)))
      (distance v
	 (intersection v c (fst e2) (snd e2))) in
  let e = List.sort (fun a b -> compare (fst a) (fst b))
    (List.map (fun other -> (get_angle_orig horiz v other, other)) others) in
  let s = List.sort (closer (snd (List.hd e)))
    (List.filter (fun (v1, v2) -> intersects_line v1 v2 v horiz) edges) in
    ((*print_vert ~before:"getting visibility for " v ~after:"\n";*)
     let rec get_vis curr_a acc curr_e curr_s =
       ((*print_vert ~before:"testing " curr_a ~after:"\n";
	Printf.printf "current s:\n";
	List.iter (fun e ->
		     print_vert (fst e) ~after:" ";
		     print_vert (snd e) ~after:"\n";) curr_s;
	Printf.printf "\n";*)
	let new_acc = (if (((List.length curr_s) > 0) &&
			     (intersects v curr_a
				(fst (List.hd curr_s)) (snd (List.hd curr_s)))) ||
			 (intersects v curr_a (fst incident) (snd incident)) then
			   acc
		       else
			 (curr_a)::acc) in
	  match curr_e with
	      [] -> new_acc
	    | hd::tl -> get_vis (snd hd)
		new_acc
		  tl
		  (List.sort (closer (snd hd))
		     ((List.filter (fun edge -> (in_edge curr_a edge) &&
				      not (List.mem edge curr_s)) edges)@
			(List.filter (fun edge -> not (in_edge curr_a edge)) curr_s))))
     in get_vis (snd (List.hd e))
	  (if (fst incident) = v then
	     []
	   else
	     [(fst incident);(snd incident)])
	  (List.tl e) s)

let get_incident v edges =
  (fst (List.hd
	  (List.filter
	     (fun e -> (snd e) = v) edges)),
   snd (List.hd
	  (List.filter
	     (fun e -> (fst e) = v) edges)))

let get_vis_graph start goal objs =
  (** Generates the visibility graph for all vertices using the
      plane sweep algorithm. *)
  let verts = start::(goal::(List.flatten objs)) in
  let connections = Hashtbl.create (List.length verts) in
  let add_connections v others obj =
    Hashtbl.add connections v (get_vis_verts_slow v others obj) in
    (add_connections start objs (start,start);
     add_connections goal objs (goal,goal);
     (List.iter (fun o -> let o_edges = get_edges o in
		   List.iter
		     (fun v ->
			let incident = get_incident v o_edges in
			  add_connections v
			    ([start]::([goal]::(Wrlist.remove o objs)))
			    incident) o)
	objs);
     connections)

let angle_sum o =
  let rec get_sum acc vert_rest =
    match vert_rest with
	[] -> acc
      | hd::tl -> match tl with
	    [] -> get_sum
	      (acc +. (get_angle hd (List.hd o) (List.nth o 1)))
	      tl
	  | tlh::tlt -> match tlt with
		[] -> get_sum
		  (acc +. (get_angle hd tlh (List.hd o)))
		  tl
	      | tltlh::tltlt -> get_sum
		  (acc +. (get_angle hd tlh tltlh))
		    tl
  in
    get_sum 0. o

let check_angles o =
  let rec check_rest acc vert_rest =
    match vert_rest with
	[] -> acc
      | hd::tl -> match tl with
	    [] -> check_rest
	      (acc && ((get_angle hd (List.hd o) (List.nth o 1)) <= 180.))
	      tl
	  | tlh::tlt -> match tlt with
		[] -> check_rest
		  (acc && ((get_angle hd tlh (List.hd o)) <= 180.))
		  tl
	      | tltlh::tltlt -> check_rest
		  (acc && ((get_angle hd tlh tltlh) <= 180.))
		    tl
  in
    check_rest true o

let is_convex o =
  (((angle_sum o) -. ((float (List.length o))*.180.)) <= 0.) &&
    (check_angles o)

let gen_object width height other_verts objs n_verts v1 bw bh =
  let edges = List.flatten (List.map get_edges objs) in
  let obj = ref [v1] in
    (for i = 2 to n_verts do
       let new_vert = ref {x = (Random.float bw)+.v1.x;
			   y = (Random.float bh)+.v1.y} in
       let new_obj = ref (List.sort
			    (fun v1 v2 ->
			       (-1*(is_ccw (get_centroid
					      (!new_vert::!obj))
				      v1 v2)))
			    (!new_vert::!obj)) in
	 while (((List.length !obj) >= 2) &&
		  not (is_convex !new_obj)) ||
	   (!new_vert.x > width || !new_vert.y > height) ||
		  (List.fold_left
		     (fun acc v -> acc ||
			(in_object v !new_obj)) false other_verts) ||
		  (List.fold_left
		     (fun acc o -> acc ||
			(in_object !new_vert o) ||
			(in_object (List.hd o) !new_obj)) false objs) ||
		  (List.fold_left
		     (fun acc e ->
			let new_edges = get_edges
			  (List.sort
			     (fun v1 v2 ->
				(-1*(is_ccw (get_centroid
					       (!new_vert::!obj))
				       v1 v2)))
			     (!new_vert::!obj)) in
			let incident =
			  get_incident !new_vert new_edges in
			  acc ||
			    (intersects !new_vert (fst incident)
			       (fst e) (snd e)) ||
			    (intersects !new_vert (snd incident)
			       (fst e) (snd e)))
		     false edges)
	 do
	   new_vert := {x = (Random.float bw)+.v1.x;
			y = (Random.float bh)+.v1.y};
	   new_obj := (List.sort
			 (fun v1 v2 ->
			    (-1*(is_ccw (get_centroid
					   (!new_vert::!obj))
				   v1 v2)))
			 (!new_vert::!obj))(*;
	   List.iter (fun v -> print_vert v) !new_obj;
	   Printf.printf "\n"*)
	 done;
	 obj:=List.sort (fun v1 v2 ->
			   (-1*(is_ccw (get_centroid
					  (!new_obj))
				  v1 v2)))
	   (!new_obj)
     done);
    !obj

let gen_instance width height start goal n_objs =
  (** Creates an instance of size [width] by [height] with [n_objs]
      objects. The size of an object is determined by deciding on a
      random number of vertices between 3 and 7 and choosing a
      bounding box for the object of random size at most 0.5*[width]
      and 0.25*[height]. Vertices are stored in clockwise
      order. [width] and [height] are not hard limits on the
      locations of objects.*)
  let objs = ref [] in
    (Random.self_init ();
     for i = 1 to n_objs do
       let v1 = ref {x = Random.float width;
		     y = Random.float height} in
	 (while List.fold_left
	    (fun acc o -> acc || (in_object !v1 o)) false !objs do
	      v1 := {x = Random.float width;
		     y = Random.float height}
	  done;
	  objs := (gen_object width height [start; goal]
		     !objs ((Random.int 4)+3) !v1
		     (Random.float (width*.0.75))
		     (Random.float (height*.0.75)))::!objs)
     done;
     {width = width; height = height; objects = !objs;
      start = start; goal = goal;
      connections = (get_vis_graph start goal !objs)})



(********************************* I/O ************************************)

let write prob ch =
  (** Outputs the problem as a file listing the width, height, start
      location, goal location and the visibility graph connecting all
      vertices of objects in the world, including start and goal. *)
  let start = prob.start
  and goal = prob.goal
  and vis_graph = prob.connections
  and objects = prob.objects in
  let vert_list = (start::(goal::(List.flatten objects))) in
  let vert_tbl = Hashtbl.create (List.length objects) in
    Wrutils.pf ch "width: %f\n" prob.width;
    Wrutils.pf ch "height: %f\n" prob.height;
    Wrlist.iteri (fun i v -> Hashtbl.add vert_tbl v i)
      vert_list;
    Wrutils.pf ch "n_verts: %i\n" (Hashtbl.length vert_tbl);
    Wrlist.iteri (fun v k -> Wrutils.pf ch "%i %f %f\n" v k.x k.y) vert_list;
    Wrutils.pf ch "start: %i\n" (Hashtbl.find vert_tbl start);
    Wrutils.pf ch "goal: %i\n" (Hashtbl.find vert_tbl goal);
    Wrutils.pf ch "n_objs: %i\n" (List.length objects);
    List.iter (fun o ->
		 Wrutils.pf ch "object:";
		 List.iter
		   (fun v -> Wrutils.pf ch " %i"
		      (Hashtbl.find vert_tbl v)) o;
		 Wrutils.pf ch "\n";) objects;
    Hashtbl.iter (fun k v ->
		    Wrutils.pf ch "vertex: %i\n" (Hashtbl.find vert_tbl k);
		    Wrutils.pf ch "visible:";
		    List.iter (fun p ->
				 Wrutils.pf ch " %i"
				   (Hashtbl.find vert_tbl p)) v;
		    Wrutils.pf ch "\n")
      vis_graph

let write_filename str prob =
  (** write a problem to a file with path [str], same as above *)
  Wrio.with_outfile str (write prob)

let read ch =
  (** Reads a problem from [ch], returining a start location, goal
      location, and hash table of connections *)
  let connections = Hashtbl.create 0 in
  let width = Wrio.skip_through_str ch "width: "; Wrio.input_float ch
  and height = Wrio.skip_through_str ch "height: "; Wrio.input_float ch in
  let as_lines = Wrio.input_lines ch in
  let replace query replacement s =
    Str.global_replace (Str.regexp query) replacement s in
  let rec get_board lines vert_array prob =
    match lines with
	[] -> prob
      | hd::tl ->
	  if Wrstr.starts_as "n_verts: " hd then
	    let n_verts = (int_of_string
			     (replace "n_verts: " "" hd)) in
	    let vert_lines, rest_lines = (Wrlist.prefix n_verts tl) in
	    let va = Array.of_list
	      (List.map
		 (fun l -> ({x=(float_of_string (Wrlist.second l));
			     y=(float_of_string (Wrlist.last l))}))
		 (List.map (fun l -> Wrstr.split_white l)
		    vert_lines)) in
	      get_board rest_lines va prob
	  else if Wrstr.starts_as "start: " hd then
	    let sg_lines = List.map (fun l ->
				       replace "start: \\|goal: " "" l)
	      [hd;(List.hd tl)] in
	      get_board (List.tl tl) vert_array
		{prob with
		   start=vert_array.(int_of_string
				       (List.hd sg_lines));
		   goal=vert_array.(int_of_string
				      (Wrlist.second sg_lines))}
	  else if Wrstr.starts_as "n_objs: " hd then
	    let n_objs = (int_of_string
			    (replace "n_objs: " "" hd)) in
	    let obj_lines, rest_lines = Wrlist.prefix n_objs tl in
	    let objs = List.map
	      (fun l -> List.map
		 (fun v ->
		    vert_array.(int_of_string v)) l)
	      (List.map (fun l -> Wrstr.split_white
			   (replace "object: " "" l))
		 obj_lines) in
	      get_board rest_lines vert_array
		{prob with
		   objects=objs}
	  else if Wrstr.starts_as "vertex: " hd then
	    let v = vert_array.(int_of_string (replace "vertex: " "" hd))
	    and vis_list = List.map (fun v -> vert_array.(int_of_string v))
	      (Wrstr.split_white
		 (replace "visible: " "" (List.hd tl))) in
	      (Hashtbl.add connections v vis_list;
	       get_board (List.tl tl) vert_array prob)
	  else
	    failwith
	      (Wrutils.str "line didn't begin with what we expected:\n%s\n" hd)
  in
  let prob = get_board as_lines
    (Array.make 0 dummy_v)
    {width = width; height = height; objects = [];
     start = dummy_v; goal = dummy_v;
     connections = connections} in
    (make_root prob.start), prob

let read_filename str =
  (** read a problem from a file with path [str], same as above *)
  Wrio.with_infile str read

(* EOF *)
