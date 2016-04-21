(* $Id: non_dynamic.ml,v 1.1 2005/01/18 17:45:41 ruml Exp ruml $

   simple x-y planner as a heuristic

   find distance from goal to current position assuming can move to all
   adjacent x,y (eg, slowest speed), then divide by max speed to get lower
   bound on time.  actions are reversible, so backwards search is OK.

   since we move one position_delta at a time, this value CAN overestimate
   compared to the large-step moves that the dynamic planner makes
   (triangle inequality for long diagonal moves).  But dividing by
   max_speed is so convervative that this shouldn't be a problem.
*)


open Drn_instance


(********** use as a distance heuristic ************)

(*** node format is (x, y) *)

type nd_node =
    { pos : int * int;
      parent : nd_node }


let make_root x y =
  let rec n = { pos = x, y;
		parent = n} in n

let corner n =
  (* returns 2 for a sharp corner, 1 for a gentle corner from 8-way movement *)
  let x1,y1 = n.pos
  and x2,y2 = n.parent.pos
  and x3,y3 = n.parent.parent.pos in
    abs (((x1 - x2) - (x2 - x3)) + ((y1 - y2) - (y2 - y3)))


let rec get_corners ?(thresh = 0) n =
    if n == n.parent
    then []
    else (if (corner n) > thresh
	  then (n::(get_corners ~thresh n.parent))
	  else (get_corners ~thresh n.parent))

let rec count_corners n =
  if n == n.parent
  then 0
  else ((corner n) + (count_corners n.parent))

let moves =
  let d = Math.round position_delta in
  let nd = - d
  and root2 = position_delta *. sqrt 2. in
    [| d, 0, position_delta;
       d, d, root2;
       0, d, position_delta;
       nd, d, root2;
       nd, 0, position_delta;
       nd, nd, root2;
       0, nd, position_delta;
       d, nd, root2; |]


let make_expand obstacles =
  (fun n d ->
     let x,y = n.pos
     and children = ref [] in
       Array.iter (fun (dx, dy, d2) ->
		     let x2 = x + dx
		     and y2 = y + dy in
		       if legal_pos obstacles x2 y2 then
			 Wrutils.push ({ pos = x2,y2; parent = n},
				       (d +. d2))
			   children)
	 moves;
       !children)


let make_goal_p gx gy =
  (fun n ->
     let x,y = n.pos in
       (near x gx) && (near y gy))


(*
  let make_h_straight gx gy =
  (** straight-line distance *)
  (fun (x,y) ->
     distance x y gx gy)
*)


let cell_distance =
  let r2 = sqrt 2. in
    (fun max_d min_d ->
       ((float min_d) *. r2) +.
       (float (max_d - min_d)))


let make_h_cells gx gy =
  (** distance when only hor/ver and diag moves are allowed *)
  (fun (x,y) ->
     let dx = abs (gx - x)
     and dy = abs (gy -y) in
       if dx >= dy then
	 cell_distance dx dy
       else
	 cell_distance dy dx)

(******** precompute ALL distances ********)


let pcell =
  (** assumes x is an exact multiple of position_delta, which is assumed to
    be integral *)
  let p = int_of_float position_delta in
    (fun x ->
       x / p)


let find_all_distances prob =
  (* breadth-first search from goal outward until all cells visited.  node
     format is (x,y), with current-best distances held in matrix. *)
  let rx,ry,_ = prob.goal in
  let root = make_root rx ry in
  let obs = prob.obstacles in
  let dist = Array.make_matrix (matx obs) (maty obs) (infinity, root)
  and expand = make_expand obs
  and q = Queue.create () in
    (dist.(pcell rx).(pcell ry) <- (0., root);
     Queue.add root q);
    while not (Queue.is_empty q) do
      let n = Queue.take q in
      let x,y = n.pos in
	List.iter (fun (m,d) ->
		     let x,y = m.pos in
		       if d < fst (dist.(pcell x).(pcell y)) then
			 (dist.(pcell x).(pcell y) <- (d,m);
			  Queue.add m q))
	  (expand n (fst dist.(pcell x).(pcell y)))
    done;
    dist


let make_get_distance problem =
  Verb.pe 4 "Computing heuristic values...%!";
  let dist = Array.map (Array.map fst) (find_all_distances problem) in
    Verb.pe 4 "done.\n%!";
    (fun x y ->
       dist.(pcell x).(pcell y))


let make_get_distance_corners problem =
  Verb.pe 4 "Computing heuristic values...%!";
  let dist = Array.map
    (Array.map (fun (distance,node) -> distance, (count_corners node)))
       (find_all_distances problem) in
    Verb.pe 4 "done.\n%!";
    (fun x y ->
       dist.(pcell x).(pcell y))


(******** now in terms of path lengths ********)
let make_d_expand obstacles =
  (fun n d ->
     let (x,y) = n.pos in
     let children = ref [] in
       Array.iter (fun (dx, dy, _) ->
		     let x2 = x + dx
		     and y2 = y + dy in
		       if legal_pos obstacles x2 y2 then
			 Wrutils.push ({pos = (x2,y2);
					parent = n}, (d + 1))
			   children)
	 moves;
       !children)


let find_all_ds prob =
  (* breadth-first search from goal outward until all cells visited.  node
     format is (x,y), with current-best distances held in matrix. *)
  let rx, ry,_ = prob.goal in
  let root = make_root rx ry in
  let obs = prob.obstacles in
  let dist = Array.make_matrix (matx obs) (maty obs) (max_int,root)
  and expand = make_d_expand obs
  and q = Queue.create () in
    (dist.(pcell rx).(pcell ry) <- (0,root);
     Queue.add root q);
    while not (Queue.is_empty q) do
      let n = Queue.take q in
      let x,y = n.pos in
	List.iter (fun (m,d) ->
		     let x,y = m.pos in
		       if d < fst (dist.(pcell x).(pcell y))
		       then (dist.(pcell x).(pcell y) <- (d,m);
			     Queue.add m q))
	  (expand n (fst dist.(pcell x).(pcell y)))
    done;
    dist


let make_get_d problem =
  Verb.pe 4 "Computing d values...%!";
  let dist = Array.map (Array.map fst) (find_all_ds problem) in
    Verb.pe 4 "done.\n%!";
    (fun x y ->
       dist.(pcell x).(pcell y))


let make_get_d_corners problem =
  Verb.pe 4 "Computing d values...%!";
  let dist = Array.map
    (Array.map (fun (d,n) -> d, count_corners n))
    (find_all_ds problem) in
    Verb.pe 4 "done.\n%!";
    (fun x y ->
       dist.(pcell x).(pcell y))



let feasible_p problem =
  let problem = expand_obstacles problem in
  let get = make_get_d problem
  and sx,sy,_ = problem.start in
    (get sx sy) < max_int


(********** as a shortest-path planner ************)


(* recursive tuple types are not supported without the -rectypes flag, so I
   use a record instead. *)
type node = {
  pos : int * int;
  p : node;
}


let make_proot x y =
  let rec r = { pos = x,y;
		p = r; } in
    r


let make_pexpand obstacles =
  (fun n d ->
     let x,y = n.pos
     and children = ref [] in
       Array.iter (fun (dx, dy, d2) ->
		     let x2 = x + dx
		     and y2 = y + dy in
		       if legal_pos obstacles x2 y2 then
			 let s = { pos = x2,y2;
				   p = n; } in
			   Wrutils.push (s, (d +. d2))
			     children)
	 moves;
       !children)


let make_pgoal_p gx gy =
  (fun n ->
     let x,y = n.pos in
       (near x gx) &&
       (near y gy))


let key n =
  n.pos

let key_to_string (x,y) =
  Wrutils.str "(%i,%i)" x y


let make_ph_cells gx gy =
  (** straight-line distance *)
  let h = make_h_cells gx gy in
    (fun n -> h n.pos)


let extract_path node =
  let rec next tail n =
    let x,y = n.pos in
    let s = { x = x;
	      y = y;
	      heading = 0;
	      speed = 1; } in
      if n.p == n then
	s::tail
      else
	next (s::tail) n.p
  in
    next [] node


let shortest_path problem =
  let problem = expand_obstacles problem in
  let sx, sy, _ = problem.start
  and gx, gy, _ = problem.goal in
  let nd_interface = Search_interface.make
    ~h:(make_ph_cells gx gy)
    ~domain_expand:(make_pexpand problem.obstacles)
    ~key:key
    ~key_print:key_to_string
    ~goal_p:(make_pgoal_p gx gy)
    Search_interface.Robot
    (make_proot sx sy)
    (fun _ _ -> false)
    (fun _ -> ()) in
    Astar.dups nd_interface


(* EOF *)
