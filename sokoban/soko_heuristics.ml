(** Heiristics for Sokoban *)

open Sokoban_instance


let h0 _ =
  (** standard h0 function *)
  0.


let make_unsolved_boxes world push_cost =
  (** returns a heuristic which counts the number of boxes not on targets*)
  (fun state ->
     Array.fold_left (fun accum (x,y) ->
			if world.(x).(y) = Target
			then accum
			else accum +. push_cost) 0. state.boxes)


let find_targets world =
  (** utility function for constructing a sorted list of target locations*)
  let targets = ref [] in
    for x = 0 to ((width world) - 1)
    do
      for y = 0 to ((height world) - 1)
      do
	if world.(x).(y) = Target then targets := (x,y)::!targets
      done
    done;
    List.sort compare !targets


let make_manhattan_nearest_target_overlap world push_cost =
  (** calculates the cost of moving each crate to its nearest target location
      using manhattan distance and ignoring obstacles.  Also ignores overlap *)
  let targets = find_targets world in
    (fun state ->
       Array.fold_left
	 (fun accum (bx,by) ->
	    (List.fold_left
	       (fun min_cost (tx,ty) ->
		  let m_dist = (abs (bx - tx)) + (abs (by - ty)) in
		    Math.fmin ((float_of_int m_dist) *. push_cost) min_cost)
	       infinity targets) +. accum)
	 0. state.boxes)



let true_target_distance_function world =
  (** Generates the distance from each target to each other square on the
      board *)
  let targets = Array.of_list (find_targets world) in
  let values = (Wrarray.init_matrix (width world) (height world)
		  (fun _ _ ->
		     (Array.create (Array.length targets) infinity))) in
  let goal_p = (fun _ -> false)
  and make_expand i = (fun (x,y) g ->
			 values.(x).(y).(i) <- g;
			 List.filter (fun ((x,y),_) ->
					world.(x).(y) <> Wall)
			   [(x,y+1), (g +. 1.);
			    (x,y-1), (g +. 1.);
			    (x+1,y), (g +. 1.);
			    (x-1,y), (g +. 1.);])
  and key n = n in
    Verb.pe Verb.debug "Generating Heuristics...\n";
    (for i = 0 to ((Array.length targets) - 1)
     do
       ignore(Uniform_cost_search.dups_silent
		(Search_interface.make
		   ~goal_p:goal_p
		   ~key:key
		   ~domain_expand:(make_expand i)
		   ~equals:(=)
		   Search_interface.Sokoban
		   targets.(i)
		   (fun _ _ -> false)
		   (fun _ -> ())) [||])
     done);
    Verb.pe Verb.debug "Done!\n";
    (fun (x,y) ->
       values.(x).(y))


let min_true_target_distance_overlap world push_cost =
  (** Using true distance values, calculates the minimum distance to get
      nodes to the nearest target *)
  let get_distances = true_target_distance_function world in
    (fun state ->
       (Array.fold_left
	 (fun accum box ->
	    (Array.fold_left
	       (fun min_dist t_distance ->
		  Math.fmin min_dist t_distance)
	       infinity (get_distances box)) +. accum)
	 0. state.boxes) *. push_cost)



let make_simplified_starts world =
  (fun x y ->
     match world.(x).(y) with
       | Wall -> []
       | Target -> [make_root [x,y] (x,y)]
       | Floor -> (List.filter
		     (fun state ->
			let lx,ly = state.location in
			  try
			    world.(lx).(ly) <> Wall
			  with _ -> false)
		     [make_root [x,y] (x-1,y);
		      make_root [x,y] (x+1,y);
		      make_root [x,y] (x,y+1);
		      make_root [x,y] (x,y-1);])
       | _ -> failwith "Sholudn't ever happen")


let heuristic_solve_simplified world move push =
  (** Places a box in each cell of the board and solves the problem optimally,
      Sums these values over all boxes *)
  let values = Array.create_matrix (width world) (height world) 0.
  and get_roots = make_simplified_starts world
  and tdistance = min_true_target_distance_overlap world push in
    Verb.pe Verb.debug "Constructing heuristic...\n";
    for x = 0 to ((width world) - 1)
    do
      for y = 0 to ((height world) -1)
      do
	(let roots = (get_roots x y) in
	   if roots = [] then values.(x).(y) <- infinity
	   else (match (Astar.seeded_dups_silent roots
			  (Search_interface.make
			     ~goal_p:(make_goal world)
			     ~key:key
			     ~h:tdistance
			     ~domain_expand:(make_expand world move push)
			     ~equals:(=)
			     Search_interface.Sokoban
			     (List.hd roots)
			     (fun _ _ -> false)
			     (fun _ -> ())) [||]) with
		   | None,_,_,_,_,_ -> values.(x).(y) <- infinity
		   | Some (_,f),_,_,_,_,_ -> values.(x).(y) <- f);
	   Verb.pe Verb.debug "(%i,%i) = %f\n" x y values.(x).(y))
      done
    done;
    Verb.pe Verb.debug "Done!\n";
    (fun state ->
       Array.fold_left (fun accum (x,y) ->
			  accum +. values.(x).(y)) 0. state.boxes), tdistance



let heuristic_solve_per_target world move push =
  (** Places a box in each cell of the board and solves the problem optimally
      for each possible target location. This must be passed into some other
      function*)
  let targets = Array.of_list (find_targets world) in
  let values = (Wrarray.init_matrix (width world) (height world)
		  (fun _ _ ->
		     (Array.create (Array.length targets) infinity)))
  and get_roots = make_simplified_starts world
  and tdistance = min_true_target_distance_overlap world push in
    Verb.pe Verb.debug "Constructing heuristic...\n";
    for x = 0 to ((width world) - 1)
    do
      for y = 0 to ((height world) -1)
      do
	(let roots = (get_roots x y) in
	   for t = 0 to ((Array.length targets) - 1)
	   do
	     (if roots = [] then values.(x).(y).(t) <- infinity
	      else (match (Astar.seeded_dups_silent roots
			     (Search_interface.make
			     ~goal_p:(make_explicit_target_goal targets.(t))
			     ~key:key
			     ~h:tdistance
			     ~domain_expand:(make_expand world move push)
			     ~equals:(=)
			     Search_interface.Sokoban
			     (List.hd roots)
			     (fun _ _ -> false)
			     (fun _ -> ())) [||]) with
		   | None,_,_,_,_,_ -> values.(x).(y).(t) <- infinity
		   | Some (_,f),_,_,_,_,_ -> values.(x).(y).(t) <- f))
	   done)
      done
    done;
    Verb.pe Verb.debug "Done!\n";
    (fun (x,y) -> values.(x).(y)), tdistance


let optimal_mincost cost_by_target =
  let make_root boxes = (Array.to_list boxes,
			 Wrlist.range ~min:0 ((Array.length boxes) - 1))
  and goal_p (a,b) = a = [] && b = []
  and expand (n_a,n_b) g =
    match n_a with
      | hd::tl ->
	  List.map (fun t -> (tl,(List.filter (fun e -> e <> t) n_b)),
		      ((cost_by_target hd).(t)) +. g) n_b
      | _ -> []
  and cached_values = Hashtbl.create 250 in
  let h (a,b) =
    let min_targets = List.map (fun box -> Array.fold_left Math.fmin infinity
				  (cost_by_target box)) a in
      List.fold_left (+.) 0. min_targets in
    (fun state ->
       try Hashtbl.find cached_values state
       with Not_found ->
	 (match (Astar.no_dups_silent
		   (Search_interface.make
		      ~goal_p:goal_p
		      ~domain_expand:expand
		      ~equals:(=)
		      ~h:h
		      Search_interface.Sokoban
		      (make_root state.boxes)
		      (fun _ _ -> false)
		      (fun _ -> ())) [||]) with
	    | None,_,_,_,_ -> (Hashtbl.add cached_values state infinity;
			       infinity)
	    | Some (_,f),_,_,_,_ -> (Hashtbl.add cached_values state f;
				     f)))


let bipart_heuristic_solve_per_target world move push =
  let cost_by_target,_ = heuristic_solve_per_target world move push in
    optimal_mincost cost_by_target

(* EOF *)
