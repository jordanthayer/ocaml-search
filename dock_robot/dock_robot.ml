(** A domain dependent formulation of the dock worker robot domain
    from the text book "Automated Planning: theory and practice" by
    Ghallab, Nau and Traverso.

    All robots start at location zero.

    TODO:

    * Multiple robots (parallel actions)

    @author eaburns
    @since 2010-03-10
*)

open Dock_robot_instance


type state = {
  piles : int Stack.t array;
  (* The cargo on each pile. *)

  container_locations : int array;
  (* The location of each container. *)

  crane_contents : int array;
  (* The cargo held by each crane. *)

  robot_contents : int array;
  (* The cargo held by each robot. *)

  robot_locations : int array;
  (* The current location of each the robot. *)

  mutable parent : state;
  (* The parent state. *)
}

let stack_mem s e =
  let ret = ref false in
    Stack.iter (fun v -> if v = e then ret := true) s;
    !ret

let validate state =
  (** doesn't work with multiple piles per location or many robots*)
  Wrarray.fold_lefti
    (fun accum container location ->
       let cont_in_crane = state.crane_contents.(location) = container
       and cont_in_stack = stack_mem state.piles.(location) container
       and cont_in_robot = state.robot_contents.(0) = container
       and robot_at_loc = state.robot_locations.(0) = location in
       let this_iteration = (cont_in_crane || cont_in_stack ||
			       (cont_in_robot && robot_at_loc)) in
	 if not this_iteration
	 then (Verb.pe Verb.debug "Failed on container %i location %i\n%!"
		 container location);
	 accum && this_iteration)
    true state.container_locations



let rec sol_length n =
  if n.parent == n then 0
  else 1 + (sol_length n.parent)

let no_cargo = ~-1
  (** [no_cargo] a token that denotes that there is no cargo on a
      crane or in a robot. *)

let crane_base = 0.05

let crane_cost piles pile =
  crane_base *. (float ((Stack.length piles.(pile)) + 1))


let load_cost = 0.01
and unload_cost = 0.01


let key state =
  (** [key state] gets a unique key value for a state. *)
  (state.piles,
   state.container_locations,
   state.crane_contents,
   state.robot_contents,
   state.robot_locations)


let string_of_key (piles, locs, crane_contents, robot_contents, robot_locs) =
  (** [string_of_key k] gets the string representation of the given
      key. *)
  let b = Buffer.create 100 in
    Buffer.add_string b "Locations:\n";
    Array.iteri (fun p location ->
		   Buffer.add_string b
		     (Printf.sprintf "\t%d,%d" p location)) locs;

    Buffer.add_string b "\nPiles:\n";
    Array.iteri (fun p stack ->
		   Buffer.add_string b (Printf.sprintf "\t%d: " p);
		   Stack.iter (fun c ->
				 Buffer.add_string b (string_of_int c))
		     stack;)
      piles;
    Buffer.add_string b "\nRobots:\n";
    for i = 0 to (Array.length robot_locs) - 1 do
      let contents = robot_contents.(i) in
	Buffer.add_string b
	  (Printf.sprintf "\t%d location=%d, cargo=%s\n"
	     i robot_locs.(i) (if contents = no_cargo
			       then "no cargo"
			       else string_of_int contents))
    done;
    Buffer.add_string b "Cranes:\n";
    for i = 0 to (Array.length crane_contents) - 1 do
      let contents = crane_contents.(i) in
	Buffer.add_string b
	  (Printf.sprintf "\t%d cargo=%s\n" i (if contents = no_cargo
					       then "no cargo"
					       else string_of_int contents))
    done;
    Buffer.contents b


let display node = Printf.eprintf "%s\n%!" (string_of_key (key node))

(** {6 Actions} ****************************************)

let do_move inst state robot =
  (** [do_move inst state robot] gets the set of states where [robot]
      has moved to an adjacent location. *)
  let loc = state.robot_locations.(robot) in
  let adj = inst.adjacent.(loc) in
  let gp = state.parent in
  let kids = ref [] in
    for n = 0 to inst.nlocations - 1 do
      if gp.robot_locations.(robot) <> n && Math.finite_p adj.(n) then begin
	let contents = state.robot_contents.(robot) in
	let new_locs = Array.copy state.robot_locations in
	let new_containers =
	  if contents <> no_cargo then begin
	    let ls = Array.copy state.container_locations in
	      ls.(contents) <- n;
	      ls
	  end else
	    state.container_locations
	in
	  new_locs.(robot) <- n;
	  let state = { state with
			  robot_locations = new_locs;
			  container_locations = new_containers;
			  parent = state; }
	  in
	    kids := (state, adj.(n)) :: !kids
      end
    done;
    !kids


let do_take inst state robot =
  (** [do_take inst state robot] gets the states where [robot] uses a
      crane at its current location to take a container from a
      stack. *)
  let loc = state.robot_locations.(robot) in
  let cranes =
    List.filter (fun c -> state.crane_contents.(c) = no_cargo)
      inst.cranes_at_location.(loc)
  and piles =
    List.filter (fun p -> not (Stack.is_empty state.piles.(p)))
      inst.piles_at_location.(loc)
  in
  let gp = state.parent in
  let do_pile crane pile =
    if gp.crane_contents.(crane) = (Stack.top state.piles.(pile)) then
      None
    else begin
      let new_piles = Array.map (fun s -> Stack.copy s) state.piles
      and new_contents = Array.copy state.crane_contents in
      let c = Stack.pop new_piles.(pile) in
	new_contents.(crane) <- c;
	Some ({ state with
		  piles = new_piles;
		  crane_contents = new_contents;
		  parent = state;
	      }, crane_cost state.piles pile)
    end
  in
  let do_crane crane = Wrlist.map_opt (do_pile crane) piles in
    Wrlist.mapcan do_crane cranes


let do_put inst state robot =
  (** [do_put inst state robot] gets the states where [robot] uses a
      crane at its current location to put a container onto a
      stack. *)
  let loc = state.robot_locations.(robot) in
  let cranes =
    List.filter (fun c -> state.crane_contents.(c) <> no_cargo)
      inst.cranes_at_location.(loc)
  and piles = inst.piles_at_location.(loc) in
  let gp = state.parent in
  let do_pile crane pile =
    if not (Stack.is_empty gp.piles.(pile))
      && ((Stack.top gp.piles.(pile)) = state.crane_contents.(crane)) then
	None
    else begin
      let new_piles = Array.map (fun s -> Stack.copy s) state.piles
      and new_contents = Array.copy state.crane_contents in
	Stack.push state.crane_contents.(crane) new_piles.(pile);
	new_contents.(crane) <- no_cargo;
	Some ({ state with
		  piles = new_piles;
		  crane_contents = new_contents;
		  parent = state;
	      }, crane_cost state.piles pile)
    end
  in
  let do_crane crane = Wrlist.map_opt (do_pile crane) piles in
    Wrlist.mapcan do_crane cranes


let do_load inst state robot =
  (** [do_load inst state robot] gets the states where [robot] is
      loaded from a crane that currently has cargo in its location. *)
  if state.robot_contents.(robot) <> no_cargo then []
  else
    let loc = state.robot_locations.(robot) in
    let cranes = (List.filter (fun c -> state.crane_contents.(c) <> no_cargo)
		    inst.cranes_at_location.(loc)) in
    let gp = state.parent in
      Wrlist.map_opt
	(fun crane ->
	   if (gp.robot_contents.(robot) = state.crane_contents.(crane))
	   then None (* ignore the command where we just put the object back *)
	   else begin
	     let new_cranes = Array.copy state.crane_contents in
	     let new_robots = Array.copy state.robot_contents in
	       new_robots.(robot) <- state.crane_contents.(crane);
	       new_cranes.(crane) <- no_cargo;
	       Some ({ state with
			 crane_contents = new_cranes;
			 robot_contents = new_robots;
			 parent = state; }, load_cost)
	   end)
	cranes


let do_unload inst state robot =
  (** [do_unload inst state robot] gets the states where [robot] is
      unloaded by a crane that currently has cargo in its location. *)
  let loc = state.robot_locations.(robot) in
  let cranes =
    List.filter (fun c -> state.crane_contents.(c) = no_cargo)
      inst.cranes_at_location.(loc)
  in
  let gp = state.parent in
    Wrlist.map_opt
      (fun crane ->
	 if gp.crane_contents.(crane) = state.robot_contents.(robot)
	 then None
	 else begin
	   let new_cranes = Array.copy state.crane_contents in
	   let new_robots = Array.copy state.robot_contents in
	     new_cranes.(crane) <- state.robot_contents.(robot);
	     new_robots.(robot) <- no_cargo;
	     Some ({ state with
		       crane_contents = new_cranes;
		       robot_contents = new_robots;
		       parent = state;
		   }, unload_cost)
	 end)
      cranes


let do_expand_robot inst state robot =
  (** [do_expand_robot inst state robot] get all of the possible
      actions for [robot]. *)
  let moves = do_move inst state robot
  and takes = do_take inst state robot
  and puts = do_put inst state robot
  and loads = do_load inst state robot
  and unloads = do_unload inst state robot in
  let check nm lst =
    List.iter (fun (next,_) ->
		 if not (validate next)
		 then (Verb.pe Verb.always "Illegal Child:\n%!";
		       display next;
		       Verb.pe Verb.always "Legal Parent\n%!";
		       display next.parent;
		       failwith nm)) lst in
    check "moves" moves;
    check "takes" takes;
    check "puts" puts;
    check "loads" loads;
    check "unlados" unloads;
    moves @ takes @ puts @ loads @ unloads


let make_hash inst =
  (** [make_hash inst] makes a function that hashes a state to get a
      (reasonably unique) key. *)
(*
  let hash =
    Array_hasher.hash_array_function inst.ncontainers inst.nlocations
  in (fun (_, container_locs, _, _, _) -> hash container_locs)
*)
  Hashtbl.hash_param 20 20


let make_init inst =
  (** [make_init inst] makes the initial state given the problem. *)
  let container_locations = Array.create inst.ncontainers 0 in
  let piles =
    Array.mapi (fun p lst ->
		  let loc = pile_location inst p in
		  let s = Stack.create () in
		    List.iter (fun c ->
				 container_locations.(c) <- loc;
				 Stack.push c s) lst;
		    s)
      inst.pile_contents
  and crane_contents = Array.create inst.ncranes no_cargo
  and robot_contents = Array.create inst.nrobots no_cargo
  and robot_locations = Array.create inst.nrobots 0 in
  let rec p = { piles = piles;
		container_locations = container_locations;
		crane_contents = crane_contents;
		robot_contents = robot_contents;
		robot_locations = robot_locations;
		parent = p;
	      }
  in p


let make_is_goal inst =
  (** [make_is_goal inst] makes a goal test funciton. *)
  (fun state ->
     Wrarray.for_alli
       (fun c dst -> state.container_locations.(c) = dst)
       inst.goal)


let make_expand inst =
  (** [make_expand inst] makes an expand function for the given
      problem. *)
  (fun state g ->
     List.map (fun (s, c) -> s, g +. c) (do_expand_robot inst state 0))


let random_goal_state t =
  (** given an instance, constructs a random goal state from that instance *)
  let rec state =
    { piles = Array.init t.npiles (fun _ -> Stack.create ());
      container_locations = (Array.init t.ncontainers
			       (fun cid -> t.goal.(cid)));
      crane_contents = Array.create t.ncranes ~-1;
      robot_contents = Array.create t.nrobots ~-1;
      robot_locations = Array.init t.nrobots (fun _ ->
						Wrarray.random_elt t.goal);
      parent = state; } in
    Array.iteri (fun cid loc -> Stack.push cid state.piles.(loc)) t.goal;
    state


let random_walk p walk_leng =
  (** performrs a random walk of length walk_leng backwards from the goal *)
  let goal = random_goal_state p in
  let exp = make_expand p in
  let rec do_walk remaining state =
    if remaining = 0 then state
    else (let children = List.map fst (exp state 0.) in
	    do_walk (remaining - 1) (Wrlist.random_elt children)) in
    do_walk walk_leng goal


let rec stack_to_list ?(accum = []) st =
  if Stack.is_empty st then accum
  else stack_to_list ~accum:((Stack.pop st) :: accum) st


let random_inst_random_walk ~nlocs ~piles_per_loc ~cranes_per_loc
    ~ncontainers ~walk_leng =
  (** the currnt problem is that we aren't guaranteed to end in a state
      that can be written down when we end *)
  let p = random_usquare ~nlocs ~piles_per_loc ~cranes_per_loc ~ncontainers in
  let rec do_it () =
    let state = random_walk p walk_leng in
    let p' = { adjacent = p.adjacent;
	       pile_contents = Array.map stack_to_list state.piles;
	       cranes_at_location = p.cranes_at_location;
	       piles_at_location = p.piles_at_location;
	       goal = p.goal;
	       nlocations = p.nlocations;
	       ncranes = p.ncranes;
	       ncontainers = p.ncontainers;
	       npiles = p.npiles;
	       nrobots = p.nrobots; } in
    let state' = make_init p' in
      if (validate state') then p'
      else do_it () in
    do_it ()

(* EOF *)
