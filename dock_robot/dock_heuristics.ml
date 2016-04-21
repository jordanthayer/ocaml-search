(**

    @author jtd7
    @since 2011-01-22

   A set of heurisctics for the dock robot domain
*)

open Dock_robot
open Dock_robot_instance

(** {6 Heuristics} ****************************************)

let stack_depth inst state container =
  (** [state inst state container] get the stack depth of the container
      (how many items are above it?). *)
  let loc = state.container_locations.(container) in
  let piles = inst.piles_at_location.(loc) in
  let d = ref 0. in
  let found_at = ref None in
  let rec get_depth = function
    | [] -> ~-.1.0 (* in crane or on robot *)
    | p :: rest ->
	d := 0.;
	Stack.iter (fun c ->
		      if c = container then found_at := Some !d;
		      d := !d +. 1.;)
	  state.piles.(p);
	begin match !found_at with
	  | None -> get_depth rest
	  | Some d -> d
	end
  in get_depth piles


let packages_on_robots inst state =
  (** counts the number of pacakes on robots AND NOT AT THEIR GOAL LOACTION
      in the current state *)
  Array.fold_left
    (fun accum e ->
       if e >= 0 && state.container_locations.(e) <> inst.goal.(e)
       then accum + 1
       else accum) 0 state.robot_contents


let packages_on_cranes inst state =
  (** counts the number of packages being held by cranes in the current state *)
  Array.fold_left
    (fun accum e ->
       if e >= 0 && state.container_locations.(e) <> inst.goal.(e)
       then accum + 1
       else accum) 0 state.crane_contents


let deepest_out_of_place_pile inst state pile =
  (** What is the deepest out fo place package on the given pile *)
  let ret = ref 0
  and ind = ref 1 in
    Stack.iter (fun c ->
		  if state.container_locations.(c) <> inst.goal.(c)
		  then ret := !ind;
		  ind := !ind + 1) pile;
    !ret


let deepest_out_of_place inst state =
  (** Maximum of deepes out of place for all piles *)
  Array.fold_left (fun accum e ->
		     max accum (deepest_out_of_place_pile inst state e)) 0
    state.piles


let shallowest_out_of_place inst state =
  (** Maximum of deepes out of place for all piles *)
  Array.fold_left (fun accum e ->
		     min accum (deepest_out_of_place_pile inst state e)) 0
    state.piles



let sum_deep inst state =
  Array.fold_left (fun accum e ->
		     accum + (deepest_out_of_place_pile inst state e)) 0
    state.piles


let out_of_place_ar inst state =
  (** Deepest out of place for each pile in the state *)
  Array.map (deepest_out_of_place_pile inst state) state.piles


let containers_out_of_place inst state =
  (** counts the number of containers out of place *)
  Wrarray.fold_lefti
    (fun accum c dst -> if state.container_locations.(c) <> dst
     then accum + 1
     else accum) 0 inst.goal



let make_h inst =
  (** [make_h inst] makes a simple heuristic function. *)
  let n = inst.nlocations in
  let dist = Array.make_matrix n n infinity in
    for l = 0 to n - 1 do
      Array.iteri (fun n c -> dist.(l).(n) <- c) inst.adjacent.(l);
      dist.(l).(l) <- 0.
    done;
    (* compute all pairs shortest paths *)
    for k = 0 to n - 1 do
      let dk = dist.(k) in
	for i = 0 to n - 1 do
	  let di = dist.(i) in
	    for j = 0 to n - 1 do
	      di.(j) <- Math.fmin di.(j) (di.(k) +. dk.(j))
	    done
	done
    done;
    (fun state ->
       let locs = state.container_locations in
	 (* find the shortest path among all remaining containers *)
	 Wrarray.fold_lefti (fun sum c dst ->
			       sum
			       +. dist.(locs.(c)).(dst))
	   0. inst.goal)

let make_hd inst =
  (** [make_h inst] makes a simple heuristic function. *)
  let n = inst.nlocations in
  let (+/) (h1,d1) (h2,d2) = h1 +. h2, d1 + d2 in
  let dist = Array.make_matrix n n (infinity,0) in
    for l = 0 to n - 1 do
      Array.iteri (fun n c -> dist.(l).(n) <- c, 1) inst.adjacent.(l);
      dist.(l).(l) <- 0.,0
    done;
    (* compute all pairs shortest paths *)
    for k = 0 to n - 1 do
      let dk = dist.(k) in
	for i = 0 to n - 1 do
	  let di = dist.(i) in
	    for j = 0 to n - 1 do
	      di.(j) <- (let sum = di.(k) +/ dk.(j) in
			   if fst di.(j) < fst sum
			   then di.(j)
			   else sum)
	    done
	done
    done;
    (fun state ->
       let locs = state.container_locations in
	 (* find the shortest path among all remaining containers *)
       let h,d = Wrarray.fold_lefti
	 (fun sum c dst ->
	    let hd = dist.(locs.(c)).(dst) in
	    sum +/ hd) (0.,0) inst.goal in
	 h, float d)


let make_hd_sup inst =
  let dar = out_of_place_ar inst
  and on_robot = packages_on_robots inst
  and on_crane = packages_on_cranes inst
  and oop = containers_out_of_place inst in
    (fun n ->
       let deepests = dar n
       and on_bots = on_robot n
       and on_crane = on_crane n in
       let fd = (Array.fold_left
		   (fun accum deepest ->
		      if deepest > 0
			(* lift, load, drive, unload, return *)
		      then (deepest * 5) - 1 + accum
		      else accum) 0 deepests)
       and oc = on_crane * 2 (* once for load, once for drive *)
       and ob = on_bots  (* once for the drive *) in
       let crane_cost =
	 Array.fold_left
	   (fun accum deepest ->
	      if deepest > 0
	      then ((float (deepest * (deepest + 1))) /. 2. *.
		      Dock_robot.crane_base +. accum)
	      else accum) 0. deepests in
       let loads = (float (oop n)) *. Dock_robot.load_cost in
	 crane_cost +. loads, fd + oc + ob)


let make_dump_pile inst =
  let dar = out_of_place_ar inst
  and on_robot = packages_on_robots inst
  and on_crane = packages_on_cranes inst
  and oop = containers_out_of_place inst in
  let dump_pile = ref false
  and shallowest = ref max_int in
    (fun n ->
       dump_pile := false;
       shallowest := max_int;
       let deepests = dar n
       and on_bots = on_robot n
       and on_crane = on_crane n in
       let fd = (Array.fold_left
		   (fun accum deepest ->
		      shallowest := min !shallowest deepest;
		      if deepest > 0
			(* lift, load, drive, unload, return *)
		      then (deepest * 5) - 1 + accum
		      else (dump_pile := true; accum)) 0 deepests)
       and oc = on_crane * 2 (* once for load, once for drive *)
       and ob = on_bots  (* once for the drive *) in
       let crane_cost =
	 Array.fold_left
	   (fun accum deepest ->
	      if deepest > 0
	      then ((float (deepest * (deepest + 1))) /. 2. *.
		      Dock_robot.crane_base +. accum)
	      else accum) 0. deepests in
       let loads = (float (oop n)) *. Dock_robot.load_cost in
       let add_d = (if !dump_pile
		    then fd + oc + ob
		    else (((!shallowest * 5) - 1) * 2) + fd + oc + ob) in
	 crane_cost +. loads, add_d)

(* EOF *)
