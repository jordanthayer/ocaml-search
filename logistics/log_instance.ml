(* Logistics Problem Instances

   The general goal is to take packages that are at a variety of locations
   and, by use of trucks and planes, transport the packages to their desired
   locations.

   Airports
   Airplanes
   Trucks
   Packages
 *)

type airplane = int
type truck = int
type package = int
type city = int
type location = int

type packageLocation =
  | OnTruck of truck
  | OnPlane of airplane
  | Location of (city * location)


(* Information changes between nodes *)
type state = {
  g : float;
  airplanes : city array;
  trucks : (city * location) array; (* first int is city, next int is location *)
  packages : packageLocation array;
  parent : state;
}


(* information known across all nodes *)
type static_info = {
  cities : int; (* maximum number of cities *)
  max_airplanes : int; (* max number of airplanes *)
  max_trucks : int; (* number of trucks per city *)
  airports : (float array) array; (* cost of going between airport *)
  locations : ((float array) array) array; (* One int list for each city *)
  goal : packageLocation array;
}

let atAirport = 0

let load_truck_cost = 5.
and load_plane_cost = 10.


let pLocationEq a b =
  (* Equality test for package locations *)
  match a with
    | OnTruck i -> (match b with
			OnTruck j -> i = j
		      | _ -> false)
    | OnPlane i -> (match b with
			OnPlane j -> i = j
		      | _ -> false)
    | Location (c1,l1) -> (match b with
			       Location (c2,l2) -> c1 = c2 && l1 = l2
			     | _ -> false)


let make_package_only_goal_p static =
  (* Goal does not care about returning trucks sol airplanes to the initial
     locations *)
  let package_goals = static.goal in
  let max = Array.length package_goals in
  let rec fn locs i =
    if i >= max
    then true
    else (if pLocationEq package_goals.(i) locs.(i)
	  then fn locs (i+1)
	  else false) in
    (fun n -> fn n.packages 0)


let key n =
  (* worst key ever *)
  failwith "not implemented"


(* Functions to help with expansion *)
let drive_truck n t_id destination cost =
  {n with trucks = Array.mapi
      (fun i e -> if i = t_id then destination else e) n.trucks;
     g = n.g +. cost;
     parent = n}

let fly_plane n p_id destination cost =
  { n with airplanes = Array.mapi
      (fun i e -> if i = p_id then destination else e) n.airplanes;
      g = n.g +. cost;
      parent = n }


let all_drivings static n =
  (* Drive all trucks one move around their cities *)
  Array.fold_left
    (fun accum e -> e @ accum)
    []
    (Array.mapi (fun truck (city,location) ->
		   let dests =
		     List.filter (fun n -> n <> -1)
		       (Array.to_list
			  (Array.init (Array.length static.locations.(city))
			     (fun i -> if i = city then -1 else i))) in
		     List.map (fun i -> drive_truck n truck (city,i)
				 static.locations.(city).(location).(i))
		       dests) n.trucks)


let all_flying static n =
  (* Fly all planes one hop *)
  Array.fold_left
    (fun accum e -> e @ accum)
    []
    (Array.mapi (fun plane city ->
		   let dests =
		     List.filter (fun n -> n <> -1)
		       (Array.to_list
			  (Array.init (Array.length static.locations)
			     (fun i -> if i = city then -1 else i))) in
		     List.map (fun i -> fly_plane n plane city
				 static.airports.(city).(i))
		       dests) n.airplanes)


let load_package_to_truck n package p_ele =
  (* Generates all possible successors where [package]
     was loaded onto a truck *)
  match p_ele with
    | OnTruck _ -> []
    | OnPlane _ -> []
    | Location (city,loc) ->
	Wrarray.fold_lefti
	  (fun accum truck (tcity,tloc) ->
	     if tcity = city && tloc = loc
	     then {n with packages =
		 Array.mapi (fun i e -> (if i = package
					 then OnTruck truck
					 else e)) n.packages;
		     g = n.g +. load_truck_cost;
		     parent = n;}::accum
	     else accum) [] n.trucks


let load_package_to_airplane n package p_ele =
  (* Generates all possible successors where [package]
     was loaded onto an airplane *)
  match p_ele with
    | OnTruck _ -> []
    | OnPlane _ -> []
    | Location (city,loc) ->
	if loc <> atAirport
	then []
	else
	Wrarray.fold_lefti
	  (fun accum plane p_city ->
	     if p_city = city
	     then {n with packages =
		 Array.mapi (fun i e -> (if i = package
					 then OnPlane plane
					 else e)) n.packages;
		  g = n.g +. load_plane_cost;
		  parent = n;}::accum
	     else accum) [] n.airplanes


let unload_package n =
  (* Generates all possible successors where packages
     were unloded from trucks or airplanes *)
  Wrarray.fold_lefti
    (fun accum p_ind package ->
       match package with
	 | OnTruck t ->
	     { n with packages =
		 Array.mapi (fun i e -> (if i <> p_ind
					 then e
					 else Location n.trucks.(t)))
		   n.packages;
	     g = n.g +. load_truck_cost;
	     parent = n;}::accum
	 | OnPlane plane ->
	     { n with packages =
		 Array.mapi (fun i e -> (if i <> p_ind
					 then e
					 else Location (n.airplanes.(plane),
							atAirport)))
		   n.packages;
	     g = n.g +. load_plane_cost;
	     parent = n;}::accum
	 | _ -> accum) [] n.packages



let make_expand static_info =
  (* Takes unchanging static info and generates an expand function
     for the problem. *)
  (fun n g ->
     let unloaded_children = unload_package n
     and trucked_children = (Array.to_list
			       (Array.mapi (fun i e ->
					      load_package_to_truck n i e)
				  n.packages))
     and planed_children = (Array.to_list
			      (Array.mapi (fun i e ->
					     load_package_to_airplane n i e)
				 n.packages))
     and driven_trucks = all_drivings static_info n
     and flown_planes = all_flying static_info n in
     let trucked_children = (List.fold_left (fun accum e -> e @ accum) []
			       trucked_children)
     and planed_children = (List.fold_left (fun accum e -> e @ accum) []
			      planed_children) in
       List.map
	 (fun n -> n,n.g)
	 (List.concat [unloaded_children;
		       trucked_children;
		       planed_children;
		       driven_trucks;
		       flown_planes]))


let equals s1 s2 =
  s1 == s2


let rec sol_length  ?(cl = 0) n =
  if equals n.parent n
  then cl
  else sol_length ~cl:(cl + 1) n.parent


(* EOF *)
