(* Hand crafted heuristics for the logistics domains

   Transit costs come from a few places
   1: Loading / Unloading Packages onto trucks
   2: Loading / Unloading Packages onto planes
   3: Driving Packages around a city
   4: Shifting packages between airports

   packages_out_of_place covers 1 and 2.
   driving_cost/distance covers 3
   flight_cost/distance covers 4

   It should also be possible to do a minimum spanning tree for the flying
   and driving legs of the calculation.  That will end up stored in a different
   file for readability's sake. -- This cannot be done, would not be admissible
   because you may be able to do separate solves for pairs of packages.

   I think the heuristics will screw up if you allow for packages being
   deliverd to the airport, so if you want to have instances that do that, go
   add an additional check in various places.

   Need to do heuristics better.  Cheapest incoming arc on destinations,
   cheapest outgoing arcs on sources
*)


(* Braindead *)
let h0 _ = 0.
and d0 _ = 0.
and hd0 _ = 0.,0.


(* Counts the minimum number / costs of loads and unloads that will be needed
   in order to get a package from the current location to the destination.*)
let packages_out_of_place_cost goal_locations =
  (fun n ->
     Wrarray.fold_lefti
       (fun accum i e ->
	  match e with
	    | Log_instance.OnTruck _ -> accum +. Log_instance.load_truck_cost
	    | Log_instance.OnPlane _ -> accum +. Log_instance.load_plane_cost
	    | Log_instance.Location (pc,pl) ->
		(if Log_instance.pLocationEq e goal_locations.(i)
		 then accum
		 else (match goal_locations.(i) with
			 | Log_instance.Location (gc,gl) ->
			     if gc = pc
			     then accum +. 2. *. Log_instance.load_truck_cost
			     else (accum +. 4. *. Log_instance.load_truck_cost
				   +. 2. *. Log_instance.load_plane_cost)
			 | _ -> failwith "Goal needs to be a destination")))
       0. n.Log_instance.packages)


let packages_out_of_place_dist goal_locations =
  (fun n ->
     Wrarray.fold_lefti
       (fun accum i e ->
	  match e with
	    | Log_instance.OnTruck _ -> accum +. 1.
	    | Log_instance.OnPlane _ -> accum +. 1.
	    | Log_instance.Location (pc,pl) ->
		(if Log_instance.pLocationEq e goal_locations.(i)
		 then accum
		 else (match goal_locations.(i) with
			 | Log_instance.Location (gc,gl) ->
			     if gc = pc
			     then accum +. 2.
			     else accum +. 6.
			 | _ -> failwith "Goal needs to be a destination")))
       0. n.Log_instance.packages)


let packages_out_of_place_hd goal_locations =
  (fun n ->
     Wrarray.fold_lefti
       (fun (h,d) i e ->
	  match e with
	    | Log_instance.OnTruck _ -> (h +. Log_instance.load_truck_cost,
					 d +. 1.)
	    | Log_instance.OnPlane _ -> (h +. Log_instance.load_plane_cost,
					 d +. 1.)
	    | Log_instance.Location (pc,pl) ->
		(if Log_instance.pLocationEq e goal_locations.(i)
		 then (h,d)
		 else (match goal_locations.(i) with
			 | Log_instance.Location (gc,gl) ->
			     if gc = pc
			     then (h +. 2. *. Log_instance.load_truck_cost,
				   d +. 2.)
			     else (h +. 4. *. Log_instance.load_truck_cost
				   +. 2. *. Log_instance.load_plane_cost,
				   d +. 6.)
			 | _ -> failwith "Goal needs to be a destination")))
       (0.,0.) n.Log_instance.packages)


(* Estimates the cost / action count of the flights that need to happn.*)
let flight_cost static_info goal_locations =
  (* Cheapest cost of flights that need to be made, so the cheapes arc
     coming into any city *)
  let shortest_edges =
    Array.init (static_info.Log_instance.cities)
      (fun city -> Array.fold_left (fun accum e -> Math.fmin accum e)
	 infinity static_info.Log_instance.airports.(city)) in
    (fun n ->
       Wrarray.fold_lefti
	 (fun accum i e ->
	    match e with
	      | Log_instance.OnTruck t ->
		  (match goal_locations.(i) with
		       Log_instance.Location (gc,gl) ->
			 (if gc <> (fst n.Log_instance.trucks.(t))
			  then accum +. shortest_edges.(gc)
			  else accum)
		     | _ -> accum)
	      | Log_instance.OnPlane p ->
		  (match goal_locations.(i) with
		       Log_instance.Location (gc,gl) ->
			 (if gc <> n.Log_instance.airplanes.(p)
			  then accum +. shortest_edges.(gc)
			  else accum)
		     | _ -> accum)
	      | Log_instance.Location (pc,pl) ->
		  (match goal_locations.(i) with
		       Log_instance.Location (gc,gl) ->
			 if gc <> pc
			 then accum +. shortest_edges.(gc)
			 else accum
		     | _ -> accum ))
	 0. n.Log_instance.packages)


let flight_distance static_info goal_locations =
  (* minimum number of flights that might need to be made -> 1 per
     package destination city *)
  (fun n ->
     Wrarray.fold_lefti
       (fun accum i e ->
	  match e with
	    | Log_instance.OnTruck t ->
		(match goal_locations.(i) with
		     Log_instance.Location (gc,_) ->
		       if gc <> (fst n.Log_instance.trucks.(t))
		       then accum +. 1.
		       else accum
		   | _ -> failwith "Goal needs to be a destination")
	    | Log_instance.OnPlane p ->
		(match goal_locations.(i) with
		   | Log_instance.Location (gc,lc) ->
		       if ((gc <> n.Log_instance.airplanes.(p))
			   || lc <> Log_instance.atAirport)
		       then accum +. 1.
		       else accum
		   | _ -> failwith "Goal needs to be a destination")
	    | Log_instance.Location (pc,_) ->
		(match goal_locations.(i) with
		   | Log_instance.Location (gc,_) -> (if pc <> gc
						     then accum +. 1.
						     else accum)
		   | _ -> failwith "Goal needs to be a destination"))
       0. n.Log_instance.packages)


let flight_hd static_info goal_locations =
  (* Cheapest cost of flights that need to be made, so the cheapes arc
     coming into any city *)
  let shortest_edges =
    Array.init (static_info.Log_instance.cities)
      (fun city -> Array.fold_left (fun accum e -> Math.fmin accum e)
	 infinity static_info.Log_instance.airports.(city)) in
    (fun n ->
       Wrarray.fold_lefti
	 (fun (h,d) i e ->
	    match e with
	      | Log_instance.OnTruck t ->
		  (match goal_locations.(i) with
		       Log_instance.Location (gc,gl) ->
			 (if gc <> (fst n.Log_instance.trucks.(t))
				   then (h +. shortest_edges.(gc),
					 d +. 1.)
				   else (h,d))
		     | _ -> (h,d))
	      | Log_instance.OnPlane p ->
		  (match goal_locations.(i) with
		       Log_instance.Location (gc,gl) ->
			 (if gc <> n.Log_instance.airplanes.(p)
				   then (h +. shortest_edges.(gc),
					 d +. 1.)
				   else (h,d))
		     | _ -> (h,d))
	      | Log_instance.Location (pc,pl) ->
		  (match goal_locations.(i) with
		       Log_instance.Location (gc,gl) ->
			 if gc <> pc
			 then (h +. shortest_edges.(gc),
			       d +. 1.)
			 else (h,d)
		     | _ -> (h,d)))
	 (0.,0.) n.Log_instance.packages)



let drive_cost static_info goal_locations =
  let shortest_edges =
    Array.init (static_info.Log_instance.cities)
      (fun city ->
	 Array.init (Array.length static_info.Log_instance.locations.(city))
	   (fun i ->
	      Array.fold_left (fun accum e -> Math.fmin accum e)
		infinity
		static_info.Log_instance.locations.(city).(i))) in
    (fun n ->
       Wrarray.fold_lefti
	 (fun accum i e ->
	    match e with
	      | Log_instance.OnTruck t ->
		  (match goal_locations.(i) with
		       Log_instance.Location (gc,gl) ->
			 (match n.Log_instance.trucks.(t) with
			      (tc,tl) ->
				(if tc <> gc
				 then (accum
				       +. shortest_edges.(tc).(Log_instance.atAirport)
				       (* is this line right if we allow airport deliveries?*)
				       +. shortest_edges.(gc).(gl))

				 else (if tl <> gl
				       then accum +. shortest_edges.(gc).(gl)
				       else accum)))
		     | _ -> failwith "Goal needs to be a destination")

	      | Log_instance.OnPlane p ->
		  (match goal_locations.(i) with
		     | Log_instance.Location (gc,lc) ->
			 if ((gc <> n.Log_instance.airplanes.(p))
			     || lc <> Log_instance.atAirport)
			 then accum +. shortest_edges.(gc).(lc)
			 else accum
		     | _ -> failwith "Goal needs to be a destination")

	      | Log_instance.Location (pc,pl) ->
		  (match goal_locations.(i) with
		     | Log_instance.Location (gc,gl) ->
			 (if pc <> gc
			  then (accum
				+. shortest_edges.(pc).(Log_instance.atAirport)
				+. shortest_edges.(gc).(gl))
			  else (if pl <> gl
				then accum +. shortest_edges.(gc).(gl)
				else accum))
		     | _ -> failwith "Goal needs to be a destination"))
	 0. n.Log_instance.packages)



let drive_distance static_info goal_locations =
  (* minimum number of drives that might need to be made *)
  (fun n ->
     Wrarray.fold_lefti
       (fun accum i e ->
	  match e with
	    | Log_instance.OnTruck t ->
		(match goal_locations.(i) with
		     Log_instance.Location (gc,gl) ->
		       (match n.Log_instance.trucks.(t) with
			    (tc,tl) -> (if tc <> gc
					then accum +. 2.
					else (if tl <> gl
					      then accum +. 1.
					      else accum)))

		   | _ -> failwith "Goal needs to be a destination")

	    | Log_instance.OnPlane p ->
		(match goal_locations.(i) with
		   | Log_instance.Location (gc,lc) ->
		       if ((gc <> n.Log_instance.airplanes.(p))
			   || lc <> Log_instance.atAirport)
		       then accum +. 1.
		       else accum
		   | _ -> failwith "Goal needs to be a destination")

	    | Log_instance.Location (pc,pl) ->
		(match goal_locations.(i) with
		   | Log_instance.Location (gc,gl) -> (if pc <> gc
						       then accum +. 2.
						       else (if pl <> gl
							     then accum +. 1.
							     else accum))
		   | _ -> failwith "Goal needs to be a destination"))
       0. n.Log_instance.packages)



let drive_hd static_info goal_locations =
  let shortest_edges =
    Array.init (static_info.Log_instance.cities)
      (fun city ->
	 Array.init (Array.length static_info.Log_instance.locations.(city))
	   (fun i ->
	      Array.fold_left (fun accum e -> Math.fmin accum e)
		infinity
		static_info.Log_instance.locations.(city).(i))) in
    (fun n ->
       Wrarray.fold_lefti
	 (fun (h,d) i e ->
	    match e with
	      | Log_instance.OnTruck t ->
		  (match goal_locations.(i) with
		       Log_instance.Location (gc,gl) ->
			 (match n.Log_instance.trucks.(t) with
			      (tc,tl) ->
				(if tc <> gc
				 then (h
				       +. shortest_edges.(tc).(Log_instance.atAirport)
				       (* is this line right if we allow airport deliveries?*)
				       +. shortest_edges.(gc).(gl),
				      d +. 2.)

				 else (if tl <> gl
				       then (h +. shortest_edges.(gc).(gl),
					     d +. 1.)
				       else (h,d))))
		     | _ -> failwith "Goal needs to be a destination")

	      | Log_instance.OnPlane p ->
		  (match goal_locations.(i) with
		     | Log_instance.Location (gc,lc) ->
			 if ((gc <> n.Log_instance.airplanes.(p))
			     || lc <> Log_instance.atAirport)
			 then (h +. shortest_edges.(gc).(lc),
			       d +. 1.)
			 else (h,d)
		     | _ -> failwith "Goal needs to be a destination")

	      | Log_instance.Location (pc,pl) ->
		  (match goal_locations.(i) with
		     | Log_instance.Location (gc,gl) ->
			 (if pc <> gc
			  then (h
				+. shortest_edges.(pc).(Log_instance.atAirport)
				+. shortest_edges.(gc).(gl),
			       d +. 2.)
			  else (if pl <> gl
				then (h +. shortest_edges.(gc).(gl),
				      d +. 1.)
				else (h,d)))
		     | _ -> failwith "Goal needs to be a destination"))
	 (0.,0.) n.Log_instance.packages)


(* Heuristics *)

let h static =
  let package_lifts = packages_out_of_place_cost static.Log_instance.goal
  and flight_cost = flight_cost static static.Log_instance.goal
  and drive_cost = drive_cost static static.Log_instance.goal in
  (fun n -> (package_lifts n) +. (flight_cost n) +. (drive_cost n))


let d static =
  let package_lifts = packages_out_of_place_dist static.Log_instance.goal
  and flight_cost = flight_distance static static.Log_instance.goal
  and drive_cost = drive_distance static static.Log_instance.goal in
  (fun n -> (package_lifts n) +. (flight_cost n) +. (drive_cost n))


let hd static =
  let package_lifts = packages_out_of_place_hd static.Log_instance.goal
  and flight_cost = flight_hd static static.Log_instance.goal
  and drive_cost = drive_hd static static.Log_instance.goal in
  (fun n ->
     List.fold_left (fun (a,b) (c,d) -> (a +. c, b +. d))
       (0.,0.)
       [(package_lifts n); (flight_cost n); (drive_cost n)])

(* EOF *)
