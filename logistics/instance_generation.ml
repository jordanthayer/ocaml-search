(** Tools for making random instances *)

let dist (x1,y1) (x2,y2) =
  sqrt (((x1 -. x2)**2.) +. ((y1 -. y2) **2.))

let geometry
    ?(max_city = 100.)
    ?(max_location_dist = 10.)
    airport_count
    ?(location_counts = Wrutils.map_n (fun _ -> 5) airport_count)
    ?(planes = Wrutils.map_n (fun _ -> 1) airport_count)
    ?(trucks = Wrutils.map_n (fun _ -> 2) airport_count)
    seed =
  Random.init seed;
  let cities = Array.of_list
    (Wrutils.map_n
       (fun _ -> Random.float max_city, Random.float max_city) airport_count) in
  let locations = Array.of_list
    (List.map2 (fun (cx,cy) count ->
		  Array.of_list
		    (Wrutils.map_n (fun _ ->
				      cx +.
					((0.5 -. (Random.float 1.))
					 *. max_location_dist),
				      cy +.
					((0.5 -. (Random.float 1.))
					 *. max_location_dist)) count))
       (Array.to_list cities) location_counts) in
  let base = {Log_instance.cities = airport_count;
	      Log_instance.max_airplanes = (List.fold_left
					      (fun accum i ->
						 accum + i) 0
					      planes);
	      Log_instance.max_trucks = (List.fold_left
					   (fun accum i ->
					      accum + i) 0
					   trucks);
	      Log_instance.airports = (Array.init airport_count
					 (fun i -> Array.init airport_count
					    (fun j ->
					       dist cities.(i) cities.(j))));
	      Log_instance.locations =
      (Array.init airport_count
	 (fun city ->
	    Array.init (Array.length locations.(city))
	      (fun l1 ->
		 Array.init (Array.length locations.(city))
		   (fun l2 -> dist locations.(city).(l1)
		      locations.(city).(l2)))));
	      Log_instance.goal = [||];} in
    (* at this point, base has the network of airports and cities,
       but trucks, packages and so on have not been placed *)
    base
(* EOF *)
