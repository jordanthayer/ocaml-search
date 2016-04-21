(** Inputs longitudes and latitudes and outputs a list of the nearest
    nodes.

    @author eaburns
    @since 2010-08-18
*)


let main () =
  let datafile = Sys.argv.(1) in
    Verb.with_level Verb.optional
      (fun () ->
	 Printf.printf "Loading %s... %!" datafile;
	 let map, time =
	   Wrsys.with_time (fun () -> Roadmap.Binary_format.load datafile)
	 in
	 let closest_point = Roadmap.nearest_node map in
	   Printf.printf "done (%f seconds)\n%!" time;
	   try
	     while true do
	       Printf.printf "enter lon and lat > %!";
	       let lon, lat = Scanf.scanf " %f %f" Fn.gather2 in
	       let coordx, coordy = Dimacs.coord_of_lon_lat ~lon ~lat in
	       let n, x, y, d = closest_point ~x:coordx ~y:coordy in
		 Printf.printf "node=%d, coords=(%d, %d), coord dist=%f\n%!"
		   n x y (sqrt d);
	     done
	   with End_of_file -> ())


let _ = main ()
