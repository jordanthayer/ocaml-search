(** Code for plotting the learned models.

    @author eaburns
    @since 2010-01-13
*)

let quad a b c =
  (** [quad a b c] makes a quadratic in the proportion of max
      depth. *)
  (fun m d ->
     let x = d /. m in
     let xx = x *. x in
       a *. xx +. b *. xx +. c)


let poly coeffs =
  (** [poly coeffs] computes a polynomial in the porportion of the max
      depth. *)
  (fun m d ->
     let sum = ref coeffs.(0) in
     let x = d /. m in
     let cur_term = ref x in
       for i = 1 to (Array.length coeffs) - 1 do
	 sum := !sum +. (coeffs.(i) *. !cur_term);
	 cur_term := !cur_term *. x;
       done;
       !sum)


let plot_poly_model title max_depth fs =
  (** [plot_poly_model title max_depth fs] plots a polynomials [fs]
      given depth on the x-axis and the maximum depth value
      [max_depth]. *)
    failwith "Needs to be fixed to output a spt-it-out input file"
(*
  let lines =
    Wrlist.mapi
      (fun i f ->
	 let pts =
	   Wrutils.map_n (fun depth ->
			    let d = float depth in
			      d, f (float max_depth) d) max_depth
	 in Array.of_list pts, string_of_int i)
      fs
  in
  let file = Plot_generator.get_filename ~stamped:false true title in
    Ps_plot.line file lines title "depth" "cost"
*)


let plot_points title points_sets =
  (** [plot_points title points_sets] plot sets of points. *)
    failwith "Needs to be fixed to output a spt-it-out input file"
(*
  let scatters = (Wrlist.mapi
		    (fun i pts -> string_of_int i, "", pts)
		    points_sets)
  in
  let file = Plot_generator.get_filename ~stamped:false true title in
    Ps_plot.scatter file scatters title "depth" "cost"
*)


let plot_separate ?(init_depth=0) max_depth max_children costs title =
  (** [plot_separate ?init_depth max_depth max_children costs title]
      plots a separate model to the given path. *)
  let ranks = Array.make_matrix max_children max_depth (0., 0.) in
    for r = 0 to max_children - 1 do
      let depths = ranks.(r) in
	for d = init_depth to max_depth - 1 do
	  let c = costs.(d * max_children + r) in
	    depths.(d) <- (float d), c
	done
    done;
    plot_points title (Array.to_list ranks)
