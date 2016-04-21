(**

    @author jtd7
    @since 2012-03-26
*)

open Lacaml.Impl.D
open Greedy_est


let poly_features degree x =
  (** [poly_features degree x] get a polynomial feature array. *)
  Array.init (degree + 1) (fun d -> x ** (float d))


let compute_poly degree coeffs x =
  (** [compute_poly degree coeffs x] compute a polynomial in [x] given
      the coefficients in a bigarray. *)
  let vl = ref 0. and t = ref 1. in
    for i = 0 to degree do
      let c = coeffs.{i + 1, 1} in
	vl := !vl +. c *. !t;
	t := !t *. x;
    done;
    !vl


let compute degree max_ind min_ind vals =
  let range = max_ind - min_ind in
    if range <= degree then (float max_ind)
    else
      let ylist = Array.fold_left (fun accum el ->
				     if el > 0. then [|el|]::accum
				     else accum) [] vals in
      let ys = Mat.of_array (Array.of_list ylist) in
      let xs = Mat.of_array (Array.init (List.length ylist)
			       (fun i -> poly_features degree
				  (float (max_ind - i)))) in
	ignore (gelsd xs ys);
	let sum = ref 0. in
	  for i = (min_ind - 1) downto 0 do
	    sum := !sum +. (max 1. (compute_poly degree ys (float i)))
	  done;
	  !sum


let compute_all degree max_ind min_ind vals =
  let range = max_ind - min_ind in
    if range <= degree then (float max_ind)
    else
      let ylist = Array.fold_left (fun accum el ->
				     if el > 0. then [|el|]::accum
				     else accum) [] vals in
      let ys = Mat.of_array (Array.of_list ylist) in
      let xs = Mat.of_array (Array.init (List.length ylist)
			       (fun i -> poly_features degree
				  (float (max_ind - i)))) in
	ignore (gelsd xs ys);
	let sum = ref 0. in
	  for i = max_ind downto 0 do
	    sum := !sum +. (max 1.
			      ((*max vals.(i)*)
				 (compute_poly degree ys (float i))))
	  done;
	  !sum


let make_est degree init_d =
  D_histo.output_col_hdr();
  let max_ind = ref (truncate init_d) in
  let values = ref (Array.create (!max_ind + 1) 0.) in
  let min_ind = ref (truncate init_d) in
  let update_and_return i node openlist children c =
    min_ind := min !min_ind (truncate node.d);
    let ind = truncate node.d in
      if ind > !max_ind
      then (values := Wrarray.extend !values (ind - !max_ind) 0.;
	    max_ind := ind);
      !values.(ind) <- !values.(ind) +. 1.;
      if c
      then (let est = compute degree !max_ind !min_ind !values in
	      est, 1. -. (est /. (est +. (float i.Limit.expanded))))
      else nan,nan
  in
  let print_histo () =
    Array.iteri
      (fun bucket count -> D_histo.output_row ~bucket
	 ~count:(truncate count)) !values in
  update_and_return, print_histo


let make_est_all degree init_d =
  D_histo.output_col_hdr();
  let max_ind = ref (truncate init_d) in
  let values = ref (Array.create (!max_ind + 1) 0.) in
  let min_ind = ref (truncate init_d) in
  let update_and_return i node openlist children c =
    min_ind := min !min_ind (truncate node.d);
    let ind = truncate node.d in
      if ind > !max_ind
      then (values := Wrarray.extend !values (ind - !max_ind) 0.;
	    max_ind := ind);
      !values.(ind) <- !values.(ind) +. 1.;
      if c
      then (let all = compute_all degree !max_ind !min_ind !values in
	    let exp = float i.Limit.expanded in
	    let est = all -. exp in
	      est, 1. -. (est /. all))
      else nan, nan
  in
  let print_histo () =
    Array.iteri
      (fun bucket count -> D_histo.output_row ~bucket
	 ~count:(truncate count)) !values in
    update_and_return, print_histo



let dups_est sface args =
  let module SI = Search_interface in
  let key = wrap sface.SI.key
  and hash = sface.SI.hash
  and equals = sface.SI.equals
  and goal = wrap sface.SI.goal_p
  and hd = sface.Search_interface.hd in
  let degree = Search_args.get_int "Wted_astar.dups" args 0 in
  let i = (Limit.make Limit.Nothing sface.SI.halt_on just_g
	     (Limit.make_default_logger (fun n -> n.g)
		(fun _ -> -1))) in
  let initial = make_init hd sface.SI.initial
  and expand = make_expand i sface.SI.domain_expand hd in
  let est,print_histo = make_est degree initial.d in
  let v = (search ~est i key hash equals goal expand initial) in
    print_histo();
    Limit.unwrap_sol6 unwrap_sol v



let dups_est_all sface args =
  let module SI = Search_interface in
  let key = wrap sface.SI.key
  and hash = sface.SI.hash
  and equals = sface.SI.equals
  and goal = wrap sface.SI.goal_p
  and hd = sface.Search_interface.hd in
  let degree = Search_args.get_int "Wted_astar.dups" args 0 in
  let i = (Limit.make Limit.Nothing sface.SI.halt_on just_g
	     (Limit.make_default_logger (fun n -> n.g)
		(fun _ -> -1))) in
  let initial = make_init hd sface.SI.initial
  and expand = make_expand i sface.SI.domain_expand hd in
  let est,ph = make_est_all degree initial.d in
  let v =       (search ~est i key hash equals goal expand initial) in
    ph ();
    Limit.unwrap_sol6 unwrap_sol v



let speedy_dups_est sface args =
  let module SI = Search_interface in
  let key = wrap sface.SI.key
  and hash = sface.SI.hash
  and equals = sface.SI.equals
  and goal = wrap sface.SI.goal_p
  and hd = sface.Search_interface.hd in
  let degree = Search_args.get_int "Wted_astar.dups" args 0 in
  let i = (Limit.make Limit.Nothing sface.SI.halt_on just_g
	     (Limit.make_default_logger (fun n -> n.g)
		(fun _ -> -1))) in
  let initial = make_init hd sface.SI.initial
  and expand = make_expand i sface.SI.domain_expand hd in
  let est, _ = make_est degree initial.d in
    Limit.unwrap_sol6 unwrap_sol
      (search ~est ~sort:d_then_g i key hash equals goal expand initial)


let speedy_dups_est_all sface args =
  let module SI = Search_interface in
  let key = wrap sface.SI.key
  and hash = sface.SI.hash
  and equals = sface.SI.equals
  and goal = wrap sface.SI.goal_p
  and hd = sface.Search_interface.hd in
  let degree = Search_args.get_int "Wted_astar.dups" args 0 in
  let i = (Limit.make Limit.Nothing sface.SI.halt_on just_g
	     (Limit.make_default_logger (fun n -> n.g)
		(fun _ -> -1))) in
  let initial = make_init hd sface.SI.initial
  and expand = make_expand i sface.SI.domain_expand hd in
  let est,ph = make_est_all degree initial.d in
    Limit.unwrap_sol6 unwrap_sol
      (search ~est ~sort:d_then_g i key hash equals goal expand initial)

