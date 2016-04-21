(**

    @author jtd7
    @since 2012-03-29
*)


open Lacaml.Impl.D
open Greedy_est


let poly_features = Gest_histo.poly_features
let compute_poly = Gest_histo.compute_poly


let compute degree max_ind min_ind vals =
  let range = max_ind - min_ind in
    if range <= degree
    then (float max_ind)
    else
      let ylist = Array.fold_left
	(fun accum element ->
	   match element with
	     | [] -> accum
	     | _ -> [|((List.fold_left (+.) 0. element) /.
			 (float (List.length element)))|]::accum) [] vals in
      let yar = Array.of_list ylist in
      let xs = Mat.of_array (Array.init (Array.length yar)
			       (fun i -> poly_features degree
				  (float (max_ind - i)))) in
      let ys = Mat.of_array yar in
	ignore (gelsd xs ys);
	max 0. (min 1. (compute_poly degree ys (float min_ind)))


let compute_slow degree max_ind min_ind vals =
  let range = max_ind - min_ind in
    if range <= degree
    then (float max_ind)
    else
      let ylist = Array.fold_left
	(fun accum element ->
	   match element with
	     | [] -> accum
	     | _ -> (List.fold_left (fun a e -> [|e|]::a) accum element))
	[] vals in
      let yar = Array.of_list ylist in
      let xs = Mat.of_array (Array.init (Array.length yar)
			       (fun i -> poly_features degree
				  (float (max_ind - i)))) in
      let ys = Mat.of_array yar in
	ignore (gelsd xs ys);
	max 0. (min 1. (compute_poly degree ys (float min_ind)))


let make_est degree init_d =
  let max_ind = ref (truncate init_d) in
  let values = ref (Array.create (!max_ind + 1) []) in
  let min_ind = ref (truncate init_d) in
  let update_and_return i node openlist children c =
    let ind = truncate node.d in
    min_ind := min !min_ind (ind);
      if ind > !max_ind
      then (values := Wrarray.extend !values (ind - !max_ind) [];
	    max_ind := ind);
      !values.(ind) <- (node.d /. init_d)::(!values.(ind));
      if c
      then (let est = 1. -. (compute degree !max_ind !min_ind !values) in
	    let exp = float i.Limit.expanded in
	    exp /. est -. exp, est)
      else nan,nan
 in
  update_and_return



let dups_est sface args =
  let module SI = Search_interface in
  let key = wrap sface.SI.key
  and hash = sface.SI.hash
  and equals = sface.SI.equals
  and goal = wrap sface.SI.goal_p
  and hd = sface.Search_interface.hd in
  let i = (Limit.make Limit.Nothing sface.SI.halt_on just_g
	     (Limit.make_default_logger (fun n -> n.g)
		(fun _ -> -1))) in
  let initial = make_init hd sface.SI.initial
  and expand = make_expand i sface.SI.domain_expand hd in
  let rb_size = Search_args.get_int "Wted_astar.dups" args 0 in
  let est = make_est rb_size initial.d in
    Limit.unwrap_sol6 unwrap_sol
      (search ~est i key hash equals goal expand initial)


let speedy_dups_est sface args =
  let module SI = Search_interface in
  let key = wrap sface.SI.key
  and hash = sface.SI.hash
  and equals = sface.SI.equals
  and goal = wrap sface.SI.goal_p
  and hd = sface.Search_interface.hd in
  let i = (Limit.make Limit.Nothing sface.SI.halt_on just_g
	     (Limit.make_default_logger (fun n -> n.g)
		(fun _ -> -1))) in
  let initial = make_init hd sface.SI.initial
  and expand = make_expand i sface.SI.domain_expand hd in
  let rb_size = Search_args.get_int "Wted_astar.dups" args 0 in
  let est = make_est rb_size initial.d in
    Limit.unwrap_sol6 unwrap_sol
      (search ~est ~sort:d_then_g i key hash equals goal expand initial)
