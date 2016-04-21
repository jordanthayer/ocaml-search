(**

    @author jtd7
    @since 2012-03-29
*)

open Greedy_est

let poly_features = Gest_histo.poly_features
let compute_poly = Gest_histo.compute_poly
let compute = Gest_histo.compute
let compute_all = Gest_histo.compute_all


let make_est degree init_d =
  let max_ind = ref (truncate init_d) in
  let values = ref (Array.create (!max_ind + 1) 0.) in
  let min_ind = ref (truncate init_d) in
  let sum = ref 0. in
  let update_and_return i node openlist children c =
    min_ind := min !min_ind (truncate node.d);
    let ind = truncate node.d in
      if ind > !max_ind
      then (values := Wrarray.extend !values (ind - !max_ind) 0.;
	    max_ind := ind);
      !values.(ind) <- !values.(ind) +. 1.;
      if c
      then (let est = compute degree !max_ind !min_ind !values in
	    let exp = (float i.Limit.expanded) in
	      sum := est +. !sum -. (exp -. 1.);
	      let est = !sum /. exp in
		est, 1. -. (est /. (est +. exp)))
      else nan,nan
 in
  update_and_return


let make_est_rb ?(rb_size = 500) degree init_d =
  let rb = Ring_buffer.init rb_size in
  let insert = Ring_buffer.insert rb in
  let rbs = float (rb_size + 1) /. 2. in
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
	    let exp = (float i.Limit.expanded) in
	      insert est;
	      let est = (Ring_buffer.get_mean rb) -. (min ((exp +. 1.) /. exp) rbs) in
		est, 1. -. (est /. (est +. exp)))
      else nan, nan
 in
  update_and_return



let make_est_all degree init_d =
  let max_ind = ref (truncate init_d) in
  let values = ref (Array.create (!max_ind + 1) 0.) in
  let min_ind = ref (truncate init_d) in
  let sum = ref 0. in
  let update_and_return i node openlist children c =
    let ind = truncate node.d in
    min_ind := min !min_ind ind;
      if ind > !max_ind
      then (values := Wrarray.extend !values (ind - !max_ind) 0.;
	    max_ind := ind);
      !values.(ind) <- !values.(ind) +. 1.;
      if c
      then (let all = compute_all degree !max_ind !min_ind !values in
	    let exp = float i.Limit.expanded in
	      sum := all +. !sum;
	      let all = !sum /. exp in
	      let est = all -. exp in
		est, 1. -. (est /. all))
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
  let degree = Search_args.get_int "Wted_astar.dups" args 0 in
  let i = (Limit.make Limit.Nothing sface.SI.halt_on just_g
	     (Limit.make_default_logger (fun n -> n.g)
		(fun _ -> -1))) in
  let initial = make_init hd sface.SI.initial
  and expand = make_expand i sface.SI.domain_expand hd in
  let est = make_est degree initial.d in
    Limit.unwrap_sol6 unwrap_sol
      (search ~est i key hash equals goal expand initial)


let dups_est_rb sface args =
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
  let est = make_est_rb degree initial.d in
    Limit.unwrap_sol6 unwrap_sol
      (search ~est i key hash equals goal expand initial)


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
  let est = make_est_all degree initial.d in
    Limit.unwrap_sol6 unwrap_sol
      (search ~est i key hash equals goal expand initial)





