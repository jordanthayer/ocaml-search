(**

    @author jtd7
    @since 2012-03-29
*)

open Astar_est

let poly_features = Gest_histo.poly_features
let compute_poly = Gest_histo.compute_poly
let compute = Gest_histo.compute
let compute_all = Gest_histo.compute_all



let make_est degree init_d =
  let max_ind = ref (truncate init_d) in
  let values = ref (Array.create (!max_ind + 1) 0.) in
  let min_ind = ref (truncate init_d) in
  let update_and_return i node openlist children =
    min_ind := min !min_ind (truncate node.fpv.d);
    let ind = truncate node.fpv.d in
      if ind > !max_ind
      then (values := Wrarray.extend !values (ind - !max_ind) 0.;
	    max_ind := ind);
      !values.(ind) <- !values.(ind) +. 1.;
      let est = compute degree !max_ind !min_ind !values in
	est, 1. -. (est /. (est +. (float i.Limit.expanded)))
 in
  update_and_return


let make_est_all degree init_d =
  let max_ind = ref (truncate init_d) in
  let values = ref (Array.create (!max_ind + 1) 0.) in
  let min_ind = ref (truncate init_d) in
  let update_and_return i node openlist children =
    min_ind := min !min_ind (truncate node.fpv.d);
    let ind = truncate node.fpv.d in
      if ind > !max_ind
      then (values := Wrarray.extend !values (ind - !max_ind) 0.;
	    max_ind := ind);
      !values.(ind) <- !values.(ind) +. 1.;
      let all = compute_all degree !max_ind !min_ind !values in
      let exp = float i.Limit.expanded in
      let est = all -. exp in
	est, 1. -. (est /. all)
  in
    update_and_return



let dups_est sface args =
  let module SI = Search_interface in
  let key = wrap sface.SI.key
  and hash = sface.SI.hash
  and equals = sface.SI.equals
  and goal = wrap sface.SI.goal_p
  and hd = sface.Search_interface.hd in
  let i = (Limit.make Limit.Nothing sface.SI.halt_on just_f
	     (Limit.make_default_logger (fun n -> n.fpv.g)
		(fun n -> n.iv.depth))) in
  let initial = make_initial sface.SI.initial hd
  and expand = make_expand i sface.SI.domain_expand hd in
  let degree = Search_args.get_int "Fooo" args 0 in
  let est = make_est degree initial.fpv.d in
    search_dups ~est initial goal key i hash equals expand;
    Limit.unwrap_sol6 unwrap_sol (Limit.results6 i)


let dups_est_all sface args =
  let module SI = Search_interface in
  let key = wrap sface.SI.key
  and hash = sface.SI.hash
  and equals = sface.SI.equals
  and goal = wrap sface.SI.goal_p
  and hd = sface.Search_interface.hd in
  let i = (Limit.make Limit.Nothing sface.SI.halt_on just_f
	     (Limit.make_default_logger (fun n -> n.fpv.g)
		(fun n -> n.iv.depth))) in
  let initial = make_initial sface.SI.initial hd
  and expand = make_expand i sface.SI.domain_expand hd in
  let degree = Search_args.get_int "Fooo" args 0 in
  let est = make_est_all degree initial.fpv.d in
    search_dups ~est initial goal key i hash equals expand;
    Limit.unwrap_sol6 unwrap_sol (Limit.results6 i)
