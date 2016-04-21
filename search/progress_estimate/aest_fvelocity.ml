(**

    @author jtd7
    @since 2012-04-04
*)

open Astar_est

let make_est opt_f root_f =
  let max_f = ref root_f in
  let update_and_return i node openlist children =
    let expanded = float i.Limit.expanded in
      max_f := max !max_f node.fpv.f;
      let diff = !max_f -. root_f in
	if diff <= 0.
	then node.fpv.d, 0.
	else
	  let dfdt = diff /. expanded in
	  let remaining_dist = opt_f -. !max_f in
	  let rem_est = remaining_dist /. dfdt in
	    rem_est, 1. -. rem_est /. (rem_est +. expanded)
 in
  update_and_return


let make_pest opt_f root_f =
  let max_f = ref root_f in
  let f_delta = opt_f -. root_f in
  let update_and_return i node openlist children =
    let expanded = float i.Limit.expanded in
      max_f := max !max_f node.fpv.f;
      let c_percent = (!max_f -. root_f) /. f_delta in
      let to_expand = expanded /. (1. -. c_percent) in
	to_expand, c_percent
 in
  update_and_return


let dups sface args =
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
  let opt_cost = Search_args.get_float "Fooo" args 0 in
  let est = make_est opt_cost initial.fpv.f in
    search_dups ~est initial goal key i hash equals expand;
    Limit.unwrap_sol6 unwrap_sol (Limit.results6 i)


let dups_pest sface args =
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
  let opt_cost = Search_args.get_float "Fooo" args 0 in
  let est = make_pest opt_cost initial.fpv.f in
    search_dups ~est initial goal key i hash equals expand;
    Limit.unwrap_sol6 unwrap_sol (Limit.results6 i)
