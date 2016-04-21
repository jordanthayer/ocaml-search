(**

    @author jtd7
    @since 2012-04-04
*)

open Astar_est

let make_pest opt_cost root_f =
  let denom = opt_cost -. root_f in
  let update_and_return i node openlist children =
    let value = (node.fpv.f -. root_f) /. denom in
    let rem_p = 1. -. value in
      rem_p *. ((float i.Limit.expanded) +. node.fpv.d) , value
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
  let est = make_pest opt_cost initial.fpv.f in
    search_dups ~est initial goal key i hash equals expand;
    Limit.unwrap_sol6 unwrap_sol (Limit.results6 i)
