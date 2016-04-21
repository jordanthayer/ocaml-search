(**

    @author jtd7
    @since 2012-03-31
*)

open Wted_astar_est

let make_est init_d =
  let min_d = ref init_d in
  let update_and_return i node openlist children _ =
    let exp = float i.Limit.expanded in
      min_d := min node.fp.d !min_d;
      let percent_complete = (!min_d /. init_d) in
      let percent_remaining = 1. -. percent_complete in
      let percent_velocity = percent_complete /. exp in
      let exp_remaining = percent_remaining /. percent_velocity in
      let new_percent = exp_remaining /. (exp +. exp_remaining) in
	exp_remaining, new_percent  in
    update_and_return


let make_pest_hat root_d =
  let min_d = ref root_d in
  let update_and_return i node openlist children _ =
    let exp = float i.Limit.expanded in
    let divisor =  max epsilon (1. -. (min 1. (!glob_d_err /. (float i.Limit.generated)))) in
    let initial_d = root_d /. divisor in
    let this_d = node.fp.d /. divisor in
      min_d := min this_d !min_d;
      let est = 1. -. (!min_d /. initial_d) in
      exp /. est -. exp, est in
    update_and_return


let make_est_prime () =
  let max_est = ref 0. in
  let update_and_return i node openlist children _ =
    let exp = float i.Limit.expanded in
    let est = 1. -. (node.fp.d /. (node.fp.d +. (float node.ints.depth))) in
      max_est := max !max_est est;
      let est = !max_est in
    exp /. est -. exp, est in
    update_and_return


let make_est_hat_prime () =
  let update_and_return i node openlist children _ =
    let exp = float i.Limit.expanded in
    let est = 1. -. (node.fp.est_d /. (node.fp.est_d +. (float node.ints.depth))) in
    exp /. est -. exp, est in
  update_and_return


let dups sface args =
  let module SI = Search_interface in
  let wt = Search_args.get_float "Wted_astar.dups" args 0 in
  let key = wrap sface.SI.key
  and hash = sface.SI.hash
  and equals = sface.SI.equals
  and goal = wrap sface.SI.goal_p
  and hd = sface.Search_interface.hd in
  let i = (Limit.make Limit.Nothing sface.SI.halt_on better_p
	     (Limit.make_default_logger (fun n -> n.fp.g)
		(fun n -> n.ints.depth))) in
  let initial = make_initial sface.SI.initial hd wt
  and expand = make_expand i sface.SI.domain_expand hd wt in
  let est = make_est initial.fp.d in
    Limit.unwrap_sol6 unwrap_sol
      (search ~est i key hash equals goal expand initial)


let dups_pest_hat sface args =
  let module SI = Search_interface in
  let wt = Search_args.get_float "Wted_astar.dups" args 0 in
  let key = wrap sface.SI.key
  and hash = sface.SI.hash
  and equals = sface.SI.equals
  and goal = wrap sface.SI.goal_p
  and hd = sface.Search_interface.hd in
  let i = (Limit.make Limit.Nothing sface.SI.halt_on better_p
	     (Limit.make_default_logger (fun n -> n.fp.g)
		(fun n -> n.ints.depth))) in
  let initial = make_initial sface.SI.initial hd wt
  and expand = make_expand i sface.SI.domain_expand hd wt in
  let est = make_pest_hat initial.fp.d in
    Limit.unwrap_sol6 unwrap_sol
      (search ~est i key hash equals goal expand initial)


let dups_prime sface args =
  let module SI = Search_interface in
  let wt = Search_args.get_float "Wted_astar.dups" args 0 in
  let key = wrap sface.SI.key
  and hash = sface.SI.hash
  and equals = sface.SI.equals
  and goal = wrap sface.SI.goal_p
  and hd = sface.Search_interface.hd in
  let i = (Limit.make Limit.Nothing sface.SI.halt_on better_p
	     (Limit.make_default_logger (fun n -> n.fp.g)
		(fun n -> n.ints.depth))) in
  let initial = make_initial sface.SI.initial hd wt
  and expand = make_expand i sface.SI.domain_expand hd wt in
  let est = make_est_prime () in
    Limit.unwrap_sol6 unwrap_sol
      (search ~est i key hash equals goal expand initial)


let dups_pest_hat_prime sface args =
  let module SI = Search_interface in
  let wt = Search_args.get_float "Wted_astar.dups" args 0 in
  let key = wrap sface.SI.key
  and hash = sface.SI.hash
  and equals = sface.SI.equals
  and goal = wrap sface.SI.goal_p
  and hd = sface.Search_interface.hd in
  let i = (Limit.make Limit.Nothing sface.SI.halt_on better_p
	     (Limit.make_default_logger (fun n -> n.fp.g)
		(fun n -> n.ints.depth))) in
  let initial = make_initial sface.SI.initial hd wt
  and expand = make_expand i sface.SI.domain_expand hd wt in
  let est = make_est_hat_prime () in
    Limit.unwrap_sol6 unwrap_sol
      (search ~est i key hash equals goal expand initial)
