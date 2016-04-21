(**

    @author jtd7
    @since 2012-03-30
*)

open Greedy_adapt_est

let make_est init_d  =
  let min_d = ref infinity in
  let update_and_return i node openlist children _ =
    let exp = float i.Limit.expanded in
    let divisor =  max epsilon (1. -. (min 1. (!glob_d_err /. (float i.Limit.generated)))) in
    let initial_d = init_d /. divisor in
    let this_d = node.fp.d /. divisor in
      min_d := min !min_d this_d;
      let rem_est = ((initial_d -. !min_d) /. exp) *. !min_d in
	rem_est, 1. -. rem_est /. (rem_est +. exp)
 in
  update_and_return


let dups sface args =
  let module SI = Search_interface in
  let key = wrap sface.SI.key
  and hash = sface.SI.hash
  and equals = sface.SI.equals
  and goal = wrap sface.SI.goal_p
  and hd = sface.Search_interface.hd in
  let i = (Limit.make Limit.Nothing sface.SI.halt_on better_p
	     (Limit.make_default_logger (fun n -> n.fp.g)
		(fun n -> n.ints.depth))) in
  let initial = make_initial sface.SI.initial hd
  and expand = make_expand i sface.SI.domain_expand hd in
  let est = make_est initial.fp.d in
    Limit.unwrap_sol6 unwrap_sol
      (search ~est i key hash equals goal expand initial)


let speedy_dups sface args =
  let module SI = Search_interface in
  let key = wrap sface.SI.key
  and hash = sface.SI.hash
  and equals = sface.SI.equals
  and goal = wrap sface.SI.goal_p
  and hd = sface.Search_interface.hd in
  let i = (Limit.make Limit.Nothing sface.SI.halt_on better_p
	     (Limit.make_default_logger (fun n -> n.fp.g)
		(fun n -> n.ints.depth))) in
  let initial = make_initial sface.SI.initial hd
  and expand = make_expand i sface.SI.domain_expand hd in
  let est = make_est initial.fp.d in
    Limit.unwrap_sol6 unwrap_sol
      (search ~est ~sort:d_sort i key hash equals goal expand initial)
