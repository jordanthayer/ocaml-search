(**

    @author jtd7
    @since 2012-03-30
*)

open Greedy_est

let make_est init_d =
  let min_d = ref init_d in
  let update_and_return i node openlist children _ =
    let exp = float i.Limit.expanded in
      min_d := min !min_d node.d;
      let rem_est = ((init_d -. !min_d) /. exp) *. !min_d in
	rem_est, 1. -. rem_est /. (rem_est +. exp)
 in
  update_and_return


let dups_est sface args =
  let module SI = Search_interface in
  let key = wrap sface.SI.key
  and hash = sface.SI.hash
  and equals = sface.SI.equals
  and goal = wrap sface.SI.goal_p
  and hd = sface.Search_interface.hd in
  let initial = make_init hd sface.SI.initial in
  let i = (Limit.make Limit.Nothing sface.SI.halt_on just_g
	     (Limit.make_default_logger (fun n -> n.g)
		(fun _ -> -1))) in
  let expand = make_expand i sface.SI.domain_expand hd in
  let est = make_est initial.d in
    Limit.unwrap_sol6 unwrap_sol
      (search ~est i key hash equals goal expand initial)


let speedy_dups_est sface args =
  let module SI = Search_interface in
  let key = wrap sface.SI.key
  and hash = sface.SI.hash
  and equals = sface.SI.equals
  and goal = wrap sface.SI.goal_p
  and hd = sface.Search_interface.hd in
  let initial = make_init hd sface.SI.initial in
  let i = (Limit.make Limit.Nothing sface.SI.halt_on just_g
	     (Limit.make_default_logger (fun n -> n.g)
		(fun _ -> -1))) in
  let expand = make_expand i sface.SI.domain_expand hd in
  let est = make_est initial.d in
    Limit.unwrap_sol6 unwrap_sol
      (search ~est ~sort:d_then_g i key hash equals goal expand initial)
