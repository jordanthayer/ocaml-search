(**

    @author jordan
    @since 2012-04-14
*)

open Greedy_est

let make_pest init_d =
  (* this appears to be equivalent to just percentage... why? *)
  let min_d = ref init_d in
  let update_and_return i node openlist children _ =
    let nd = node.d in
    let exp = float i.Limit.expanded in
      min_d := min !min_d nd;
      let percent_complete = (!min_d /. init_d) in
      let percent_remaining = 1. -. percent_complete in
      let percent_velocity = percent_complete /. exp in
      let exp_remaining = percent_remaining /. percent_velocity in
      let new_percent = exp_remaining /. (exp +. exp_remaining) in
	exp_remaining, new_percent  in
    update_and_return


let dups_pest sface args =
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
  let est = make_pest initial.d in
    Limit.unwrap_sol6 unwrap_sol
      (search ~est i key hash equals goal expand initial)

