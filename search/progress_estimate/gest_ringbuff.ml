(**

    @author jtd7
    @since 2012-03-23
*)

open Greedy_est

let make_pest init_d =
  let min_d = ref init_d in
  let update_and_return i node openlist children _ =
    let nd = node.d in
    let exp = float i.Limit.expanded in
      min_d := min !min_d nd;
      let percent_est = 1. -. (!min_d /. init_d) in
	exp /. percent_est -. exp, percent_est in
    update_and_return


let make_hpest init_h =
  let min_h = ref init_h in
  let update_and_return i node openlist children _ =
    let nd = node.h in
    let exp = float i.Limit.expanded in
      min_h := min !min_h nd;
      let percent_est = 1. -. (!min_h /. init_h) in
	exp /. percent_est -. exp, percent_est in
    update_and_return


let make_fpest () =
  let max_p = ref 0. in
  let update_and_return i node openlist children _ =
    let nh = node.h in
    let nf = node.g +. node.h in
    let percent = nh /. nf in
    let exp = float i.Limit.expanded in
      max_p := max !max_p (1. -. percent);
      exp /. !max_p, !max_p in
    update_and_return



let make_pest_prime () =
  let max_percent = ref 0. in
  let update_and_return i node openlist children _ =
    let exp = float i.Limit.expanded in
    let percent_est = 1. -. (node.d /. (node.d +. node.depth)) in
      max_percent := max percent_est !max_percent;
      exp /. !max_percent -. exp, !max_percent in
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


let dups_hpest sface args =
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
  let est = make_hpest initial.h in
    Limit.unwrap_sol6 unwrap_sol
      (search ~est i key hash equals goal expand initial)


let speedy_dups_pest sface args =
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
      (search ~est ~sort:d_then_g i key hash equals goal expand initial)


let dups_pest_prime sface args =
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
  let est = make_pest_prime () in
    Limit.unwrap_sol6 unwrap_sol
      (search ~est i key hash equals goal expand initial)


let dups_fpest sface args =
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
  let est = make_fpest () in
    Limit.unwrap_sol6 unwrap_sol
      (search ~est i key hash equals goal expand initial)


let speedy_dups_pest_prime sface args =
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
  let est = make_pest_prime () in
    Limit.unwrap_sol6 unwrap_sol
      (search ~est ~sort:d_then_g i key hash equals goal expand initial)
