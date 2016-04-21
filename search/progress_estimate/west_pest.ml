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
      let est = 1. -. !min_d /. init_d in
	exp /. est -. exp, est in
    update_and_return


let make_hest () =
  let max_percent = ref 0. in
  let update_and_return i node openlist children _ =
    let exp = float i.Limit.expanded in
      max_percent := max !max_percent (node.fp.g /. node.fp.wf);
      let est = !max_percent in
	exp /. est -. exp, est in
    update_and_return


let make_hest_prime () =
  let max_percent = ref 0. in
  let update_and_return i node openlist children _ =
    let exp = float i.Limit.expanded in
      max_percent := max !max_percent (node.fp.g /. node.fp.f);
      let est = !max_percent in
	exp /. est -. exp, est in
    update_and_return


let make_hest_prime_2 hroot =
  let hmin = ref hroot in
  let update_and_return i node openlist children _ =
    let exp = float i.Limit.expanded in
    hmin := min !hmin node.fp.h;
      let est = 1. -. !hmin /. hroot in
	exp /. est -. exp, est in
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
  let max_percent = ref 0. in
  let update_and_return i node openlist children _ =
    let exp = float i.Limit.expanded in
    let est = 1. -. (node.fp.d /. (node.fp.d +. (float node.ints.depth))) in
      max_percent := max !max_percent est;
      let est = !max_percent in
      exp /. est -. exp, est in
    update_and_return


let make_est_hat_prime () =
  let max_percent = ref 0. in
  let update_and_return i node openlist children _ =
    let exp = float i.Limit.expanded in
    let est = 1. -. (node.fp.est_d /.
		       (node.fp.est_d +. (float node.ints.depth))) in
      max_percent := max !max_percent est;
      let est = !max_percent in
	exp /. est -. exp, est in
    update_and_return


let make_horrible_hack root_h w =
  let min_h = ref root_h
  and max_percent = ref 0. in
  let astarness = 1. /. w in
  let greediness = 1. -. astarness in
  let update_and_return i node openlist children _ =
    let exp = float i.Limit.expanded in
      min_h := min !min_h node.fp.h;
      max_percent := max !max_percent (node.fp.g /. node.fp.f);
      let est = ((astarness *. !max_percent) +.
		   (greediness *. (1. -. !min_h /. root_h))) in
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


let dups_hack sface args =
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
  let est = make_horrible_hack initial.fp.h wt in
    Limit.unwrap_sol6 unwrap_sol
      (search ~est i key hash equals goal expand initial)


let dups_hest sface args =
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
  let est = make_hest () in
    Limit.unwrap_sol6 unwrap_sol
      (search ~est i key hash equals goal expand initial)


let dups_hest_prime sface args =
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
  let est = make_hest_prime () in
    Limit.unwrap_sol6 unwrap_sol
      (search ~est i key hash equals goal expand initial)


let dups_hest_prime2 sface args =
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
  let est = make_hest_prime_2 initial.fp.h in
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
