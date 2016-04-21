(**

    @author jtd7
    @since 2012-03-31
*)
open Wted_astar_est


let make_est rb_size =
  let delay_rb = Ring_buffer.init rb_size in
  let dist_rb = Ring_buffer.init rb_size in
  let insert_dist = Ring_buffer.insert dist_rb
  and insert_delay = Ring_buffer.insert delay_rb in
  let update_and_return i node openlist children _ =
    let delay = i.Limit.expanded - node.ints.generated in
    let exp = float i.Limit.expanded in
      insert_dist node.fp.d;
      insert_delay (float delay);
    let mdelay = Ring_buffer.get_mean delay_rb
    and mdist = Ring_buffer.get_mean dist_rb in
    let rem_est = mdelay *. mdist in
      rem_est, 1. -. rem_est /. (rem_est +. exp) in
    update_and_return


let make_est_hat rb_size =
  let delay_rb = Ring_buffer.init rb_size in
  let dist_rb = Ring_buffer.init rb_size in
  let insert_dist = Ring_buffer.insert dist_rb
  and insert_delay = Ring_buffer.insert delay_rb in
  let update_and_return i node openlist children _ =
    let delay = i.Limit.expanded - node.ints.generated in
    let exp = float i.Limit.expanded in
      insert_dist node.fp.est_d;
      insert_delay (float delay);
    let mdelay = Ring_buffer.get_mean delay_rb
    and mdist = Ring_buffer.get_mean dist_rb in
    let rem_est = mdelay *. mdist in
      rem_est, 1. -. rem_est /. (rem_est +. exp) in
    update_and_return


let dups_est sface args =
  let module SI = Search_interface in
  let wt = Search_args.get_float "Wted_astar.dups" args 0 in
  let rb_size = Search_args.get_int "Wted_astar.dups" args 1 in
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
  let est = make_est rb_size in
    Limit.unwrap_sol6 unwrap_sol
      (search ~est i key hash equals goal expand initial)


let dups_est_hat sface args =
  let module SI = Search_interface in
  let wt = Search_args.get_float "Wted_astar.dups" args 0 in
  let rb_size = Search_args.get_int "Wted_astar.dups" args 1 in
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
  let est = make_est_hat rb_size in
    Limit.unwrap_sol6 unwrap_sol
      (search ~est i key hash equals goal expand initial)
