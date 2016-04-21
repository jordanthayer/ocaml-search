(**

    @author jtd7
    @since 2012-03-26
*)

open Astar_est

let make_est init_d rb_size =
  let rb = Ring_buffer.init rb_size in
  let insert = Ring_buffer.insert rb in
  let update_and_return i node openlist children =
    let exp = float i.Limit.expanded in
      insert ((init_d -. node.fpv.d) /. exp);
      let rem_est = (Ring_buffer.get_mean rb) *. node.fpv.d in
	rem_est, 1. -. rem_est /. (rem_est +. exp)
 in
  update_and_return


let make_pest rb_size =
  let max_p = ref 0. in
  let update_and_return i node openlist children =
    max_p := max !max_p (node.fpv.g /. node.fpv.f);
    let pcomp = !max_p in
    let rem_p = 1. -. pcomp in
      rem_p *. ((float i.Limit.expanded) +. node.fpv.d) , pcomp
  in
    update_and_return


let make_vest rb_size =
  let rb = Ring_buffer.init rb_size in
  let insert = Ring_buffer.insert rb in
  let update_and_return i node openlist children =
    let vac = i.Limit.expanded - node.iv.generated in
      insert (float vac);
      let mvac = Ring_buffer.get_mean rb in
      let r_exp = mvac *. node.fpv.d in
	r_exp, 1. -. r_exp /. (r_exp +. float i.Limit.expanded) in
    update_and_return



let make_vest' rb_size =
  let rb = Ring_buffer.init rb_size in
  let rb' = Ring_buffer.init rb_size in
  let insert = Ring_buffer.insert rb
  and insert' = Ring_buffer.insert rb' in
  let update_and_return i node openlist children =
    let vac = i.Limit.expanded - node.iv.generated in
      insert (float vac);
      insert' node.fpv.d;
      let mvac = Ring_buffer.get_mean rb in
      let md = Ring_buffer.get_mean rb' in
      let r_exp = mvac *. md in
	r_exp, 1. -. r_exp /. (r_exp +. float i.Limit.expanded) in
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
  let rb_size = Search_args.get_int "Fooo" args 0 in
  let est = make_est initial.fpv.d rb_size in
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
  let est = make_pest (-1) in
    search_dups ~est initial goal key i hash equals expand;
    Limit.unwrap_sol6 unwrap_sol (Limit.results6 i)


let dups_vest sface args =
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
  let rb_size = Search_args.get_int "Fooo" args 0 in
  let est = make_vest rb_size in
    search_dups ~est initial goal key i hash equals expand;
    Limit.unwrap_sol6 unwrap_sol (Limit.results6 i)


let dups_vest' sface args =
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
  let rb_size = Search_args.get_int "Fooo" args 0 in
  let est = make_vest' rb_size in
    search_dups ~est initial goal key i hash equals expand;
    Limit.unwrap_sol6 unwrap_sol (Limit.results6 i)
