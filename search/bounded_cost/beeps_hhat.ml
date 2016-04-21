(**

    @author jordan
    @since 2011-06-18
*)
open Beeps

let make_expand expand hd cost_bound =
  let no_pos = Dpq.no_position in
  let expand n =
    let nfp = n.fp in
    let nd = n.ints.depth + 1
    and pf = nfp.f -. nfp.h_err
    and pd = nfp.d -. 1. -. nfp.d_err in
    let fnd = float nd in
    List.fold_left (fun (dac,fac) (s, g) ->
      let h,d = hd s in
      let f = g +. h in
      if f > !cost_bound
      then fac,dac
      else
	(let h_err = f -. pf (* becomes f -. nfp.f +. nfp.h_err *)
	and d_err = d -. pd (* becomes d -. n.d +. 1. +. nfp.d_err *) in
	 let h_err = if Math.finite_p h_err then h_err else n.fp.h_err
	 and d_err = if Math.finite_p d_err then d_err else n.fp.d_err in
	 let dstep = d_err /. fnd in
	 let est_d = Math.fmax d (if dstep >= 1. then d /. epsilon
	   else d /. (1. -. dstep)) in
	 let est_h = h +. (Math.fmax 0. ((h_err /. fnd) *. est_d)) in
	 let est_f = g +. est_h in
	 let fp = { h = h; d = d; h_err = h_err; d_err = d_err; g = g; f = f;
		    est_f = est_f; est_d = est_d; potential = est_h /. (!cost_bound -. g)}
	 and ints = {pos = no_pos; depth = nd;} in
	 assert (est_d >= 0.);
	 assert (est_f >= f);
	 let kid = { data = s; ints = ints; fp = fp; q = UNKNOWN} in
	 if est_f < !cost_bound
	 then kid::dac, fac
	 else dac, kid::fac))
      ([],[]) (expand n.data nfp.g)
  in expand

let make_expand_pm expand hd cost_bound =
  let no_pos = Dpq.no_position in
  let expand n =
    let nfp = n.fp in
    let nd = n.ints.depth + 1
    and pf = nfp.f -. nfp.h_err
    and pd = nfp.d -. 1. -. nfp.d_err in
    let fnd = float nd in
    List.fold_left (fun (dac,fac) (s, g) ->
      let h,d = hd s
      and t_cost = g -. n.fp.g in
      let h = Math.fmax h (n.fp.h -. t_cost) in
      let f = g +. h in
      if f > !cost_bound
      then fac,dac
      else
	(let h_err = f -. pf (* becomes f -. nfp.f +. nfp.h_err *)
	and d_err = d -. pd (* becomes d -. n.d +. 1. +. nfp.d_err *) in
	 let h_err = if Math.finite_p h_err then h_err else n.fp.h_err
	 and d_err = if Math.finite_p d_err then d_err else n.fp.d_err in
	 let dstep = d_err /. fnd in
	 let est_d = Math.fmax d (if dstep >= 1. then d /. epsilon
	   else d /. (1. -. dstep)) in
	 let est_h = h +. (Math.fmax 0. ((h_err /. fnd) *. est_d)) in
	 let est_f = g +. est_h in
	 let fp = { h = h; d = d; h_err = h_err; d_err = d_err; g = g; f = f;
		    est_f = est_f; est_d = est_d; potential = est_h /. (!cost_bound -. g)}
	 and ints = {pos = no_pos; depth = nd;} in
	 assert (est_d >= 0.);
	 assert (est_f >= f);
	 let kid = { data = s; ints = ints; fp = fp; q = UNKNOWN} in
	 if est_f < !cost_bound
	 then kid::dac, fac
	 else dac, kid::fac))
      ([],[]) (expand n.data nfp.g)
  in expand


let dups sface args =
  let cost_bound = ref (Search_args.get_float "BEES" args 0) in
  let hd = sface.Search_interface.hd in
  let i = (Limit.make Limit.Nothing sface.Search_interface.halt_on better_p
	     (Limit.make_default_logger (fun n -> n.fp.f)
		(fun n -> n.ints.depth)))
  and root = make_initial sface.Search_interface.initial hd cost_bound
  and expand = (make_expand sface.Search_interface.domain_expand
		  hd cost_bound)
  and goal_p = wrap sface.Search_interface.goal_p
  and key = wrap sface.Search_interface.key
  and hash = sface.Search_interface.hash
  and eq = sface.Search_interface.equals in
    do_search i root expand goal_p key hash eq cost_bound;
    Limit.unwrap_sol6 unwrap_sol (Limit.results6 i)


let dups_pm sface args =
  let cost_bound = ref (Search_args.get_float "BEES" args 0) in
  let hd = sface.Search_interface.hd in
  let i = (Limit.make Limit.Nothing sface.Search_interface.halt_on better_p
	     (Limit.make_default_logger (fun n -> n.fp.f)
		(fun n -> n.ints.depth)))
  and root = make_initial sface.Search_interface.initial hd cost_bound
  and expand = (make_expand_pm sface.Search_interface.domain_expand
		  hd cost_bound)
  and goal_p = wrap sface.Search_interface.goal_p
  and key = wrap sface.Search_interface.key
  and hash = sface.Search_interface.hash
  and eq = sface.Search_interface.equals in
    do_search i root expand goal_p key hash eq cost_bound;
    Limit.unwrap_sol6 unwrap_sol (Limit.results6 i)
