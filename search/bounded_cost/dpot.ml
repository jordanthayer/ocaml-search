(**

    @author jordan
    @since 2011-07-06
   Is a combination of the potential search constructed via learning and
   d.  Combines the two values by multiplication, which is obviously total
   bullshit, but I think may work well in practice.
*)
open Potential_hhat

let make_expand expand hd cost_bound =
  let no_pos = Dpq.no_position in
  let expand n =
    let nfp = n.fp in
    let nd = n.ints.depth + 1
    and pf = nfp.f -. nfp.h_err
    and pd = nfp.d -. 1. -. nfp.d_err in
    let fnd = float nd in
    List.fold_left (fun kids (s, g) ->
      let h,d = hd s in
      let f = g +. h in
      if f > cost_bound
      then kids
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
		    est_f = est_f; est_d = est_d;
		    potential = est_d *. est_h /. (cost_bound -. g)}
	 and ints = {pos = no_pos; depth = nd;} in
	 { data = s; ints = ints; fp = fp;}::kids)) [] (expand n.data nfp.g)
  in expand


let dups sface args =
  (** Performs an A* search from the initial state to a goal,
      for domains where duplicates are frequently encountered. *)
  let cost_bound = Search_args.get_float "Potential Search" args 0 in
  let search_interface = make_sface sface cost_bound in
    Limit.unwrap_sol6 unwrap_sol
      (Best_first.search_dups
	 (* must have g=0 as base for others, and
	    f<others to prevent re-opening *)
	 search_interface
	 potential
	 better_p
	 setpos
	 getpos)


(* EOF *)
