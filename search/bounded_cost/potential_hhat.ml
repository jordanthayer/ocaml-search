(**

    @author jordan
    @since 2011-06-18
*)


type fp_values = {
  h     : float;
  d     : float;
  h_err : float;
  d_err : float;
  g     : float;
  f     : float;
  est_f : float;
  est_d : float;
  potential : float;
}

type int_vals = {
mutable  pos : int;
  depth : int;
}

type 'a node = {
  data : 'a;
  ints : int_vals;
  fp : fp_values;
}

let potential a b =
  let afp = a.fp
  and bfp = b.fp in
  let ap = afp.potential
  and bp = bfp.potential in
  ap < bp ||
    ap = bp && afp.est_d < bfp.est_d

let better_p a b = (a.fp.f) <= (b.fp.f)

(***************  Utility functions **************)

let unwrap_sol = function
  | Limit.Incumbent (q, n) -> Some (n.data, n.fp.g)
  | _ -> None


let wrap fn = (fun n -> fn n.data)

let setpos n i = n.ints.pos <- i
and getpos n = n.ints.pos


let make_initial initial hd c =
  let np = Dpq.no_position in
  let h, d = neg_infinity, neg_infinity in
  let fp = { h = h; d = d; h_err = 0.; d_err = 0.; g = 0.; f = h;
	     est_f = h; est_d = d; potential = h /. 1.}
  and iv = {pos = np; depth = 0;} in
  let rec n = { data = initial;
		ints = iv;
		fp = fp;} in n


let epsilon = 0.0001

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
		    potential = est_h /. (1. -. g /. cost_bound)}
	 and ints = {pos = no_pos; depth = nd;} in
	 { data = s; ints = ints; fp = fp;}::kids)) [] (expand n.data nfp.g)
  in expand


let make_sface sface cost_bound =
  let def_log = Limit.make_default_logger
    (fun n -> if n.ints.pos <> -1337 then n.fp.f else infinity)
    (fun n -> n.ints.depth) in
    Search_interface.make
      ~node_expand:(make_expand sface.Search_interface.domain_expand
		      sface.Search_interface.hd cost_bound)
      ~goal_p:(wrap sface.Search_interface.goal_p)
      ~key:(wrap sface.Search_interface.key)
      ~hash:sface.Search_interface.hash
      ~equals:sface.Search_interface.equals
      ~halt_on:sface.Search_interface.halt_on
      sface.Search_interface.domain
      (make_initial sface.Search_interface.initial
	 sface.Search_interface.hd cost_bound)
      better_p
      (fun i ->
	 sface.Search_interface.info.Limit.log
	   (Limit.unwrap_info (fun n -> n.data) i);
	 def_log i)

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



