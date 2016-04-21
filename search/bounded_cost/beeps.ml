(**

    @author jordan
    @since 2011-06-17
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

type q =
  | DQ
  | FQ
  | UNKNOWN


type 'a node = {
  data : 'a;
  ints : int_vals;
  fp : fp_values;
  mutable q : q
}

(**************** Ordering Predicates *********************)

let dcheapest a b =
  let afp = a.fp
  and bfp = b.fp in
  let aed = afp.est_d
  and bed = bfp.est_d
  and aef = afp.est_f
  and bef = bfp.est_f in
  (aed < bed) ||
    ((aed = bed) && (aef <= bef))

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
	     est_f = h; est_d = d; potential = h}
  and iv = {pos = np; depth = 0;} in
  let rec n = { data = initial;
		ints = iv;
		fp = fp;
		q = UNKNOWN} in n


let epsilon = 0.0001

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
		    est_f = est_f; est_d = est_d;
		    potential = est_h /. (1. -. g /. !cost_bound)}
	 and ints = {pos = no_pos; depth = nd;} in
	 assert (est_d >= 0.);
	 assert (est_f >= f);
	 let kid = { data = s; ints = ints; fp = fp; q = UNKNOWN} in
	 if est_f < !cost_bound
	 then kid::dac, fac
	 else dac, kid::fac))
      (* so based on the argument I made about the optimal way to
	 prove that a given cost bound is unfeasible, this may really
	 be the wrong way to approach things.  How true that is in a
	 practical sense may have something to do withhow much
	 confidence I happen to have in my estimations.  I think in
	 practice that sorting the subsequent queue on f probably
	 isn't the right thing to do, but only because I know that my
	 estimates are not going to be very good in advance. *)
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
		    est_f = est_f; est_d = est_d; potential = h /. (!cost_bound -. g)}
	 and ints = {pos = no_pos; depth = nd;} in
	 assert (est_d >= 0.);
	 assert (est_f >= f);
	 let kid = { data = s; ints = ints; fp = fp; q = UNKNOWN} in
	 if est_f < !cost_bound
	 then kid::dac, fac
	 else dac, kid::fac))
      ([],[]) (expand n.data nfp.g)
  in expand



let do_search ?(continue = false) i root expand goal_p key hash eq cost_bound =
  let closed = Htable.create hash eq 100
  and dq = Dpq.create dcheapest setpos 100 root
  and pq = Dpq.create potential setpos 100 root in

  let insert_d n =
    n.q <- DQ;
    Limit.incr_gen i;
    let state = key n in
      try
	let prev = Htable.find closed state in
	  Limit.incr_dups i;
	  if n.fp.f < prev.fp.f
	  then (Htable.replace closed state n;
		if prev.ints.pos = Dpq.no_position
		then Dpq.insert dq n
		else (match prev.q with
			| DQ -> Dpq.swap dq prev.ints.pos n
			| FQ -> (Dpq.remove pq prev.ints.pos;
				 Dpq.insert dq n)
			| _ -> failwith "Impossible"))
      with Not_found ->
	Dpq.insert dq n;
	Htable.replace closed state n in

  let insert_f n =
    n.q <- FQ;
    Limit.incr_gen i;
    let state = key n in
      try
	let prev = Htable.find closed state in
	  Limit.incr_dups i;
	  if n.fp.f < prev.fp.f
	  then (Htable.replace closed state n;
		if prev.ints.pos = Dpq.no_position
		then Dpq.insert pq n
		else (match prev.q with
			| FQ -> Dpq.swap pq prev.ints.pos n
			| DQ -> (Dpq.remove dq prev.ints.pos;
				 Dpq.insert pq n)
			| _ -> failwith "Impossible"))
      with Not_found ->
	Dpq.insert pq n;
	Htable.replace closed state n in

  let rec expand_best () =
    let edq = Dpq.empty_p dq
    and efq = Dpq.empty_p pq in
      if not (Limit.halt_p i) && (not (edq && efq))
      then (let n = Dpq.extract_first (if not edq then dq else pq) in
	      n.ints.pos <- Dpq.no_position;
	      if goal_p n
	      then (Limit.new_incumbent i (Limit.Incumbent (0.,n));
		    if continue
		    then (cost_bound := n.fp.g;
			  expand_best()))
	      else (Limit.incr_exp i;
		    let dqkids, fqkids = expand n in
		      List.iter insert_d dqkids;
		      List.iter insert_f fqkids;
		      Limit.curr_q i ((Dpq.count dq) + (Dpq.count pq));
		      expand_best ())) in
    Htable.add closed (key root) root;
    Dpq.insert dq root;
    expand_best ();
    i.Limit.log i


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


let anytime_dups sface args =
  let cost_bound = ref infinity in
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
    do_search ~continue:true i root expand goal_p key hash eq cost_bound;
    Limit.unwrap_sol6 unwrap_sol (Limit.results6 i)


let anytime_dups_pm sface args =
  let cost_bound = ref infinity in
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
    do_search ~continue:true i root expand goal_p key hash eq cost_bound;
    Limit.unwrap_sol6 unwrap_sol (Limit.results6 i)

(* EOF *)
