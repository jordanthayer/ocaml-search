(**

    @author jtd7
    @since 2011-05-04

   Bounded cost EES
*)

type vals =
    { h_err : float;
      d_err : float;
      depth : float;
      g : float;
      f : float;
      d : float;
      est_d : float;
      est_f : float; }


type q =
  | DQ
  | FQ
  | UNKNOWN

type 'a node = {
  data : 'a;
  vals : vals;
  mutable pos : int;
  mutable q : q}


let wrap f =
  (** Wraps a function [f] which works on domain data and makes it so
      that it can be applied to nodes *)
  (fun n -> f n.data)


let setpos n i =
  n.pos <- i


let unwrap_sol s =
  (** Decomposes the solution [s] into a form which the domains are expecting
      when doing validation *)
  match s with
      Limit.Incumbent (q, n) -> Some (n.data, n.vals.f)
    | _ -> None



let dhat_order a b =
  let av = a.vals
  and bv = b.vals in
  let aed = av.est_d
  and bed = bv.est_d in
    aed < bed || (aed = bed && av.est_f <= bv.est_f)

let fhat_order a b =
  let av = a.vals
  and bv = b.vals in
  let aef = av.f
  and bef = bv.f in
    aef < bef || (aef = bef && av.est_d <= bv.est_d)


let better_p a b =
  (** Determines which of the nodes represents a better solution *)
  a.vals.f <= b.vals.f


let make_expand expand hd fd_calc cost_bound =
  (** [expand] is the domain expand
      [hd] is a cost and distance estimator
      [f_calc] g h, d, depth h_err, d_err to calculate f^ estimates *)
  (fun n ->
     let nv = n.vals in
     let depth' = nv.depth +. 1. in
       List.fold_left
	 (fun (dac,fac) (s,g) ->
	    let h,d = hd s in
	    let f = g +. h in
	      if f > cost_bound then (dac,fac)
	      else
		let h_err = (f -. (nv.f)) +. nv.h_err
		and d_err = d -. nv.d +. 1. +. nv.d_err in
		let ef,ed = fd_calc ~g ~h ~d ~depth:depth' ~h_err ~d_err in
		let c = {data = s;
			 pos = Dpq.no_position;
			 q = UNKNOWN;
			 vals = { h_err = h_err;
				  d_err = d_err;
				  depth = depth';
				  g = g;
				  f = f;
				  d = d;
				  est_d = ed;
				  est_f = ef; };} in
		  if ef < cost_bound
		  then (c::dac),fac
		  else dac, (c::fac))
	 ([],[]) (expand n.data n.vals.g))


let make_root h d data =
  (** Constructs a root node *)
  { data = data;
    pos = Dpq.no_position;
    q = UNKNOWN;
    vals = { h_err = 0.;
	     d_err = 0.;
	     depth = 0.;
	     g = 0.;
	     f = h;
	     d = d;
	     est_d = h;
	     est_f = d; }}


let do_search i root expand goal_p key hash eq cost_bound =
  let closed = Htable.create hash eq 100
  and dq = Dpq.create dhat_order setpos 100 root
  and fhq = Dpq.create fhat_order setpos 100 root in

  let insert_d n =
    n.q <- DQ;
    Limit.incr_gen i;
    let state = key n in
      try
	let prev = Htable.find closed state in
	  Limit.incr_dups i;
	  if n.vals.f < prev.vals.f
	  then (Htable.replace closed state n;
		if prev.pos = Dpq.no_position
		then Dpq.insert dq n
		else (match prev.q with
			| DQ -> Dpq.swap dq prev.pos n
			| FQ -> (Dpq.remove fhq prev.pos;
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
	  if n.vals.f < prev.vals.f
	  then (Htable.replace closed state n;
		if prev.pos = Dpq.no_position
		then Dpq.insert fhq n
		else (match prev.q with
			| FQ -> Dpq.swap fhq prev.pos n
			| DQ -> (Dpq.remove dq prev.pos;
				 Dpq.insert fhq n)
			| _ -> failwith "Impossible"))
      with Not_found ->
	Dpq.insert fhq n;
	Htable.replace closed state n in

  let board = Grid_vis_tool.vis_board in

  let rec expand_best () =
    let edq = Dpq.empty_p dq
    and efq = Dpq.empty_p fhq in
      if not (Limit.halt_p i) && (not edq || not efq)
      then (let n = Dpq.extract_first (if not edq
				       then dq
				       else fhq) in
	    let x,y = n.data.Grid.pos in
	      if not edq
	      then !board.(x).(y) <- 1.::!board.(x).(y)
	      else !board.(x).(y) <- 2.::!board.(x).(y);
	      n.pos <- Dpq.no_position;
	      if goal_p n then Limit.new_incumbent i (Limit.Incumbent (0.,n))
	      else (Limit.incr_exp i;
		    let dqkids,fqkids = expand n in
		      List.iter insert_d dqkids;
		      List.iter insert_f fqkids;
		      Limit.curr_q i ((Dpq.count dq) + (Dpq.count fhq));
		      expand_best ())) in
    Htable.add closed (key root) root;
    Dpq.insert dq root;
    expand_best ();
    i.Limit.log i


(**************************************************************************)

let unclamped_austin ~g ~h ~d ~depth ~h_err ~d_err =
  (** A correction of the single step error correction wheeler and I
	did, corrected to a geometric series by austin *)
  let d_step = Math.fmin (d_err /. depth) 1.
  and h_step = h_err /. depth in
  let nd = d /. (1. -. d_step) in
  let fh = if Math.finite_p nd then g +. h +. nd *. h_step else infinity in
    assert (fh = fh);
    assert (nd = nd);
    fh, nd


let dups fd_calc sface args =
  let cost_bound = Search_args.get_float "BEES" args 0 in
  let hd = sface.Search_interface.hd in
  let h,d = hd sface.Search_interface.initial in
  let i = (Limit.make Limit.Nothing sface.Search_interface.halt_on better_p
	     (Limit.make_default_logger (fun n -> n.vals.f)
		(fun n -> truncate n.vals.depth)))
  and root = make_root h d sface.Search_interface.initial
  and expand = (make_expand sface.Search_interface.domain_expand
		  hd fd_calc cost_bound)
  and goal_p = wrap sface.Search_interface.goal_p
  and key = wrap sface.Search_interface.key
  and hash = sface.Search_interface.hash
  and eq = sface.Search_interface.equals in
    do_search i root expand goal_p key hash eq cost_bound;
    Limit.unwrap_sol6 unwrap_sol (Limit.results6 i)



let austin_dups sface args = dups unclamped_austin sface args

(* EOF *)
