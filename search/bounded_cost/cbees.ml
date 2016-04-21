(**

    @author jtd7
    @since 2012-03-14
*)
open Bees


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

  let rec expand_best () =
    let edq = Dpq.empty_p dq
    and efq = Dpq.empty_p fhq in
      if not (Limit.halt_p i) && (not edq || not efq)
      then (let n = Dpq.extract_first (if not edq then dq else fhq) in
	      n.pos <- Dpq.no_position;
	      if goal_p n
	      then (Limit.new_incumbent i (Limit.Incumbent (0.,n));
		      expand_best())
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
  let cost_bound = infinity in
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


let dups_pm fd_calc sface args =
  let cost_bound = infinity in
  let hd = sface.Search_interface.hd in
  let h,d = hd sface.Search_interface.initial in
  let i = (Limit.make Limit.Nothing sface.Search_interface.halt_on better_p
	     (Limit.make_default_logger (fun n -> n.vals.f)
		(fun n -> truncate n.vals.depth)))
  and root = make_root h d sface.Search_interface.initial
  and expand = (make_expand_pm sface.Search_interface.domain_expand
		  hd fd_calc cost_bound)
  and goal_p = wrap sface.Search_interface.goal_p
  and key = wrap sface.Search_interface.key
  and hash = sface.Search_interface.hash
  and eq = sface.Search_interface.equals in
    do_search i root expand goal_p key hash eq cost_bound;
    Limit.unwrap_sol6 unwrap_sol (Limit.results6 i)



let austin_dups sface args = dups unclamped_austin sface args

let austin_dups_pm sface args = dups unclamped_austin sface args
