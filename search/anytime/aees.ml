type fp_values = {
  h     : float;
  d     : float;
  h_err : float;
  d_err : float;
  g     : float;
  f     : float;
  est_f : float;
  est_d : float;
}

type int_vals = {
  mutable  clean_pos : int;
  mutable  open_pos : int;
  depth : int;
}

type 'a node = {
  data : 'a;
  mutable  geqe : 'a node Double_geq.entry;
  ints : int_vals;
  fp : fp_values;
  preferred : bool;
}

(**************** Ordering Predicates *********************)

let open_sort a b =
  let afp = a.fp
  and bfp = b.fp in
  let aef = afp.est_f
  and bef = bfp.est_f
  and ag = afp.g
  and bg = bfp.g in
    aef < bef  ||  (* sort by fhat *)
      (aef = bef && ag > bg) ||
      (aef = bef && ag = bg &&  (afp.d < bfp.d)) (* break ties on high g *)


let focal_sort a b =
  let afp = a.fp
  and bfp = b.fp in
  let aed = afp.est_d
  and bed = bfp.est_d in
    (aed < bed) ||
      (let aef = afp.est_f
       and bef = bfp.est_f in
	 ((aed = bed) && (aef < bef)) ||
	   ((aed = bed) && (aef = bef) && (afp.g >= bfp.g)))


let clean_sort a b =
  let afp = a.fp
  and bfp = b.fp in
  let af = afp.f
  and bf= bfp.f in
  af < bf ||
    (af = bf &&
	afp.g > bfp.g)


let make_close_enough bound =
  let close_enough a b =
    let wf = a.fp.est_f *. bound in
      (b.fp.est_f <= wf) in
  close_enough


let better_p a b = (a.fp.f) <= (b.fp.f)


(***************  Utility functions **************)

let unwrap_sol = function
  | Limit.Incumbent (q, n) -> Some (n.data, n.fp.g)
  | _ -> None


let wrap fn = (fun n -> fn n.data)


let on_fhat = ref 0
and on_dhat = ref 0
and on_f = ref 0
and delayed = ref 0


let reset () =
  on_fhat := 0;
  on_dhat := 0;
  on_f := 0;
  delayed := 0

let incr r =
  r := !r + 1


let alt_col_name = "served"

let output_col_hd () =
  Datafile.write_alt_colnames stdout alt_col_name
    ["on_fhat"; "on_dhat"; "on_f"; "delayed";]


let output_row () =
  Datafile.write_alt_row_prefix stdout alt_col_name;
  Verb.pr Verb.always "%i\t%i\t%i\t%i\n" !on_fhat !on_dhat !on_f !delayed


let output_geometric_sched ?(duration = 2) output =
  output_col_hd ();
  let i = ref 0
  and next = ref duration in
    (fun force ->
       if !i >= !next || force
       then (i := !i + 1;
	     next := (!next * 15) / 10;
	     output ())
       else i := !i + 1)


let set_open n i = n.ints.open_pos <- i
and set_clean n i = n.ints.clean_pos <- i
and set_geqe n ge = n.geqe <- ge
and get_open n = n.ints.open_pos
and get_clean n = n.ints.clean_pos
and get_geqe n = n.geqe


(************** Search functions **************)
let make_initial initial hd =
  let np = Dpq.no_position in
  let h, d = neg_infinity, neg_infinity in
  let fp = { h = h; d = d; h_err = 0.; d_err = 0.; g = 0.; f = h;
	     est_f = h; est_d = d;}
  and iv = {clean_pos = np; open_pos = np; depth = 0;} in
  let rec n = { data = initial;
		geqe = Double_geq.make_dummy_entry ();
		ints = iv;
		fp = fp;
		preferred = false;} in n


let epsilon = 0.0001

let make_expand expand hd =
  let init = Double_geq.make_dummy_entry() in
  let no_pos = Dpq.no_position in
  let expand n =
    let nfp = n.fp in
    let nd = n.ints.depth + 1
    and pf = nfp.f -. nfp.h_err
    and pd = nfp.d -. 1. -. nfp.d_err in
    let fnd = float nd in
    List.map (fun (s, g) ->
      let h,d = hd s in
      let f = g +. h in
      let h_err = f -. pf (* becomes f -. nfp.f +. nfp.h_err *)
      and d_err = d -. pd (* becomes d -. n.d +. 1. +. nfp.d_err *) in
      let h_err = if Math.finite_p h_err then h_err else n.fp.h_err
      and d_err = if Math.finite_p d_err then d_err else n.fp.d_err in
      let dstep = d_err /. fnd in
      let est_d = Math.fmax d (if dstep >= 1. then d /. epsilon
	                       else d /. (1. -. dstep)) in
      let preferred = nfp.est_d > est_d in
      let est_h = h +. (Math.fmax 0. ((h_err /. fnd) *. est_d)) in
      let est_f = g +. est_h in
      let fp = { h = h;
		 d = d;
		 h_err = h_err;
		 d_err = d_err;
		 g = g;
		 f = f;
		 est_f = est_f; est_d = est_d;}
      and ints = { clean_pos = no_pos; open_pos = no_pos;
		   depth = nd;} in
      { data = s; geqe = init; ints = ints; fp = fp; preferred = preferred})
      (expand n.data nfp.g) in expand


let make_expand_pathmax expand hd =
  let init = Double_geq.make_dummy_entry() in
  let no_pos = Dpq.no_position in
  let expand n =
    let nfp = n.fp in
    let nd = n.ints.depth + 1
    and pf = nfp.f -. nfp.h_err
    and pd = nfp.d -. 1. -. nfp.d_err in
    let fnd = float nd in
    List.map (fun (s, g) ->
      let h,d = hd s
      and t_cost = g -. n.fp.g in
      let preferred = nfp.h > h in
      let h = Math.fmax h (n.fp.h -. t_cost) in
      let f = g +. h in
      let h_err = f -. pf (* becomes f -. nfp.f +. nfp.h_err *)
      and d_err = d -. pd (* becomes d -. n.d +. 1. +. nfp.d_err *) in
      let h_err = if Math.finite_p h_err then h_err else n.fp.h_err
      and d_err = if Math.finite_p d_err then d_err else n.fp.d_err in
      let dstep = d_err /. fnd in
      let est_d = Math.fmax d (if dstep >= 1. then d /. epsilon
	                       else d /. (1. -. dstep)) in
      let est_h = h +. (Math.fmax 0. ((h_err /. fnd) *. est_d)) in
      let est_f = g +. est_h in
      let fp = { h = h; d = d; h_err = h_err; d_err = d_err; g = g; f = f;
		 est_f = est_f; est_d = est_d;}
      and ints = { clean_pos = no_pos; open_pos = no_pos;
		   depth = nd;} in
      { data = s; geqe = init; ints = ints; fp = fp; preferred = preferred})
      (expand n.data nfp.g) in expand


(*********** The search algorithm itself **************)
let search i key hash equals goal expand initial b =
  let bound = ref b in
  let max_guess = max (truncate initial.fp.d) 100 in
  let openlist = ref (Double_geq.create_with open_sort focal_sort (make_close_enough !bound)
			set_open get_open (fun n -> n.preferred)
			initial)
  and clean = ref (Dpq.create clean_sort set_clean max_guess initial)
  and closed = Htable.create hash equals max_guess in
  let remove n =
    assert(n.ints.clean_pos >= 0);
    Dpq.remove !clean n.ints.clean_pos;
    Double_geq.remove !openlist n.geqe;
    set_open n Dpq.no_position;
    set_clean n Dpq.no_position in

  let new_geq () =
    let best_f = Dpq.peek_first !clean in
    let wf = best_f.fp.f *. !bound in
    let nclean = Dpq.create clean_sort set_clean max_guess initial in
    Dpq.iter (fun node ->
		if node.fp.f <= wf
		then (Dpq.insert nclean node)
		else (Double_geq.remove !openlist node.geqe;
		      set_open node Dpq.no_position;
		      set_clean node Dpq.no_position)) !clean;
    Double_geq.update_close_enough !openlist (make_close_enough !bound);
    clean := nclean  in

  let insert n =
    Limit.incr_gen i;
    if goal n
    then (Limit.new_incumbent i (Limit.Incumbent (0., n));
	  let best_f = Dpq.peek_first !clean in
	  let bound' = Math.fmin !bound (n.fp.g /. best_f.fp.f) in
	    if !bound <> bound'
	    then (bound := bound';
		  Verb.pe Verb.always "New Bound: %f\n%!" !bound;
		  new_geq()))
    else (if not (Limit.promising_p i n)
	  then Limit.incr_prune i
	  else (let state = key n in
		  if (Htable.mem closed state)
		  then (let prev = Htable.find closed state in
			  Limit.incr_dups i;
			  if prev.fp.f > n.fp.f
			  then (Htable.replace closed state n;
				if (prev.ints.clean_pos <> Dpq.no_position)
				then remove prev;
				Dpq.insert !clean n;
				let ge = Double_geq.insert !openlist n in
				  set_geqe n ge ))
		  else (Dpq.insert !clean n;
			let ge = Double_geq.insert !openlist n in
			  set_geqe n ge))) in

  let select_and_expand () =
    Limit.incr_exp i;
    let best_f = Dpq.peek_first !clean
    and best_fh = Double_geq.peek_doset !openlist
    and best_d = Double_geq.peek_best !openlist in
    let wf = best_f.fp.f *. !bound in
    if best_d.fp.est_f <= wf
    then best_d
    else if best_fh.fp.est_f <= wf then best_fh
    else best_f in

  let rec do_loop () =
    if not (Limit.halt_p i) && ((Double_geq.count !openlist) > 0)
    then (let next = select_and_expand () in
	  let children = expand next in
	    remove next;
	    List.iter insert children;
	    Limit.curr_q i (Double_geq.count !openlist);
	    do_loop ()) in
  Dpq.insert !clean initial;
  set_geqe initial (Double_geq.insert !openlist initial);
  Htable.add closed (key initial) initial;
  do_loop ();
  i

(**** The interface code that calls the search algorithm *****)
let dups sface args =
  let module SI = Search_interface in
  let key = wrap sface.SI.key
  and hash = sface.SI.hash
  and equals = sface.SI.equals
  and goal = wrap sface.SI.goal_p
  and hd = sface.Search_interface.hd in
  let initial = make_initial sface.SI.initial hd
  and expand = make_expand sface.SI.domain_expand hd in
  let bound = infinity in
  let i = (Limit.make Limit.Nothing sface.SI.halt_on better_p
	     (Limit.make_default_logger (fun n -> n.fp.g) (fun n -> n.ints.depth))) in
  reset();
  let i = search i key hash equals goal expand initial bound in
  Limit.unwrap_sol6 unwrap_sol (Limit.results6 i)

let dups_pm sface args =
  let module SI = Search_interface in
  let key = wrap sface.SI.key
  and hash = sface.SI.hash
  and equals = sface.SI.equals
  and goal = wrap sface.SI.goal_p
  and hd = sface.Search_interface.hd in
  let initial = make_initial sface.SI.initial hd
  and expand = make_expand_pathmax sface.SI.domain_expand hd in
  let bound = infinity in
  let i = (Limit.make Limit.Nothing sface.SI.halt_on better_p
	     (Limit.make_default_logger (fun n -> n.fp.g) (fun n -> n.ints.depth))) in
  reset();
  let i = search i key hash equals goal expand initial bound in
  Limit.unwrap_sol6 unwrap_sol (Limit.results6 i)
