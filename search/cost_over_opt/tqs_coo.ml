(**

    @author jordan
    @since 2011-12-28
   Three Queue Search for Roni's new Constant over optimal problem
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
}

type int_vals = {
mutable  clean_pos : int;
mutable  open_pos : int;
  depth : int;
}

type 'a node = {
  data : 'a;
mutable  geqe : 'a node Geq.entry;
  ints : int_vals;
  fp : fp_values;
}

(**************** Ordering Predicates *********************)

let open_sort a b =
  let afp = a.fp
  and bfp = b.fp in
  let aef = afp.est_f
  and bef = bfp.est_f
  and ag = afp.g
  and bg = bfp.g in
  (aef < bef)  ||  (* sort by fhat *)
    ((aef = bef) &&
	((ag >= bg)
	 ||       (* break ties on low d *)
	   ((ag = bg) &&
	       (afp.d < bfp.d)))) (* break ties on high g *)


let focal_sort a b =
  let afp = a.fp
  and bfp = b.fp in
  let aed = afp.est_d
  and bed = bfp.est_d
  and aef = afp.est_f
  and bef = bfp.est_f in
  (aed < bed) ||
    ((aed = bed) && ((aef < bef) ||
			((aef = bef) && (afp.g >= bfp.g))))


let clean_sort a b =
  let afp = a.fp
  and bfp = b.fp in
  let af = afp.f
  and bf= bfp.f in
  af < bf ||
    (af = bf &&
	afp.g >= bfp.g)


let make_close_enough bound =
  let close_enough a b = (b.fp.est_f <= (a.fp.est_f +. bound)) in
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
		geqe = Geq.make_dummy_entry ();
		ints = iv;
		fp = fp; } in n


let epsilon = 0.0001

let make_expand expand hd =
  let init = Geq.make_dummy_entry() in
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
      let est_h = h +. (Math.fmax 0. ((h_err /. fnd) *. est_d)) in
      let est_f = g +. est_h in
      let fp = { h = h; d = d; h_err = h_err; d_err = d_err; g = g; f = f;
		 est_f = est_f; est_d = est_d;}
      and ints = { clean_pos = no_pos; open_pos = no_pos; depth = nd;} in
      assert (est_d >= 0.);
      assert (est_f >= f);
      { data = s; geqe = init; ints = ints; fp = fp;})
      (expand n.data nfp.g) in expand


let make_expand_pathmax expand hd =
  let init = Geq.make_dummy_entry() in
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
      and ints = { clean_pos = no_pos; open_pos = no_pos; depth = nd;} in
      assert (est_d >= 0.);
      assert (est_f >= f);
      { data = s; geqe = init; ints = ints; fp = fp;})
      (expand n.data nfp.g) in expand



let make_get_node bound f_q geq i =
  let get_node () =
    let best_f = Dpq.peek_first f_q
    and best_fh = Geq.peek_doset geq
    and best_d = Geq.peek_best geq in
    let wf = (best_f.fp.f) +. bound in
    if best_d.fp.est_f  <= wf
    then (incr on_dhat; best_d)
    else (if best_fh.fp.est_f <= wf
      then (incr on_fhat; best_fh)
      else (incr on_f; best_f)) in
  get_node


(*********** The search algorithm itself **************)
let search i key hash equals goal expand initial bound =
  let max_guess = truncate initial.fp.d in
  let openlist = (Geq.create_with open_sort focal_sort (make_close_enough bound)
		    set_open get_open initial)
  and clean = Dpq.create clean_sort set_clean max_guess initial
  and closed = Htable.create hash equals max_guess in
  let get_node = make_get_node bound clean openlist i in

  let insert node state =
    Dpq.insert clean node;
    let ge = Geq.insert openlist node in
    set_geqe node ge;
    Htable.replace closed state node in

  let add_node n =
    Limit.incr_gen i;
    if not (Limit.promising_p i n) then Limit.incr_prune i
    else (let state = key n in
	  try (let prev = Htable.find closed state in
	       Limit.incr_dups i;
	       if prev.fp.f > n.fp.f
	       then (if prev.ints.clean_pos <> Dpq.no_position
		     then (Dpq.remove clean prev.ints.clean_pos;
			   Geq.remove openlist prev.geqe;
		           insert n state)
		     else insert n state))
	  with Not_found -> (* new state *)
	    insert n state) in

  let do_expand n =
    Limit.incr_exp i;
    Geq.remove openlist (get_geqe n);
    Dpq.remove clean (n.ints.clean_pos);
    set_open n Dpq.no_position;
    set_clean n Dpq.no_position;
    if not (Limit.promising_p i n) then (Limit.incr_prune i; [])
    else expand n in

  let rec do_loop () =
    if not (Limit.halt_p i) && ((Geq.count openlist) > 0)
    then (let n = get_node () in
	  if goal n then Limit.new_incumbent i (Limit.Incumbent (bound, n))
	  else (let children = do_expand n in
		List.iter add_node children;
		Limit.curr_q i (Geq.count openlist);
		do_loop())) in

  (* this is the part that actually does the search *)
  Dpq.insert clean initial;
  set_geqe initial (Geq.insert openlist initial);
  Htable.add closed (key initial) initial;
  do_loop ();
  let post_bound =
    (if Dpq.empty_p clean
     then 1.
     else (let bestf = Dpq.peek_first clean in
	   let inc = (match i.Limit.incumbent with
			| Limit.Nothing -> initial
			| Limit.Incumbent (_,n) -> n) in
	     inc.fp.g -. bestf.fp.f)) in
      Datafile.write_pairs stdout ["post_bound", string_of_float post_bound];
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
  let bound = Search_args.get_float "Tqs_rewrite.dups" args 0 in
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
  let bound = Search_args.get_float "Tqs_rewrite.dups" args 0 in
  let i = (Limit.make Limit.Nothing sface.SI.halt_on better_p
	     (Limit.make_default_logger (fun n -> n.fp.g) (fun n -> n.ints.depth))) in
  reset();
  let i = search i key hash equals goal expand initial bound in
  Limit.unwrap_sol6 unwrap_sol (Limit.results6 i)
