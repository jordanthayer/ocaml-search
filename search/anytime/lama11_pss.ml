(**

   @author jordan @since 2011-12-14

   Supposed to mimic the behavior of the newest lama in domain
   independent planning This is a pure search strategy implementation
   of lama. It ignores things like several heuristics and so on,
   instead just using a single heuristic and mulitple iterated
   searches.  It's a way to measure how much the additional stuff is
   helping us in the domain specific planners.
*)

let base_list = Restarts_wastar.richterl2

type fp_values = {
  mutable fp : float;
  f : float;
  g : float;
  h : float;
}

type 'a node = {
  fpv : fp_values;
  data: 'a;
  mutable open_pos : int;
}


let closed_pos = -17
let prev_it = -42


let update n wt' =
  let nfp = n.fpv in
  nfp.fp <- wt' *. nfp.h +. nfp.g


let make_expand expand h =
  (fun wt parent ->
    List.map (fun (n,g) ->
      let h = h n in
      let cfp = { fp = g +. wt *. h;
		  f = g +. h;
		  g = g;
		  h = h; } in
      { fpv = cfp;
	data = n;
	open_pos = Dpq.no_position; })
      (expand parent.data parent.fpv.g))


let make_speedy_expand expand hd =
  (fun parent -> List.map (fun (n,g) ->
    let h,fp = hd n in
    let cfp = { fp = fp;
		f = g +. h;
		g = g;
		h = h; } in
    { fpv = cfp;
      data = n;
      open_pos = Dpq.no_position; })
    (expand parent.data parent.fpv.g))


let make_greedy_expand expand h =
  (fun parent -> List.map (fun (n,g) ->
    let h  = h n in
    let cfp = { fp = h;
		f = g +. h;
		g = g;
		h = h; } in
    { fpv = cfp;
      data = n;
      open_pos = Dpq.no_position; })
    (expand parent.data parent.fpv.g))



let better_p a b =
  (** Determines if [a] represents a better solution than [b] *)
  a.fpv.f <= b.fpv.f


let ordered_p a b =
  let a = a.fpv
  and b = b.fpv in
  (** are [a] and [b] in order of increase wf? *)
  (a.fp < b.fp) ||
  ((a.fp = b.fp) &&
   ((a.f < b.f) ||
    ((a.f = b.f) &&
     (a.g >= b.g))))


let f_then_g a b =
  (** are [a] and [b] in f order? *)
  let a = a.fpv
  and b = b.fpv in
  a.f <= b.f ||
    a.f = b.f &&
      a.g >= b.g


let make_root initial h wt =
  (** takes a root in the domain and returns the root of the search space *)
  let h = h initial in
  let fpv = { fp = wt *. h ;
	      f = h;
	      g = 0.;
	      h = h; } in
  { fpv = fpv;
    data = initial;
    open_pos = Dpq.no_position; }


let wrap_incumbent i =
  (** takes an incumbent solution and returns a limit.info *)
  match i with
    None -> Limit.Nothing
  | Some (n, g) -> Limit.Incumbent (0.,
				    {fpv = { fp = g;
					     f = g;
					     g = g;
					     h = 0.; };
				      data = n;
				      open_pos = Dpq.no_position;})

let unwrap_sol s =
  (** takes the solution found by the search and returns something that the
      domains can easily operate on *)
  match s with
    Limit.Incumbent (q,n) -> Some (n.data, n.fpv.g)
    | _ -> None


let search key hash equals i root wts goal_p expand max_it nodes =
  (** [ordered_p] is true if in order or equal, [better_p] is true if
      superior or equal (eg, for comparing duplicates), [key] gives
      state data for detecting duplicates, [setpos] and [getpos] are
      for efficient swapping into the openlist.  This function is
      intended to be a building block for others - it expects an
      [expand] that takes the openlist as the first argument (in case
      [expand] wants to mess with it). *)
  let openlist = Dpq.create ordered_p (fun n i -> n.open_pos <- i) 100 root
  and iteration = ref 0 in

  let consider_child n =
    Limit.incr_gen i;
    if not (Limit.promising_p i n)
    then Limit.incr_prune i
    else
      let state = key n in
      try
	let prev = Htable.find nodes state in
	Limit.incr_dups i;
	let opos = prev.open_pos in
	if not (better_p prev n)
	then (Htable.replace nodes state n;
	      if opos >= 0
	      then Dpq.swap openlist opos n
	      else Dpq.insert openlist n)
	else (if opos == prev_it
	      then (if better_p prev n
	            then (update prev (List.nth wts !iteration);
			  Dpq.insert openlist prev)
		    else (Htable.replace nodes state n;
			  Dpq.insert openlist n)))
      with Not_found ->
	  (* new state *)
	Htable.add nodes state n;
	Dpq.insert openlist n in

  let rec expand_best () =
    let empty = Dpq.empty_p openlist
    and halt = Limit.halt_p i in
    if (not empty) && (not halt)
    then (let n = Dpq.extract_first openlist in
	  n.open_pos <- closed_pos;
	  if not (Limit.promising_p i n)
	  then (Limit.incr_prune i;
		Htable.remove nodes (key n);
		expand_best ())
	  else if goal_p n
	  then (match i.Limit.incumbent with
	    | Limit.Nothing ->
	      (Limit.new_incumbent i (Limit.Incumbent (0.,n));
	       Dpq.clear openlist;
	       Htable.iter (fun _ n -> n.open_pos <- prev_it;) nodes;
	       Dpq.insert openlist root;
	       iteration := !iteration + 1;
	       if !iteration < max_it
	       then expand_best())
	    | Limit.Incumbent (qual,node) ->
	      Limit.new_incumbent i (Limit.Incumbent (0.,n));
	      Dpq.clear openlist;
	      Htable.iter (fun _ n -> n.open_pos <- prev_it;) nodes;
	      Dpq.insert openlist root;
	   (* new itteration only begins on truly new incumbent*)
	      if better_p n node
	      then iteration := !iteration + 1;
	      if !iteration < max_it
	      then expand_best())
	  else (Limit.incr_exp i;
		let children = expand (List.nth wts !iteration) n in
		List.iter consider_child children;
		Limit.curr_q i (Dpq.count openlist);
		expand_best ()))
  in
  Htable.add nodes (key root) root;
  Dpq.insert openlist root;
  expand_best ();
  i


let speedy ?(closed = None) key hash equals i root goal_p expand =
  let openlist = Dpq.create ordered_p (fun n i -> n.open_pos <- i) 100 root
  and nodes = match closed with
    | None -> Htable.create hash equals 100
    | Some cl -> cl in
  let consider_child n =
    Limit.incr_gen i;
    if not (Limit.promising_p i n)
    then Limit.incr_prune i
    else (let state = key n in
	    try
	      let prev = Htable.find nodes state in
		Limit.incr_dups i;
		if not (better_p prev n)
		then (let pos = prev.open_pos in
			if pos >= 0 then
			  (Dpq.swap openlist pos n;
			   Htable.replace nodes state n))
	    with Not_found ->
	      (* new state *)
	      Dpq.insert openlist n;
	      Htable.add nodes state n) in

  let rec expand_best () =
    if (not (Dpq.empty_p openlist)) && (not (Limit.halt_p i)) then
      (let n = Dpq.extract_first openlist in
	 if not (Limit.promising_p i n) then
	   (Limit.incr_prune i;
	    (* any future duplicate will be pruned by bound *)
	    Htable.remove nodes (key n);
	    expand_best ())
	 else if goal_p n then
	   Limit.new_incumbent i (Limit.Incumbent (0., n))
	 else
	   (n.open_pos <- Dpq.no_position;
	    Limit.incr_exp i;
	    List.iter consider_child (expand n);
	    Limit.curr_q i (Dpq.count openlist);
	    expand_best ()))
  in
    Dpq.insert openlist root;
    expand_best ();
    Htable.iter (fun _ n -> n.open_pos <- prev_it) nodes;
    nodes


let wrap fn n =
  fn n.data


let dups ?(uc = true) wt_list sface _ =
  let module SI = Search_interface in
  let key = wrap sface.SI.key
  and hash = sface.SI.hash
  and equals = sface.SI.equals
  and goal_p = wrap sface.SI.goal_p in
  let max_it = List.length wt_list in
  let h = sface.SI.h in
  let expand = make_expand sface.SI.domain_expand h in
  let speedy_expand = make_speedy_expand sface.SI.domain_expand sface.SI.hd in
  let greedy_expand = make_greedy_expand sface.SI.domain_expand sface.SI.h in
  let root = make_root sface.SI.initial h (List.hd wt_list) in
  let i = (Limit.make Limit.Nothing sface.SI.halt_on better_p
	     (Limit.make_default_logger (fun n -> n.fpv.g) (fun n -> -1))) in
  let closed = speedy key hash equals i root goal_p speedy_expand in
  let closed' = (if uc then closed
		 else speedy ~closed:(Some closed)
		   key hash equals i root goal_p greedy_expand) in
    ignore (search key hash equals i root wt_list goal_p expand max_it closed');
    Limit.unwrap_sol6 unwrap_sol (Limit.results6 i)


let wt_list1 = Restarts_wastar.richterl1
and wt_list2 = Restarts_wastar.richterl2

let dups_rl1 sface args = dups ~uc:false wt_list1 sface args
let dups_rl2 sface args = dups ~uc:false wt_list2 sface args
let dups_rl1_uc sface args = dups wt_list1 sface args
let dups_rl2_uc sface args = dups wt_list2 sface args



(* EOF *)
