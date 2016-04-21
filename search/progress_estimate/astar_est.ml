(**
   A* search algorithm that estimates progress
*)

type fp_values = {
  g : float;
  h : float;
  f : float;
  d : float;
}

type int_values = {
  depth : int;
  generated: int;
  mutable pos : int;
}

type 'a node = {
  data : 'a;          (* Data Payload *)
  fpv : fp_values;
  iv : int_values;
}


let wrap f =
  (** takes a function to be applied to the data payload
      such as the goal-test or the domain heuristic and
      wraps it so that it can be applied to the entire
      node *)
  (fun n -> f n.data)


let unwrap_sol s =
  (** Unwraps a solution which is in the form of a search node and presents
      it in the format the domain expects it, which is domain data followed
      by cost *)
  match s with
    | Limit.Nothing -> None
    | Limit.Incumbent (q,n) -> Some (n.data, n.fpv.g)


let f_then_g a b =
  (** expansion ordering predicate, and also works for ordering duplicates
      assuming that h is the same for both
      (hence f will be lower when g is lower). *)
  let afp = a.fpv
  and bfp = b.fpv in
  ((afp.f : float) < bfp.f) ||
    ((afp.f = bfp.f) && (afp.g >= bfp.g))


let just_f a b =
  (** Sorts nodes solely on total cost information *)
  (a.fpv.f : float) <= b.fpv.f


let make_expand i expand hd =
  (** Takes the domain expand function and a heuristic calculator
      and creates an expand function which returns search nodes. *)
  (fun n ->
     let depth' = n.iv.depth + 1 in
       List.map (fun (data, g) ->
		   let h,d = hd data in
		     { data = data;
		       fpv = { f = g +. h; h = h; g = g; d = d; };
		       iv = { depth = depth';
			      generated = i.Limit.expanded;
			      pos = Dpq.no_position;}})
	 (expand n.data n.fpv.g))


let search_dups ?(est = (fun i n oplst child -> nan, nan))
    root goal key i hash equals expand =
  let est = (fun i n oplist child bool -> est i n oplist child) in
  let closed = Htable.create hash equals 100 in
  let openlist = Dpq.create f_then_g (fun n i -> n.iv.pos <- i) 100 root in
  let record,fop,output = (Progress_est.make_output_stream
			     Progress_est.output_row) in

  let consider_child n =
    Limit.incr_gen i;
    if not (Limit.promising_p i n)
    then Limit.incr_prune i
    else (let state = key n in
	    try
	      let prev = Htable.find closed state in
		Limit.incr_dups i;
		if (prev.fpv.f > n.fpv.f)
		then (Htable.replace closed state n;
		      let pos = prev.iv.pos in
			if pos == Dpq.no_position
			then Dpq.insert openlist n
			else Dpq.swap openlist pos n)
	    with Not_found -> (* new state *)
	      Dpq.insert openlist n;
	      Htable.add closed state n) in

  let rec expand_best () =
    if (not (Dpq.empty_p openlist)) && (not (Limit.halt_p i))
    then (let n = Dpq.extract_first openlist in
	    n.iv.pos <- Dpq.no_position;
	    if not (Limit.promising_p i n)
	    then (Limit.incr_prune i;
		  Htable.remove closed (key n);
		  expand_best ())
	    else if goal n
	    then (Limit.new_incumbent i (Limit.Incumbent (0.,n));
		  fop ();
		  Progress_est.output_row
		    i.Limit.expanded n.iv.depth (0.,1.))
	    else (let children = expand n in
		    Limit.incr_exp i;
		    List.iter consider_child children;
		    Limit.curr_q i (Dpq.count openlist);
		    record i.Limit.expanded n.iv.depth
		      (est i n openlist children);
		    expand_best ()))
  in
    Progress_est.output_row
      i.Limit.expanded 0 (root.fpv.d,0.);
    Dpq.insert openlist root;
    expand_best ()


let make_initial root hd =
  let h,d = hd root in
    { data = root;
      fpv = { f = h; h = h; g = 0.; d = d;};
      iv = {depth = 0; generated = -1; pos = Dpq.no_position};}


let dups sface args =
  (** Performs an A* search from the initial state to a goal,
      for domains where duplicates are frequently encountered. *)
  Search_args.is_empty "Astar.dups" args;
  let module SI = Search_interface in
  let key = wrap sface.SI.key
  and hash = sface.SI.hash
  and equals = sface.SI.equals
  and goal = wrap sface.SI.goal_p
  and hd = sface.Search_interface.hd in
  let initial = make_initial sface.SI.initial hd in
  let i = (Limit.make Limit.Nothing sface.SI.halt_on just_f
	     (Limit.make_default_logger (fun n -> n.fpv.g)
		(fun n -> n.iv.depth))) in
  let expand = make_expand i sface.SI.domain_expand hd in

    search_dups initial goal key i hash equals expand;
    Limit.unwrap_sol6 unwrap_sol (Limit.results6 i)


(* EOF *)
