(**

    @author jtd7
    @since 2011-10-18
*)
type vals = {
  g : float;
  d : float;
  h : float;
  f : float;
  depth : float;
}

type 'a node = {
  data : 'a;
  values : vals;
}

let wrap f = (fun n -> f n.data)

let unwrap_sol s =
  (** Unwraps a solution which is in the form of a search node and presents
      it in the format the domain expects it, which is domain data followed
      by cost *)
  match s with
      Limit.Nothing -> None
    | Limit.Incumbent (q,n) -> Some (n.data, n.values.g)

let better_p a b =
  (** Sorts nodes solely on total cost information *)
  a.values.g <= b.values.g

let compute_fhat root node =
  let rvals = root.values in
  let nvals = node.values in
  let derr = (nvals.d +. nvals.depth -. rvals.d) /. nvals.depth
  and herr = (nvals.f -. rvals.f) /. nvals.depth in
  let d' = Math.fmax nvals.d (nvals.d /. (1. -. derr)) in
    nvals.f +. (Math.fmax 0. (herr *. d'))


let make_expand i expand hd =
  (** Takes the domain expand function and a heuristic calculator
      and creates an expand function which returns search nodes. *)
  (fun n ->
     let depth' = n.values.depth +. 1. in
     List.map (fun (data, g) ->
		 Limit.incr_gen i;
		 let h,d = hd data in
		 let values = { g = g; d = d; h = h; depth = depth';
				f = g +. h;} in
		 { data = data;
		   values = values;}) (expand n.data n.values.g))


let do_iteration goal i expand bound root =
  let next_bound = ref infinity in
  let exp = ref 0 in
  let rec help node =
    if node.values.f > bound || !next_bound < 0.
    then next_bound := min !next_bound (compute_fhat root node)
    else (if goal node
	  then (next_bound := -1.;
		Limit.new_incumbent i (Limit.Incumbent (0.,node)))
	  else (Limit.incr_exp i;
		exp := !exp + 1;
		List.iter help (expand node))) in
    next_bound := infinity;
    Verb.pe Verb.always "Bound %f: %!" bound;
    help root;
    Verb.pe Verb.always "%i expansions\n%!" !exp;
    !next_bound


let do_search goal i expand root =
  let rec help current_bound =
    let next_bound = do_iteration goal i expand current_bound root in
      if next_bound > 0.
      then help next_bound in
    help (root.values.g +. root.values.h)


let dups sface args =
  (** Performs an A* search from the initial state to a goal,
      for domains with no duplicates. *)
  let module SF = Search_interface in
  let goal = wrap sface.SF.goal_p
  and log = (Limit.make_default_logger (fun n -> n.values.g)
	       (wrap sface.SF.get_sol_length)) in
  let h,d = sface.SF.hd sface.SF.initial in
  let root = { data = sface.SF.initial;
	       values = { g = 0.; h = h; d = d; depth = 1.;
			  f =  h;}; } in
  let i = Limit.make Limit.Nothing sface.SF.halt_on better_p log in
  let expand = make_expand i sface.SF.domain_expand sface.SF.hd in
    do_search goal i expand root;
    Limit.unwrap_sol6 unwrap_sol (Limit.results6 i)


