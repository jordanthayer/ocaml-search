(**

    @author jtd7
    @since 2011-10-18
   IDA* on our graph stuff. No inplace modification,
   probably going to use child sorting.
*)


type 'a node = {
  data : 'a;
  g : float;
  f : float;
}

let wrap f = (fun n -> f n.data)

let unwrap_sol s =
  (** Unwraps a solution which is in the form of a search node and presents
      it in the format the domain expects it, which is domain data followed
      by cost *)
  match s with
      Limit.Nothing -> None
    | Limit.Incumbent (q,n) -> Some (n.data, n.g)

let better_p a b =
  (** Sorts nodes solely on total cost information *)
  a.g <= b.g

let make_expand i expand h =
  (** Takes the domain expand function and a heuristic calculator
      and creates an expand function which returns search nodes. *)
  (fun n ->
     List.map (fun (d, g) ->
		 Limit.incr_gen i;
		 { data = d;
		   g = g;
		   f = g +. h d;}) (expand n.data n.g))


let do_iteration goal i expand bound root =
  let next_bound = ref infinity
  and exp = ref 0 in
  let rec help node =
    if node.f > bound || !next_bound < 0.
    then next_bound := min !next_bound node.f
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
    help root.f


let dups sface args =
  (** Performs an A* search from the initial state to a goal,
      for domains with no duplicates. *)
  let module SF = Search_interface in
  let goal = wrap sface.SF.goal_p
  and log = (Limit.make_default_logger (fun n -> n.g)
	       (wrap sface.SF.get_sol_length))
  and root = { data = sface.SF.initial;
	       g = 0.;
	       f = sface.SF.h sface.SF.initial} in
  let i = Limit.make Limit.Nothing sface.SF.halt_on better_p log in
  let expand = make_expand i sface.SF.domain_expand sface.SF.h in
    do_search goal i expand root;
    Limit.unwrap_sol6 unwrap_sol (Limit.results6 i)
