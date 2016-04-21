(**

    @author jtd7
    @since 2011-02-21

   IDA* with a transposition table
*)


type 'a node = {
  data : 'a;
  g : float;
  f : float;
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
      Limit.Nothing -> None
    | Limit.Incumbent (q,n) -> Some (n.data, n.g)


let better_p a b =
  (** Sorts nodes solely on total cost information *)
  a.g <= b.g


let make_expand expand h =
  (** Takes the domain expand function and a heuristic calculator
      and creates an expand function which returns search nodes. *)
  (fun n ->
     List.map (fun (c, g) -> { data = c;
			       g = g;
			       f = g +. (h c)}) (expand n.data n.g))


let make_do_iteration expand info root closed key goal_p bound iteration =
  let rec consider_child c =
    Limit.incr_gen info;
    if Limit.halt_p info
    then neg_infinity
    else
      (if goal_p c
       then (Limit.new_incumbent info (Limit.Incumbent (0., c));
	     c.f)
       else (let state = key c in
	       if Htable.mem closed state
	       then
		 (let (prev,pit) = Htable.find closed state in
		    if prev.f > c.f || (pit <> iteration && prev.f = c.f)
		    then (Htable.replace closed state (c, iteration);
			  if c.f > bound
			  then c.f
			  else (Limit.incr_exp info;
				List.fold_left
				  (fun accum c' -> Math.fmin accum
				     (consider_child c')) infinity (expand c)))
		    else infinity)
	       else
		 (if c.f > bound
		  then c.f
		  else (Limit.incr_exp info;
			Htable.add closed state (c,iteration);
			List.fold_left
			  (fun accum c' -> Math.fmin accum (consider_child c'))
			  infinity (expand c))))) in
  consider_child root


let do_search sface args =
  let h = Search_interface.get_default_fixed sface in
  let root = { data = sface.Search_interface.initial;
	       g = 0.;
	       f = h sface.Search_interface.initial; }
  and goal_p = wrap sface.Search_interface.goal_p
  and key = wrap sface.Search_interface.key
  and eq = sface.Search_interface.equals
  and hash = sface.Search_interface.hash
  and expand = make_expand sface.Search_interface.domain_expand h in
  let closed = Htable.create hash eq 100 in
  let info = (Limit.make Limit.Nothing sface.Search_interface.halt_on better_p
		(Limit.make_default_logger (fun n -> n.f)
		   (wrap (sface.Search_interface.get_sol_length)))) in
  let do_iteration = make_do_iteration expand info root closed key goal_p in
  let bound = ref root.f
  and iteration = ref 0 in
  let check_inc () = (match info.Limit.incumbent with
			| Limit.Nothing -> true
			| Limit.Incumbent (q,inc) -> inc.f > !bound) in
    while not (Limit.halt_p info) && (check_inc ())
    do
      (Verb.pe Verb.toplvl "Iteration %i Bound %f\n%!" !iteration !bound;
	bound := do_iteration !bound !iteration;
	iteration := !iteration + 1)
    done;
    Limit.unwrap_sol6 unwrap_sol (Limit.results6 info)

(* EOF *)
