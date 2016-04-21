(**

    @author jtd7
    @since 2012-02-10
*)

type 'a node = {
  f : float;
  g : float;
  d : float;
  mutable q_pos : int;
  mutable  geqe : 'a node Geq.entry;
  data : 'a;
}

let just_f a b =
  (** quality ordering predicate.  distinctions between nodes with the same
    f are made by the convenience ordering predicate. *)
  (*a.f <= b.f*)
  let dif = b.f -. a.f in
    Math.is_zero dif || Math.is_positive dif

let d_then_f_then_g a b =
  (** convenience ordering predicate *)
  (a.d < b.d) ||
  ((a.d = b.d) && ((a.f < b.f) ||
		   ((a.f = b.f) && (a.g >= b.g))))


let d_sort b a =
  if a.d < b.d
  then -1
  else if b.d < b.d
  then 1
  else if (a.f < b.f)
  then -1
  else if (b.f < a.f)
  then 1
  else 0


let make_close_enough_p weight =
  (** builds the close enough predicate required by the geq *)
  assert (weight >= 1.);
  (fun a b ->
     (** is b good enough compared to the benchmark a? *)
     b.f <= (a.f *. weight))


let make_root initial hd =
  (** builds the root of the search tree *)
  let h,d = hd initial in
  { f = h;
    g = 0.;
    d = d;
    q_pos = Dpq.no_position;
    geqe = Geq.make_dummy_entry();
    data = initial; }


let unwrap n =
  (** takes the solution found by the search and returns it in a format that
      the domain expects *)
  match n with
      Limit.Incumbent (q,n) -> Some (n.data, n.g)
    | _ -> None


let wrap fn =
  (** wraps the function [fn] which is made to operate on domain objects so
      that it can be applied to search objects *)
  (fun n -> fn n.data)


let make_expand expand hd =
  (fun n ->
     List.map (fun (n, g) ->
		 let h, d = hd n in
		   { f = g +. h;
		     g = g;
		     d = d;
		     q_pos = Dpq.no_position;
		     geqe = Geq.make_dummy_entry();
		     data = n; })
       (expand n.data n.g))


let search i key hash equals goal root expand bound persevere =
  let max_guess = truncate root.d in
  let good_enough = make_close_enough_p bound in
  let openlist = Geq.create_with just_f d_then_f_then_g good_enough
    (fun n i -> n.q_pos <- i) (fun n -> n.q_pos) root in
  let closed = Htable.create hash equals max_guess in

  let update_closed n =
    let state = key n in
      try (let prev = Htable.find closed state in
	     Limit.incr_dups i;
	     if prev.f > n.f
	     then (if prev.q_pos <> Dpq.no_position
		   then Geq.remove openlist prev.geqe;
		   Htable.replace closed state n;
		   true)
	     else false)
      with Not_found -> (Htable.add closed state n;
			 true) in

  let add_node n =
    Limit.incr_gen i;
    if not (Limit.promising_p i n) then Limit.incr_prune i
    else if update_closed n
    then n.geqe <- Geq.insert openlist n in


  let pursue_children n =
    if goal n
    then (Limit.new_incumbent i (Limit.Incumbent (bound, n));
	  None)
    else
    match (List.sort d_sort (expand n)) with
      | [] -> None
      | lst -> (List.fold_left
		  (fun accum child ->
		     match accum with
		       | None -> (if update_closed child
				  then Some child
				  else None)
		       | Some _ -> (add_node child;
				    accum)) None lst) in

  let rec pursue n =
    if (Limit.halt_p i) then ()
    else if Geq.empty_p openlist
      then (match pursue_children n with
	      | None -> ()
	      | Some c -> pursue c)
      else (let best_f = Geq.peek_doset openlist in
	      if best_f.f *. bound >= n.f
	      then (if persevere bound openlist n
		    then (let children = expand best_f in
			    Geq.remove openlist best_f.geqe;
			    best_f.q_pos <- Dpq.no_position;
			    List.iter add_node children;
			    pursue n)
		    else add_node n)
	      else (match pursue_children n with
		      | None -> ()
		      | Some c -> pursue c)) in


  let rec do_step () =
    if (not (Geq.empty_p openlist)) && (not (Limit.halt_p i)) &&
      (i.Limit.incumbent = Limit.Nothing)
    then (let n = Geq.remove_best openlist in
	    n.q_pos <- Dpq.no_position;
	    if goal n
	    then Limit.new_incumbent i (Limit.Incumbent (bound, n))
	    else (pursue n;
		  do_step())) in

    add_node root;
    do_step()


let aseps_persevere _ _ _ = false

let make_close_persevere percent root =
  assert(percent < 1.);
  assert(percent > 0.);
  let ced = root.d *. percent in
    (fun _ _ n -> n.d <= ced)


let make_f_increase_persevere threshold =
  assert (threshold >= 0.);
  (fun _ openlist n ->
     let best_f = Geq.peek_doset openlist in
       (n.f /. best_f.f) <= threshold)


let dups_close sface args =
  let module SI = Search_interface in
  let key = wrap sface.SI.key
  and hash = sface.SI.hash
  and equals = sface.SI.equals
  and goal = wrap sface.SI.goal_p
  and hd = sface.Search_interface.hd in
  let initial = make_root sface.SI.initial hd
  and expand = make_expand sface.SI.domain_expand hd in
  let bound = Search_args.get_float "Tqs_rewrite.dups" args 0 in
  let i = (Limit.make Limit.Nothing sface.SI.halt_on just_f
	     (Limit.make_default_logger (fun n -> n.g) (fun _ -> -1))) in
    search i key hash equals goal initial expand bound
      (make_close_persevere 0.1 initial);
    Limit.unwrap_sol6 unwrap (Limit.results6 i)


let dups_finc sface args =
  let module SI = Search_interface in
  let key = wrap sface.SI.key
  and hash = sface.SI.hash
  and equals = sface.SI.equals
  and goal = wrap sface.SI.goal_p
  and hd = sface.Search_interface.hd in
  let initial = make_root sface.SI.initial hd
  and expand = make_expand sface.SI.domain_expand hd in
  let bound = Search_args.get_float "Tqs_rewrite.dups" args 0 in
  let i = (Limit.make Limit.Nothing sface.SI.halt_on just_f
	     (Limit.make_default_logger (fun n -> n.g) (fun _ -> -1))) in
    search i key hash equals goal initial expand bound
      (make_f_increase_persevere (bound *. 1.1));
    Limit.unwrap_sol6 unwrap (Limit.results6 i)
