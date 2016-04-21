(**

   @author jtd7
   @since 2011-03-11

   A generic beam search
*)


let breadth_first_dups ?(dd = true) info beam_width root goal_p expand
    ordered_p better_p hash equals key setpos getpos =
  let nodes = Htable.create hash equals 100 in
  let rev_order a b = not (ordered_p a b) in
  let beam1 = Dpq.create rev_order setpos beam_width root
  and beam2 = Dpq.create rev_order setpos beam_width root in

  let insert beam n =
    let beam_contents = Dpq.count beam in
      if beam_contents >= beam_width
      then ignore (Dpq.extract_first beam);
      Dpq.insert beam n in

  let consider_child beam n =
    Limit.incr_gen info;
    if goal_p n
    then (Limit.new_incumbent info (Limit.Incumbent (0., n));
	  false)
    else if not (Limit.promising_p info n)
    then (Limit.incr_prune info;
	  true)
    else (let state = key n in
	    try
	      let prev = Htable.find nodes state in
		Limit.incr_dups info;
		if not (better_p prev n) && not dd
		then (Htable.replace nodes state n;
		      let pos = getpos prev in
			if pos = Dpq.no_position
			then insert beam n
			else Dpq.swap beam pos n);
		true
	    with Not_found ->
	      Htable.add nodes state n;
	      insert beam n;
	      true) in

  let rec iteration current_beam next_beam =
    Dpq.iter_unsafe
      (fun node ->
	 setpos node Dpq.no_position;
	 Limit.incr_exp info;
	 let continue = (List.fold_left (fun accum n ->
					   accum &&
					     (not (Limit.halt_p info)) &&
					     (consider_child next_beam n)) true
			   (expand node)) in
	   continue) current_beam;
    if (not (Limit.halt_p info)) && (info.Limit.incumbent = Limit.Nothing)
    then (Dpq.clear current_beam;
	  iteration next_beam current_beam) in

    Dpq.insert beam1 root;
    Htable.add nodes (key root) root;
    iteration beam1 beam2


let best_first_dups  ?(dd = false) info beam_width root goal_p expand ordered_p
    better_p hash equals key setpos getpos =
  let nodes = Htable.create hash equals 100 in
  let beam = Mmh.create ~update_function:setpos ordered_p beam_width root in

  let consider_child n =
    Limit.incr_gen info;
    if goal_p n
    then Limit.new_incumbent info (Limit.Incumbent (0.,n))
    else if not (Limit.promising_p info n)
    then Limit.incr_prune info
    else (let state = key n in
	    try
	      let prev = Htable.find nodes state in
		Limit.incr_dups info;
		if not (better_p prev n) && not dd
		then (Htable.replace nodes state n;
		      let pos = getpos prev in
			if pos = Dpq.no_position
			then ignore (Mmh.insert beam n)
			else ignore (Mmh.replace_at beam n pos))
	    with Not_found ->
	      Htable.add nodes state n;
	      ignore (Mmh.insert beam n)) in

  let rec expand_next n =
      Limit.incr_exp info;
      List.iter consider_child (expand n);
      if not (Limit.halt_p info) && (info.Limit.incumbent = Limit.Nothing)
	&& not (Mmh.empty_p beam)
      then expand_next (Mmh.extract_first beam) in

    Htable.add nodes (key root) root;
    expand_next root


(*** Some test code for generic beam ***)

type 'a node = {
  data : 'a;
  g : float;
  f : float;
  mutable pos : int;
}

let make_expand exp h =
  (fun n -> List.map (fun (d,g) ->
			{ data = d;
			  g = g;
			  f = (h d);
			  pos = Dpq.no_position;}) (exp n.data n.g))

let make_f_expand exp h =
  (fun n -> List.map (fun (d,g) ->
			{ data = d;
			  g = g;
			  f = g +. (h d);
			  pos = Dpq.no_position;}) (exp n.data n.g))


let ordered_p a b = a.f < b.f || (a.f = b.f && a.g < b.g)
and better_p a b = a.g < b.g
and setpos n i = n.pos <- i
and getpos n = n.pos
and wrap fn n = fn n.data
and unwrap_sol s =
  match s with
      Limit.Nothing -> None
    | Limit.Incumbent (q,n) -> Some (n.data, n.g)


let breadth_beam_dups sface args =
  let beam_width = Search_args.get_int "Generic_beam" args 0
  and key = wrap sface.Search_interface.key
  and goalp = wrap sface.Search_interface.goal_p
  and info = (Limit.make Limit.Nothing sface.Search_interface.halt_on
		better_p (Limit.make_default_logger (fun n -> n.g)
			    (fun n -> -1)))
  and expand = (make_expand sface.Search_interface.domain_expand
		  sface.Search_interface.h)
  and root = { data = sface.Search_interface.initial;
	       g = 0.; f = 0.; pos = Dpq.no_position; } in
    breadth_first_dups info beam_width root goalp expand ordered_p better_p
      sface.Search_interface.hash sface.Search_interface.equals
      key setpos getpos;
    Limit.unwrap_sol6 unwrap_sol (Limit.results6 info)


let f_beam_dups sface args =
  let beam_width = Search_args.get_int "Generic_beam" args 0
  and key = wrap sface.Search_interface.key
  and goalp = wrap sface.Search_interface.goal_p
  and info = (Limit.make Limit.Nothing sface.Search_interface.halt_on
		better_p (Limit.make_default_logger (fun n -> n.g)
			    (fun n -> -1)))
  and expand = (make_f_expand sface.Search_interface.domain_expand
		  sface.Search_interface.h)
  and root = { data = sface.Search_interface.initial;
	       g = 0.; f = 0.; pos = Dpq.no_position; } in
    breadth_first_dups info beam_width root goalp expand ordered_p better_p
      sface.Search_interface.hash sface.Search_interface.equals
      key setpos getpos;
    Limit.unwrap_sol6 unwrap_sol (Limit.results6 info)


(* EOF *)
