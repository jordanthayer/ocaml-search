(**

    @author jtd7, eaburns
    @since 2010-06-24
*)

type 'a node = {
  data : 'a;
  g : float;
  mutable f : float;
  mutable pos : int;
}

type 'a search_node =
  | Forward of 'a node
  | Backward of 'a node

let dv = Verb.never

let get_base snode =
  match snode with
    | Forward n -> n
    | Backward n -> n


let better_p a b = a.g < b.g
and w_better_p a b = (get_base a).g < (get_base b).g
and sorted a b =
  let a = (get_base a)
  and b = (get_base b) in a.f < b.f || (a.f = b.f && a.g >= b.g)
and set_pos a i = (get_base a).pos <- i
and get_pos a = a.pos


let update key forward backward q n =
  let state = key (get_base n) in
    try
      let other =
	(match n with
	   | Forward m -> Htable.find backward state
	   | Backward m -> Htable.find forward state) in
	other.f <- other.g +. (get_base n).g;
	(if other.pos <> Dpq.no_position then Dpq.see_update q (other.pos))
    with Not_found -> ()


let rec fix_sol gp up fs bs =
  let bs' = gp bs in
    up bs fs;
    if bs != bs'
    then fix_sol gp up bs bs'
    else bs


let unwrap_sol forward backward get_parent update_parent key s =
  (** Unwraps a solution which is in the form of a search node and presents
      it in the format the domain expects it, which is domain data followed
      by cost *)
  let fix_sol = fix_sol get_parent update_parent in
  match s with
      Limit.Nothing -> None
    | Limit.Incumbent (q,n) ->
	(let m = get_base n in
	 let state = key m in
	 let goal_data =
	   (match n with
	      | Forward s -> fix_sol s.data (Htable.find backward state).data
	      | Backward s -> fix_sol (Htable.find forward state).data s.data)
	 in
	   Some (goal_data, m.f))



let make_structures hash equals def_size dummy =
  (** return open, forward closed list, backward closed list *)
  (Dpq.create sorted set_pos def_size dummy,
   Htable.create hash equals def_size,
   Htable.create hash equals def_size)


let best_first_goal kp forward backward key =
  function
       | Forward m -> (Verb.pe dv "Forward goal";
		       kp m;
		       let state = key m in
			 try (let s = Htable.find backward state in
				(get_pos s) = Dpq.no_position)
			 with Not_found -> false)
       | Backward m -> (Verb.pe dv "Backward goal";
		       kp m;
		       let state = key m in
			 try (let s = Htable.find forward state in
				(get_pos s) = Dpq.no_position)
			 with Not_found -> false)


let anytime_goal forward backward key n =
  let state = key (get_base n) in
      (Htable.mem forward state &&
	 Htable.mem backward state)


let make_expand kp fexp bexp fh bh forward backward key =
  let fh = (fun d ->
	      try let n = (Htable.find backward (key d)) in
		if n.pos = Dpq.no_position then n.g else fh d
	      with Not_found -> fh d)
  and bh = (fun d ->
	      try let n = (Htable.find forward (key d)) in
		if n.pos = Dpq.no_position then n.g else bh d
	      with Not_found -> bh d) in
  let fexps = ref 0
  and bexps = ref 0 in
  let f_expand n =
    Verb.pe dv "Expanding ";
    kp n;
    fexps := !fexps + 1;
    (List.map (fun (d, g) ->
		 Forward { data = d;
			   f = g +. (fh d);
			   g = g;
			   pos = Dpq.no_position; }) (fexp n.data n.g))
  and b_expand n =
    Verb.pe dv "Expanding ";
    kp n;
    bexps := !bexps + 1;
    (List.map (fun (d, g) ->
		 Backward { data = d;
			    f = g +. (bh d);
			    g = g;
			    pos = Dpq.no_position; }) (bexp n.data n.g)) in
    (fun n ->
       if ((!fexps + !bexps) mod 1000) = 0 then
       Verb.pe dv "%i fexp\t%i bexp\n%!" !fexps !bexps;
       match n with
	 | Forward n -> f_expand n
	 | Backward n -> b_expand n)


let search_dups i q forward backward key expand goal =

  let consider_child mine theirs wrap n =
    let state = key n in
      try
	(let prev = Htable.find mine state in
	   Limit.incr_dups i;
	   if (better_p n prev)
	   then (Htable.replace mine state n;
(*		 (n.f <- prev.f -. (prev.g -. n.g));*)
		 let pos = get_pos prev in
		   if pos = Dpq.no_position
		   then Dpq.insert q (wrap n)
		   else Dpq.swap q pos (wrap n)))
      with Not_found ->
	(Dpq.insert q (wrap n);
	 Htable.add mine state n) in

  let consider_child n =
    Limit.incr_gen i;
    if not (Limit.promising_p i n)
    then Limit.incr_prune i
    else (match n with
	    | Forward m -> (consider_child forward backward
			      (fun n -> Forward n) m)
	    | Backward m -> (consider_child backward forward
			       (fun n -> Backward n) m)) in

  let rec do_step () =
    if (not (Dpq.empty_p q)) && (not (Limit.halt_p i))
    then
      (let n = Dpq.extract_first q in
	 set_pos n Dpq.no_position;
	 update key forward backward q n;
	 if not (Limit.promising_p i n)
	 then (Limit.incr_prune i;
	       do_step ())
	 else (if goal n
	       then Limit.new_incumbent i (Limit.Incumbent (1., n))
	       else (let children = expand n in
		       Limit.incr_exp i;
		       List.iter consider_child children;
		       Limit.curr_q i (Dpq.count q);
		       do_step ()))) in
    do_step ();
    Limit.results6 i


let call_search kp fexp bexp fh bh start goal hash equals key limit gp up =
  let dummy = { data = start;
		g = nan;
		f = nan;
		pos = Dpq.no_position } in
  let okey = key in
  let q,forward,backward = make_structures hash equals 100 (Forward dummy)
  and ifwd = { data = start; g = 0.; f = fh start; pos = Dpq.no_position}
  and ibwd = { data = goal; g = 0.; f = bh goal; pos = Dpq.no_position}
  and key = (fun n -> key n.data)
  and i = (Limit.make Limit.Nothing limit w_better_p
	     (Limit.make_default_logger (fun n -> (get_base n).f)
		(fun _ -> -1))) in
  let expand = make_expand kp fexp bexp fh bh forward backward okey
  and goal = best_first_goal kp forward backward key in
    Dpq.insert q (Forward ifwd);
    Dpq.insert q (Backward ibwd);
    Htable.add forward (key ifwd) ifwd;
    Htable.add backward (key ibwd) ibwd;
    Verb.pe dv "Starting Search\n%!";
    Limit.unwrap_sol6 (unwrap_sol forward backward gp up key)
    (search_dups i q forward backward key expand goal)


let call_search_from_iface goal gp sface args =
  let kp = (fun n ->
	      Verb.pe dv "%s\n%!" (sface.Search_interface.key_printer
				     (sface.Search_interface.key n.data))) in
    Search_args.is_empty "Single_queue_bidirectional" args;
      (call_search
	 kp
	 sface.Search_interface.domain_expand
	 sface.Search_interface.domain_expand
	 sface.Search_interface.h
	 sface.Search_interface.rev_h
	 sface.Search_interface.initial
	 goal
	 sface.Search_interface.hash
	 sface.Search_interface.equals
	 sface.Search_interface.key
	 sface.Search_interface.halt_on
	 gp
	 sface.Search_interface.parent_update
      )

(* EOF *)
