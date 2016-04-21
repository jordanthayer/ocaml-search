(** Iterative tunneling A* as proposed by Furcy *)

(* Search greedily to a goal.
   Iteratively Search the ith neighborhood for improvemed solutions
   Probably represent the neightborhood as a hashtable. *)

type 'a node = {
  g : float;
  cost : float;
  data : 'a;
  mutable q_pos : int;
  parent : 'a node;
}


type ('key,'data) neighborhood = {
  interior : ('data,'key) Htable.t;
  mutable perimeter:  ('data,'key) Htable.t;
}


(*************************************************************************)
let prev_it = -17

let get_parent n = n.parent

let ordered a b = a.cost < b.cost || (a.cost = b.cost && a.g > b.g)

let better_p a b = a.g < b.g

let promising a b = a.g <= b.cost

let wrap fn = (fun n -> fn n.data)

let setpos n i =
  n.q_pos <- i

let getpos n =
  n.q_pos

let unwrap_sol s =
  (** Unwraps a solution which is in the form of a search node and presents
      it in the format the domain expects it, which is domain data followed
      by cost *)
  match s with
      Limit.Nothing -> None
    | Limit.Incumbent (q,n) -> Some (n.data, n.g)


let neighborhood_size nbhd =
  (Htable.length nbhd.interior) + (Htable.length nbhd.perimeter)

let in_neighborhood key nbhd =
  (* is the node [n] in the neighborhood? *)
  (fun n ->
     let v = (key n) in
       (Htable.mem nbhd.interior v) || (Htable.mem nbhd.perimeter v))


let grow_neighborhood key hash equal expand nbhd =
  (* progresses the neightborhood for the next iteration *)
  let next_perimeter = Htable.create hash equal
    (Htable.length nbhd.perimeter) in
    Htable.iter (fun k value ->
		   Htable.replace nbhd.interior k value;
		   List.iter (fun d ->
				Htable.replace next_perimeter (key d) d)
		     (expand value)) nbhd.perimeter;
    nbhd.perimeter <- next_perimeter


let initial_neighborhood key hash equal sol_node =
  (* constructs the initial neighborhood based on the solution [sol_node] *)
  let nbhd = { interior = Htable.create hash equal 100;
	       perimeter = Htable.create hash equal 100;} in
  let rec add_sol n =
    Htable.replace nbhd.perimeter (key n) n.data;
    let n_p = get_parent n in
      if n_p != n
      then add_sol n_p in
    add_sol sol_node;
    nbhd


let greedy_expand expand h =
  (** Takes the domain expand function and a heuristic calculator
      and creates an expand function which returns search nodes. *)
  (fun n ->
     List.map (fun (d, g) -> { data = d;
			       cost = (h d);
			       g = g;
			       q_pos = Dpq.no_position;
			       parent = n;}) (expand n.data n.g))


let make_expand expand h =
  (** Takes the domain expand function and a heuristic calculator
      and creates an expand function which returns search nodes. *)
  (fun n ->
     List.map (fun (d, g) -> { data = d;
			       cost = g +. (h d);
			       g = g;
			       q_pos = Dpq.no_position;
			       parent = n;}) (expand n.data n.g))

let neighborhood_expand expand n =
  List.map (fun (d, _) -> d) (expand n 0.)


let greedy_phase key i exp goal closed openlist initial =
  let consider_child n =
    Limit.incr_gen i;
    let state = key n in
      try
	let prev = Htable.find closed state in
	let pos  = getpos prev in
	  Limit.incr_dups i;
	  if (better_p n prev)
	    (* n is better than previous *)
	  then  (Htable.replace closed state n;
		 if pos <> Dpq.no_position
		 then Dpq.swap openlist pos n)
      with Not_found ->
	Dpq.insert openlist n;
	Htable.add closed state n in

  let rec expand () =
    if not (Limit.halt_p i) && not (Dpq.empty_p openlist)
    then
      (let n = Dpq.extract_first openlist in
	 setpos n Dpq.no_position;
	 if not (Limit.promising_p i n)
	 then (Limit.incr_prune i;
	       expand ())
	 else if goal n
	 then Limit.new_incumbent i (Limit.Incumbent (0.,n))
	 else
	   (List.iter consider_child (exp n);
	    Limit.incr_exp i;
	    Limit.curr_q i (Dpq.count openlist);
	    expand())) in
    Dpq.insert openlist initial;
    expand ()


let search_iteration nbhd sface closed openlist =
  let i = sface.Search_interface.info
  and key = sface.Search_interface.key
  and inadmiss_prune = ref false in
  let in_hood = in_neighborhood key nbhd in
  let consider_child n =
    Limit.incr_gen i;
    if not (Limit.promising_p i n)
    then Limit.incr_prune i
    else
      if in_hood n
      then (let state = key n in
	      try
		let prev = Htable.find closed state in
		let pos  = getpos prev in
		  Limit.incr_dups i;
		  if (better_p n prev) (* prev.g >= n.g *)
		  then (Htable.replace closed state n;
			if pos = Dpq.no_position || pos = prev_it
			then Dpq.insert openlist n
			else Dpq.swap openlist pos n)
		  else (if pos = prev_it
			then Dpq.insert openlist prev)
	      with Not_found ->
		Dpq.insert openlist n;
		Htable.add closed state n)
      else (Limit.incr_prune i;
	    inadmiss_prune := true) in

  let rec expand () =
    if not (Limit.halt_p i) && not (Dpq.empty_p openlist)
    then
      (let n = Dpq.extract_first openlist in
	 setpos n Dpq.no_position;
	 if not (Limit.promising_p i n)
	 then (Limit.incr_prune i;
	       expand ())
	 else if sface.Search_interface.goal_p n
	 then Limit.new_incumbent i (Limit.Incumbent (0.,n))
	 else
	   (List.iter consider_child (sface.Search_interface.node_expand n);
	    Limit.incr_exp i;
	    Limit.curr_q i (Dpq.count openlist);
	    expand())) in

    (fun () ->
       inadmiss_prune := false;
       Dpq.clear openlist;
       Htable.iter (fun _ value -> setpos value prev_it) closed;
       Dpq.insert openlist sface.Search_interface.initial;
       expand ();
       !inadmiss_prune)


let make_sface sface incumbent =
  let def_log = Limit.make_default_logger (fun n -> n.g)
    (wrap sface.Search_interface.get_sol_length) in
    Search_interface.make
      ~node_expand:(make_expand sface.Search_interface.domain_expand
		      sface.Search_interface.h)
      ~goal_p:(wrap sface.Search_interface.goal_p)
      ~key:(wrap sface.Search_interface.key)
      ~hash:sface.Search_interface.hash
      ~equals:sface.Search_interface.equals
      ~halt_on:sface.Search_interface.halt_on
      sface.Search_interface.domain
      (let rec inode = { cost = 0.;
		     g = 0.;
		     data = sface.Search_interface.initial;
		     q_pos = Dpq.no_position;
		     parent = inode;} in inode)
      promising
      (fun i ->
	 sface.Search_interface.info.Limit.log
	   (Limit.unwrap_info (fun n -> n.data) i);
	 def_log i)



let improve_solution raw_key dom_exp sface nbrhd closed openlist =
  let next_it = search_iteration nbrhd sface closed openlist
  and nb_exp = neighborhood_expand dom_exp in
    grow_neighborhood raw_key sface.Search_interface.hash
      sface.Search_interface.equals nb_exp nbrhd;
    while not (Limit.halt_p sface.Search_interface.info) && (next_it ())
    do
      grow_neighborhood raw_key sface.Search_interface.hash
	sface.Search_interface.equals nb_exp nbrhd
    done


let search sface args =
  let new_interface = make_sface sface sface.Search_interface.initial in
  let closed = (Htable.create new_interface.Search_interface.hash
		  new_interface.Search_interface.equals 100) in
  let openlist = (Dpq.create ordered setpos 100
		    new_interface.Search_interface.initial) in
    greedy_phase new_interface.Search_interface.key
      new_interface.Search_interface.info
      (greedy_expand sface.Search_interface.domain_expand
	 sface.Search_interface.h) (new_interface.Search_interface.goal_p)
      closed openlist new_interface.Search_interface.initial;
    Htable.clear closed;
    match new_interface.Search_interface.info.Limit.incumbent with
      | Limit.Nothing -> (Limit.unwrap_sol5 unwrap_sol
			    (Limit.results5
			       new_interface.Search_interface.info))
      | Limit.Incumbent (_,inc) ->
	  let nbrhd = (initial_neighborhood new_interface.Search_interface.key
			 new_interface.Search_interface.hash
			 new_interface.Search_interface.equals inc) in
	    improve_solution
	      sface.Search_interface.key
	      sface.Search_interface.domain_expand
	      new_interface nbrhd closed openlist;
	    Limit.unwrap_sol5 unwrap_sol
	      (Limit.results5 new_interface.Search_interface.info)


let search_dups sface args =
  let new_interface = make_sface sface sface.Search_interface.initial in
  let closed = (Htable.create new_interface.Search_interface.hash
		  new_interface.Search_interface.equals 100) in
  let openlist = (Dpq.create ordered setpos 100
		    new_interface.Search_interface.initial) in
    greedy_phase new_interface.Search_interface.key
      new_interface.Search_interface.info
      (greedy_expand sface.Search_interface.domain_expand
	 sface.Search_interface.h) (new_interface.Search_interface.goal_p)
      closed openlist new_interface.Search_interface.initial;
    Htable.clear closed;
    match new_interface.Search_interface.info.Limit.incumbent with
      | Limit.Nothing -> (Limit.unwrap_sol6 unwrap_sol
			    (Limit.results6
			       new_interface.Search_interface.info))
      | Limit.Incumbent (_,inc) ->
	  let nbrhd = (initial_neighborhood new_interface.Search_interface.key
			 new_interface.Search_interface.hash
			 new_interface.Search_interface.equals inc) in
	    improve_solution
	      sface.Search_interface.key
	      sface.Search_interface.domain_expand
	      new_interface nbrhd closed openlist;
	    Limit.unwrap_sol6 unwrap_sol
	      (Limit.results6 new_interface.Search_interface.info)

(* EOF *)
