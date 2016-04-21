(**

    @author jtd7
    @since 2011-03-09

   An exhaustive A* search which will generate all optimal paths
   This sort of thing is hopelessly inefficient, probably better to work from
   truth files instead of doing things this way.
*)

type 'a node = {
  data : 'a;
  g : float;
  f : float;
  depth : int;
  mutable cost : float;
  mutable parent : 'a node list;
  mutable qp : int;
}

let order a b =
  Math.fcompare b.cost a.cost
(*  if a.cost < b.cost then -1
  else if a.cost > b.cost then 1
  else (if (Random.int 2) = 0 then -1 else 1) *)



let closed_pos = -2

let delta ar1 ar2 =
  (** Computes the delta between two weight arrays so that we know if
      we can stop or not when calling the learning portion of laso br *)
  assert ((Array.length ar1) = (Array.length ar2));
  Wrarray.fold_left2 (fun accum a b -> accum +. (abs_float (a -. b))) 0.
    ar1 ar2


let make_expand exp_fn h =
  (fun n ->
     let depth' = n.depth + 1 in
       List.map (fun (d,g) ->
		   { data = d;
		     g = g;
		     f = g +. (h d);
		     cost = nan;
		     depth = depth';
		     parent = [n];
		     qp = Dpq.no_position}) (exp_fn n.data n.g))

let make_key key n = key n.data, n.g


let ordered_p a b =  (a.f : float) < b.f || (a.f = b.f && a.g >= b.g)
and prune_p a b = (a.g : float) <= b.f
and setpos n i = n.qp <- i


let search expand key goal hash eq root i =
  let openlist = Dpq.create ordered_p setpos 100 root
  and closed = Htable.create hash eq 100
  and goals = ref [] in
  let consider_child n =
    Limit.incr_gen i;
    if not (Limit.promising_p i n)
    then Limit.incr_prune i
    else (let state = key n in
	    try
	      let prev = Htable.find closed state in
		Limit.incr_dups i;
		if n.g = prev.g
		then prev.parent <- n.parent @ prev.parent
		else if n.g < prev.g
		then (Htable.replace closed state n;
		      let pos = prev.qp in
			if pos == closed_pos
			then Dpq.insert openlist n
			else Dpq.swap openlist pos n)
	    with Not_found -> (* new state *)
	      Dpq.insert openlist n;
	      Htable.add closed state n) in

  let rec expand_best () =
    if (not (Dpq.empty_p openlist)) && (not (Limit.halt_p i))
    then
      (let n = Dpq.extract_first openlist in
	 setpos n closed_pos;
	 if not (Limit.promising_p i n) then
	   (Limit.incr_prune i;
	    Htable.remove closed (key n);
	    expand_best ())
	 else if goal n then
	   (Verb.pe Verb.toplvl "Adding Goal\n%!";
	    Limit.new_incumbent i (Limit.Incumbent (1.,n));
	    goals := n::!goals;
	    expand_best ())
	 else
	   (let children = expand n in
	      Limit.incr_exp i;
	      List.iter consider_child children;
	      Limit.curr_q i (Dpq.count openlist);
	      expand_best ())) in
    Dpq.insert openlist root;
    Htable.add closed (key root) root;
    expand_best ();
    !goals


let get_goals sface =
  let expand = (make_expand sface.Search_interface.domain_expand
		  sface.Search_interface.h)
  and key = make_key sface.Search_interface.key
  and goal n = sface.Search_interface.goal_p n.data in
  let root = {data = sface.Search_interface.initial;
	      g = 0.;
	      f = sface.Search_interface.h sface.Search_interface.initial;
	      parent = [];
	      depth = 0;
	      cost = nan;
	      qp = Dpq.no_position;} in
    (search expand key goal (fun (a,_) -> sface.Search_interface.hash a)
       (fun (a,_) (b,_) -> sface.Search_interface.equals a b) root
       (Limit.make Limit.Nothing [] prune_p
	  Fn.no_op1))


let make_kgei sface =
  make_key sface.Search_interface.key,
  (fun n -> sface.Search_interface.goal_p n.data),
  (make_expand sface.Search_interface.domain_expand
     sface.Search_interface.h),
  {data = sface.Search_interface.initial;
   g = 0.;
   f = sface.Search_interface.h sface.Search_interface.initial;
   depth = 0;
   parent = [];
   cost = nan;
   qp = Dpq.no_position;}


let make_update_cost get_features =
  (** Computes the weight vector based cost of the node and updates the cost
      of the node accordingly *)
  (fun weights n -> n.cost <- Laso_br.do_calc weights (get_features n))


let load_proper_nodes_bfs sface goals =
  let hash = sface.Search_interface.hash
  and eq = sface.Search_interface.equals
  and key n = sface.Search_interface.key n.data in
  let nodes = Htable.create hash eq 100 in
  let deepest = List.fold_left (fun accum n -> max accum n.depth) 0 goals in
  let nodes_by_depth = Array.create (deepest + 1) [] in
  let rec traverse n =
    Htable.replace nodes (key n) n;
    if n.parent <> []
    then List.iter (fun p -> if not (Htable.mem nodes (key p))
		    then traverse p) n.parent in
    List.iter traverse goals;
    Htable.iter (fun _ ele -> let d = ele.depth in
		   nodes_by_depth.(d) <- ele::nodes_by_depth.(d)) nodes;
    nodes_by_depth

let load_proper_nodes_breadth sface goals =
  let key n = sface.Search_interface.key n.data in
  let deepest = List.fold_left (fun accum n -> max accum n.depth) 0 goals in
  let nodes_by_depth = Array.create (deepest + 1) [] in
    List.iter (fun n -> nodes_by_depth.(n.depth) <- [n]) goals;
    for i = deepest downto 1 do
      (nodes_by_depth.(i) <- Wrlist.remove_dups key nodes_by_depth.(i);
       List.iter (fun n -> List.iter (fun p -> nodes_by_depth.(i-1) <-
					p::nodes_by_depth.(i-1)) n.parent)
	 nodes_by_depth.(i))
    done;
    Array.to_list nodes_by_depth


let make_get_standard_features sface =
  let hd = sface.Search_interface.hd in
  let max_h, max_d = hd sface.Search_interface.initial in
    (fun child ->
       let parent = List.hd child.parent in
       let h,d = hd child.data
       and c = child.g -. parent.g in
	 [| child.g /. max_h ;
	    (child.f -. parent.f) /. max_h;
	    ((snd (hd parent.data)) -. d) /. max_d;
	    h /. max_h;
	    d /. max_d;
	    c /. max_h|])


let make_get_unit_features sface =
  let h = sface.Search_interface.h in
  let max_h = h sface.Search_interface.initial in
    (fun child ->
       let parent = List.hd child.parent in
       let h = h child.data
       and c = child.g -. parent.g in
	 [| child.g /. max_h ;
	    (child.f -. parent.f) /. max_h;
	    h /. max_h; c /. max_h|])



let kegi_by_brfs_nodes tdata =
  (List.map (fun (sface, goals) ->
	       make_kgei sface,
	       load_proper_nodes_breadth sface goals)) tdata

let kegi_by_bfs_nodes tdata =
  List.map (fun (sface,goals) ->
               make_kgei sface,
               load_proper_nodes_bfs sface goals) tdata


