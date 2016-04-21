(**

    @author jtd7
    @since 2011-01-10

   Find optimal solution path using a normal A* search
     Record Maximum Depth during process
   Clear open list
   Iterate through closed list, placing all nodes into proper tiered structure
   Find rank of every node along the optimal path.

   Designed only to work for dups because all of the domains have duplicates
   now
*)

type 'a node = {
  data : 'a;
  f : float;
  g : float;
  depth : int;
  mutable q_pos : int;
  parent : 'a node;
}


type 'a layer = {
  open_list : 'a Dpq.t;
  max_count : int;
  mutable count : int;
}


let make_expand expand h =
  let make_child depth parent (data, g) =
    { data = data;
      f = g +. (h data);
      g = g;
      depth = depth;
      q_pos = Dpq.no_position;
      parent = parent; } in
  (fun n ->
     let next_depth = n.depth + 1 in
       List.map (make_child next_depth n)
	 (expand n.data n.g))


let rec extract_path node =
    if node.parent == node then []
    else node::(extract_path node.parent)


let make_initial sface =
  let rec init = { data = sface.Search_interface.initial;
		   f = sface.Search_interface.h sface.Search_interface.initial;
		   g = 0.;
		   depth = 0;
		   q_pos = Dpq.no_position;
		   parent = init; } in init


let solve root expand goal key eq hash =
  let op = Dpq.create (fun a b -> a.f < b.f) (fun a i -> a.q_pos <- i) 100 root
  and cl = Htable.create hash eq 100
  and max_depth = ref 0 in
  let consider_child c =
    (let kval = key c in
       max_depth := Math.imax !max_depth c.depth;
       try
	 let prev = Htable.find cl kval in
	   if prev.g > c.g
	   then (Htable.replace cl kval c;
		 Dpq.insert op c)
       with Not_found ->(Htable.add cl kval c;
			 Dpq.insert op c)) in
  let rec expand_eval () =
    if (not (Dpq.empty_p op))
    then (let n = Dpq.extract_first op in
	    if goal n then n, cl, !max_depth
	    else (List.iter consider_child (expand n);
		  expand_eval ()))
    else failwith "Problem unsolvable" in
    Dpq.insert op root;
    expand_eval ()


let dups sface args =
  (** Performs an A* search from the initial state to a goal,
      for domains where duplicates are frequently encountered. *)
  Search_args.is_empty "Astar.dups" args;
  let init = make_initial sface
  and expand = (make_expand sface.Search_interface.domain_expand
		  sface.Search_interface.h)
  and key n = sface.Search_interface.key n.data
  and eq = sface.Search_interface.equals
  and hash = sface.Search_interface.hash
  and goal n = sface.Search_interface.goal_p n.data in
  let sol_node, closed_list, max_d = solve init expand goal key eq hash in
    Verb.pe Verb.always "Found solution\n%!";
    let path = extract_path sol_node in
  let open_by_depth = (Array.init (max_d + 1)
			 (fun _ -> Dpq.create
			    (fun a b -> a.f < b.f) (fun a i -> a.q_pos <- i)
			    100 init)) in
    Htable.iter (fun key value ->
		   Dpq.insert open_by_depth.(value.depth) value) closed_list;
    List.fold_left (fun accum ele ->
		      (ele.depth,ele.q_pos) :: accum) [] path, max_d
      (* and the path nodes come out in order, so that is awesome *)

(* EOF *)
