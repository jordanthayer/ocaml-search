(* $Id: astar.ml,v 1.4 2006/06/18 21:31:38 ruml Exp $

   simple tsp
*)


type 'a node = {
  location : int;
  remaining : int list;
  cost_so_far : float;
  tour : int list;
  depth : int;
  mutable heuristic_data : 'a;
  mutable h_calculated : bool;
  mutable parent : 'a node;
  mutable h : float;
}


let update_parent node new_parent =
  node.parent <- new_parent


let unwrap_results six_tuple =
  Limit.unwrap_sol6 (fun s ->
		       match s with
			   None -> None
			 | Some (n, f) ->
			     Some ((List.rev n.tour), n.cost_so_far))
    six_tuple


let unwrap_results_dep five_tuple =
  Limit.unwrap_sol5 (fun s ->
		       match s with
			   None -> None
			 | Some (n, f) ->
			     Some ((List.rev n.tour), n.cost_so_far))
    five_tuple


let key n =
  n.location::n.remaining


let old_key n =
  n.tour


let key_to_string int_list =
  List.fold_left (fun accum i -> Wrutils.str "%s%i " accum i) "" int_list


let rec int_list min max =
  (** list of ints from [min] through [max] inclusive.  not tail
      recursive. *)
  if min > max then
    []
  else
    min::(int_list (min+1) max)


let prob_size p =
  Array.length p


type edge = {
  weight : float;
  u : int;
  v : int;
}


let make_edges p symm =
  (** all edges sorted in increasing weight *)
  let elist = ref []
  and n = (prob_size p) - 1 in
    for c1 = 0 to n do
      for c2 = (if symm then c1+1 else 0) to n do
	if not (c1 == c2) then
	  let e = {weight = p.(c1).(c2);
		   u = c1;
		   v = c2} in
	    Wrutils.push e elist;
      done;
    done;
    List.sort (fun e1 e2 ->
		 if e1.weight < e2.weight then -1
		 else if e1.weight > e2.weight then 1
		 else 0)
      !elist


let start_city = 0
and delayed_city = 1

let make_initial p symm =
  (** initialize source city. Randomly pick the initial node from a
      list of cities *)
  if (prob_size p) < 6 then failwith "fewer than 6 cities?";
  let remaining = int_list 2 ((prob_size p) - 1) in
  let rec n =
    { location = start_city;
      remaining =  remaining;
      cost_so_far = 0.;
      tour = [start_city];
      depth = 0;
      heuristic_data = make_edges p symm;
      h_calculated = true;
      parent = n;
      h = ~-.1.0;
    } in n


let print_int_list l =
  Verb.pr Verb.debug "\n\n";
  List.iter (fun c -> Verb.pr Verb.debug "%d\t%!" c) l;
  Verb.pr Verb.debug "\n\n"


let print_node n =
  Verb.pr Verb.debug "\n\n\nlocation = %d\n" n.location;
  Verb.pr Verb.debug "remaining: ";
  print_int_list n.remaining;
  Verb.pr Verb.debug "cost_so_far = %f\n" n.cost_so_far;
  Verb.pr Verb.debug "tour: ";
  print_int_list n.tour;
  Verb.pr Verb.debug "\ndepth = %d\n\n\n" n.depth;
  ()


let goal_p n =
  n.remaining = []


let delay_layer p symm =
  (** depth at which we should insert the delayed city back into
      remaining, so that it has a node at depth+1 *)
  if symm then
    (* half way *)
    int_of_float (ceil ((float_of_int ((prob_size p) - 2)) /. 2.))
  else
    (* from the beginning *)
    0


let make_expand p symm =
  let delay_depth = delay_layer p symm in
    (fun n g ->
       let cities = (if (n.depth == delay_depth)
		     then delayed_city::n.remaining
		     else n.remaining) in
      	 List.map (fun c ->
		     let remaining = Wrlist.remove_firstq c cities in
		     let c =
		       (match remaining with
			    (* if ony one remaining, complete tour *)
			    x::[] -> { location = start_city;
				       remaining = [];
				       cost_so_far = (g +.
							p.(n.location).(c) +.
							p.(c).(x) +.
							p.(x).(start_city));
				       tour = start_city::x::c::n.tour;
				       depth = n.depth + 3;
				       heuristic_data = n.heuristic_data;
				       h_calculated = false;
				       parent = n;
				       h = ~-.1.0;
				     }
			  | _ -> { location = c;
				   remaining = remaining;
				   cost_so_far = g +. p.(n.location).(c);
				   tour = c::n.tour;
				   depth = n.depth + 1;
				   heuristic_data = n.heuristic_data;
				   h_calculated = false;
				   parent = n;
				   h = ~-.1.0;
				 }) in
		       c, c.cost_so_far)
	   cities)


(************** verify **************)


let print_and_fail tour string =
  Wrutils.pr "Tour:";
  List.iter (fun x -> Wrutils.pr " %d" x) tour;
  Wrutils.pr "\n";
  failwith string


let check_tour d tour cost =
  (** ensure tour length, visiting every city, cost *)
  let n = prob_size d in
    if (List.length tour) != (n+1) then
      print_and_fail tour (Wrutils.str "tour has length %d, not %d"
			     (List.length tour) (n+1));
    for i = 0 to n-1 do
      if not (List.mem i tour) then
	print_and_fail tour (Wrutils.str "tour misses city %d" i);
    done;
    match tour with
	[] -> failwith "empty tour?"
      | first::rest ->
	  let curr = ref first
	  and g = ref 0. in
	    List.iter (fun x ->
			 g := !g +. d.(!curr).(x);
			 curr := x)
	      rest;
	    if !curr != first then
	      print_and_fail tour "tour didn't end at start";
	    if (abs_float (!g -. cost)) > 0.0001 then
	      print_and_fail tour (Wrutils.str  "cost is %f, not %f" !g cost)


(************* cost to go heuristics *************)


let make_h_pk d_matrix symm =
  let delay_layer = delay_layer d_matrix symm in
    (fun n ->
       (** Given by Pearl & Kim
	   Return the maximum of:
	   the sum of the minimum value in each row of the distance matrix or
	   the sum of the minimum value in each column of the distance matrix*)
       (* remaining = list of remaining cities *)
       let r_cities = (if n.depth > delay_layer then start_city::n.remaining
		       else start_city::delayed_city::n.remaining) in
       let sum_row_min = ref 0.
       and sum_col_min = ref 0. in
	 List.iter (fun c1 ->
		      (* Find the minimum in row c1 and column c1 *)
		      let r_min = ref infinity
		      and c_min = ref infinity in
			List.iter (fun c2 ->
				     if not(c1 == c2) then
				       (if d_matrix.(c1).(c2) < !r_min  then
					  r_min := d_matrix.(c1).(c2);
					if d_matrix.(c2).(c1) < !c_min then
					  c_min := d_matrix.(c2).(c1) ))
			  r_cities;
			sum_row_min := !sum_row_min +. !r_min;
			sum_col_min := !sum_col_min +. !c_min )
	   r_cities;
	 Math.fmin !sum_row_min !sum_col_min)


(************* MST heuristic *************)


(*** For testing MST code ***)
(* caller located in instances.ml *)

let old_mst p symm =
  (** Pure MST code for testing purposes. Does not reinsert city that
      was forced into the second half of the tour *)
  (fun n ->
     let cost = ref 0.
     and nodes = (if n.depth <= (delay_layer p symm)
		  then start_city::delayed_city::n.remaining
		  else start_city::n.remaining) in
     let uf = Duf.make (Array.length p) in
       (List.iter (fun c ->
		     Duf.add uf c)
	  nodes);
       let dummy = {weight = 0.;
		    u = 0;
		    v = 0} in
       let pq = (Dpq.create_with (fun e1 e2 ->
				    e1.weight < e2.weight)
		   dummy) in
	 (List.iter (fun c1 ->
		       List.iter (fun c2 ->
				    if not (c1 == c2) then
				      let e = {weight = p.(c1).(c2);
					       u = c1;
					       v = c2} in
					Dpq.insert pq e)
			 nodes)
	    nodes;
	  while not (Dpq.empty_p pq) do
	    let e = Dpq.extract_first pq in
	      try
		if not (Duf.same_p uf e.u e.v) then
		  (cost := !cost +. e.weight;
		   Duf.join uf (Duf.rep uf e.u) (Duf.rep uf e.v));
	      with Not_found ->
		Verb.pe Verb.debug "\n\nproblem with Dpq\n\n"
	  done);
	 Verb.pe Verb.debug "\n\nOld MST: %f\n" !cost;
	 print_int_list nodes;
	 !cost)


(**** more efficient version of mst ***)
let make_h_mst p symm =
  (** assumes that node's heuristic_info has been initialized to a
      sorted list of edges, including delayed city's edges *)
  Verb.pe Verb.debug "Making heuristic\n";
  let delay_layer = delay_layer p symm
  and uf = Dufi.make (prob_size p) in
  let rec build_tree edges size max_size cost =
    (** returns cost of MST on remaining cities *)
    if size == max_size then
      cost
    else
      match edges with
	  [] -> 0.
	    (*always returned for aggressive *)
	| e::rest ->
	    if not (Dufi.same_p uf e.u e.v) then
	      (Dufi.join uf e.u e.v;
	       build_tree rest (size+1) max_size (cost +. e.weight))
	    else
	      build_tree rest size max_size cost
  in
    Verb.pe Verb.debug "Done Making Heuristic\n";
    (fun n ->
       (** Computes the minimum spanning tree *)
       (* filter edges from last addition *)
       if not n.h_calculated
       then (let added = List.hd n.tour in
	     let new_h_dat =
	       List.filter
		 (fun e -> e.u <> added && e.v <> added)
		 n.heuristic_data
	     in
	       n.heuristic_data <- new_h_dat;
	       n.h_calculated <- true);
       match n.remaining with
	   [] -> 0.
	 | x::[] -> failwith "didn't visit last city in expand?";
	 | _ ->
	     let tree_size = ref (List.length n.remaining) in
	       Dufi.init uf start_city;
	       if n.depth <= delay_layer then
		 (Dufi.init uf delayed_city;
		  tree_size := !tree_size + 1);
	       List.iter (fun c -> Dufi.init uf c) n.remaining;
	       let cost = build_tree n.heuristic_data 0 !tree_size 0. in
		 (*Wrutils.pr "\nNew MST: %f\n" !cost;*)
		 cost)


let make_h_mst_pathmax p symm =
  (** [make_h_mst_pathmax p symm] makes a minimal spanning tree
      heuristic that uses pathmax. *)
  let h_mst = make_h_mst p symm in
    (fun n ->
       let g = n.cost_so_far -. n.parent.cost_so_far in
	 n.h <- h_mst n;
	 if n.h < n.parent.h -. g
	 then n.h <- n.parent.h -. g;
	 n.h)



let spanning_tree verts distance_mat =
  (** Calculates the minimum spanning tree of the vertex list. *)
  let ln = (List.length verts) in
  let uf = Duf.make ln in
  let cost = ref 0. in
    List.iter (fun c -> Duf.add uf c) verts;
    let dummy = {weight = 0.;
		 u = -1;
		 v = -1;} in
    let pq = (Dpq.create_with (fun e1 e2 -> e1.weight <= e2.weight) dummy) in
      (List.iter (fun c1 ->
		    List.iter (fun c2 ->
				 if c1 <> c2
				 then Dpq.insert pq
				   {weight = distance_mat.(c1).(c2);
				    u = c1;
				    v = c2})
		      verts) verts;
       while not (Dpq.empty_p pq) do
	 let e = Dpq.extract_first pq in
	   if not (Duf.same_p uf e.u e.v) then
	     (cost := !cost +. e.weight;
	      Duf.join uf (Duf.rep uf e.u) (Duf.rep uf e.v))
       done);
      !cost


let make_rev_h p symm =
  (assert symm);
  let all_cities = Array.to_list (Array.init (Array.length p) (fun i -> i)) in
  let find_visited r = List.filter (fun a -> not (List.mem a r)) all_cities in
    (fun n ->
       let to_span = n.location::(find_visited n.remaining) in
	 spanning_tree to_span p)



(************* search distance to go *************)


let make_d p =
  let size = prob_size p in
    (fun n ->
       (** Returns the number of cities remaining *)
	      let v = size - n.depth in
		if v = 0 then assert (goal_p n);
		float_of_int v)


let make_rev_d p =
  (fun n -> float_of_int n.depth)


let make_hd p symm =
  (** returns float * float **)
  let h = make_h_mst p symm
  and d = make_d p in
    (fun n ->
       h n, d n)

let make_hd_old p symm =
  (** returns float * float **)
  let h = old_mst p symm
  and d = make_d p in
    (fun n ->
       h n, d n)


(* EOF *)
