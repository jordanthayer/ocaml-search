(** Reverse Cleanup Search

    Reverse cleaup expands nodes in f-order until some critical point,
    at which it expands nodes in estimated f order until an incumbent is
    reached.  Then it reverts to aggressive search

    Jordan Thayer - August 2009 *)


type 'a node = {
  mutable est_f : float;
  h : float;
  d : float;
  mutable g : float;
  mutable ppos : int;
  mutable fpos: int;
  mutable dpos: int;
  data : 'a;
}


let ordered_f a b =
  (** are nodes [a] and [b] in order of increasing f? *)
  (a.g +. a.h) <= (b.g +. b.h)


let ordered_p a b =
  (** are [a] and [b] in best first order *)
  let af = a.g +. a.h
  and bf = b.g +. b.h in
  (a.est_f < b.est_f) ||
  ((a.est_f = b.est_f) &&
   ((af < bf) ||
    ((af = bf) &&
     (a.g >= b.g))))


let fp_then_d_then_g a b =
  (** are [a] and [b] in best first order *)
  (a.est_f < b.est_f) ||
    ((a.est_f = b.est_f &&
       (a.est_f = b.est_f && a.g >= b.g)))


let better_p a b =
  (** is [a] a better solution than [b] *)
  ordered_f a b


let get_f_pos a =
  (** returns the f position of node a *)
  a.fpos

let get_d_pos a =
  (** returns the delayed position of node [a] *)
  a.dpos

let get_pq_pos a =
  (** returns the open position of node [a] *)
  a.ppos


let set_f_pos a i =
  (** sets the cleanup position of node [a] to [i] *)
  a.fpos <- i


let set_pq_pos a i =
  (** sets the open position of node [a] to [i] *)
  a.ppos <- i

let set_d_pos a i =
  (** sets the delay position of node [a] to [i] *)
  a.dpos <- i


let wrap f =
  (** takes a function [f] meant for domain nodes and makes it so that it can
      be applied to search nodes *)
  (fun n -> f n.data)


let unwrap_sol s =
  (** Takes a search space solution and converts it into a domain space
      solution *)
  match s with
    Limit.Incumbent (q,n) -> Some (n.data, n.g)
    | _ -> None


let make_updater f_calc =
  (** Updates the estimated f values of all nodes in a given dpq *)
  (fun dpq ->
     Dpq.iter (fun n -> n.est_f <- f_calc n) dpq)


let make_expand expand hd timer calc_h_data f_calc =
  (** [expand] is the domain expand
      [hd] is a cost and distance estimator
      [timer] returns true every so often, to tell the openlist to resort
      [calc_h_data] takes the parent, the best child, and all children
      in order to make a better h estimator
      [f_calc] uses the estimated h values to calculate the bounded f
      estimates *)
  (fun n ->
     let best_f = ref infinity
     and best_child = ref n
     and reorder = timer() in
     let children = (List.map (fun (s, g) ->
				 let h, d = hd s in
				 let f = g +. h in
				 let c =
				   { est_f = f;
				     h = h;
				     d = d;
				     g = g;
				     ppos = Dpq.no_position;
				     fpos = Dpq.no_position;
				     dpos = Dpq.no_position;
				     data = s;} in
				   if  f < !best_f then
				     (best_child := c;
				      best_f := f)
				   else if f = !best_f then
				     (if d < !best_child.d then
					(best_child := c;
					 best_f := f));
				   c)
		       (expand n.data n.g))
     in
       if not ((List.length children) = 0)
       then
	 (calc_h_data n !best_child children;
	  List.iter (fun c -> c.est_f <- f_calc c) children);
       reorder, children)


let get_node bound =
  (** Returns the next node to be expanded *)
  (fun fq pq i ->
     let fn = Dpq.peek_first fq
     and incumbent = i.Limit.incumbent in
       match incumbent with
	   Limit.Nothing ->
	     raise Optimistic_framework.NoIncumbent
	 | Limit.Incumbent(qual,inc) ->
	     if ((fn.g +. fn.h) *. bound) >= (inc.g +. inc.h)
	     then fn, false
	     else (let fpn = Dpq.peek_first pq in
		     if fpn.est_f < (inc.g +. inc.h) then
		       (Dpq.remove pq fpn.ppos;
			Dpq.remove fq fpn.fpos;
			fpn,true)
		     else
		       (let trf = fn.fpos
			and trp = fn.ppos in
			  Dpq.remove fq trf;
			  Dpq.remove pq trp;
			  fn,true)))


(** Various continue policies *)

let mimic_astar_continue = (fun _ _ _ _ -> true)
and mimic_wastar_continue = (fun _ _ _ _ -> false)
and mimic_optimistic_continue = (fun _ _ _ _ -> false)
and fixed_duration count = (** Returns a continue function which expands nodes
			       in A* order for a fixed duration of [count] *)
  let step = ref 0 in
    (fun _ _ _ _ -> step := !step + 1;
       !step < count)


and min_fhat_admiss bound = (** Returns a continue function which expands nodes
				in A* order until the node just expanded has
				an estimated f within the desired bound *)
  (fun _ next _ _ ->
     not ((next.g +. next.h) *. bound >= next.est_f))


and simple bound =  (** Returns a function which continues until it appears
			that we can show the node which is at the front of the
			prime queue is within the desired bound *)
  (fun _ _ fq pq ->  (* is switching way too quickly *)
     let fmin = Dpq.peek_first fq
     and pmin = Dpq.peek_first pq in
       not ((fmin.g +. fmin.h) *. bound >=  pmin.est_f))


and simple_min_cleanup count bound =
  (** attempts to address the problem of simple switching too fast by requiring
      a fixed number of cleanup expansions *)
  (fun _ _ fq pq ->
     let i = ref 0
     and fmin = Dpq.peek_first fq
     and pmin = Dpq.peek_first pq in
       i := !i + 1;
       not ((((fmin.g +. fmin.h) *. bound >=  pmin.est_f)) || !i < count ))

let no_dups sface bound continue timer calc_h_data f_calc =
  let search_interface =
    Search_interface.make
      ~resort_expand:(make_expand sface.Search_interface.domain_expand
			sface.Search_interface.hd timer calc_h_data f_calc)
      ~goal_p:(wrap sface.Search_interface.goal_p)
      ~halt_on:(sface.Search_interface.halt_on)
      ~hash:sface.Search_interface.hash
      ~equals:sface.Search_interface.equals
      sface.Search_interface.domain
      { est_f = neg_infinity;
	h = neg_infinity;
	d = neg_infinity;
	g = 0.;
	ppos = Dpq.no_position;
	fpos = Dpq.no_position;
	dpos = Dpq.no_position;
	data = sface.Search_interface.initial;}
      better_p
      (Limit.make_default_logger (fun n -> n.g +. n.h)
	 (wrap sface.Search_interface.get_sol_length))
  in
    Limit.unwrap_sol5 unwrap_sol
      (Reverse_cleanup_framework.search
	 search_interface
	 (get_node bound)
	 ordered_f
	 ordered_p
	 (make_updater f_calc)
	 get_f_pos
	 get_pq_pos
	 continue
	 set_f_pos
	 set_pq_pos)


let dups sface bound continue timer calc_h_data f_calc =
  let search_interface =
    Search_interface.make
      ~resort_expand:(make_expand sface.Search_interface.domain_expand
			sface.Search_interface.hd timer calc_h_data f_calc)
      ~goal_p:(wrap sface.Search_interface.goal_p)
      ~halt_on:(sface.Search_interface.halt_on)
      ~hash:sface.Search_interface.hash
      ~equals:sface.Search_interface.equals
      ~key:(wrap sface.Search_interface.key)
      sface.Search_interface.domain
      { est_f = neg_infinity;
	h = neg_infinity;
	d = neg_infinity;
	g = 0.;
	ppos = Dpq.no_position;
	fpos = Dpq.no_position;
	dpos = Dpq.no_position;
	data = sface.Search_interface.initial;}
      better_p
      (Limit.make_default_logger (fun n -> n.g +. n.h)
	 (wrap sface.Search_interface.get_sol_length))
  in
    Limit.unwrap_sol6 unwrap_sol
      (Reverse_cleanup_framework_dups.search
	 search_interface
	 (get_node bound)
	 ordered_f
	 ordered_p
	 (make_updater f_calc)
	 get_f_pos
	 get_pq_pos
	 continue
	 set_f_pos
	 set_pq_pos
	 better_p)


let delay_dups sface = failwith "Not Implemented"

(* EOF *)
