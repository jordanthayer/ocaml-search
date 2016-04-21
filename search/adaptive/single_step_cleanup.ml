(** Single step error searches for cleanup searches *)


type 'a node = {
  mutable est_f : float;
  h : float;
  d : float;
  g : float;
  mutable ppos : int;
  mutable fpos: int;
  mutable dpos: int;
  data : 'a;
}

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
  (** Wraps a function [f] which works on domain data and makes it so
      that it can be applied to nodes *)
  (fun n -> f n.data)


let est_f_then_d_then_g a b =
  (** sorts nodes in order of estimated f, breaking ties in favor of
      low d values, and then in favor of high g *)
  (a.est_f < b.est_f) ||  (* sort by fhat *)
    ((a.est_f = b.est_f) &&
       a.d < b.d) ||
    (a.est_f = b.est_f && a.d = b.d &&
	((a.g >= b.g)))


let better_p a b =
  (** Determines which of the nodes represents a better solution *)
  (a.g +. a.h) <= (b.g +. b.h)


let unwrap_sol s =
  (** Decomposes the solution [s] into a form which the domains are expecting
      when doing validation *)
  match s with
      Limit.Incumbent (q, n) -> if q = 0. then None else Some (n.data, n.g)
    | _ -> None


let wrap_incumbent i =
  (** Wraps an incumbent solution [i] and returns a Limit.info based on it *)
  match i with
      None -> Limit.Nothing
    | Some (n) -> Limit.Incumbent (0., n)


let make_expand expand hd timer calc_h_data update =
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
	  List.iter (fun c -> update c) children);
       reorder, children)


let make_updater f_calc =
  (** Updates the estimated f values of all nodes in a given dpq *)
  (fun dpq ->
     Dpq.iter (fun n -> n.est_f <- f_calc n) dpq)


let make_fd_updater fd_calc =
  (** Updates the estimated f values of all nodes in a given dpq *)
  (fun dpq ->
     Dpq.iter (fun n -> n.est_f <- fst (fd_calc n)) dpq)


let get_node fq pq i bound =
  (** Returns the next node to be expanded *)
  let fn = Dpq.peek_first fq
  and incumbent = i.Limit.incumbent in
    match incumbent with
	Limit.Nothing ->
	  raise Optimistic_framework.NoIncumbent
      | Limit.Incumbent(qual,inc) ->
	  if ((fn.g +. fn.h) *. bound) >= (inc.g)
	  then raise Reorderable_optimistic_framework.Done;
(*	  let fpn = Dpq.peek_first pq in
	    if fpn.est_f < (inc.g) then
	      (Dpq.remove pq fpn.ppos;
	       Dpq.remove fq fpn.fpos;
	       fpn)
	    else*)
	      (let trf = fn.fpos
	       and trp = fn.ppos in
		 Dpq.remove fq trf;
		 Dpq.remove pq trp;
		 fn)


let f_calc_update fcalc =
  (fun n -> n.est_f <- fcalc n)


let fd_calc_update fdcalc =
  (fun n -> n.est_f <- fst (fdcalc n))


let make_sface timer h_calc f_calc update sface =
  let update = update f_calc in
  let ih,id = sface.Search_interface.hd sface.Search_interface.initial in
  Search_interface.make
    ~resort_expand:(make_expand sface.Search_interface.domain_expand
		      sface.Search_interface.hd timer h_calc update)
    ~goal_p:(wrap sface.Search_interface.goal_p)
    ~halt_on:(sface.Search_interface.halt_on)
    ~hash:sface.Search_interface.hash
    ~key:(wrap sface.Search_interface.key)
    ~equals:sface.Search_interface.equals
    sface.Search_interface.domain
    { est_f = ih;
      h = ih;
      d = id;
      g = 0.;
      ppos = Dpq.no_position;
      dpos = Dpq.no_position;
      fpos = Dpq.no_position;
      data = sface.Search_interface.initial;}
    better_p
    (Limit.make_default_logger (fun n -> n.g +. n.h)
       (wrap sface.Search_interface.get_sol_length))

(*************************** Searches ***************************)

let no_dups sface bound timer h_calc f_calc =
  (** Performs a search in domains where there are no duplicates.
      [sface] is the domain's search interface
      [bound] is the desired quality bound
      [timer] is a timer from Timers.ml
      [h_calc] takes the parent, best child, and all children, returns unit
      [f_calc] takes a node and returns a float *)
    Limit.unwrap_sol5 unwrap_sol
      (Reorderable_optimistic_framework.no_dups
	 (make_sface timer h_calc f_calc f_calc_update sface)
	 (make_updater f_calc)
	 get_node
	 est_f_then_d_then_g
	 bound
	 better_p
	 better_p
	 set_pq_pos
	 set_f_pos)


let dups sface bound timer h_calc f_calc =
  (** Performs a search in domains where there are duplicates.
      [sface] is the domain's search interface
      [bound] is the desired quality bound
      [timer] is a timer from Timers.ml
      [h_calc] takes the parent, best child, and all children, returns unit
      [f_calc] takes a node and returns a float *)
    Limit.unwrap_sol6 unwrap_sol
      (Reorderable_optimistic_framework.dups
	 (make_sface timer h_calc f_calc f_calc_update sface)
	 (make_updater f_calc)
	 get_node
	 est_f_then_d_then_g
	 bound
	 better_p
	 better_p
	 set_pq_pos
	 set_f_pos
	 get_pq_pos
	 get_f_pos)


let delay_dups sface bound timer h_calc f_calc =
  (** Performs a search in domains where there are duplicates.
      [sface] is the domain's search interface
      [bound] is the desired quality bound
      [timer] is a timer from Timers.ml
      [h_calc] takes the parent, best child, and all children, returns unit
      [f_calc] takes a node and returns a float *)
    Limit.unwrap_sol6 unwrap_sol
      (Reorderable_optimistic_framework.delay
	 (make_sface timer h_calc f_calc f_calc_update sface)
	 (make_updater f_calc)
	 get_node
	 est_f_then_d_then_g
	 bound
	 better_p
	 better_p
	 set_pq_pos
	 set_d_pos
	 set_f_pos
	 get_pq_pos
	 get_d_pos
	 get_f_pos)



let no_dups_fd sface bound timer hd_calc fd_calc =
  (** Performs a search in domains where there are no duplicates.
      [sface] is the domain's search interface
      [bound] is the desired quality bound
      [timer] is a timer from Timers.ml
      [h_calc] takes the parent, best child, and all children, returns unit
      [f_calc] takes a node and returns a float *)
    Limit.unwrap_sol5 unwrap_sol
      (Reorderable_optimistic_framework.no_dups
	 (make_sface timer hd_calc fd_calc fd_calc_update sface)
	 (make_fd_updater fd_calc)
	 get_node
	 est_f_then_d_then_g
	 bound
	 better_p
	 better_p
	 set_pq_pos
	 set_f_pos)


let dups_fd sface bound timer hd_calc fd_calc =
  (** Performs a search in domains where there are duplicates.
      [sface] is the domain's search interface
      [bound] is the desired quality bound
      [timer] is a timer from Timers.ml
      [h_calc] takes the parent, best child, and all children, returns unit
      [f_calc] takes a node and returns a float *)
    Limit.unwrap_sol6 unwrap_sol
      (Reorderable_optimistic_framework.dups
	 (make_sface timer hd_calc fd_calc fd_calc_update sface)
	 (make_fd_updater fd_calc)
	 get_node
	 est_f_then_d_then_g
	 bound
	 better_p
	 better_p
	 set_pq_pos
	 set_f_pos
	 get_pq_pos
	 get_f_pos)


let delay_dups_fd sface bound timer hd_calc fd_calc =
  (** Performs a search in domains where there are duplicates.
      [sface] is the domain's search interface
      [bound] is the desired quality bound
      [timer] is a timer from Timers.ml
      [h_calc] takes the parent, best child, and all children, returns unit
      [f_calc] takes a node and returns a float *)
    Limit.unwrap_sol6 unwrap_sol
      (Reorderable_optimistic_framework.delay
	 (make_sface timer hd_calc fd_calc fd_calc_update sface)
	 (make_fd_updater fd_calc)
	 get_node
	 est_f_then_d_then_g
	 bound
	 better_p
	 better_p
	 set_pq_pos
	 set_d_pos
	 set_f_pos
	 get_pq_pos
	 get_d_pos
	 get_f_pos)


(* EOF *)
