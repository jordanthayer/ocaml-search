(** Searches which learn a correction of fhat from single step error
    models.  Hands the clamping weight into the f_calculation *)

type 'a node = {
  mutable est_f : float;
  h : float;
  d : float;
  g : float;
  mutable q_pos : int;   (* for Dpq *)
  data : 'a;
}


let setpos n i =
  (** Updates the [q_pos] of node [n], setting it to [i] *)
  n.q_pos <- i


let getpos n =
  (** returns the current [q_pos] of node [n] *)
  n.q_pos


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
      Limit.Incumbent (q, n) -> Some (n.data, n.g)
    | _ -> None


let wrap_incumbent i =
  (** Wraps an incumbent solution [i] and returns a Limit.info based on it *)
  match i with
      None -> Limit.Nothing
    | Some (n) -> Limit.Incumbent (0., n)


let make_expand expand hd timer calc_h_data f_calc wtlist =
  (** [expand] is the domain expand
      [hd] is a cost and distance estimator
      [timer] returns true every so often, to tell the openlist to resort
      [calc_h_data] takes the parent, the best child, and all children
      in order to make a better h estimator
      [f_calc] uses the estimated h values to calculate the bounded f
      estimates *)
  let wts = ref wtlist in
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
				     q_pos = Dpq.no_position;
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
	 (let wt = List.hd wtlist in
	    calc_h_data n !best_child children;
	    List.iter (fun c -> c.est_f <- f_calc c wt) children);
       reorder, children),
  (fun () -> (if List.length !wts > 0
	      then (wts := List.tl !wts; true) else true)),
  (fun dpq -> let wt = List.hd !wts in
     Dpq.iter (fun n -> n.est_f <- f_calc n wt) dpq)


(************************ Search Callers, internal only **********************)
let make_interface sface h_calc f_calc timer wtlist =
  let expand,next,update = make_expand sface.Search_interface.domain_expand
    sface.Search_interface.hd timer h_calc f_calc wtlist in
    Search_interface.make
      ~resort_expand:expand
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
	q_pos = Dpq.no_position;
	data = sface.Search_interface.initial;}
      better_p
      (Limit.make_default_logger (fun n -> n.g +. n.h)
	 (wrap sface.Search_interface.get_sol_length)), next, update

let no_dups sface timer h_calc f_calc bound =
  (** Performs a search in domains where there are no duplicates.
      [sface] is the domain's search interface
      [bound] is the desired quality bound
      [timer] is a timer from Timers.ml
      [h_calc] takes the parent, best child, and all children, returns unit
      [f_calc] takes a node and returns a float *)
  let wtlist = Arastar.mk_wtlist bound 0.2 in
  let search_interface, next, update =
    make_interface sface h_calc f_calc timer wtlist in
    Limit.unwrap_sol5 unwrap_sol
      (Restarting_reorderable.search
	 next
	 search_interface
	 est_f_then_d_then_g
	 better_p
	 update)


let dups sface timer h_calc f_calc bound =
  (** Performs a search in domains where there are duplicates.
      [sface] is the domain's search interface
      [bound] is the desired quality bound
      [timer] is a timer from Timers.ml
      [h_calc] takes the parent, best child, and all children, returns unit
      [f_calc] takes a node and returns a float *)
  let wtlist = Arastar.mk_wtlist bound 0.2 in
  let search_interface, next, update =
    make_interface sface h_calc f_calc timer wtlist in
    Limit.unwrap_sol6 unwrap_sol
      (Restarting_reorderable.search_dups
	 next
	 search_interface
	 est_f_then_d_then_g
	 better_p
	 setpos
	 getpos
	 update)
(* EOF *)
