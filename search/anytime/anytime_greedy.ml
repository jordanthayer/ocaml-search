(**

    @author jtd7
    @since 2010-05-20

   Continued greedy / speedy search
*)



type 'a node = {
  data : 'a;          (* Data Payload *)
  h: float;          (* Search order w/ a weighted heuristic value *)
  f : float;          (* Total cost of a node*)
  g : float;          (* Cost of reaching a node *)
  depth : int;        (* Depth of node in tree.  Root @ 0 *)
  mutable pos : int;  (* Position info for dpq *)
}


let wrap f =
  (** takes a function to be applied to the data payload
      such as the goal-test or the domain heuristic and
      wraps it so that it can be applied to the entire
      node *)
  (fun n -> f n.data)


let unwrap_sol s =
  (** Unwraps a solution which is in the form of a search node and presents
      it in the format the domain expects it, which is domain data followed
      by cost *)
  match s with
      Limit.Nothing -> None
    | Limit.Incumbent (q,n) -> Some (n.data, n.g)


let ordered_p a b =
  (** Ordered predicate used for search.  Compares f', then f, then g.
      true if a is better than b.
  *)
  (a.h < b.h) ||
  ((a.h = b.h) &&
   ((a.f < b.f) ||
    ((a.f = b.f) &&
     (a.g >= b.g))))


let just_f a b =
  (** Sorts nodes solely on total cost information *)
  a.f <= b.f


let setpos n i =
  (** Sets the location of a node, used by dpq's *)
  n.pos <- i


let getpos n =
  (** Returns the position of the node in its dpq.
      Useful for swapping nodes around on the open list *)
  n.pos


let make_expand expand h =
  (** Takes the domain [expand] function and a [h]euristic calculator.
      Needs the [wt] which will be applied to the heuristic.
      Creates an expand function which returns search nodes. *)
  (fun n ->
     List.map (fun (d, g) ->
		 let hv = h d in
		   { data = d;
		     h = hv;
		     f = g +. hv;
		     g = g;
		     depth = n.depth + 1;
		     pos = Dpq.no_position; }) (expand n.data n.g))



let make_iface sface =
  let def_log = Limit.make_default_logger (fun n -> n.f)
    (wrap sface.Search_interface.get_sol_length) in
    Search_interface.make
      ~node_expand:(make_expand sface.Search_interface.domain_expand
		      sface.Search_interface.h)
      ~goal_p:(wrap sface.Search_interface.goal_p)
      ~halt_on:sface.Search_interface.halt_on
      ~hash:sface.Search_interface.hash
      ~key:(wrap sface.Search_interface.key)
      ~equals:sface.Search_interface.equals
      sface.Search_interface.domain
      { data = sface.Search_interface.initial;
	h = neg_infinity;
	f = neg_infinity;
	g = 0.;
	depth = 0;
	pos = Dpq.no_position;}
      just_f
      (fun i ->
	 sface.Search_interface.info.Limit.log
	   (Limit.unwrap_info (fun n -> n.data) i);
	 def_log i)


let no_dups sface args =
  Limit.unwrap_sol5 unwrap_sol
    (Continued_search.no_dups
       (make_iface sface)
       ordered_p)


let dups sface args =
  Limit.unwrap_sol6 unwrap_sol
    (Continued_search.dups
       (* must have g=0 as base for others, and
	  f<others to prevent re-opening *)
       (make_iface sface)
       just_f
       ordered_p
       setpos getpos)


(* EOF *)
