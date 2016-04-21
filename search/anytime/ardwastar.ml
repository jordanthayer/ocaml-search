(** Anytime Repairing Dynamically Weighted A*
    Jordan Thayer - Feb 2010 *)


type 'a node = {
  data : 'a;          (* Data Payload *)
  mutable wf: float;          (* Search order w/ a weighted heuristic value *)
  f : float;          (* Total cost of a node*)
  g : float;          (* Cost of reaching a node *)
  h : float;
  depth : int;        (* Depth of node in tree.  Root @ 0 *)
  mutable pos_fp : int;  (* Position info for dpq *)
  mutable pos_f : int;  (* Position info for dpq *)
}


let update goal_depth n wt =
  let epsilon = wt -. 1. in
  let factor = Math.fmax 1.
    (1. +. (epsilon *. (1. -. ((float n.depth) /. goal_depth)))) in
    n.wf <- n.g +. wt *. factor *. n.h

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
  (a.wf < b.wf) ||
  ((a.wf = b.wf) &&
   ((a.f < b.f) ||
    ((a.f = b.f) &&
     (a.g >= b.g))))


let just_f a b =
  a.f <= b.f


let setpos_fp n i =
  n.pos_fp <- i


let getpos_fp n =
  n.pos_fp


let setpos_f n i =
  n.pos_f <- i

let getpos_f n =
  n.pos_f


let make_expand expand h =
    (fun n ->
       List.map (fun (d, g) ->
		   let hv = h d in
		     { data = d;
		       wf = neg_infinity;
		       f = g +. hv;
		       g = g;
		       h = hv;
		       depth = n.depth + 1;
		       pos_fp = Dpq.no_position;
		       pos_f = Dpq.no_position;}) (expand n.data n.g))

(****************************** Searches *******************************)
let make_sface sface =
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
      wf = neg_infinity;
      f = neg_infinity;
      h = neg_infinity;
      g = 0.;
      depth = 0;
      pos_fp = Dpq.no_position;
      pos_f = Dpq.no_position;}
    just_f
    (Limit.make_default_logger (fun n -> n.f)
       (wrap sface.Search_interface.get_sol_length))


let no_dups sface args =
  (** Performs ARA* on domains with few duplicate states. *)
  let wt = Search_args.get_float "Ardwastar.no_dups" args 0 in
  let wts = Arastar.mk_wtlist wt 0.2 in
  let search_interface = make_sface sface in
    assert (wts <> []);
    Limit.unwrap_sol5 unwrap_sol
      (Repairing_search.no_dups
	 search_interface
	 wts
	 ordered_p
	 just_f
	 just_f
	 just_f
	 (update (sface.Search_interface.d sface.Search_interface.initial))
	 setpos_fp
	 setpos_f
	 getpos_fp
	 getpos_f
	 (fun n -> n.f))


let dups sface args =
  (** Performs ARA* on domains with duplicate states. *)
  let wt = Search_args.get_float "Ardwastar.dups" args 0 in
  let wts = Arastar.mk_wtlist wt 0.2 in
  let search_interface = make_sface sface in
    assert (wts <> []);
    Limit.unwrap_sol6 unwrap_sol
      (Repairing_search.dups
	 search_interface
	 wts
	 ordered_p
	 just_f
	 just_f
	 just_f
	 (update (sface.Search_interface.d sface.Search_interface.initial))
	 setpos_fp
	 setpos_f
	 getpos_fp
	 getpos_f
	 (fun n -> n.f))

(* EOF *)
