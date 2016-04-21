(**
   Tie breaking study for weighted A* search
   Jordan - July 2009
*)

type 'a node = {
  data : 'a;          (* Data Payload *)
  wf: float;          (* Search order w/ a weighted heuristic value *)
  f : float;          (* Total cost of a node*)
  d : float;
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
  (a.wf < b.wf) ||
  ((a.wf = b.wf) &&
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


let make_expand expand hd wt =
  (** Takes the domain [expand] function and a [h]euristic calculator.
      Needs the [wt] which will be applied to the heuristic.
      Creates an expand function which returns search nodes. *)
  (fun n ->
     List.map (fun (dat, g) ->
		 let h,d = hd dat in
		   { data = dat;
		     wf = g +. wt *.h;
		     d = d;
		     f = g +. h;
		     g = g;
		     depth = n.depth + 1;
		     pos = Dpq.no_position; }) (expand n.data n.g))



let g_tie_breaking a b =
  (a.wf < b.wf) ||
    ((a.wf = b.wf) &&
       a.g >= b.g)


let d_tie_breaking a b =
    (a.wf < b.wf) ||
    ((a.wf = b.wf) &&
       a.d >= b.d)


let f_tie_breaking a b =
    (a.wf < b.wf) ||
    ((a.wf = b.wf) &&
       a.f >= b.f)


let h_tie_breaking a b =
    (a.wf < b.wf) ||
    ((a.wf = b.wf) &&
       (a.f -. a.g) >= (b.f -. b.g))


let depth_tie_breaking a b =
    (a.wf < b.wf) ||
    ((a.wf = b.wf) &&
       a.depth >= b.depth)


let random_tie_breaking a b =
  (a.wf < b.wf) ||
    ((a.wf = b.wf) &&
       ((Random.float 1.) > 0.5))


(*************************** Internal search calls **********************)
let no_dups ordered sface args =
  (** Performs a weighted A* search from the initial state to a goal,
      for domains where duplicates are frequently encountered. *)
  let wt = Search_args.get_float "Wastar_tiebreaking.no_dups" args 0 in
  let search_interface = Search_interface.make
    ~node_expand:(make_expand sface.Search_interface.domain_expand
		    sface.Search_interface.hd wt)
    ~goal_p:(wrap sface.Search_interface.goal_p)
    ~halt_on:sface.Search_interface.halt_on
    ~hash:sface.Search_interface.hash
    ~equals:sface.Search_interface.equals
    sface.Search_interface.domain
    { data = sface.Search_interface.initial;
      wf = neg_infinity;
      f = neg_infinity;
      d = neg_infinity;
      g = 0.;
      depth = 0;
      pos = Dpq.no_position}
    just_f
    (Limit.make_default_logger (fun n -> n.f)
       (wrap sface.Search_interface.get_sol_length)) in
  Limit.unwrap_sol5 unwrap_sol
    (Best_first.search
       (* must have g=0 as base for others, and
	  f<others to prevent re-opening *)
       search_interface
       ordered
       just_f)


let dups ordered sface args =
  (** Performs a weighted A* search from the initial state to a goal,
      for domains where duplicates are frequently encountered. *)
  let wt = Search_args.get_float "Wastar_tiebreaking.dups" args 0 in
  let search_interface = Search_interface.make
    ~node_expand:(make_expand sface.Search_interface.domain_expand
		    sface.Search_interface.hd wt)
    ~goal_p:(wrap sface.Search_interface.goal_p)
    ~halt_on:sface.Search_interface.halt_on
    ~key:(wrap sface.Search_interface.key)
    ~hash:sface.Search_interface.hash
    ~equals:sface.Search_interface.equals
    sface.Search_interface.domain
    { data = sface.Search_interface.initial;
      wf = neg_infinity;
      f = neg_infinity;
      d = neg_infinity;
      g = 0.;
      depth = 0;
      pos = Dpq.no_position}
    just_f
    (Limit.make_default_logger (fun n -> n.f)
       (wrap sface.Search_interface.get_sol_length)) in
  Limit.unwrap_sol6 unwrap_sol
    (Best_first.search_dups
       (* must have g=0 as base for others, and
	  f<others to prevent re-opening *)
       search_interface
       ordered
       just_f
       setpos getpos)


let drop_dups ordered sface args =
  (** Performs a weighted A* search from the initial state to a goal,
      for domains where duplicates are frequently encountered. *)
  let wt = Search_args.get_float "Wastar_tiebreaking.drop_dups" args 0 in
  let search_interface = Search_interface.make
    ~node_expand:(make_expand sface.Search_interface.domain_expand
		    sface.Search_interface.hd wt)
    ~goal_p:(wrap sface.Search_interface.goal_p)
    ~halt_on:sface.Search_interface.halt_on
    ~key:(wrap sface.Search_interface.key)
    ~hash:sface.Search_interface.hash
    ~equals:sface.Search_interface.equals
    sface.Search_interface.domain
    { data = sface.Search_interface.initial;
      wf = neg_infinity;
      f = neg_infinity;
      d = neg_infinity;
      g = 0.;
      depth = 0;
      pos = Dpq.no_position}
    just_f
    (Limit.make_default_logger (fun n -> n.f)
       (wrap sface.Search_interface.get_sol_length)) in
  Limit.unwrap_sol6 unwrap_sol
    (Best_first.search_drop_dups
       (* must have g=0 as base for others, and
	  f<others to prevent re-opening *)
       search_interface
       ordered
       just_f
       setpos getpos)


(************************** Calls ************************************)

let g sface wt = no_dups g_tie_breaking sface wt
and g_dups sface wt = dups g_tie_breaking sface wt
and g_dd sface wt = drop_dups g_tie_breaking sface wt

and h sface wt = no_dups h_tie_breaking sface wt
and h_dups sface wt = dups h_tie_breaking sface wt
and h_dd sface wt = drop_dups h_tie_breaking sface wt

and f sface wt = no_dups f_tie_breaking sface wt
and f_dups sface wt = dups f_tie_breaking sface wt
and f_dd sface wt = drop_dups f_tie_breaking sface wt

and d sface wt = no_dups d_tie_breaking sface wt
and d_dups sface wt = dups d_tie_breaking sface wt
and d_dd sface wt = drop_dups d_tie_breaking sface wt

and depth sface wt = no_dups depth_tie_breaking sface wt
and depth_dups sface wt = dups depth_tie_breaking sface wt
and depth_dd sface wt = drop_dups depth_tie_breaking sface wt

and random sface wt = no_dups random_tie_breaking sface wt
and random_dups sface wt = dups random_tie_breaking sface wt
and random_dd sface wt = drop_dups random_tie_breaking sface wt


(* EOF *)
