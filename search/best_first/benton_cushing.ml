(**

    @author jtd7
    @since 2011-05-25
   J and Will's A* on length algorithm
*)

type 'a node = {
  data : 'a;
  l : float;
  f : float;
  g : float;
  depth : float;
  mutable pos : int;
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
    | Limit.Nothing -> None
    | Limit.Incumbent (q,n) -> Some (n.data, n.g)


let l_then_f_then_g a b =
  (** expansion ordering predicate, and also works for ordering duplicates
      assuming that h is the same for both
      (hence f will be lower when g is lower). *)
  ((a.l : float) < b.l) ||
    (a.l = b.l && a.f < b.f) ||
    (a.l = b.l && a.f = b.f && a.g > b.g)


let just_f a b =
  (** Sorts nodes solely on total cost information *)
  (a.f : float) <= b.f


let setpos n i =
  (** Sets the location of a node, used by dpq's *)
  n.pos <- i


let getpos n =
  (** Returns the position of the node in its dpq.
      Useful for swapping nodes around on the open list *)
  n.pos


let make_expand expand hd =
  (** Takes the domain expand function and a heuristic calculator
      and creates an expand function which returns search nodes. *)
  (fun n ->
     let depth' = n.depth +. 1. in
       List.map (fun (data, g) ->
		   let h,d = hd data in
		     { data = data;
		       l = depth' +. d;
		       f = g +. h;
		       g = g;
		       depth = depth';
		       pos = Dpq.no_position; }) (expand n.data n.g))

let make_wexpand expand hd w =
  (** Takes the domain expand function and a heuristic calculator
      and creates an expand function which returns search nodes. *)
  assert (w >= 1.);
  (fun n ->
     let depth' = n.depth +. 1. in
       List.map (fun (data, g) ->
		   let h,d = hd data in
		     { data = data;
		       l = depth' +. d *. w;
		       f = g +. h;
		       g = g;
		       depth = depth';
		       pos = Dpq.no_position; }) (expand n.data n.g))



let make_sface sface =
  let def_log = (Limit.make_default_logger (fun n -> n.f)
		   (wrap sface.Search_interface.get_sol_length)) in
    Search_interface.make
      ~node_expand:(make_expand sface.Search_interface.domain_expand
		      sface.Search_interface.hd)
      ~goal_p:(wrap sface.Search_interface.goal_p)
      ~key:(wrap sface.Search_interface.key)
      ~hash:sface.Search_interface.hash
      ~equals:sface.Search_interface.equals
      ~halt_on:sface.Search_interface.halt_on
      sface.Search_interface.domain
      {data = sface.Search_interface.initial;
       l = sface.Search_interface.d sface.Search_interface.initial;
       f = sface.Search_interface.h sface.Search_interface.initial;
       g = 0.;
       depth = 0.;
       pos = Dpq.no_position;}
      just_f
      (fun i ->
	 sface.Search_interface.info.Limit.log
	   (Limit.unwrap_info (fun n -> n.data) i);
	 def_log i)


let no_dups sface args =
  (** Performs an A* search from the initial state to a goal,
      for domains with no duplicates. *)
  let search_interface = make_sface sface in
    Limit.unwrap_sol5 unwrap_sol
      (Best_first.search
	 search_interface
	 l_then_f_then_g
	 just_f)


let dups sface args =
  (** Performs an A* search from the initial state to a goal,
      for domains where duplicates are frequently encountered. *)
  let search_interface = make_sface sface in
    Limit.unwrap_sol6 unwrap_sol
      (Continued_search.dups
	 search_interface
	 just_f
	 l_then_f_then_g
	 setpos
	 getpos)


let dd sface args =
  (** Performs an A* search from the initial state to a goal,
      for domains where duplicates are frequently encountered. *)
  let search_interface = make_sface sface in
    Limit.unwrap_sol6 unwrap_sol
      (Continued_search.delay_dups
	 search_interface
	 l_then_f_then_g
	 just_f
	 setpos
	 getpos)


let make_wted_sface wt sface =
  let def_log = (Limit.make_default_logger (fun n -> n.f)
		   (fun n -> truncate n.depth)) in
    Search_interface.make
      ~node_expand:(make_wexpand sface.Search_interface.domain_expand
		      sface.Search_interface.hd wt)
      ~goal_p:(wrap sface.Search_interface.goal_p)
      ~key:(wrap sface.Search_interface.key)
      ~hash:sface.Search_interface.hash
      ~equals:sface.Search_interface.equals
      ~halt_on:sface.Search_interface.halt_on
      sface.Search_interface.domain
      {data = sface.Search_interface.initial;
       f = sface.Search_interface.h sface.Search_interface.initial;
       g = 0.;
       depth = 0.;
       l = sface.Search_interface.d sface.Search_interface.initial;
       pos = Dpq.no_position;}
      just_f
      (fun i ->
	 sface.Search_interface.info.Limit.log
	   (Limit.unwrap_info (fun n -> n.data) i);
	 def_log i)


let wted_dups sface args =
  (** Performs an A* search from the initial state to a goal,
      for domains where duplicates are frequently encountered. *)
  let wt = Search_args.get_float "Wted_astar.dups" args 0 in
  let search_interface = make_wted_sface wt sface in
    Limit.unwrap_sol6 unwrap_sol
      (Continued_search.dups
	 search_interface
	 just_f
	 l_then_f_then_g
	 setpos
	 getpos)


let wted_dd sface args =
  (** Performs an A* search from the initial state to a goal,
      for domains where duplicates are frequently encountered. *)
  let wt = Search_args.get_float "Wted_astar.dups" args 0 in
  let search_interface = make_wted_sface wt sface in
    Limit.unwrap_sol6 unwrap_sol
      (Best_first.search_drop_dups
	 search_interface
	 l_then_f_then_g
	 just_f
	 setpos
	 getpos)

(* EOF *)
