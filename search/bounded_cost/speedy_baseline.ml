(**

    @author jtd7
    @since 2011-05-03

   Speedy best first search for cost bounded search.
*)

type 'a node = {
  data : 'a;          (* Data Payload *)
  d : float;          (* Heuristic at node*)
  f : float;          (* admissible estimate of completing a node *)
  g : float;
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
    | Limit.Incumbent (q,n) -> (if n.pos = -1337 then None
				else Some (n.data, n.f))

let d_then_f a b =
  (** expansion ordering predicate, and also works for ordering duplicates
      assuming that h is the same for both
      (hence f will be lower when g is lower). *)
  (a.d < b.d) ||
  ((a.d = b.d) && (a.f <= b.f))

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


let make_expand expand hd =
  (** Takes the domain expand function and a heuristic calculator
      and creates an expand function which returns search nodes. *)
  (fun n ->
     List.map (fun (dat, g) ->
		 let h,d = hd dat in
		 { data = dat;
		   d = d;
		   f = g +. h;
		   g = g;
		   pos = Dpq.no_position; }) (expand n.data n.g))


(*** Call em ***)

let no_dups sface args =
  (** Performs a greedy search from the initial state to a goal,
      for domains with no duplicates. *)
  let cost_bound = Search_args.get_float "Speedy_baseline" args 0 in
  let dummy_incumbent = { data = sface.Search_interface.initial;
			  d = nan;
			  f = cost_bound;
			  g = cost_bound;
			  pos = -1337;} in
  let h,d = sface.Search_interface.hd sface.Search_interface.initial in
  let search_interface =
    Search_interface.make
      ~node_expand:(make_expand sface.Search_interface.domain_expand
		      sface.Search_interface.hd)
      ~goal_p:(wrap sface.Search_interface.goal_p)
      ~halt_on:sface.Search_interface.halt_on
      ~hash:sface.Search_interface.hash
      ~equals:sface.Search_interface.equals
      ~incumbent:(Limit.Incumbent (0., dummy_incumbent))
      sface.Search_interface.domain
      { data = sface.Search_interface.initial;
	d = d;
	f = h;
	g = 0.;
	pos = Dpq.no_position;}
    just_f
    (Limit.make_default_logger (fun n -> n.g)
       (wrap sface.Search_interface.get_sol_length)) in
	Limit.unwrap_sol5 unwrap_sol
      (Best_first.search
	 search_interface
	 d_then_f
	 just_f)


let dups sface args =
  (** Performs a greedy search from the initial state to a goal,
      for domains with no duplicates. *)
  let cost_bound = Search_args.get_float "Speedy_baseline" args 0 in
  let dummy_incumbent = { data = sface.Search_interface.initial;
			  d = nan;
			  f = cost_bound;
			  g = cost_bound;
			  pos = -1337;} in
  let h,d = sface.Search_interface.hd sface.Search_interface.initial in
  let search_interface =
    Search_interface.make
      ~node_expand:(make_expand sface.Search_interface.domain_expand
		      sface.Search_interface.hd)
      ~goal_p:(wrap sface.Search_interface.goal_p)
      ~halt_on:sface.Search_interface.halt_on
      ~hash:sface.Search_interface.hash
      ~equals:sface.Search_interface.equals
      ~key:(wrap sface.Search_interface.key)
      ~incumbent:(Limit.Incumbent (0., dummy_incumbent))
      sface.Search_interface.domain
      { data = sface.Search_interface.initial;
	d = d;
	f = h;
	g = 0.;
	pos = Dpq.no_position;}
    just_f
    (Limit.make_default_logger (fun n -> n.g)
       (wrap sface.Search_interface.get_sol_length)) in
	Limit.unwrap_sol6 unwrap_sol
      (Best_first.search_dups
	 search_interface
	 d_then_f
	 just_f
	 setpos
	 getpos)


let dd sface args =
  (** Performs a greedy search from the initial state to a goal,
      for domains with no duplicates.  Note, not complete *)
  let cost_bound = Search_args.get_float "Speedy_baseline" args 0 in
  let dummy_incumbent = { data = sface.Search_interface.initial;
			  d = nan;
			  f = cost_bound;
			  g = cost_bound;
			  pos = -1337;} in
  let h,d = sface.Search_interface.hd sface.Search_interface.initial in
  let search_interface =
    Search_interface.make
      ~node_expand:(make_expand sface.Search_interface.domain_expand
		      sface.Search_interface.hd)
      ~goal_p:(wrap sface.Search_interface.goal_p)
      ~halt_on:sface.Search_interface.halt_on
      ~hash:sface.Search_interface.hash
      ~equals:sface.Search_interface.equals
      ~key:(wrap sface.Search_interface.key)
      ~incumbent:(Limit.Incumbent (0., dummy_incumbent))
      sface.Search_interface.domain
      { data = sface.Search_interface.initial;
	d = d;
	f = h;
	g = 0.;
	pos = Dpq.no_position;}
    just_f
    (Limit.make_default_logger (fun n -> n.g)
       (wrap sface.Search_interface.get_sol_length)) in
	Limit.unwrap_sol6 unwrap_sol
      (Best_first.search_drop_dups
	 search_interface
	 d_then_f
	 just_f
	 setpos
	 getpos)

(* EOF *)



