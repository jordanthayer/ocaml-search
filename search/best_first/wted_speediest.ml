(**

    @author jtd7
    @since 2012-07-21
*)

(**
    Greedy search algorithm as through the search interface
    Jordan Thayer - July 2009
*)

type 'a node = {
  data : 'a;          (* Data Payload *)
  wd : float;
  g  : float;          (* Cost of reaching a node *)
  depth : int;        (* Depth of node in tree.  Root @ 0 *)
  d : float;          (* Heuristic at node*)
  mutable pos : int;  (* Position info for dpq *)
}


let wrap f =
  (** takes a function to be applied to the data payload
      such as the goal-test or the domain heuristic and
      wraps it so that it can be applied to the entire
      node *)
  (fun n -> f n.data)

let wrap_2 f =
  (fun a b -> f a.data b.data)


let unwrap_sol s =
  (** Unwraps a solution which is in the form of a search node and presents
      it in the format the domain expects it, which is domain data followed
      by cost *)
  match s with
      Limit.Nothing -> None
    | Limit.Incumbent (q,n) -> Some (n.data, n.g)


let d_then_g a b =
  (* this is probably flawed, we want to look at f next instead *)
  (** expansion ordering predicate, and also works for ordering duplicates
      assuming that h is the same for both
      (hence f will be lower when g is lower). *)
  (a.wd < b.wd) ||
    ((a.wd = b.wd) && (a.d < b.d)) ||
    ((a.wd = b.wd) && (a.d = b.d) && (a.g <= b.g))


let just_g a b =
  (** Sorts nodes solely on total cost information *)
  a.g <= b.g


let setpos n i =
  (** Sets the location of a node, used by dpq's *)
  n.pos <- i


let getpos n =
  (** Returns the position of the node in its dpq.
      Useful for swapping nodes around on the open list *)
  n.pos


let make_expand w expand dist =
  (** Takes the domain expand function and a heuristic calculator
      and creates an expand function which returns search nodes. *)
  assert (w > 0.);
  (fun n ->
    let next_depth = n.depth + 1 in
    let fdepth = w *. (float next_depth) in
    List.map (fun (dat, g) ->
      let d = dist dat in
      { data = dat;
	wd = d /. (d +. fdepth);
	d = d;
	g = g;
	depth = next_depth;
	pos = Dpq.no_position; }) (expand n.data n.g))


let no_dups sface args =
  (** Performs a greedy search from the initial state to a goal,
      for domains with no duplicates. *)
  let wt = Search_args.get_float "Wted_speediest.no_dups" args 0 in
  let search_interface =
    Search_interface.make
      ~node_expand:(make_expand wt sface.Search_interface.domain_expand
		      sface.Search_interface.d)
      ~goal_p:(wrap sface.Search_interface.goal_p)
      ~halt_on:sface.Search_interface.halt_on
      ~hash:sface.Search_interface.hash
      ~equals:sface.Search_interface.equals
      sface.Search_interface.domain
      { data = sface.Search_interface.initial;
	wd = neg_infinity;
	d = neg_infinity;
	g = 0.;
	depth = 0;
	pos = Dpq.no_position;}
    just_g
    (Limit.make_default_logger (fun n -> n.g)
       (wrap sface.Search_interface.get_sol_length)) in
    Limit.unwrap_sol5 unwrap_sol
      (Best_first.search
	 search_interface
	 d_then_g
	 just_g)



let dups_internal ?(silent = false) sface args =
  (** Performs a greedy search from the initial state to a goal,
      for domains where duplicates are frequently encountered. *)
  let wt = Search_args.get_float "Wted_speediest.no_dups" args 0 in
  let search_interface =
    Search_interface.make
      ~node_expand:(make_expand wt sface.Search_interface.domain_expand
		      sface.Search_interface.d)
      ~goal_p:(wrap sface.Search_interface.goal_p)
      ~halt_on:sface.Search_interface.halt_on
      ~key:(wrap sface.Search_interface.key)
      ~hash:sface.Search_interface.hash
      ~equals:(fun a b -> a = b)
      sface.Search_interface.domain
      { data = sface.Search_interface.initial;
	wd = neg_infinity;
	d = neg_infinity;
	g = 0.;
	depth = 0;
	pos = Dpq.no_position;}
      just_g
      (if not silent
       then (Limit.make_default_logger ~silent (fun n -> n.g)
	       (wrap sface.Search_interface.get_sol_length))
       else (fun _ -> ())) in
      (Best_first.search_dups
	 (* must have g=0 as base for others, and
	    f<others to prevent re-opening *)
	 search_interface
	 d_then_g
	 just_g
	 setpos
	 getpos)



let dups sface args =
    Limit.unwrap_sol6 unwrap_sol (dups_internal sface args)



let drop_dups_internal sface args =
  (** Performs a greedy search from the initial state to a goal,
      for domains where duplicates are frequently encountered.
      When duplicates are seen, they are ignored immediately. *)
  let wt = Search_args.get_float "Wted_speediest.no_dups" args 0 in
  let search_interface =
    Search_interface.make
      ~node_expand:(make_expand wt sface.Search_interface.domain_expand
		      sface.Search_interface.d)
      ~goal_p:(wrap sface.Search_interface.goal_p)
      ~halt_on:sface.Search_interface.halt_on
      ~key:(wrap sface.Search_interface.key)
      ~hash:sface.Search_interface.hash
      ~equals:sface.Search_interface.equals
      sface.Search_interface.domain
      { data = sface.Search_interface.initial;
	wd = neg_infinity;
	d = neg_infinity;
	g = 0.;
	depth = 0;
	pos = Dpq.no_position;}
    just_g
    (Limit.make_default_logger (fun n -> n.g)
       (wrap sface.Search_interface.get_sol_length)) in
    (Best_first.search_drop_dups
       (* must have g=0 as base for others, and
	  f<others to prevent re-opening *)
       search_interface
       d_then_g
       just_g
       setpos
       getpos)


let drop_dups sface args =
  Limit.unwrap_sol6 unwrap_sol (drop_dups_internal sface args)


(* EOF *)
