(**

    @author jtd7
    @since 2011-05-04
   Based on Roni Stern et al's 2011 ICAPS paper
*)

type 'a node = {
  data : 'a;
  f : float;
  g : float;
  potential : float;
  mutable pos : int; }

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

let potential_then_g a b =
  ((a.potential : float) < b.potential) ||
    ((a.potential = b.potential) && (a.f >= b.f))

let better a b =
  (** Sorts nodes solely on total cost information *)
  (a.f : float) <= b.f

let setpos n i =
  (** Sets the location of a node, used by dpq's *)
  n.pos <- i


let getpos n =
  (** Returns the position of the node in its dpq.
      Useful for swapping nodes around on the open list *)
  n.pos


let make_expand expand c hd =
  (** Takes the domain expand function and a heuristic calculator
      and creates an expand function which returns search nodes. *)
  (fun n ->
     (List.map (fun (data, g) ->
		  let h,d = hd data in
		    { data = data;
		      f = g +. h;
		      g = g;
		      (* looks backwards but is equivalent in exp. order *)
		      potential = (h *. d) /. (c -. g) ;
		      pos = Dpq.no_position; })
	(expand n.data n.g)))


let make_sface sface cost_bound =
  let def_log = Limit.make_default_logger (fun n -> n.f)
    (wrap sface.Search_interface.get_sol_length) in
  let dummy_incumbent = { data = sface.Search_interface.initial;
			  potential = nan;
			  f = cost_bound;
			  g = cost_bound;
			  pos = -1337;} in
  let h = sface.Search_interface.h sface.Search_interface.initial in
    Search_interface.make
      ~node_expand:(make_expand sface.Search_interface.domain_expand cost_bound
		      sface.Search_interface.hd)
      ~goal_p:(wrap sface.Search_interface.goal_p)
      ~key:(wrap sface.Search_interface.key)
      ~hash:sface.Search_interface.hash
      ~equals:sface.Search_interface.equals
      ~halt_on:sface.Search_interface.halt_on
      ~incumbent:(Limit.Incumbent (0., dummy_incumbent))
      sface.Search_interface.domain
      {data = sface.Search_interface.initial;
       f = h;
       g = 0.;
       potential = h /. cost_bound;
       pos = Dpq.no_position;}
      better
      (fun i ->
	 sface.Search_interface.info.Limit.log
	   (Limit.unwrap_info (fun n -> n.data) i);
	 def_log i)


let no_dups sface args =
  let cost_bound = Search_args.get_float "Potential Search" args 0 in
  let search_interface = make_sface sface cost_bound in
    Limit.unwrap_sol5 unwrap_sol
      (Best_first.search
	 search_interface
	 potential_then_g
	 better)


let dups sface args =
  (** Performs an A* search from the initial state to a goal,
      for domains where duplicates are frequently encountered. *)
  let cost_bound = Search_args.get_float "Potential Search" args 0 in
  let search_interface = make_sface sface cost_bound in
    Limit.unwrap_sol6 unwrap_sol
      (Best_first.search_dups
	 (* must have g=0 as base for others, and
	    f<others to prevent re-opening *)
	 search_interface
	 potential_then_g
	 better
	 setpos
	 getpos)



