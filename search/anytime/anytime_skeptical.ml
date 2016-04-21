(**

    @author jtd7
    @since 2011-02-20

   Anytime variant of skeptical search algorithm
*)

type 'a node = {
  data : 'a;
  mutable fhp : float;
  f : float;
  g : float;
  d : float;
  h_hat : float;
  h_err : float;
  d_err : float;
  depth : int;
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
      Limit.Nothing -> None
    | Limit.Incumbent (q,n) -> Some (n.data, n.g)


let ordered_p a b =
  (** Ordered predicate used for search.  Compares fhp, then f, then g.
      true if a is better than b. *)
  (a.fhp < b.fhp) ||
  ((a.fhp = b.fhp) &&
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


let make_expand expand hd h_calc =
  (fun n ->
     List.map (fun (data,g) ->
		 let h,d = hd data in
		 let f = g +. h in
		 let c = f -. n.f in
		 let h_err = (if (Math.finite_p c) && c > 0.
			      then ((f -. n.f) /. c) +. n.h_err
			      else n.h_err)
		 and d_err = d -. n.d +. 1. +. n.d_err in
		 let depth = n.depth + 1 in
		 let h_hat = h_calc n in
		   { fhp = neg_infinity (*g +. wt *. h_hat*);
		     data = data;
		     h_err = h_err;
		     d_err = d_err;
		     h_hat = h_hat;
		     depth = depth;
		     f = f;
		     g = g;
		     d = d;
		     pos = Dpq.no_position;}) (expand n.data n.g))


let update wt n =
  n.fhp <- n.g +. wt *. n.h_hat


(********************* Searches *********************************)
let make_iface sface h_calc =
  let def_log = Limit.make_default_logger (fun n -> n.f)
    (wrap sface.Search_interface.get_sol_length) in
  let hd = (Search_interface.get_default_hd sface) in
  let h,d = hd sface.Search_interface.initial in
    Search_interface.make
      ~node_expand:(make_expand sface.Search_interface.domain_expand hd h_calc)
      ~goal_p:(wrap sface.Search_interface.goal_p)
      ~key:(wrap sface.Search_interface.key)
      ~halt_on:sface.Search_interface.halt_on
      ~hash:sface.Search_interface.hash
      ~equals:sface.Search_interface.equals
      sface.Search_interface.domain
      { data = sface.Search_interface.initial;
	fhp = h;
	f = h;
	g = 0.;
	h_hat = h;
	d = d;
	h_err = 0.;
	d_err = 0.;
	depth = 0;
	pos = Dpq.no_position;}
      just_f
      (fun i ->
	 sface.Search_interface.info.Limit.log
	   (Limit.unwrap_info (fun n -> n.data) i);
	 def_log i)



let dups h_calc sface args =
  (** Performs a weighted A* search from the initial state to a goal,
      for domains where duplicates are frequently encountered. *)
  let wtlist = Array.to_list
    (Search_args.get_float_array "Anytime_astar_wtlist.dups" args) in
  let search_interface = make_iface sface h_calc in
    Limit.unwrap_sol6 unwrap_sol
      (Continued_wtlist.dups
	 (* must have g=0 as base for others, and
	    f < others to prevent re-opening *)
	 search_interface
	 ordered_p
	 just_f
	 setpos
	 getpos
	 (List.rev wtlist)
	 update)


let delay_dups h_calc sface args =
  (** Performs a weighted A* search from the initial state to a goal,
      for domains where duplicates are frequently encountered. *)
  let wtlist = Array.to_list
    (Search_args.get_float_array "Anytime_astar_wtlist.delay_dups" args) in
  let search_interface = make_iface sface h_calc in
    Limit.unwrap_sol6 unwrap_sol
      (Continued_wtlist.delay_dups
	 (* must have g=0 as base for others, and
	    f<others to prevent re-opening *)
	 search_interface
	 ordered_p
	 just_f
	 setpos
	 getpos
	 wtlist
	 update)


(***** Some Sample Algorithms ***********************************************)

let austin_dups sface args =
  dups (Path_fd_ss.make_unbounded_correction_austin_hcalc
	  (fun n -> n.f -. n.g) (fun n -> n.d) (fun n -> n.depth)
	  (fun n -> n.h_err) (fun n -> n.d_err)) sface args


let austin_delay sface args =
  delay_dups (Path_fd_ss.make_unbounded_correction_austin_hcalc
		(fun n -> n.f -. n.g) (fun n -> n.d) (fun n -> n.depth)
		(fun n -> n.h_err) (fun n -> n.d_err)) sface args


(* EOF *)
