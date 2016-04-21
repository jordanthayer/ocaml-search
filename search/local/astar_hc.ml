(** Astar based hill climber - Jordan Feb 2010 *)

type 'a node = {
  data : 'a;          (* Data Payload *)
  f : float;          (* Total cost of a node*)
  h : float;
  g : float;          (* Cost of reaching a node *)
}

let just_f a b =
  (** Sorts nodes solely on total cost information *)
  (a.f : float) <= b.f


let better a b =
  a.h < b.h

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



let make_expand expand h =
  (** Takes the domain expand function and a heuristic calculator
      and creates an expand function which returns search nodes. *)
  (fun n ->
     List.map (fun (d, g) -> let hv = h d in
		 { data = d;
		   f = g +. hv;
		   h = hv;
		   g = g;}) (expand n.data n.g))


let make_sface sface =
  let def_log = Limit.make_default_logger (fun n -> n.f)
    (wrap sface.Search_interface.get_sol_length) in
    Search_interface.make
      ~node_expand:(make_expand sface.Search_interface.domain_expand
		      sface.Search_interface.h)
      ~goal_p:(wrap sface.Search_interface.goal_p)
      ~key:(wrap sface.Search_interface.key)
      ~hash:sface.Search_interface.hash
      ~equals:sface.Search_interface.equals
      ~halt_on:sface.Search_interface.halt_on
      ~key_print:sface.Search_interface.key_printer
      sface.Search_interface.domain
      {data = sface.Search_interface.initial;
       f = infinity;
       h = infinity;
       g = 0.;}
      just_f
      (fun i ->
	 sface.Search_interface.info.Limit.log
	   (Limit.unwrap_info (fun n -> n.data) i);
	 def_log i)


let no_dups sface args =
  Search_args.is_empty "Astar_hc.no_dups" args;
  let search_interface = make_sface sface in
    Limit.unwrap_sol5 unwrap_sol
      (Limit.results5
	 (Hill_climbing.search
	    search_interface
	    better))

let dups sface args =
  Search_args.is_empty "Astar_hc.dups" args;
  let search_interface = make_sface sface in
    Limit.unwrap_sol6 unwrap_sol
      (Limit.results6
	 (Hill_climbing.search
	    search_interface
	    better))

let no_dups_enforced sface args =
  Search_args.is_empty "Astar_hc.no_dups" args;
  let search_interface = make_sface sface in
    Limit.unwrap_sol5 unwrap_sol
      (Limit.results5
	 (Hill_climbing.enforced_search
	    search_interface
	    better))

let dups_enforced sface args =
  Search_args.is_empty "Astar_hc.dups" args;
  let search_interface = make_sface sface in
    Limit.unwrap_sol6 unwrap_sol
      (Limit.results6
	 (Hill_climbing.enforced_search
	    search_interface
	    better))


let no_dups_nextbest sface args =
  Search_args.is_empty "Astar_hc.no_dups" args;
  let search_interface = make_sface sface in
    Limit.unwrap_sol5 unwrap_sol
      (Limit.results5
	 (Hill_climbing.next_best_search
	    search_interface
	    better))

let dups_nextbest sface args =
  Search_args.is_empty "Astar_hc.dups" args;
  let search_interface = make_sface sface in
    Limit.unwrap_sol6 unwrap_sol
      (Limit.results6
	 (Hill_climbing.next_best_search
	    search_interface
	    better))


let no_dups_random sface args =
  Search_args.is_empty "Astar_hc.no_dups" args;
  let search_interface = make_sface sface in
    Limit.unwrap_sol5 unwrap_sol
      (Limit.results5
	 (Hill_climbing.random_walk_search
	    search_interface
	    better))

let dups_random sface args =
  Search_args.is_empty "Astar_hc.dups" args;
  let search_interface = make_sface sface in
    Limit.unwrap_sol6 unwrap_sol
      (Limit.results6
	 (Hill_climbing.random_walk_search
	    search_interface
	    better))
(* EOF *)
