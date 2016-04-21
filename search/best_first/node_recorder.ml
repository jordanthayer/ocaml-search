

type 'a node = {
  data : 'a;          (* Data Payload *)
  f : float;          (* Total cost of a node*)
  g : float;          (* Cost of reaching a node *)
  d : float;
  depth : int;        (* Depth of node in tree.  Root @ 0 *)
  mutable pos : int;  (* Position info for dpq *)
  parent : 'a node;
}

let rec print_node_helper n = 
  if (n.parent == n) then ""
    (*(Printf.sprintf "%f %f %f %d\n" n.f n.g n.d n.depth)*)
  else (Printf.sprintf "%f %f %f %f %d\n" (n.f -. n.g) n.f n.g n.d n.depth) ^
    (print_node_helper n.parent)

let print_node_status name n =
  let to_print = print_node_helper n in
  let outch = open_out_gen [Open_append] 0 name in
    Printf.fprintf outch "h f g d depth\n%s" to_print;
    close_out outch

let wrap f =
  (** takes a function to be applied to the data payload
      such as the goal-test or the domain heuristic and
      wraps it so that it can be applied to the entire
      node *)
  (fun n -> f n.data)



let f_then_g a b =
  (** expansion ordering predicate, and also works for ordering duplicates
      assuming that h is the same for both
      (hence f will be lower when g is lower). *)
  ((a.f : float) < b.f) ||
  ((a.f = b.f) && (a.g >= b.g))


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



let make_expand expand h dist =
  (** Takes the domain expand function and a heuristic calculator
      and creates an expand function which returns search nodes. *)
  (fun n ->
     List.filter (fun n -> n.f < infinity)
     (List.map (fun (d, g) -> {data = d;
			       f = g +. (h d);
			       g = g;
			       d = (dist d);
			       depth = n.depth + 1;
			       pos = Dpq.no_position; 
			       parent = n;
			      }) (expand n.data n.g)))


let make_sface sface =
  let def_log = Limit.make_default_logger (fun n -> n.f)
    (wrap sface.Search_interface.get_sol_length) in
  let rec n = 
    {data = sface.Search_interface.initial;
     f = neg_infinity;
     g = 0.;
     d = neg_infinity;
     depth = 0;
     pos = Dpq.no_position;
     parent = n;} in
    Search_interface.make
      ~node_expand:(make_expand sface.Search_interface.domain_expand
		      sface.Search_interface.h
		      sface.Search_interface.d
		   )
      ~goal_p:(wrap sface.Search_interface.goal_p)
      ~key:(wrap sface.Search_interface.key)
      ~hash:sface.Search_interface.hash
      ~equals:sface.Search_interface.equals
      ~halt_on:sface.Search_interface.halt_on
      sface.Search_interface.domain
      n
      just_f
      (fun i ->
	 sface.Search_interface.info.Limit.log
	   (Limit.unwrap_info (fun n -> n.data) i);
	 def_log i)

let unwrap_sol filename s =
  (** Unwraps a solution which is in the form of a search node and presents
      it in the format the domain expects it, which is domain data followed
      by cost *)
  match s with
      Limit.Nothing -> None
    | Limit.Incumbent (q,n) -> 
	(
	  print_node_status filename n;
	  Some (n.data, n.g)
	)



let dups sface args =
  (** Performs an A* search from the initial state to a goal,
      for domains where duplicates are frequently encountered. *)
  let filename = Search_args.get_string "solution printing astar" args
    0 in
  let search_interface = make_sface sface in
    Limit.unwrap_sol6 (unwrap_sol filename)
      (Best_first.search_dups
	 (* must have g=0 as base for others, and
	    f<others to prevent re-opening *)
	 search_interface
	 f_then_g
	 just_f
	 setpos
	 getpos)

