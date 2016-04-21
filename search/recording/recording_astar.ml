(** A* search variant for recording search data online *)


type 'a node = {
  parent : 'a node;
  data : 'a;          (* Data Payload *)
  f : float;          (* Total cost of a node*)
  g : float;          (* Cost of reaching a node *)
  h : float;
  d : float;
  rev_h : float;
  rev_d : float;
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


let f_then_g a b =
  (** expansion ordering predicate, and also works for ordering duplicates
      assuming that h is the same for both
      (hence f will be lower when g is lower). *)
  (a.f < b.f) ||
  ((a.f = b.f) && (a.g >= b.g))


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


let make_expand recorder expand f_hd r_hd =
  (** Takes the domain expand function and a heuristic calculator
      and creates an expand function which returns search nodes. *)
  (fun n ->
     let children =
       List.map (fun (dat, g) ->
		   let h,d = f_hd dat
		   and rh, rd = r_hd dat in
		     { data = dat;
		       parent = n;
		       f = g +. h;
		       g = g;
		       h = h;
		       d = d;
		       rev_h = rh;
		       rev_d = rd;
		       depth = n.depth + 1;
		       pos = Dpq.no_position; }) (expand n.data n.g)  in
       recorder n n.parent children;
       children)


let make_root dat f_hd =
  let h,d = f_hd dat in
  let rec root =
    { data = dat;
      parent = root;
      f = h;
      g = 0.;
      h = h;
      d = d;
      rev_h = 0.;
      rev_d = 0.;
      depth = 0;
      pos = Dpq.no_position;} in root


(************************* Searches Base ************************************)
let base_interface sface node_record create_expand =
  let init = Search_interface.make
    ~goal_p:(fun n -> sface.Search_interface.goal_p n.data)
    ~halt_on:sface.Search_interface.halt_on
    ~hash:sface.Search_interface.hash
    ~key:(wrap sface.Search_interface.key)
    ~equals:sface.Search_interface.equals
    sface.Search_interface.domain
    (make_root sface.Search_interface.initial sface.Search_interface.hd)
    just_f
    (Limit.make_default_logger (fun n -> n.f)
       (wrap sface.Search_interface.get_sol_length)) in
  let recorder = node_record init.Search_interface.info in
    (Search_interface.alter
       ~node_expand:(Some (create_expand
			     recorder
			     sface.Search_interface.domain_expand
			     sface.Search_interface.hd
			     sface.Search_interface.rev_hd))
       ~goal_p:(Some
		  (fun n ->
		     let goal = sface.Search_interface.goal_p n.data in
		       if goal then recorder n n.parent [];
		       goal))
       init)


let nogoal_interface sface node_record create_expand =
  let init = Search_interface.make
    ~goal_p:(fun _ -> false)
    ~halt_on:sface.Search_interface.halt_on
    ~hash:sface.Search_interface.hash
    ~key:(wrap sface.Search_interface.key)
    ~equals:sface.Search_interface.equals
    sface.Search_interface.domain
    (make_root sface.Search_interface.initial sface.Search_interface.hd)
    just_f
    (Limit.make_default_logger (fun n -> n.f)
       (wrap sface.Search_interface.get_sol_length)) in
    (Search_interface.alter
       ~node_expand:(Some (create_expand
			     (node_record init.Search_interface.info)
			     sface.Search_interface.domain_expand
			     sface.Search_interface.hd
			     sface.Search_interface.rev_hd)) init)


let no_dups ?(create_expand = make_expand) sface node_record queue_record =
  (** Performs an A* search from the initial state to a goal,
      for domains with no duplicates. *)
  let search_interface = base_interface sface node_record create_expand in
    Limit.unwrap_sol5 unwrap_sol
      (Best_first.search
	 ~record:queue_record
	 search_interface
	 f_then_g
	 just_f)


let dups ?(create_expand = make_expand) sface node_record queue_record =
  (** Performs an A* search from the initial state to a goal,
      for domains where duplicates are frequently encountered. *)
  let search_interface = base_interface sface node_record create_expand in
    Limit.unwrap_sol6 unwrap_sol
      (Best_first.search_dups
	 ~record:queue_record
	 search_interface
	 f_then_g
	 just_f
	 setpos
	 getpos)

let all_no_dups ?(create_expand = make_expand) sface node_record queue_record =
  (** Performs an A* search from the initial state to a goal,
      for domains with no duplicates. *)
  let search_interface = nogoal_interface sface node_record create_expand in
    Limit.unwrap_sol5 unwrap_sol
      (Best_first.search
	 ~record:queue_record
	 search_interface
	 f_then_g
	 just_f)



let all_dups ?(create_expand = make_expand) sface node_record queue_record =
  (** Performs an A* search from the initial state to a goal,
      for domains where duplicates are frequently encountered. *)
  let search_interface = nogoal_interface sface node_record create_expand in
    Limit.unwrap_sol6 unwrap_sol
      (Best_first.search_dups
	 ~record:queue_record
	 search_interface
	 f_then_g
	 just_f
	 setpos
	 getpos)


let drop ?(create_expand = make_expand) sface node_record queue_record =
  (** Performs an A* search from the initial state to a goal,
      for domains where duplicates are frequently encountered. *)
  let search_interface = base_interface sface node_record create_expand in
    Limit.unwrap_sol6 unwrap_sol
      (Best_first.search_drop_dups
	 ~record:queue_record
	 search_interface
	 f_then_g
	 just_f
	 setpos
	 getpos)

let drop_nogoal ?(create_expand = make_expand) sface node_record queue_record =
  (** Performs an A* search from the initial state to a goal,
      for domains where duplicates are frequently encountered. *)
  let search_interface = nogoal_interface sface node_record create_expand in
    Limit.unwrap_sol6 unwrap_sol
      (Best_first.search_drop_dups
	 ~record:queue_record
	 search_interface
	 f_then_g
	 just_f
	 setpos
	 getpos)



(****************************************************************************)

(*** Recorders ***)
let exp_rec key_printer key =
  Recorders.expansion_recorder key_printer (wrap key)
    (fun n -> n.g) (fun n -> n.depth) (fun n -> n.f)

let queue_rec key_printer key = Recorders.dpq_recorder key_printer (wrap key)

let truth_rec key_printer key =
  (* note that for simplicities sake, forward heuristics still point forward*)
  Recorders.truth_recorder key_printer (wrap key) (fun n -> n.g)
    (fun n -> n.depth) (fun n -> n.h) (fun n -> n.d)

let tab_rec key_printer key =
  (* note that for simplicities sake, forward heuristics still point forward*)
  Recorders.tab_delim_recorder key_printer (wrap key) (fun n -> n.g)
    (fun n -> n.depth) (fun n -> n.h) (fun n -> n.d) (fun n -> n.rev_h)
    (fun n -> n.rev_d)


(****************************************************************************)

let sanity_nodups sface args =
  Search_args.is_empty "Recording_astar.sanity_nodups" args;
  no_dups sface Recorders.no_node_record Recorders.none

and sanity_dups sface args =
  Search_args.is_empty "Recording_astar.sanity_dups" args;
  dups sface Recorders.no_node_record Recorders.none


and expand_recorder_nodups sface args =
  Search_args.is_empty "Recording_astar.expand_recorder_nodups" args;
  no_dups sface (exp_rec sface.Search_interface.key_printer
		   sface.Search_interface.key) Recorders.none

and expand_recorder_dups sface args =
  Search_args.is_empty "Recording_astar.expand_recorder_dups" args;
  dups sface (exp_rec sface.Search_interface.key_printer
		sface.Search_interface.key) Recorders.none

and queue_recorder_nodups sface args =
  Search_args.is_empty "Recording_astar.queue_recorder_nodups" args;
  no_dups sface Recorders.no_node_record
    (queue_rec sface.Search_interface.key_printer sface.Search_interface.key)

and queue_recorder_dups sface args =
  Search_args.is_empty "Recording_astar.queue_recorder_dups" args;
  dups sface Recorders.no_node_record
    (queue_rec sface.Search_interface.key_printer sface.Search_interface.key)

and truth_record_nodups sface args =
  Search_args.is_empty "Recording_astar.truth_recorder_nodups" args;
  no_dups sface
    (truth_rec sface.Search_interface.key_printer
       sface.Search_interface.key) Recorders.none

and truth_record_dups sface args =
  Search_args.is_empty "Recording_astar.truth_recorder_dups" args;
  dups sface (truth_rec sface.Search_interface.key_printer
		sface.Search_interface.key) Recorders.none

and tab_record_nodups sface args =
  Search_args.is_empty "Recording_astar.tab_record_nodups" args;
  no_dups sface (tab_rec sface.Search_interface.key_printer
		   sface.Search_interface.key) Recorders.none

and tab_record_dups sface args =
  (** when used for recording truth this expects to be reversed *)
  Search_args.is_empty "Recording_astar.tab_recorder_dups" args;
  dups sface (tab_rec sface.Search_interface.key_printer
		sface.Search_interface.key) Recorders.none


(* EOF *)
