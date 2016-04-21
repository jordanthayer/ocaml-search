(**

   Christopher Wilt

   March 23, 2011

   Simplified MA* per Stuary Russel ECAI'92 paper

   This algorithm appears to be implemented correctly, but according
   to Khorsland it can go into infinite loops on graphs, which appears
   to be the case because I've seen it loop infinitely.  Not
   recommended for use for this reason.

   There is an extension that figured out how the hell to deal with
   graphs, but its complicated as hell.

*)

type 'a node =
    {
      data : 'a;
      mutable wf: float;          (* Search order w/ a weighted heuristic value *)
  mutable f : float;          (* Total cost of a node*)
  mutable cheapest_child : float;
  g : float;          (* Cost of reaching a node *)
  depth : int;        (* Depth of node in tree.  Root @ 0 *)
  mutable pos : int;  (* Position info for dpq *)
  parent: 'a node;
  mutable expanded: bool; (*Tells the node if it has been expanded yet.*)
  mutable stand_in: bool;
  mutable children: 'a node list;
}


let set_expanded n new_value = 
  n.expanded <- new_value

let set_stand_in n new_value = 
  n.stand_in <- new_value

let check_values n = 
  if(n.expanded && n.stand_in) then ()
  else if(not n.expanded && not n.stand_in) then ()
  else if(not n.expanded && n.stand_in) then failwith "bad nonsense"
  else ()


let print_node kp n message= 
  Printf.fprintf stderr "--------------------------------------------%!";
  Printf.fprintf stderr "\n%s:\n%sh: %f g: %f expanded: %b stand_in: %b\n%!" message
    (kp n.data)
    (n.f -. n.g)
    n.g n.expanded n.stand_in;
  Printf.fprintf stderr "--------------------------------------------\n%!"



let is_done n = 
  n.expanded


let backup n = 
  let rec backup_helper tn f_v = 
    if(n == tn) then ()
    else if(tn.f < f_v) then
      (
	assert(f_v < 100000.0);
	tn.f <- f_v;
	backup_helper tn.parent f_v;)
    else ()
  in 
    if(n.cheapest_child > 1000000.0) then
      (
	failwith "cheapest child too expensive";
      );
    backup_helper n.parent n.cheapest_child


let wrap f =
  (** takes a function to be applied to the data payload
      such as the goal-test or the domain heuristic and
      wraps it so that it can be applied to the entire
      node *)
  (fun n -> f n.data)


let wrap2 f = 
  (fun n v -> f n.data v)


let unwrap_sol s =
  (** Unwraps a solution which is in the form of a search node and presents
      it in the format the domain expects it, which is domain data followed
      by cost *)
  match s with
      Limit.Nothing -> None
    | Limit.Incumbent (q,n) -> Some (n.data, n.g)


let ordered_p a b =
  (** Ordered predicate used for search.  Compares f', then depth
      true if a is better than b.
  *)
  (a.wf < b.wf) ||
    ((a.wf = b.wf) &&
       (a.depth > b.depth))

let just_f a b =
  (** Sorts nodes solely on total cost information *)
  a.f <= b.f

let better_p a b =
  (** Sorts nodes solely on total already incurred cost information *)
  a.g <= b.g

let setpos n i =
  (** Sets the location of a node, used by dpq's *)
  n.pos <- i


let getpos n =
  (** Returns the position of the node in its dpq.
      Useful for swapping nodes around on the open list *)
  n.pos


let make_expand expand h wt =
  (** Takes the domain [expand] function and a [h]euristic calculator.
      Needs the [wt] which will be applied to the heuristic.
      Creates an expand function which puts the children on the
      parent to be processed as desired. *)
  (fun n ->
     let children =
       List.map (fun (d, g) ->
		   let hv = h d in
		   let child = 
		     { data = d;
		       wf = max n.wf (g +. wt *.hv);
		       f = max n.f (g +. hv);
		       cheapest_child = max_float;
		       g = g;
		       depth = n.depth + 1;
		       pos = Dpq.no_position;
		       parent = n;
		       expanded = false;
		       stand_in = false;
		       children = [];
		     }
		   in 
		     if(n.cheapest_child > child.f) then
		       n.cheapest_child <- child.f;
		     child) (expand n.data n.g) in
       (*when we expand a node, it isn't standing in for anything any more.*)
       n.stand_in <- false;
       n.children <- children
  )

exception No_children

let smastar sif args =
  let wt = Search_args.get_float "smastar.ml (wt)" args 0 in
  let mem_capacity = Search_args.get_int "smastar.ml (capacity)" args 1 in

  let initial_h = (sif.Search_interface.h
		     sif.Search_interface.initial) in
  let rec initial = {
    data = sif.Search_interface.initial;
    wf = initial_h *. wt;
    f = initial_h;
    cheapest_child = max_float;
    g = 0.0;
    depth = 0;
    pos = (-1);
    parent = initial;
    expanded = false;
    stand_in = false;
    children = [];
  } in

  let open_list = Mmh.create ~update_function:setpos ~resize:false
    ordered_p mem_capacity initial in

  let limit_t = (Limit.make Limit.Nothing sif.Search_interface.halt_on
		   just_f
                   (Limit.make_default_logger
		      (fun n -> n.g)
		      (wrap sif.Search_interface.get_sol_length))) in

  let closed_list = (Htable.create sif.Search_interface.hash
		       sif.Search_interface.equals 10000) in

  let key n = (wrap sif.Search_interface.key) n in
  let ht_add n = Htable.replace closed_list (key n) n in
  let ht_remove n = Htable.remove closed_list (key n) in
  let ht_find n = Htable.find closed_list (key n) in
  let is_goal n = wrap sif.Search_interface.goal_p n in

  let expand = make_expand 
    (sif.Search_interface.domain_expand)
    (sif.Search_interface.h) wt in 

  let node_count = ref 0 in

  let get_next_child n = 
    if((not n.expanded) && (n.children = [])) then 
      (
	(*
	  Printf.fprintf stderr "expanding \n%snode\n"
	  (sif.Search_interface.key_printer (sif.Search_interface.key n.data));
	  flush stderr;
	*)
	Limit.incr_exp limit_t;
	expand n;
	(*
	  Printf.fprintf stderr "produced %d children\n" (List.length n.children);
	  flush stderr;
	*)
      );
    match n.children with
	[] -> raise No_children
      | c :: [] -> 
	  (
	    Printf.fprintf stderr "done with this node\n%!";
	    n.expanded <- true;
	    n.children <- [];
	    c)
      | c :: tl ->
	  (
	    n.stand_in <- false;
	    n.children <- tl;
	    c) in 

  let add_child c = 
    (
      Limit.incr_gen limit_t;
      try 
	let prev = ht_find c in
	  Limit.incr_dups limit_t;
	  if not (better_p prev c) then
	    (
	      ht_add c;
	      let pos:int = getpos prev in
		if(pos = (-1)) then
		  ignore (Mmh.insert open_list c)
		else 
		  ignore (Mmh.replace_at open_list c pos);
	    )
      with Not_found -> (*child is new*)
	ht_add c;
	ignore (Mmh.insert open_list c);
    ) in 
    ignore (Mmh.insert open_list initial);
    ht_add initial;

    let deleted = ref 0 in

    let rec do_search () = 
      Printf.fprintf stderr "doing search %d %d\n%!" 
	limit_t.Limit.expanded !node_count;
      if(Mmh.empty_p open_list) then ()
      else if (Limit.halt_p limit_t) then ()
      else if (limit_t.Limit.incumbent != Limit.Nothing) then ()
      else
	(
	  let best = Mmh.peek_first open_list in
	    (*install as incumbent*)
	    if(is_goal best) then 
	      (Limit.new_incumbent limit_t (Limit.Incumbent (0., best)))
	    else 
	      (
		(try (
		   let succ = get_next_child best in
		     succ.f <- (max best.f succ.f);

		     print_node 
		       (fun n -> sif.Search_interface.key_printer
			  (sif.Search_interface.key n))
		       best
		       "best";


		     if (is_done best) then (backup best);
		     if (best.expanded)
		     then
		       (
			 Printf.fprintf stderr "removing this node from the open list\n%!";
			 best.stand_in <- false;
			 ignore (Mmh.extract_first open_list);
		       );
		     node_count := !node_count + 1;
		     if(!node_count > mem_capacity) then
		       (
			 deleted := !deleted + 1;
			 let to_remove = ref (Mmh.extract_worst open_list) in
			   (*check if it is the root*)
			   if(((!to_remove).parent) == (!to_remove)) then
			     (
			       let real_removal = 
				 (Mmh.extract_worst open_list) in 
			       let ret = (Mmh.insert open_list !to_remove) in 
				 if(ret != []) then 
				   (failwith "Mmh removed something\n"); 
				 to_remove := real_removal; 
				 Printf.fprintf stderr
				   "trying to remove the root%!"; 
				 flush stderr;
			     );
			   print_node 
			     (fun n -> sif.Search_interface.key_printer
				(sif.Search_interface.key n))
			     !to_remove
			     "Removing";
			   ht_remove !to_remove;
			   let parent = (!to_remove).parent in 
			     if(parent.pos = (-1)) then
			       ignore (Mmh.insert open_list parent);
			     parent.stand_in <- true;
			     parent.expanded <- false;
			     parent.children <- !to_remove :: parent.children;
			     node_count := !node_count - 1;
			     (*have to clean out the child.*)
			     !to_remove.children <- [];
			     !to_remove.stand_in <- false;
			     !to_remove.expanded <- false;
		       );
		     add_child succ;
		 )
		   (*Didn't have any children, close the node and move on*)
		 with no_children -> 
		   (
		     print_node 
		       (fun n -> sif.Search_interface.key_printer
			  (sif.Search_interface.key n))
		       best
		       "best";
		     ignore (Mmh.extract_first open_list);
		     best.f <- max_float;
		     best.wf <- max_float;
		     best.expanded <- true;
		     Printf.fprintf stderr  "this can't happen in tiles\n%!";
		   ););
		do_search();
	      )
	) in
      do_search ();

      Datafile.write_pairs stdout 
	["removed",(string_of_int !deleted)];

      Limit.unwrap_sol6 unwrap_sol (Limit.results6 limit_t)

