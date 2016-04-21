(** Beam Stack Search
    Based on the Rong & Eric Paper *)

type beam_stack_info =
    { depth : int;
      min : float;
      max : float;}

let prune beam_stack depth minimum_pruned_cost =
  (* Rule for updating the beam stack information assuming that we need
     to inadmissibly prune a node.  Assumes that the current top of the
     beam stack is the previous layer *)
  if Stack.is_empty beam_stack
  then ()
  else
    (let prev_info = Stack.pop beam_stack in
       Stack.push {depth = depth;
		   min = prev_info.min;
		   max = minimum_pruned_cost;} beam_stack)

let backtrack beam_stack beams upper_bound =
  (* Back tracking rule for beam stack search. *)
  let rec fn () =
    if Stack.is_empty beam_stack
    then ()
    else
      (let top_bsi = Stack.pop beam_stack
       and top_beam = Stack.pop beams in
	 if top_bsi.max < upper_bound
	 then (Stack.push {depth = top_bsi.depth;
			   min = top_bsi.max;
			   max = upper_bound;} beam_stack;
	       Stack.push top_beam beams)
	 else fn ())
  in
    fn ()

let set_upper_bound ?(exact_bound=None) get_cost ubound_ref i =
  match exact_bound with
      None -> (
	match i.Limit.incumbent with
	  | Limit.Nothing -> ubound_ref := max_float
	  | Limit.Incumbent (q,n) -> ubound_ref := (get_cost n))
    | Some ub ->
	(
	match i.Limit.incumbent with
	  | Limit.Nothing -> ubound_ref := ub
	  | Limit.Incumbent (q,n) -> ubound_ref := (get_cost n))


(*This should be able to work even if the initial search fails, if it
  doesn't know when to turn around just don't turn around (run with no
  depth bound, or more to the point a depth bound that is so high as
  to be infinite for all practical purposes).  cmw 5/15/2010*)
(*
  let set_upper_bound get_cost ubound_ref i =
  match i.Limit.incumbent with
  | Limit.Nothing -> failwith ("Beam Stack Search requires an upper"^
  " bound. No Incumbent was supplied.")
  | Limit.Incumbent (q,n) -> ubound_ref := (get_cost n)
*)

let search sface beam_width get_cost ordered_p better_p =
  let goal_p = sface.Search_interface.goal_p
  and expand = sface.Search_interface.node_expand
  and i = sface.Search_interface.info
  and upper_bound = ref infinity
  and beam_stack = Stack.create ()
  and beams = Stack.create ()
  and unordered = (fun n1 n2 -> not (ordered_p n1 n2))
  and initial = sface.Search_interface.initial in

  let rec do_expansion current_info current replacement next_layer worst =
    if Lq.empty_p current
    then worst
    else (let discarded =
	    (List.map
	       (fun c ->
		  Limit.incr_gen i;
		  if Limit.promising_p i c
		  then (if goal_p c
			then (Limit.new_incumbent i
				(Limit.Incumbent (0.,c));
			      set_upper_bound get_cost upper_bound i;
			      [])
			else (if (get_cost c) >= current_info.min
			      then Lq.insert next_layer c
			      else (Limit.incr_prune i; [])))
		  else (Limit.incr_prune i; []))
	       (let n = Lq.extract_first current in
		  ignore (Lq.insert replacement n);
		  Limit.incr_exp i;
		  expand n))
	  in
	    do_expansion current_info current replacement next_layer
	      (List.fold_left
		 (fun accum ele ->
		    match ele with
			[] -> accum
		      | n::[] ->
			  (match accum with
			       [] -> ele
			     | m::[] -> (if better_p n m
					 then ele
					 else accum)
			     | m::tl -> failwith "more than one pruned")
		      | n::tl -> failwith "more than one pruned"
		 ) [] discarded)) in
  let do_layer () =
    assert ((Stack.length beams) = (Stack.length beam_stack));
    let current_beam = Stack.pop beams
    and current_info = Stack.top beam_stack
    and next_layer = Lq.create unordered beam_width initial
    and replacement_beam = Lq.create unordered beam_width initial in
      if (Lq.empty_p current_beam)
      then (Stack.push current_beam beams;
	    backtrack beam_stack beams !upper_bound)
      else (let best_discarded =
	      do_expansion
		current_info current_beam replacement_beam next_layer [] in
	      (match best_discarded with
		 | n::[] -> prune beam_stack (current_info.depth + 1)
		     (get_cost n);
		 | _ -> ());
	      Stack.push {depth = current_info.depth + 1;
			  min = 0.;
			  max = !upper_bound} beam_stack;
	      Stack.push replacement_beam beams;
	      Stack.push next_layer beams)
  in
    (* Initial search setup *)
    set_upper_bound get_cost upper_bound i;
    Stack.push {depth = 0; min = 0.; max = !upper_bound;} beam_stack;
    Stack.push (Lq.create unordered beam_width initial) beams;
    ignore (Lq.insert (Stack.top beams) initial);

    (* Search loop *)
    while (not (Limit.halt_p i)) && (not (Stack.is_empty beams)) &&
      (not (Stack.is_empty beam_stack))
    do
      do_layer ()
    done;
    Limit.results5 i




let search_dups ?(exact_bound=None)
    sface beam_width get_cost get_depth ordered_p better_p =
  let set_upper_bound = set_upper_bound ~exact_bound:exact_bound in
  let goal_p = sface.Search_interface.goal_p
  and expand = sface.Search_interface.node_expand
  and i = sface.Search_interface.info
  and upper_bound = ref infinity
  and beam_stack = Stack.create ()
  and beams = Stack.create ()
  and unordered = (fun n1 n2 -> not (ordered_p n1 n2))
  and initial = sface.Search_interface.initial
  and closed = Htable.create sface.Search_interface.hash
    sface.Search_interface.equals 100
  and key = sface.Search_interface.key
  and it = ref 0 in

  let rec do_expansion current_info current replacement next_layer worst =
    if Lq.empty_p current
    then worst
    else (let discarded =
	    (List.map
	       (fun c ->
		  Limit.incr_gen i;
		  if Limit.promising_p i c
		  then (if goal_p c
			 then (Limit.new_incumbent i
				 (Limit.Incumbent (0.,c));
			       set_upper_bound get_cost upper_bound i;
			       [])
			 else try
		      let prev,prev_it = Htable.find closed (key c) in
			(* either c is better than previous
			   or previous belongs on this beam due to depth*)
			if (better_p c prev)
			then
			  (Htable.replace closed (key c) (c,!it);
			   (if (get_cost c) >= current_info.min
			    then Lq.insert next_layer c
			    else (Limit.incr_prune i; [])))
			else
			  (* we're re-expanding a node after backtrack*)
			  (if (get_depth prev) = (get_depth c) &&
			     (prev_it < !it)
			   then
			     (Htable.replace closed (key c) (prev,!it);
			      (if (get_cost c) >= current_info.min
			       then Lq.insert next_layer prev
			       else (Limit.incr_prune i; [])))
			   else [])
		    with Not_found ->
		      (Htable.add closed (key c) (c,!it);
		       (if (get_cost c) >= current_info.min
			     then Lq.insert next_layer c
			     else (Limit.incr_prune i; []))))
		  else (Limit.incr_prune i; []))
	       (let n = Lq.extract_first current in
		  ignore (Lq.insert replacement n);
		  Limit.incr_exp i;
		  expand n))
	  in
	    do_expansion current_info current replacement next_layer
	      (List.fold_left
		 (fun accum ele ->
		    match ele with
			[] -> accum
		      | n::[] ->
			  (match accum with
			       [] -> ele
			     | m::[] -> (if better_p n m
					 then ele
					 else accum)
			     | m :: tl -> failwith "prune list too long")
		      | n::tl -> failwith "prune list too long")
		 [] discarded)) in
  let do_layer () =
    assert ((Stack.length beams) = (Stack.length beam_stack));
    let current_beam = Stack.pop beams
    and current_info = Stack.top beam_stack
    and next_layer = Lq.create unordered beam_width initial
    and replacement_beam = Lq.create unordered beam_width initial in
      if (Lq.empty_p current_beam)
      then (Stack.push current_beam beams;
	    backtrack beam_stack beams !upper_bound)
      else (let best_discarded =
	      do_expansion
		current_info current_beam replacement_beam next_layer [] in
	      (match best_discarded with
		 | n::[] -> prune beam_stack (current_info.depth + 1)
		     (get_cost n);
		 | _ -> ());
	      Stack.push {depth = current_info.depth + 1;
			  min = 0.;
			  max = !upper_bound} beam_stack;
	      Stack.push replacement_beam beams;
	      Stack.push next_layer beams)
  in
    (* Initial search setup *)
    set_upper_bound get_cost upper_bound i;
    Stack.push {depth = 0; min = 0.; max = !upper_bound;} beam_stack;
    Stack.push (Lq.create unordered beam_width initial) beams;
    ignore (Lq.insert (Stack.top beams) initial);
    Htable.add closed (key initial) (initial,0);
    (* Search loop *)
    while (
      not (Limit.halt_p i)) &&
      (not (Stack.is_empty beams)) &&
      (not (Stack.is_empty beam_stack)) &&
      (exact_bound = None || (i.Limit.incumbent = Limit.Nothing))
    do
      (do_layer ();
       it := !it + 1)
    done;
    Limit.results6 i


(* EOF *)
