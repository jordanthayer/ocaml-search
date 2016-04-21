(*
  counterpart of regression.ml. Many similarities can be found
   (symmetry between action.pre and action.add).

  NOTE: Assume TGP's action representation:
77777777  1. Mutex rule.
  2. Add (useful things) only happen at the action end time.
  3. Delete happen right away.
*)


open Ground


(******* selecting actions to add *******)

let set_basetime, basetime =
  let bt = ref 0. in
    (fun t -> bt := t),
    (fun () -> !bt)


type state = {
  (* WALL-CLOCK time (reference to starting time of the
     program -- not base-time) of this search state *)
  st : float;
  (* earliest action start: check if plan in this state violate wallclock time *)
  east : float;
  (* progressed 'steps' from the root note *)
  steps : int;
  (* grouped by their wall-clock END time: latest-first *)
  plan : (ground_action list * float) list;
  (* atoms true at current time *)
  current : ground_atom list;
  (* `progression time' remaining: no time-related ordering *)
  in_progress : (ground_action * float) list;
  (* plans for previous goals (only action not finished by st). Sorted
     in reverse order of [plan]: first group finish EARLIEST *)
  prev_plan : (ground_action list *float) list;
}


let time_from_start s =
  s.st  -. (basetime ())


let print_plan ch plan =
  match plan with
      [] -> Wrutils.pf ch "no actions\n"
    | plan ->
	List.iter (fun (actions,time) ->
		     List.iter (fun a ->
				  Wrutils.pf ch "%f: %s\n" (time -. a.dur)
				  a.printout)
		     actions)
	  (List.rev plan)


let print_plan_state ch s =
  print_plan ch s.plan


let print_state ch s =
  Wrutils.pf ch "%.3f from start, %d action sets, %d actions in progress\n"
    (time_from_start s) (List.length s.plan) (List.length s.in_progress);
  Wrutils.pf ch "plan so far:\n";
  print_plan ch s.plan;
  Wrutils.pf ch "current facts:\n";
  print_atoms ch s.current;
  Wrutils.pf ch "in progress:\n";
  List.iter (fun (a,t) -> Wrutils.pf ch "  %f: %s\n" t a.printout)
    (Wrlist.sort_on snd s.in_progress)


(*********** expand: advance time ***************)

let remove_del atoms actions =
  List.fold_left (fun l act -> Wrlist.subtractq l act.delete) atoms actions

let merge_add atoms actions =
  (** return unions of [atoms] with add effects of [actions] *)
   List.fold_left (fun prev action ->
		    List.fold_left (fun prev this ->
				      if List.memq this prev then
					prev
				      else
					this::prev)
		    prev action.add)
    atoms actions

let advance_time = 20.0

let advance s =
  (** advance state [s] in the forward direction to the earliest ending
    time (from init) of any action in 's.in_progress' or 's.prev_plan' *)
  let delta_c =
    if (List.length s.in_progress) > 0 then Wrlist.minf_by snd s.in_progress
    else infinity
  and delta_p =
    if (List.length s.prev_plan) > 0 then (snd (List.hd s.prev_plan)) -. s.st
    else infinity
  in
    if delta_c < delta_p then
      (* advance based on next happening in 'in-progress' *)
      (let done_a,in_progress = List.partition (fun (_,d) -> d = delta_c)
				  s.in_progress in
       let in_progress = List.map (fun (act,t) -> (act, t -. delta_c)) in_progress in
       let done_a = List.map fst done_a in
	 [{ st = s.st +. delta_c;
	   east = s.east;
	   steps = s.steps + 1;
	   plan = (done_a, s.st +. delta_c)::s.plan;
	   current = merge_add s.current done_a;
	   in_progress = in_progress;
	   prev_plan = s.prev_plan; }])
    else
      if delta_c > delta_p then
	(* advance based on next happending in 'prev-plan' *)
	(let done_a,prev_plan = fst (List.hd s.prev_plan),(List.tl s.prev_plan) in
	   [{ st = s.st +. delta_p;
	     east = s.east;
	     steps = s.steps + 1;
	     plan = s.plan;
	     current = merge_add (remove_del s.current done_a) done_a;
	     in_progress = List.map (fun (a,t) -> (a, t -. delta_p))
			     s.in_progress;
	     prev_plan = prev_plan; }])
      else
	(* advance based on both *)
	if (delta_c < infinity) then
	  let delta = delta_c in
	  let done_a_c,in_progress = List.partition (fun (_,d) -> d = delta)
				       s.in_progress
	  and done_a_p,prev_plan =
	    fst (List.hd s.prev_plan),(List.tl s.prev_plan) in
	  let in_progress = List.map (fun (act,t) -> (act, t -. delta)) in_progress in
	  let done_a_c = (List.map fst done_a_c)  in
	    [{ st = s.st +. delta;
	      east = s.east;
	      steps = s.steps + 1;
	      plan = (done_a_c, s.st +. delta)::s.plan;
	      in_progress = in_progress;
	      current = merge_add (remove_del s.current done_a_p)
			  (done_a_c @ done_a_p);
	      prev_plan = prev_plan;}]
	else
	  [ ]
	   (* both [in_progress] & [prev_plan] are empty
	    [{ s with
		 st = s.st +. advance_time;
		 steps = s.steps + 1; }] *)


let  advance_to_end s =
  (* IGNORING of prev_plan in simulating forward s.current = atoms *)
  List.fold_left (fun accu (act,_) -> act.add@accu) s.current s.in_progress


 (******* expand: apply action *******)

let overlap_acts p t =
  (** find actions in plan [p] that overlap with interval
    [t1,t2]. Used to check again plans of previous goals *)
  List.fold_left (fun accu (acts,et) ->
		    List.fold_left
		    (fun l act ->
		       if (et -. act.dur) <= t then act::l
		       else accu)
		    accu acts)
    [] p


let left_shift_possible s a =
  (** [a] only need facts progressed by "no-op" to [s] and [a] is compatible
    with parent.in_progress. Thus [a] could have been added to parent. *)
  if (List.length s.plan = 0) then false
  else
    let acts = fst (List.hd s.plan) in
    let atoms = merge_add [] acts in
      (* noops only approximate real noops set because some actions
	 end now may add some facts belong to the noops set *)
    let others,noops = List.partition (fun f -> List.memq f atoms) s.current in
      (not (Wrlist.intersects_q others a.pre)) &&
      (List.for_all (compatible a) acts)


let compatible_in_prog a1 (a2,remain) =
  if (a1.id <> a2.id) then (compatible a1 a2)
  else (a1.dur <> remain)


let filter_applicables s acts supportees =
  (** [acts] are actions having one precond satisfied by s.current. Filter:
    1. Not totally satisfied by s.current.
    2. Not compatible with any of s.in_progress.
    3. No "left_shift" *)
  List.filter (fun a ->
		 (not (List.memq a supportees)) &&
		 (List.for_all (fun atom -> List.memq atom s.current) a.pre) &&
		 (List.for_all (compatible_in_prog a) s.in_progress) &&
		 (* comparable with previous plan *)
		 (List.for_all (compatible a)
		    (overlap_acts s.prev_plan (s.st +. a.dur))) &&
		 (not (left_shift_possible s a)))
    acts


let select_applicables p state =
  (** select actions that have all preconditions satisfied by s.current *)
  let acts = List.fold_left (fun prev atom ->
		(filter_applicables state (p.supportees atom) prev) @ prev)
	       [] state.current in
    Verb.pe 4 "Applicable actions: %d \n%s\n" (List.length acts)
      (String.concat "\n" (List.map (fun a -> a.Ground.printout) acts));
    flush_all ();
    acts


let expand p =
  (** generate children of a given state s. [p]: ground_problem instance *)
  (fun s ->
     Verb.pe 5 "---------  expanding  -------------\n";
     Verb.force 5 (lazy (print_state stderr s));
     flush_all ();
     if (Time.timeofday_to_reftime ()) > s.east then
       ( Verb.pe 5 " this search state start after base time\n";
	 [ ])
     else
       let children =
	 List.map (fun a ->
		     if (s.plan = []) && (s.in_progress = []) then
		       { s with
			   east = s.st;
			   steps = s.steps + 1;
			   current = Wrlist.subtractq s.current a.delete;
			   in_progress = (a,a.dur)::s.in_progress;}
		     else
		       { s with
			   steps = s.steps + 1;
			   current = Wrlist.subtractq s.current a.delete;
			   in_progress = (a,a.dur)::s.in_progress;})
	   (select_applicables p s)
       in
	 Verb.pe 5 "%d children (including Advance)\n%!"
	   (List.length children +1);
	 (advance s)@children)



let g_expand_steps e =
  (fun s _ ->
      List.map (fun c ->
		 c, (float) c.steps)
     (e s))


let g_expand_makespan e =
  (** takes node and g to list of children with g values *)
  (fun s _ ->
     List.map (fun c ->
		 c, time_from_start c)
     (e s))

let g_expand e =
  match !Args.obj with
      Args.MAKESPAN -> g_expand_makespan e
    | Args.STEPS -> g_expand_steps e


(* for BUGSY  *)
let op_expand e =
  (** takes node and g to list of children and op costs *)
  (fun s ->
     let t = time_from_start s in
       List.map (fun c ->
		   c, ((time_from_start c) -. t))
	 (e s))


(******************** search interface ************************)


let make_root prob prev_plan time =
  (** correct initial state is already computed when ground problem
    for the new goal; compute the remain plan *)
  set_basetime time;
  { st = time;
    east = infinity;
    steps = 0;
    plan = [];
    current = init_atoms prob;
    in_progress = [];
    (* note the [List.rev] operation *)
    prev_plan = List.filter (fun (acts,t) -> t > time) (List.rev prev_plan);
  }


let goal_p p =
  (fun s ->
     (* p.satisfy_goal (advance_to_end s) *)
     p.satisfy_goal s.current)


let make_rp p =
  (** return the heuristic func: for any progressed search node, take
    two arguments: s.current and s.in_progress to return the relaxed
    plan with less number of remaining 'step' (actions + advance). May
    extend to also take s.prev_plan to improve heuristic estimate *)
  let h = Cg.distance_p p (Rtpg_c.closest_p p) in
    (fun s -> (float) (snd (h s.current s.in_progress)))


let make_apsp () =
  (** using all-pair-shortest-path instead of relaxed plan *)
  (fun s ->
     Apsp.get_heu_value (advance_to_end s))


let make_state_h p =
  (** measure the h value of a given state using relaxed-plan heuristic.
    Only measure the number of step (note the 'snd' function) and ignore
    the makespan measurement.*)
  let h = match !Args.obj with
      Args.MAKESPAN -> make_apsp ()
    | Args.STEPS ->
	match !Args.s_heu with
	    Args.APSP -> make_apsp ()
	  | Args.RP ->
	      make_rp p
  in
    (fun s ->
       let h_val =  h s in
	 Verb.pe 5 "heuristic: h = %f\n" h_val;
	 h_val)



let interface p plan time =
  (** call after starting timer but before the search algorithm *)
  let root = make_root  p plan time in
  let expand = expand p
  and goal_p = goal_p p
  and h = make_state_h p  in
    flush_all ();
    root, expand, goal_p , h


let init problem plan time =
  (**  fully grounded [problem]; global [plan]; and plan start [time] *)
  let r, e, g, h = interface problem plan time  in
    r, (g_expand e), g, h


(*
let init_op domain  init_prob new_prob prev_plan start_time =
  (* for BUGSY: not used right now *)
  let r, e, g, h = interface domain  init_prob new_prob in
    r, (op_expand e), g, h
*)


(******** Interfaces *********)

let default_interface prob p t =
  let root, expand, goal_p, h = init prob p t in
  Search_interface.make
    ~domain_expand:expand
    ~goal_p:goal_p
    ~h:h
    Search_interface.OPlan
    root
    (fun _ _ -> false)
    (fun _ -> ())


(* EOF *)


