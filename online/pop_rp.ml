(*
  State representation and expansion rules for a partial-order planner.
  Contains a STN as part of the state representation
*)

open Ground

let  delta = 5

let pplan_size = ref 0 and base_time = ref 0.

type state = {
    (* action * start-time-point in the STN *)
    plan : (ground_action * Stn.id) array;
    plan_size : int;
    (* open conditions: ground_atom * index of action in [plan] *)
    oc : (ground_atom * int) list;
    (* causal links: atom,start-act,end-act *)
    cl : (ground_atom * int * int) list;
    (* temporal relations *)
    stn : Stn.t;
  }


let print_plan ch s stn =
  for i = 0 to (s.plan_size -1) do
    let (act,tp) = s.plan.(i) in
      Wrutils.pf ch "(%s:%f) " act.printout (Time.int_to_dur (Stn.earliest stn tp))
  done


let print_oc ch oc =
  List.iter (fun (atom,act) ->
    Wrutils.pf ch "(%s:%d)" (atom_str atom) act)
    oc


let print_cl ch cl =
  List.iter (fun (atom,a1,a2) ->
	       Wrutils.pf ch "(%d:%s:%d)" a1 (atom_str atom) a2)
    cl


let print_state ch s =
  Wrutils.pf ch "\nPlan: ";
  print_plan ch s s.stn;
  Wrutils.pf ch "\nOpen condition: ";
  print_oc ch s.oc;
  Wrutils.pf ch "\nCausal links: ";
  print_cl ch s.cl;
  Wrutils.pf ch "\n"

let a_init_i =
  (fun () -> !pplan_size)

let a_goal_i =
  (fun () -> !pplan_size + 1)

(*** expand:
                (1) no disjunction maintained;
                (2) mutexes and thread are handled by temporal constraints in STN
***)

let rec supporter_index atom state i sp =
  (** find the supporters for a given open condition [atom] from actions in the
    plan [plan] with the current examining action index [i]. [sp] is the
    accumulated set of supporters *)
  if i >= state.plan_size then sp
  else
    let (act,_) = state.plan.(i) in
      if (List.mem atom act.add)
      then supporter_index atom state (i+1) (i::sp)
      else supporter_index atom state (i+1) sp


let rec violator_index atom state i v =
  (** find a set of actions (their indexes) that delete a fact [atom].
    In reverse of supporter_index *)
  if i >= state.plan_size then v
  else
    let (act,_) = state.plan.(i) in
      if (List.mem atom act.delete)
      then violator_index atom state (i+1) (i::v)
      else violator_index atom state (i+1) v


let rec mutex_index act state i m =
  (** find all action in state.plan mutex with [act] *)
  if i >= state.plan_size then m
  else
    let (a,_) = state.plan.(i) in
      if (compatible act a)
      then mutex_index act state (i+1) m
      else mutex_index act state (i+1) (i::m)


let violated_cl state act =
  (** find set of causal links in state.cl that is violated by action [act] *)
  List.fold_left (fun (sas,eas) (atom,sai,eai) ->
		    if (List.mem atom act.delete) then
		      (sai::sas,eai::eas)
		    else (sas,eas))
    ([ ],[ ]) state.cl


let add_ordering stn sid eid sa =
  (** copy stn & add (mutex) order between two action starting times sid --> eid *)
  let new_stn = Stn.copy stn in
    try
      Stn.add_constraint new_stn
	(Stn.lower_constraint sid eid (Time.dur_to_int sa.dur));
      [new_stn]
    with Stn.Inconsistent -> [ ]


(*** internal expansion: add support for open-cond within the found plan *****)


let add_support (atom,eai) sai state =
  (** add the support: sa --atom--> ea to state [state]. Return a set
    of states resulted from this addition *)
  let cl = (atom,sai,eai)::state.cl
  and stn = Stn.copy state.stn
  and (sa,sid) = state.plan.(sai) and (ea,eid) = state.plan.(eai) in
    (* add ordering between two actions *)
    try
      Stn.add_constraint stn (Stn.lower_constraint sid eid (Time.dur_to_int sa.dur));
      (* resolve potential threads: may add duplicated constraints here *)
      let v = violator_index atom state 0 [ ] in
	match v with
	    [ ] -> [{state with
		     oc = List.tl state.oc;
		     cl = cl;
		     stn = stn;}]
	  | _ ->
	      let stns =
		List.fold_left
		  (fun stns vi ->
		     if vi = sai or vi = eai then stns
		     else
		       let v,vid = state.plan.(vi) in
			 List.fold_left
			   (fun accu stn ->
			      (add_ordering stn vid sid v) @
			      (add_ordering stn eid vid ea) @ accu)
			   [ ] stns)
		  [stn] v in
		List.map (fun stn ->
			    {state with oc = List.tl state.oc;
			       cl = cl;
			       stn = stn;})
		  stns
    with Stn.Inconsistent -> [ ]


 let internal_support s =
   (** supporting the first open condition in s.oc using an action
     already selected (i.e. in s.plan or s.prev_plan) *)
   let (atom,act) = List.hd s.oc in
   let sa = supporter_index atom s 0 [ ] in
     List.fold_left (fun accu i ->
		       (add_support (atom,act) i s)@accu)
       [ ] sa


(*** external expansion: add new action to plan to support open-cond ****)

let expand_plan s act =
  Verb.pe 5 "Expand plan: add action %s\n" act.printout;
  let plan =
    if s.plan_size = (Array.length s.plan) then
      Wrarray.extend s.plan delta (dummy_act,Stn.zero)
    else
      Array.copy s.plan
  and stn = Stn.copy s.stn in
  let id = Stn.add_point stn in
    plan.(s.plan_size) <- (act,id);
    plan,stn,id


let expand_oc s act =
   (List.map (fun atom -> (atom,s.plan_size)) act.pre)@(List.tl s.oc)


let add_new_support (atom,ai) new_act s =
  (** add new action [new_act] to support a given precondition [atom]
    of a given action [ai]. Return a list of new states *)
  let a,aid = s.plan.(ai)
  and new_plan,new_stn,new_id = expand_plan s new_act
  and oc = expand_oc s new_act
  and cl = (atom,s.plan_size,ai)::s.cl in
    try
      Stn.add_constraint new_stn
	(Stn.lower_constraint Stn.zero new_id (Time.dur_to_int !base_time));
      Stn.add_constraint new_stn
	(Stn.lower_constraint new_id aid (Time.dur_to_int new_act.dur));
      (* Resolve violated threads by this new act *)
      let sais,eais = violated_cl s new_act in
	(* Wrutils.pr "Before resolving possible thread\n"; *)
      let stns =
	List.fold_left
	  (fun stns (sai,eai) ->
	     (* each stn is expanded with either demotion or promotion *)
	     let sa,sid = s.plan.(sai) and ea,eid = s.plan.(eai) in
	       List.fold_left
		 (fun accu stn ->
		    if (sai <> (a_init_i ())) then
		      (add_ordering stn new_id sid new_act) @
		      (add_ordering stn eid new_id ea) @ accu
		    else
		      (add_ordering stn eid new_id ea) @ accu)
		 [ ] stns)
	  [new_stn] (List.combine sais eais) in
	(* Resolve mutexes *)
	(* Wrutils.pr "Done resolving threads: stns = %d\n" (List.length stns); *)
      let mutexes =
	List.filter (fun aid -> (not (List.mem aid sais)) & (not (List.mem aid eais)))
	  (mutex_index new_act s 0 []) in
      let stns =
	List.fold_left
	  (fun new_stns mi ->
	     let ma,mid = s.plan.(mi) in
	       List.fold_left
		 (fun accu stn ->
		    if (mi = ai) or (mi =  (a_init_i ())) then stn::accu
		    else
		      if not (List.mem atom ma.delete) then
			(add_ordering stn new_id mid new_act) @
			(add_ordering stn mid new_id ma) @ accu
		      else
			(add_ordering stn mid new_id ma) @
			(add_ordering stn aid mid a) @ accu)
		 [ ] new_stns)
	  stns mutexes
      in
	(* Wrutils.pr "Done resolving mutexes: stns = %d\n" (List.length stns); *)
	List.map (fun stn ->
		    { plan = new_plan;
		      plan_size = s.plan_size + 1;
		      oc = oc;
		      cl = cl;
		      stn = stn; })
	  stns
    with Stn.Inconsistent -> []


let external_support p s =
  (** support an open condition by adding new action, [p] is a planning
     problem instance and [s] is a state to be expanded *)
  let (atom,act) = List.hd s.oc in
  let sa = p.establishers atom in
  List.fold_left (fun accu new_act ->
    (add_new_support (atom,act) new_act s)@accu)
    [ ] sa


let expand p =
  (** generate children of a given state s. [p]: ground_problem instance *)
  (fun s ->
     Verb.pe 5 "---------  expanding  -------------\n";
     Verb.force 5 (lazy (print_state stderr s));
     flush_all ();
     let children =
       (internal_support s) @ (external_support p s)
     in
       Verb.pe 5 "%d children \n" (List.length children);
       children)


let g_expand_act e =
  (fun s _ ->
     List.map (fun c ->
		 c, (float) c.plan_size)
     (e s))


(***** search interface ******)

let make_root prob plan time =
  let a_init = create_act (-1) "A:init" [ ] (init_atoms prob) [ ] 0.
  and a_goal = create_act (-1) "A:goal" prob.goal_atoms [ ] [ ] 0. in
  let plan = List.filter (fun (acts,t) -> t > time) plan
  and stn = Stn.create () in
    (* initialize STN with (fixed) start times of (unfinished) actions in plan *)
  let plan =
    List.fold_left
      (fun accu (acts,et) ->
	 List.fold_left (fun accu a ->
			   let id = Stn.add_point stn in
			     Stn.add_constraint stn
			       (Stn.lower_constraint Stn.zero id
				  (Time.dur_to_int (et -. a.dur)));
			     (a,id)::accu)
	 accu acts)
      [] plan in
    pplan_size := (List.length plan);
    base_time := time;
    let  agi = !pplan_size + 1
    and aiid = Stn.add_point stn
    and agid = Stn.add_point stn in
      Stn.add_constraint stn (Stn.fixed_constraint Stn.zero aiid (Time.dur_to_int time));
      Stn.add_constraint stn (Stn.lower_constraint Stn.zero agid (Time.dur_to_int time));
      { plan = Array.of_list (plan@[(a_init,aiid);(a_goal,agid)]);
	plan_size = !pplan_size + 2;
	(* initialize with goals *)
	oc = List.map (fun atom -> (atom,agi)) prob.goal_atoms;
	cl = [ ];
	stn = stn; }


let temporal_consistent s =
  (** check if all actions in the plan (except ones got from global
    plan when make root)can start after the current WALLCLOCK time.
    Note: right now employing lazy check for only the potential goal
    node where s.oc is empty (no open cond) *)
  let ctime = Time.timeofday_to_reftime () in
    try
      for i = (!pplan_size +1) to (s.plan_size - 1) do
	Stn.add_constraint s.stn
	  (Stn.lower_constraint Stn.zero (snd s.plan.(i)) (Time.dur_to_int ctime))
      done;
      true
    with Stn.Inconsistent -> false


let goal_p p =
  (fun s ->
     (s.oc = [ ]) && (temporal_consistent s))


let make_rp p =
  (** return a function that measures the number of actions in the
    relaxed plan; not 'steps' as in the progression/regression planner *)
  Cg.distance_pop p (Rtpg_c.closest_r p)


let make_state_h h =
  (fun s ->
     let h_val = (float) (snd (h s.oc)) in
       Verb.pe 5 "heuristic: h = %f\n" h_val;
       h_val)


let interface p plan time =
  let root = make_root p plan time in
  let expand = expand p
  and goal_p = goal_p p
  and h = make_state_h (make_rp p) in
    flush_all ();
    root, expand, goal_p, h


let init problem plan time =
  (** ground [problem], previous (global) [plan] and current [time] *)
  let r,e,g,h = interface problem plan time in
    r, (g_expand_act e), g, h


(* EOF *)
