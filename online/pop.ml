(*
  State representation and expansion rules for a partial-order planner.
  Contains a STN as part of the state representation
*)

open Ground

let delta = 5

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
  if (List.mem (atom,sai,eai) state.cl) then []
  else
    let cl = (atom,sai,eai)::state.cl
    and remaining_oc = Wrlist.remove_first (atom,eai) state.oc
    and stn = Stn.copy state.stn
    and (sa,sid) = state.plan.(sai) and (ea,eid) = state.plan.(eai) in
      (* add ordering between two actions *)
      try
	Stn.add_constraint stn
	  (Stn.lower_constraint sid eid (Time.dur_to_int sa.dur));
	(* resolve potential threads: may add duplicate constraints here *)
	let v = violator_index atom state 0 [ ] in
	  match v with
	      [ ] -> [{state with
			 oc = remaining_oc;
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
			      {state with
				 oc = remaining_oc;
				 cl = cl;
				 stn = stn;})
		    stns
      with Stn.Inconsistent -> [ ]



(***  add new action to plan to support open-cond ****)

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


let expand_oc rem_oc s act =
   (List.map (fun atom -> (atom,s.plan_size)) act.pre)@rem_oc


let oc_to_atoms oc =
  List.map (fun (atom,_) -> atom) oc


let add_new_support (atom,ai) new_act s =
  (** add new action [new_act] to support a given precondition [atom]
    of a given action [ai]. Return a list of new states *)
  let oc = expand_oc (Wrlist.remove_first (atom,ai) s.oc) s new_act in
  let a,aid = s.plan.(ai)
  and new_plan,new_stn,new_id = expand_plan s new_act
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


let add_action p s new_act =
  (* for each action: (1) add possible support; (2) add possible supports
       from other actions in the plan *)
  (* Wrutils.pr "Pop.add act: %s\n" new_act.Ground.printout; *)
  let new_ai = s.plan_size in
  let sp_oc = List.find_all (fun (atom,_) -> List.mem atom new_act.add) s.oc in
  let rec add_one_support ocs children =
    match ocs with
	[] -> children
      | (atom,act)::rest ->
	  add_one_support rest
	    ((add_new_support  (atom,act)  new_act s)
	     @ children) in
    (* create base children set with exactly one new causal link
       supports an existing open condition *)
  let one_supports = add_one_support sp_oc [] in
    (* branch on supporting one or more remaining oc *)
    (* Wrutils.pr "One_supports size: %d\n" (List.length one_supports); *)
  let rec add_more_support ocs accu=
    match ocs with
	[] -> accu
      | oc::rest ->
	  add_more_support rest
	    (List.fold_left (fun so_far s1 ->
			      (add_support oc new_ai s1)@so_far)
	      [] accu) @ accu in
  let multi_supports = add_more_support sp_oc one_supports in
    (* branch on getting supported by other actions in the current plan *)
    (* Wrutils.pr "Multi_supports size: %d\n" (List.length multi_supports); *)
  let rec support_last atoms accu =
    match atoms with
	[] -> accu
      | atom::rest ->
	  support_last rest
	    (List.fold_left
	       (fun sofar1 s1 ->
		  let sa = supporter_index atom s1 0 [ ] in
		    (List.fold_left
		       (fun sofar2 i ->
			  (add_support (atom,new_ai)  i s1)@sofar2)
		       [ ] sa) @ sofar1)
	       [ ] accu) @ accu
  in
  let x = support_last new_act.pre multi_supports in
    (* Wrutils.pr "Suport_last size: %d\n" (List.length x); *)
    x


let gather_supporters p s =
  let supporters =
    List.fold_left (fun accu (atom,_) -> (p.establishers atom) @ accu) [] s.oc in
    Wrlist.remove_duplicates supporters


let expand p =
  (** generate children of a given state s. [p]: ground_problem instance *)
  (fun s ->
     Verb.pe 4 "---------  expanding  -------------\n";
     Verb.force 4 (lazy (print_state stderr s)); flush_all ();
     let children =
       List.fold_left (fun accu act -> (add_action p s act)@accu)
	 [] (gather_supporters p s) in
     let children =
       List.filter (fun s -> (Apsp.get_heu_value (oc_to_atoms s.oc)) < infinity) children
     in
       Verb.pe 4 "%d children \n" (List.length children);
       children)


let g_expand_act e =
  (** measure the number of "steps" as g value *)
  (fun s _ ->
     List.map (fun c -> c, (float) c.plan_size)
     (e s))


let makespan s =
  let _, agid = s.plan.(!pplan_size + 1) in
    (Time.int_to_dur (Stn.earliest s.stn agid)) -. !base_time


let g_expand_mk e =
  (** measure the makespan = est of a_goal as g value *)
  (fun s _ ->
     List.map
     (fun c ->
	(* print_state stderr c;
	Verb.pe 3 " -- g = %f" (makespan c);
	flush_all (); *)
	c, makespan c)
     (e s))


let g_expand e =
  match !Args.obj with
      Args.MAKESPAN -> g_expand_mk e
    | Args.STEPS -> g_expand_act e


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


let oc_satisfied_by_init s =
  if (s.oc = []) then true
  else
    let a_init = fst s.plan.(!pplan_size) in
      if (List.exists (fun (atom,_) -> not (List.mem atom a_init.add)) s.oc) then
	false
      else
	let rec process_oc ocs state =
	  match ocs with
	      [] -> true
	    | oc::rest ->
		let children = add_support oc (!pplan_size) state in
		  if (children = []) then false
		  else process_oc rest (List.hd children)
	in
	  process_oc s.oc s

let goal_p p =
  (fun s ->
     (oc_satisfied_by_init s) && (temporal_consistent s))


let make_state_h h =
  (fun s ->
     let atoms = oc_to_atoms s.oc in
     let h_val = h atoms in
       (* Verb.pe 3 "heuristic: h = %f\n" h_val; *)
       h_val)


let interface p plan time =
  let root = make_root p plan time in
  let expand = expand p
  and goal_p = goal_p p
  and h = make_state_h Apsp.get_heu_value in
    flush_all ();
    root, expand, goal_p, h


let init problem plan time =
  (** ground [problem], previous (global) [plan] and current [time] *)
  let r,e,g,h = interface problem plan time in
    r, (g_expand e), g, h



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
