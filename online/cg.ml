(*
  Causal graph: source to extract the relaxed plan (cost or time-sensitive) with
  guidances from either rtpg_c or rtpg_t.

  For the first version of the progression planner, only rtpg_c is used for
  fast planning time.
*)

open Ground

type t = {
  (** THIS IS DESIGNED FOR REGRESSION PLANNER *)
  to_check: ground_atom -> ground_action list;
  achievers : ground_atom -> ground_action list;
  (* supported by the initial state or not *)
  in_init: bool array;
  (* time-stamped subgoals sorted according to "time" or "cost" *)
  sgs: (ground_atom*float) Dpq.t;
  (* facts supported by actions in the relaxed plan *)
  sp: (ground_atom, unit) Hashtbl.t;

  (* function to select cheapest/earliest action when extract relaxed-plan *)
  selector: ground_action list -> ground_action;
}


let make_graph_r p f s =
  (** [f]: sorting function for subgoals; [p]: problem; *)
  let init = Array.make (List.length p.atoms) false in
    List.iter (fun atom -> init.(atom) <- (p.in_initial atom)) p.atoms;
    { to_check = p.supportees;
      achievers = p.establishers;
      in_init = init;
      sgs = Dpq.create_with f (-1,neg_infinity);
      sp = Hashtbl.create 100;
      selector = s; }


let make_graph_p p f s =
  (** for progression where the initial state change *)
    let init = Array.make (List.length p.atoms) false in
    { to_check = p.supportees;
      achievers = p.establishers;
      in_init = init;
      sgs = Dpq.create_with f (-1,neg_infinity);
      sp = Hashtbl.create 100;
      selector = s; }


let add_sp, supported, clear_sp =
  (** manipulated supported facts *)
  (fun f cg ->
     if not (Hashtbl.mem cg.sp f) then Hashtbl.add cg.sp f () else ()),
  (fun f cg ->
     cg.in_init.(f) or (Hashtbl.mem cg.sp f)),
  (fun cg -> Hashtbl.clear cg.sp)


let add_sg, no_sg, clear_sg, pop_sg, supporter =
  (** manipulating sub-goals *)
  (fun (g,t) cg ->
     if (supported g cg) then ()
     else Dpq.insert cg.sgs (g,t)),
  (fun cg -> Dpq.empty_p cg.sgs),
  (fun cg -> Dpq.clear cg.sgs),
  (fun cg -> Dpq.extract_first cg.sgs),
  (fun g cg ->
     let acts = cg.achievers g in
       if (List.length acts) = 0 then
	 None
       else
	 Some (cg.selector (cg.achievers g)))


(**** relaxed plan extraction ***)
    
let init_r cg current in_prog  =
  (** for 'Regression': initialize: (1) set goal set, (2) clear supporter *)
  clear_sp cg;
  List.iter (fun f -> add_sg (f,0.) cg) current;
  List.iter (fun (a,d) -> List.iter (fun f -> add_sg (f,d) cg) a.pre) in_prog



let init_p cg current in_prog goals =
  (** for 'Progression': initialize: (1) set goal set, (2) clear supporter *)
  (* setup initial *)
  clear_sp cg;
  List.iter (fun atom -> add_sp atom cg) current;
  List.iter (fun act -> List.iter (fun atom -> add_sp atom cg) act.add)
    (List.map fst in_prog);
  (* setup goals *)
  List.iter (fun g -> add_sg (g,0.) cg)  goals


let init_pop cg oc =
  (** for 'POP': (1) set goal set based on open-condition, (2) clear
    supporter.  Note: do not set supporting conditions based on actions
    in the partial plan *)
  clear_sp cg;
  List.iter (fun atom -> add_sg (atom,0.) cg) oc

    

let rec extract_rp cg rp  =
  (** extract the relaxed plan for our regression planner;  *)
  if (no_sg cg) then (Some rp)
  else
    let (g,t) = (pop_sg cg) in
      (* Verb.pe 4 " sg: %s \n" (Ground.atom_str g); *)
      (match (supporter g cg) with
	   None -> None
	 | Some a ->
	     (* Verb.pe 4 " supporting act: %s\n" (Ground.action_str a); *)
	     List.iter (fun f -> add_sg (f,(t+.a.dur)) cg) a.pre;
	     List.iter (fun f -> add_sp f cg) a.add;
	     extract_rp cg ((a,(t+.a.dur))::rp))


let count_steps  =
  (** the relaxed plan is "time-stamped"; combine with the in-progress
    actions in a given state, then measure the remaining steps, which
    is equal to number of actions + number of "advance-time". Return
    steps  & makespan.

    NOTE: for better heu, may want to account for the advance-steps in
    the search_node.prev_plan similar to search_node.in_progress too
  *)
  (fun rp in_prog ->
     let rec cal_steps acts steps makespan =
       match acts with
	   [] -> List.length steps, makespan
	 | (_,d)::rest ->
	     if (List.exists (fun s -> s = d) steps) then
	       cal_steps rest steps makespan
	     else
	       cal_steps rest (d::steps) (Math.fmax d makespan) in
     let steps,makespan = cal_steps (in_prog@rp) [] 0. in
       makespan, (List.length rp) + steps)
    

(*** main interface *)
    
let distance_r p (sg_sorter,act_selector) =
  (** used for 'regression'; [p]: grounded problem spec *)
  let cg = make_graph_r p sg_sorter act_selector in
    (** return the number of 'plan-step' in the extracted relaxed-plan
      and its makespan based on two functions: [sg_sorter]: sort
      subgoals; [act_selector]: section supporting act *)
    (fun current in_prog ->
       if (current = []) && (in_prog = []) then (0.,0)
       else
	 ( init_r cg current in_prog;
	   match (extract_rp cg []) with
	       None -> (infinity,max_int)
	     | Some acts ->
		 count_steps acts in_prog))

let distance_p p (reset_heu,sg_sorter,act_selector) =
  (** used for 'progression': reset/recompute heu for each node *)
  let cg = make_graph_p p sg_sorter act_selector in
    (** return the number of 'plan-step' in the extracted relaxed-plan
      and its makespan based on two functions: [sg_sorter]: sort
      subgoals; [act_selector]: section supporting act *)
    (fun current in_prog ->
       if (current = []) && (in_prog = []) then (0.,0)
       else
	 ( init_p cg current in_prog p.Ground.goal_atoms;
	   reset_heu current in_prog;
	   match (extract_rp cg []) with
	       None -> (infinity,max_int)
	     | Some acts ->
		 count_steps acts in_prog))


let distance_pop p (sg_sorter,act_selector) =
  (** used for 'partial-order'; similar to regression. However, right
    now only care about the number of remaining actions, not steps like
    regression.  May extend for something related to temporal quality
    (e.g. makespan) measurement later. Moreover, do not use actions in
    the partial plan to support open-conditions -> likely to
    over-estimate *)
  let cg = make_graph_r p sg_sorter act_selector in
    (fun oc ->
       if (oc = [ ]) then (0.,0)
       else
	 let oc = List.map (fun (atom,_) -> atom) oc in
	   (* Verb.pe 4 "Open conditions: ";
	      List.iter (fun atom -> Verb.pe 4 " %s " (Ground.atom_str atom)) oc;
	      Verb.pe 4 "\n"; *)
	   init_pop cg oc;
	   match (extract_rp cg []) with
	       None -> (infinity,max_int)
	     | Some acts ->
		 infinity, List.length acts)
    
    
(* EOF *)
