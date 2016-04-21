(*
  Basic causal graph structure. Used to extract the relaxed plan based
  on some guidances on goal orderings and supporting actions ordering
*)

open Tpl_ground

type t = {
  (** THIS IS DESIGNED FOR REGRESSION PLANNER *)
  to_check: ground_atom -> ground_action list;
  achievers : ground_atom -> ground_action list;
  (* supported by the initial state or not *)
  in_init: ground_atom -> bool;
  (* time-stamped subgoals sorted according to "time" or "cost" *)
  sgs: (ground_atom*float) Dpq.t;
  (* facts supported by actions in the relaxed plan *)
  sp: (ground_atom, unit) Hashtbl.t;

  (* function to select cheapest/earliest action when extract relaxed-plan *)
  selector: ground_action list -> ground_action;
}


let make_graph p f s =
  (* [f]: sorting function for subgoals; [p]: problem; *)
  { to_check = p.supportees;
    achievers = p.establishers;
    in_init = p.in_initial;
    sgs = Dpq.create_with f (-1,neg_infinity);
    sp = Hashtbl.create 100;
    selector = s; }


let add_sp, supported, clear_sp =
  (** manipulated supported facts *)
  (fun f cg ->
     if not (Hashtbl.mem cg.sp f) then Hashtbl.add cg.sp f () else ()),
  (fun f cg ->
     (cg.in_init f) or (Hashtbl.mem cg.sp f)),
  (fun cg -> Hashtbl.clear cg.sp)


let add_sg, no_sg, clear_sg, pop_sg, supporter =
  (** manipulating sub-goals *)
  (fun (g,t) cg ->
     if (supported g cg) then ()
     else Dpq.insert cg.sgs (g,t)),
  (fun cg -> Dpq.empty_p cg.sgs),
  (fun cg -> Dpq.clear cg.sgs),
  (fun cg -> Dpq.extract_first cg.sgs),
  (fun g cg -> cg.selector (cg.achievers g))


(**** relaxed plan extraction ***)
    
let init cg current in_prog  =
  (** initialize: set goal set, clear supporter *)
  List.iter (fun f -> add_sg (f,0.) cg) current;
  List.iter (fun (a,d) -> List.iter (fun f -> add_sg (f,d) cg) a.pre)
    in_prog; 
  clear_sp cg


let rec extract_rp cg rp  =
  (** extract the relaxed plan for our regression planner;  *)
  if (no_sg cg) then rp
  else
    let (g,t) = (pop_sg cg) in
    let a = supporter g cg in
      List.iter (fun f -> add_sg (f,(t+.a.dur)) cg) a.pre;
      List.iter (fun f -> add_sp f cg) a.add;
      extract_rp cg ((a,(t+.a.dur))::rp)


let count_steps  =
  (** the relaxed plan is "time-stamped"; combine with the in-progress
    actions in a given state, then measure the remaining steps, which
    is equal to number of actions + number of "advance-time" *)
  (fun rp in_prog ->
     let rec cal_steps acts steps max =
       match acts with
	   [] -> List.length steps, max
	 | (_,d)::rest ->
	     if (List.exists (fun s -> s = d) steps) then
	       cal_steps rest steps max
	     else
	       cal_steps rest (d::steps) (Math.fmax d max) in
     let steps,max = cal_steps (in_prog@rp) [] 0. in
       max, float ((List.length rp) + steps))
 

(*** main interface *)
    
let distance p (sg_sorter,act_selector) =
  let cg = make_graph p sg_sorter act_selector in
    (** return a relaxed-plan and its makespan based on two functions:
    [sg_sorter]: sort subgoals; [act_selector]: section supporting act *)
    (fun current in_prog ->
       if (current = []) && (in_prog = []) then (0.,0.)
       else
	 ( init cg current in_prog;
	   count_steps (extract_rp cg []) in_prog))


(* EOF *)
