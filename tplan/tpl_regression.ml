(* $Id: regression.ml,v 1.1 2005/04/06 18:55:30 ruml Exp ruml $

   search space for tplan regression
*)


open Tpl_ground
open Tpl_domain


(******************** state ************************)


type state = {
  (** things true about all states reachable by this plan tail.  This time
    is such that the first group of actions in the plan are just starting,
    so we may need to prepend establishers for their preconditions.  *)
  (* grouped by time from their start to the goal.  the first group starts
     now, and gives the current time to the goal (i.e. current makespan) *)
  plan : (ground_action list * float) list;
  (* precoditions of actions that start now (collected at generation time) *)
  current : ground_atom list;
  (* `regression time' remaining *)
  in_progress : (ground_action * float) list;
}

type ret_state = {
  s : state;
  h : float;
  d : float;
}


let key s =
  ((List.sort compare s.s.current) , (List.sort (fun a b ->
					     compare (fst a).id (fst b).id)
				  s.s.in_progress))

let key_to_string (current,in_progress) =
  let current_str = Wrutils.str "%s]"
    (List.fold_left (fun accum ele -> accum ^ ", " ^ (string_of_int ele))
       "[" current) in
  let ongoing_str = Wrutils.str "%s]"
    (List.fold_left (fun accum (act,cost) ->
		       Wrutils.str
			 "%s, (%i %f)" accum act.id cost) "[" in_progress) in
    current_str ^ " " ^ ongoing_str

let time_from_end s =
  match s.plan with
      [] -> 0.
    | (_,t)::_ -> t


let print_plan ch s =
  match s.plan with
    [] ->
      Wrutils.pf ch "\nno actions: ";
      Tpl_rtpg_c.print_state s.current s.in_progress;
      Wrutils.pf ch "\n\n";
      flush ch
  | ((_,makespan)::_ as plan) ->
      List.iter (fun (actions,time) ->
		   List.iter (fun a ->
				Wrutils.pf ch "%f: %s\n" (makespan -. time)
				a.printout)
		   actions)
	plan;
      flush ch


let print_state ch s =
  Wrutils.pf ch "%.3f from end, %d action sets, %d actions in progress\n"
    (time_from_end s) (List.length s.plan) (List.length s.in_progress);
  Wrutils.pf ch "plan so far:\n";
  print_plan ch s;
  Wrutils.pf ch "open preconditions:\n";
  print_atoms ch s.current;
  Wrutils.pf ch "in progress:\n";
  List.iter (fun (a,t) -> Wrutils.pf ch "  %f: %s\n" t a.printout)
    (Wrlist.sort_on snd s.in_progress)


let merge_pre atoms actions =
  (** return unions of [atoms] with preconditions of [actions] *)
   List.fold_left (fun prev action ->
		    List.fold_left (fun prev this ->
				      if List.memq this prev then
					prev
				      else
					this::prev)
		    prev action.pre)
    atoms actions


(******* selecting actions to add *******)


let right_shift_possible s a =
  (** [a] only adds atoms regressed by "no-op" and [a] is compatible
    with parent.in_progress. Thus, could have been applied in parent *)
  if (List.length s.s.plan = 0) then false
  else
    let acts = fst (List.hd s.s.plan) in
    let atoms = merge_pre [] acts in
      (* NOTE: atoms \not\subset s.current because there may be in-prog
	 actions support some of atoms and thus removed from current *)
    let non_noops =
      List.filter (fun f -> List.memq f atoms) s.s.current
    in
      (not (Wrlist.intersects_q non_noops a.add)) &&
      (List.for_all (compatible a) acts)


let duplicate in_prog act =
  (** try to avoid two branches generating the same sequence
    of actions ((a1,a2),t) & ((a2,a1),t) *)
    List.exists (fun (a,_) -> a.id >= act.id)
      (List.filter (fun (a,d) -> a.dur = d) in_prog)


let filter_achievers s acts achievers =
  (** [acts] are actions supporting a fact in s.currents. Filter:
    1. Delete any atom in s.currents.
    2. Not compatible with any s.in_progress.
    3. No "right-shift"
    4. No duplicates in plans (e.g. ((a1,a2),t) & ((a2,a1),t) *)
  List.filter (fun a ->
		 (not (List.memq a achievers)) &&
		 (not (Wrlist.intersects_q a.delete s.s.current)) &&
		 (List.for_all (compatible a) (List.map fst s.s.in_progress)) &&
		 (not (right_shift_possible s a)) &&
		 (not (duplicate s.s.in_progress a)))
    acts


let select_establishers p state =
  (** return actions that support at least one open condition *)
  List.fold_left (fun prev atom ->
		    (filter_achievers state (p.establishers atom) prev) @ prev)
    [] state.s.current


let advance s =
  (** advance state [s] in the backward direction to the earliest starting
    time (from goal) of any action in s.in_progress *)
  let delta = Wrlist.minf_by snd s.s.in_progress in
  let done_a,in_progress = List.partition (fun (_,d) -> d = delta)
			   s.s.in_progress in
  let in_progress = List.map (fun (a,d) -> (a, d -. delta)) in_progress in
  let done_a = List.map fst done_a in
    { plan = (done_a,(time_from_end s.s) +. delta)::s.s.plan;
      current = merge_pre s.s.current done_a;
      in_progress = in_progress;}


(****** expand ******)


let expand p =
  (** generate children of a given state s. [p]: ground_problem instance *)
  (fun s ->
     Verb.pe 5 "expanding-------------\n";
	Verb.force 5 (lazy (print_state stderr s.s));
     let children = List.map (fun a ->
				  { plan = s.s.plan;
				    current = Wrlist.subtractq s.s.current
						a.add;
				    in_progress = (a,a.dur)::s.s.in_progress;})
		      (select_establishers p s) in
       if (List.length s.s.in_progress = 0) then
	  children
       else
	  (advance s)::children)


let g_expand e hd =
  (** takes node and g to list of children with g values *)
  (fun s _ ->
     let states = List.map (fun c -> c, time_from_end c) (e s) in
     let ret_states = List.map (fun (c,g) ->
				  let h,d = hd c in
				    ({ s = c;
				       h = h;
				       d = d} , g)) states in
       List.filter
	 (fun (c,g) -> c.h <> infinity && c.d <> infinity)
	 ret_states)

(******************** search interface ************************)


let make_root problem hd =
  let s = { plan = [];
	  current = List.map intern_atom problem.goal;
	  in_progress = [];} in
  let h,d = hd s in
    { s = s;
      h = h;
      d = d; }


let goal_p p =
  (fun s ->
     (s.s.in_progress = []) &&
     (List.for_all p.in_initial s.s.current))


let compare_ip (_,d1) (_,d2) = Math.fcompare d1 d2


let make_state_h_cheap h =
  (** measure h2(s) for a state (makespan to shortest makespan solution) *)
  (fun s ->
     if List.length s.in_progress = 0 then h s.current
     else
       let h2,accum =
	 List.fold_right
	   (fun (act,d) (makespan,accum)->
	      let atoms = Wrlist.remove_dups Fn.identity act.pre@accum in
		(Math.fmax makespan ((h atoms) +. d),atoms))
	    (List.sort compare_ip s.in_progress) (0.,[])
       in
       Math.fmax h2 (h (s.current@accum)))


let make_state_d_cheap d_cheap h_cheap =
  (** estimate the d_cheap (steps to shortest plan in terms of makespan) *)
  (fun s ->
     if (h_cheap s) = infinity then
	infinity
     else
       snd (d_cheap s.current s.in_progress))


let make_state_hd_close hd_close h_cheap =
  (** used for Bugsy, measure the remaining "steps" of the shortest
    plan in terms of steps to achieve a given state *)
  (fun s ->
     let h_cheap = (h_cheap s) in
     if h_cheap = infinity then
	infinity, infinity
     else
       let h_close, d_close = hd_close s.current s.in_progress in
	 Math.fmax h_cheap h_close, d_close)


let share p domain problem hd =
  let root = make_root problem hd in
  let expand = expand p
  and goal_p = goal_p p in
    root, expand, goal_p



let sg_cheap, as_cheap =
  (** subgoal/action selector for d_cheap; favor "earlier" (close to
    S_init) goals first (higher value from current regression state) *)
  (fun (_,t1) (_,t2) -> t1 >= t2),
  (fun h2 ->
     (fun acts -> fst (Wrlist.min_by (fun a -> (h2 a.pre) +. a.dur) acts)))


let d_cheap_init p h2 =
  Tpl_cg.distance p (sg_cheap,as_cheap h2)


let hd_close_init p =
  Tpl_cg.distance p (Tpl_rtpg_c.closest p)


(*** interface to different search algorithms ****)


let interface_hcheap domain problem =
  (** return h2(s): used for most simple algorithms *)
  let p = ground_problem domain problem in
  let h = make_state_h_cheap (Tpl_h2.h2_reg p) in
  let root, expand, goal_p = share p domain problem (fun x -> h x, -.1.) in
    root, expand, goal_p, h


let interface_hdclose domain problem =
  (** hd_close: used for speedy *)
  let p = ground_problem domain problem in
  let h2 = Tpl_h2.h2_reg p in
  let h_cheap = make_state_h_cheap h2 in
  let hd_close = make_state_hd_close (hd_close_init p) h_cheap in
  let root, expand, goal_p = share p domain problem hd_close in
    root, expand, goal_p, hd_close


let interface_hdcheap domain problem =
  (** hd_cheap: used for A*eps *)
  let p = ground_problem domain problem in
  let h2 = Tpl_h2.h2_reg p in
  let h_cheap = make_state_h_cheap h2 in
  let d_cheap = (make_state_d_cheap (d_cheap_init p h2) h_cheap) in
  let root, expand, goal_p = share p domain problem
    (fun x -> h_cheap x, d_cheap x) in root, expand, goal_p, h_cheap, d_cheap


let interface_hdcheap_hdclose domain problem =
  (** for bugsy *)
  let p = ground_problem domain problem in
  let h2 = Tpl_h2.h2_reg p in
  let h_cheap = make_state_h_cheap h2 in
  let hd_close =  make_state_hd_close (hd_close_init p) h_cheap
  and d_cheap = make_state_d_cheap (d_cheap_init p h2) h_cheap in
  let root, expand, goal_p = share p domain problem
    (fun x -> h_cheap x, d_cheap x) in
    root, expand, goal_p, h_cheap, d_cheap, hd_close


let init domain problem =
  (** other search algorithm except Bugsy *)
  let r, e, g, h_cheap = interface_hcheap domain problem in
    r, (g_expand e (fun n -> h_cheap n, -.1.)), g, (fun c -> c.h)


let init_speedy domain problem =
  (** only used for Speedy *)
  let r, e, g, hd_close = interface_hdclose domain problem in
    r, (g_expand e hd_close), g, (fun c -> c.h,c.d)


let init_aseps domain problem =
  let r, e, g, h_cheap, d_cheap = interface_hdcheap domain problem in
    r, (g_expand e (fun x -> h_cheap x, d_cheap x)), g,
  (fun s -> s.h, s.d)


let init_op domain problem =
  (** only used for Bugsy *)
  let r, e, g, h_cheap, d_cheap, hd_close =
    interface_hdcheap_hdclose domain problem in
    r, (g_expand e (fun x -> h_cheap x, d_cheap x)), g,
  (fun x -> x.h), (fun x -> x.d), (fun x -> x.h,x.d)


(**************************** Interface *******************************)

let default_interface (domain, problem) lim =
  let init, exp, goal_p,hd = init_aseps domain problem in
  (Search_interface.make
    ~h:(fun n -> let h,_ = hd n in h)
    ~d:(fun n -> let _,d = hd n in d)
    ~hd:hd
    ~domain_expand:exp
    ~key:key
    ~key_print:key_to_string
    ~goal_p:goal_p
    ~equals:(fun a b -> a = b)
    ~t:(fun _ -> 0)
(*  ~rev_h:
    ~rev_d:
    ~rev_hd:
    ~get_sol_length:*)
    ~halt_on:lim
    Search_interface.TPlan
    init (fun _ _ -> false) (fun _ -> ()))

(* EOF *)
