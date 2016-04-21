(*
  1. Maintain the goal queue.
  2. Maintain and update the future commitement. Help checking for
  inconsistency during search.
*)


(******* maintaining the goal queue ******)

type goals = {
  done_g: Domain.problem Queue.t;
  unsolved: Domain.problem Queue.t;
}


let init_goals () =
  { done_g = Queue.create ();
    unsolved = Queue.create ();}


let has_goal goals =
  not (Queue.is_empty goals.unsolved)

let add_goal g goals =
  Queue.add g goals.unsolved

let get_goal goals =
  let g = Queue.pop goals.unsolved in
    Queue.add g goals.done_g;
    g


(******* maintaining the future committment ****

let has_update, get_update, add_update =
  let available = ref false
  and updates = ref [] in
    (fun () -> !available),
    (fun () -> available := false;
       let
*)



(***** filter the global plan and output the right portion of the plan *****)

let print_all, print_remain =
  (fun plan ->
     (** output all actions in the global plan *)
     Progression.print_plan stdout plan),
  (fun plan time ->
     (** output the remain of plan ending after [time] *)
     Progression.print_plan stdout (List.filter (fun (_,t) -> t >= time) plan))


let merge_plan p1 p2 =
  (** stored in reverse order: first group in each plan ends latest. Each sorted
    by actions finishing at the LATEST time*)
  let et p = snd (List.hd p) in
  let rec merge p p1 p2 =
    if (List.length p1) = 0 then (List.rev ((List.rev p2) @ p))
    else if (List.length p2) = 0 then (List.rev ((List.rev p1) @ p))
    else
      let t1 = et p1 and t2 = et p2 in
	if t1 > t2 then merge ((List.hd p1)::p) (List.tl p1) p2
	else if t1 < t2 then merge ((List.hd p2)::p) p1 (List.tl p2)
	else merge (((fst (List.hd p1))@(fst (List.hd p2)),t1)::p) (List.tl p1) (List.tl p2)
  in
    merge [] p1 p2


(**** process the POP plan & return the plan in the same format with 'progression' *)

let fix_plan_time s =
  (** (1) fix all starting time at earliest possible starting time; (2) group by
    latest finishing time --> same format with progression *)
  let rec gather_plan index accu =
    if (index >= s.Pop.plan_size) then accu
    else
      let a,id = s.Pop.plan.(index) in
	gather_plan (index+1)
	  ((a, (Time.int_to_dur (Stn.earliest s.Pop.stn id)) +. a.Ground.dur)::accu)
  in
  let plan = List.sort (fun a1 a2 -> compare (snd a1) (snd a2))
	       (gather_plan (!Pop.pplan_size +2) []) in
    List.fold_left
      (fun accu (a,et) ->
	 if accu = [] then
	   ([a],et)::accu
	 else
	   let (s,t) = List.hd accu in
	     if et = t then (a::s,t)::(List.tl accu)
	     else ([a],et)::accu)
      [ ] plan


(*** greedily trying to shift actions in the plan by 'progression' to the left **)

let earliest_support atom t plan gplan =
  (** find the earliest support for [atom] at [t] in the fixed time [plan] or [gplan] such
    that there is no other action delete it after the support*)
  ()


let latest_support atom t plan gplan =
  (** find the latest support for [atom] at [t]. Faster version but not as good quality
    as [earliest_support] *)
  let rec l_support p =
    match p with
	 [ ] -> None
      | (acts,et)::rest ->
	  if et > t then (l_support rest)
	  else
	    try
	      Some (List.find (fun act -> List.mem atom act.Ground.add) acts,et)
	    with Not_found -> l_support rest in
  let p_s = l_support plan and gp_s = l_support gplan in
    match (p_s,gp_s) with
	(None,None) -> None
      | (None, Some (a,_)) -> Some a
      | (Some (a,_), None) -> Some a
      | (Some (a1,et1), Some (a2,et2)) ->
	  if (et1 > et2) then Some a1
	  else Some a2


let find_mutex a t plan gplan =
  (** find actions mutex with action [a] in [plan] and [gplan]. end time [t] of
    [a] is used to decide the orders between actions *)
  let mutex p so_far =
    List.fold_left (fun accu (acts,et) ->
		    if (et = t) then accu
		    else
		      List.fold_left
			(fun (em,lm) act ->
			   if (Ground.compatible a act) then (em,lm)
			   else
			     if (et < t) then (act::em,lm)
			     else (em,act::lm))
			accu acts)
    so_far p
  in
    mutex plan (mutex gplan ([],[]))


let init_stn stn act_id plan gplan ctime =
  (* fix starting time of actions in the global plan *)
  List.iter
    (fun (acts,et) ->
       List.iter (fun a ->
		    let id = Stn.add_point stn in
		      Hashtbl.add act_id a id;
		      Stn.add_constraint stn
			(Stn.fixed_constraint Stn.zero id
			   (Time.dur_to_int (et -. a.Ground.dur))))
       acts)
    gplan;
  (* set the right lower-bound on the starting time of actions in the new plan *)
  List.iter
    (fun (acts,et) ->
       List.iter (fun a ->
		    let id = Stn.add_point stn in
		      Hashtbl.add act_id a id;
		      Stn.add_constraint stn
			(Stn.lower_constraint Stn.zero id
			   (Time.dur_to_int ctime)))
       acts)
    plan


let shift_plan_left plan global_plan =
  let ctime = Time.timeofday_to_reftime () in
  let gplan = List.filter (fun (acts,t) -> t > ctime) global_plan in
  let stn = Stn.create () in
  let act_id = Hashtbl.create ((List.length plan) + (List.length gplan)) in
  let id act = Hashtbl.find act_id act in
  let add_order a1 a2 =
    Stn.add_constraint stn
      (Stn.lower_constraint (id a1) (id a2) (Time.dur_to_int a1.Ground.dur))
  and earliest_start a = Time.int_to_dur (Stn.earliest stn (id a)) in
    (** NOTE: BE CAREFUL OF TWO SAME ACTIONS IN THE PLAN *)
    init_stn stn act_id plan gplan ctime;
    let rec process_act p =
      match p with
	  [ ] -> ()
	| (acts,et)::rest ->
	    List.iter
	      (fun act ->
		 (* supporting condition ordering *)
		 List.iter
		 (fun atom ->
		    match (latest_support atom (et -. act.Ground.dur) plan gplan) with
			None -> ()
		      | Some es ->  add_order es act)
		 act.Ground.pre;
		 (* mutex ordering *)
		 let em,lm = find_mutex act et plan gplan in
		   List.iter (fun ea -> add_order ea act) em;
		   List.iter (fun la -> add_order act la) lm)
	      acts;
	    process_act rest
    in
      (* find the earliest starting time *)
      process_act plan;
      let new_plan =
	List.fold_left
	  (fun accu (acts,_) ->
	     List.fold_left
	     (fun so_far act -> (act, (earliest_start act) +. act.Ground.dur)::so_far)
	     accu acts)
	  [ ] plan in
      let new_plan = List.sort (fun a1 a2 -> compare (snd a1) (snd a2)) new_plan in
	List.fold_left (fun accu (a,et) ->
			  if accu = [] then
			    [([a],et)]
			  else
			    let (acts,t) = List.hd accu in
			      if et = t then (a::acts,t)::(List.tl accu)
			      else ([a],et)::accu)
	  [ ] new_plan


(****** log the time start working on the goals for the simulator *****)
let output_goal_literals goal_lits =
  let t = Time.timeofday_to_reftime () in
  let  print_goal lit =
    Wrutils.pf stdout "%s %.4f\n" (Domain.short_lit_str lit) t
  in
    List.iter print_goal goal_lits

(* EOF *)
