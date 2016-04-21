(*
  Heuristic source by all-pair-shortest path. Actually, it calculate the
  shortest distance from any "input" node to any other node (to be
  used with the pop planner) and from any node to any "output" node
  (to be used with the progression planner).
*)


(* type node_type = INPUT | OUTPUT | INTERMEDIATE *)


type machine = {
  id : int;
  name : string;
  is_out : bool;
}


type machine_config = {
  machines : machine list;
  attributes : string list;
  connections : (machine, machine list) Hashtbl.t;
  gives : (machine, string list) Hashtbl.t;
}

type state = {
  loc : machine;
  has_attrs : bool array;
}


type node = {
  s : state;
  (* score = makespan from the initial state/goal *)
  score : float;
}


let count_attrs attrs =
  (** count the number of attributes that are true *)
  Array.fold_left (fun count attr -> if attr then (count +. 1.) else count)
    0. attrs


let add_machine, add_attribute, get_machine, get_attribute,
     get_attr_from_index, all_machines, num_attr =
  let mi = ref 0
  and ai = ref 0
  and machines = Hashtbl.create 30
  and attributes = Hashtbl.create 10
  and attr_index = Hashtbl.create 10 in
    (* add_machine *)
    (fun m_name is_out ->
       let m =      { id = !mi;
		      name = m_name;
		      is_out = is_out; }
       in
	 incr mi;
	 Hashtbl.add machines m_name m;
	 m),
    (* add_attribute *)
    (fun a_name ->
       Hashtbl.add attributes a_name !ai;
       Hashtbl.add attr_index !ai a_name;
       incr ai),
    (* get_machine *)
    (fun m_name ->
       try
	 Hashtbl.find machines m_name
       with Not_found ->
	 failwith (Wrutils.str "Apsp.get_machine: %s not added" m_name)),
    (* get_attribute *)
    (fun a_name ->
       try
	 Hashtbl.find attributes a_name
       with Not_found ->
	 failwith (Wrutils.str "Apsp.get_attribute: %s not added" a_name)),
    (* get_attr_from_index *)
    (fun ai ->
       try
	 Hashtbl.find attr_index ai
       with Not_found ->
	 failwith (Wrutils.str "Apsp.get_attr_from_index: %d not a valid index" ai)),
    (* all_machines *)
    (fun () ->
       Hashtbl.fold (fun _ m accu ->  m::accu) machines []),
    (* num_attr *)
    (fun () -> !ai)


let make_state m attrs =
  let a = Array.make (num_attr ()) false in
    List.iter (fun attr -> a.(get_attribute attr) <- true) attrs;
    { loc = get_machine m;
      has_attrs = a;}


let state_str s =
  Wrutils.str "(%s,%s)" s.loc.name
    (String.concat " "
    (Array.to_list (Array.mapi (fun i x -> if x then (get_attr_from_index i) else "")
		      s.has_attrs)))


let node_str n =
  Wrutils.str "(%s,%f)" (state_str n.s) n.score


let process_object objects =
  (** for object list from the problem file *)
  List.fold_left
    (fun (mnames,anames) o ->
       match o.Domain.typename with
	   "machine" ->  (o.Domain.label::mnames,anames)
	 | "attribute" -> (mnames,o.Domain.label::anames)
	 | s -> failwith
	     (Wrutils.str "Apsp.process_object: do not recognize object type %s" s))
    ([],[]) objects


let process_init init =
  (** for the facts specified in the initial state of the problem file *)
  List.fold_left
    (fun (c,o,g) l ->
       match l.Domain.pred with
	   "connected" ->
	     let n1 = (List.hd l.Domain.args).Domain.label
	     and n2 = (Wrlist.last l.Domain.args).Domain.label in
	       ((n1,n2)::c,o,g)
	 | "is_output" ->
	     let n1 = (List.hd l.Domain.args).Domain.label in
	       (c,n1::o,g)
	 | "gives" ->
	     let n1 = (List.hd l.Domain.args).Domain.label
	     and a1 = (Wrlist.last l.Domain.args).Domain.label in
	       (c,o,(n1,a1)::g)
	 | "empty" -> (c,o,g)
	 | s -> failwith
	     (Wrutils.str "Apsp.process_init: do not recognize lit name %s" s))
    ([],[],[]) init


let build_machine_config prob =
  (** given a grounded problem, build the machine configuration *)
  let (mnames,anames) = process_object prob.Domain.objects
  and (c,o,g) = process_init prob.Domain.init in
  let machines =
    List.map (fun m -> add_machine m (List.mem m o)) mnames
  and connections = Hashtbl.create 10
  and gives = Hashtbl.create 10 in
    List.iter (fun a -> add_attribute a) anames;
    List.iter
      (fun m ->
	 let connected =
	   Wrlist.map_opt
	     (fun (m1,m2) -> if m = m1
	      then Some (get_machine m2) else None) c
	 and attrs =
	   Wrlist.map_opt (fun (m1,a1) -> if m = m1 then Some a1 else None) g in
	   Hashtbl.add connections (get_machine m) connected;
	   Hashtbl.add gives (get_machine m) attrs)
      mnames;
    { machines = machines;
      attributes = anames;
      connections = connections;
      gives = gives; }


(************ building the scores ***********)

let make_root m =
  {s  = make_state m [];
   score = 0.;
  }


let in_order a b =
  a.score <= b.score


let dups = ref 0 (* logging the number of duplicates *)

let delta_score node new_s =
  match !Args.obj with
      Args.MAKESPAN ->
	if (node.s.has_attrs = new_s.has_attrs) then 10.
	else (10. +. 2.)
    | Args.STEPS ->
	if (node.s.has_attrs = new_s.has_attrs) then 1.
	else
	  1. +. ((count_attrs new_s.has_attrs) -. (count_attrs node.s.has_attrs))


let gen_child node new_m mc scores =
  (** for each node and a connected successor machine [new_m] generate
    child nodes by moving to [new_m] and add one or more attributes at
    [new_m] that is not currently assigned to [node] already *)
  let curr_attrs = node.s.has_attrs
  and new_attrs = Hashtbl.find mc.gives new_m in
  let rec add_attr attrs accu =
    match attrs with
	[] -> accu
      | a::rest ->
	  let ai = get_attribute a in
	    if curr_attrs.(ai) then add_attr rest accu
	    else
	      let added_attr = List.map (fun x ->
					   let y = Array.copy x in
					     y.(ai) <- true;
					     y)
				 accu in
		add_attr rest (added_attr @ accu)
  in
  let new_attrs = add_attr new_attrs [curr_attrs] in
  let new_states =
    List.map (fun attrs -> {loc = new_m; has_attrs = attrs;}) new_attrs
  in
    Wrlist.map_opt
      (fun s ->
	 if (Hashtbl.mem scores s)
	 then (incr dups; None)
	 else Some { s = s;
		     score = node.score +. (delta_score node s);})
      new_states



let expand mc scores =
  (fun n _->
     (** expand by BFS search and store the node score in [scores] *)
     let state = n.s in
       if (Hashtbl.mem scores state) then
	 (Verb.pe 5 "ignore: %s\n" (node_str n); flush_all (); [])
       else
	 (Verb.pe 5 "expand: %s\n" (node_str n); flush_all ();
	  Hashtbl.add scores state n.score;
	  let machine = n.s.loc in
	  let connected = Hashtbl.find mc.connections machine in
	    List.fold_left
	      (fun accu new_m ->
		 (List.map (fun n -> n,0.) (gen_child n new_m mc scores)) @ accu)
	      [] connected))



(********** setup initial and goal locations for new materials ********)


let add_locs, get_init_loc, get_goal_loc, get_goal_score,
     set_curr_mats, is_curr_mat =
  let init_locs = Hashtbl.create 50
  and goal_locs = Hashtbl.create 50
  and goal_scores = Hashtbl.create 50
  and curr_mats = ref [] in
    (* add_locs: material [m], init loc [i], goal loc [g] *)
    (fun m i g score ->
       Hashtbl.add init_locs m i;
       Hashtbl.add goal_locs m g;
       Hashtbl.add goal_scores m score),
    (fun m ->
       Hashtbl.find init_locs m),
    (fun m ->
       Hashtbl.find goal_locs m),
    (fun m ->
       Hashtbl.find goal_scores m),
    (fun mats ->
       curr_mats := mats),
    (fun m ->
       List.mem m !curr_mats)



(************** queries **********)

let translate atoms =
  (* take the grounded atoms and translate into a set of states for different
     materials. Note that we will remove duplications in the set of locations
     that a material can be at *)
  let la = Hashtbl.create 10 in
  let lits = List.map (fun atom -> Ground.lookup_atom atom) atoms in
    List.iter
      (fun l ->
	 match l.Domain.pred with
	     "at" ->
	       (let mat = (List.hd l.Domain.args).Domain.label
	        and loc = (Wrlist.last l.Domain.args).Domain.label in
		  try
		    let (locs,attr) = Hashtbl.find la mat in
		      if not (List.mem loc locs) then
			Hashtbl.replace la mat (loc::locs,attr)
		  with Not_found ->
		    Hashtbl.add la mat ([loc],[]))
	   | "has_attribute" ->
	      (let mat = (List.hd l.Domain.args).Domain.label
	       and attr = (Wrlist.last l.Domain.args).Domain.label in
		 try
		   let (locs,attrs) = Hashtbl.find la mat in
		     if not (List.mem attr attrs) then
		       Hashtbl.replace la mat (locs,attr::attrs)
		 with Not_found ->
		   Hashtbl.add la mat ([],[attr]))
	   | _ -> ())
      lits;
    Hashtbl.fold (fun m x accu -> (m,x)::accu) la []


let build_scores, get_score, get_reg_mk, get_pro_mk,
  get_reg_steps, get_pro_steps =
  (** do uniform-cost search (A* with h = 0) to build the scores for
      all reachable state; return a heuristic evaluation function *)
  let mks = Hashtbl.create 100
  and steps = Hashtbl.create 100 in
    (* build_scores: build a set of single-source-all-destinations shortest
       path from any node to all nodes *)
    (fun problem ->
       let start = ref (Time.curr_time ()) in
       let mc = build_machine_config problem in
       let single_source machine =
	 let single_score = Hashtbl.create 1000 in
	 let s_interface = Search_interface.make
	   ~h:(fun _ -> 0.)
	   ~domain_expand:(expand mc single_score)
	   ~key:(fun n -> n.s)
	   ~goal_p:(fun _ -> false)
	   Search_interface.OPlan
	   (make_root machine)
	   (fun _ _ -> false)
	   (fun _ -> ()) in
	 let _, expanded, generated, pruned, max_q_len =
	   (* detect duplicates in the expand function *)
	   Astar.no_dups s_interface [||]
	 in
	   Verb.pe 4 "Apsp.build_scores: starting loc = %s :  exp = %d; gen = %d; max_q_len = %d; dups = %d.\n" machine expanded generated max_q_len !dups;
	   single_score
       in
	 (* all-pair-shortest-path with makespan/steps expansion function *)
	 match !Args.obj with
	     Args.MAKESPAN ->
	       List.iter
		 (fun m -> Hashtbl.add mks m.name
		    (single_source m.name))
		 mc.machines;
	       Verb.pe 3 "Done APSP for makespan in %f sec\n"
		 ((Time.curr_time ()) -. !start)
	   | Args.STEPS ->
	       List.iter
		 (fun m -> Hashtbl.add steps m.name
		    (single_source m.name))
		 mc.machines;
	       Verb.pe 3 "Done APSP for steps in %f sec\n"
		 ((Time.curr_time ()) -. !start)),

  (* get score: get score given an init-loc, (loc,attributes) *)
  (fun init_loc (loc,attrs) ->
     let single_score =
       match !Args.obj with
	   Args.MAKESPAN -> (Hashtbl.find mks init_loc)
	 | Args.STEPS -> (Hashtbl.find steps init_loc)
     in
       try
	 Hashtbl.find single_score (make_state loc attrs)
       with Not_found -> infinity),

  (* get_reg_mk: get estimated makespan for regression & POP *)
  (fun atoms ->
     let l = translate atoms in
     let rec get_score l mk =
       match l with
	   [] -> mk
	 | (mat,(locs,attrs))::rest ->
	     if (List.length locs) > 1 then infinity
	     else if (locs = []) then
	       (if (attrs = []) then (get_score rest mk)
		else infinity)
	     else
	       let sloc = get_init_loc mat in
	       let single_score = Hashtbl.find mks sloc in
		 try
		   let score = Hashtbl.find single_score
		     (make_state (List.hd locs) attrs) in
 		     get_score rest (max score mk)
		 with Not_found -> infinity
     in
       get_score l 0.),

  (* get_pro_mk: get estimated makespan for progression *)
  (fun atoms ->
     let l = translate atoms in
     let rec get_score l mk =
       match l with
	   [] -> mk
	 | (mat,(locs,attrs))::rest ->
	     if not (is_curr_mat mat) then get_score rest mk
	     else
	       if (List.length locs) <> 1
	       then (failwith "Apsp.get_pro_mk: not single loc")
	       else
		 let goal_score = get_goal_score mat in
		 let sloc = get_init_loc mat in
		 let single_score = Hashtbl.find mks sloc in
		   try
		     let score = Hashtbl.find single_score
		       (make_state (List.hd locs) attrs) in
		       get_score rest (max mk (goal_score -. score))
		   with Not_found ->
		     failwith "Apsp.get_pro_mk: can't find score for a state"
     in
       get_score l 0.),

  (* get_reg_steps: get estimated steps for regression & POP *)
  (fun atoms ->
     let l = translate atoms in
     let rec get_score l step_count =
       match l with
	   [] -> step_count
	 | (mat,(locs,attrs))::rest ->
	     if (List.length locs) > 1 then infinity
	     else if (locs = []) then
	       (if (attrs = []) then (get_score rest step_count)
		else infinity)
	     else
	       let sloc = get_init_loc mat in
	       let single_score = Hashtbl.find steps sloc in
		 try
		   let score = Hashtbl.find single_score
		     (make_state (List.hd locs) attrs) in
		     get_score rest (score +. step_count)
		 with Not_found -> infinity
     in
       get_score l 0.),

  (* get_pro_steps: get estimated steps for progression *)
  (fun atoms ->
     let l = translate atoms in
     let rec get_score l step_count =
       match l with
	   [] -> step_count
	 | (mat,(locs,attrs))::rest ->
	     if not (is_curr_mat mat) then (get_score rest step_count)
	     else
	       if (List.length locs) <> 1
	       then (failwith "Apsp.get_pro_steps: not single loc")
	       else
		 let goal_score = get_goal_score mat in
		 let sloc = get_init_loc mat in
		 let single_score = Hashtbl.find steps sloc in
		   try
		     let score = Hashtbl.find single_score
		       (make_state (List.hd locs) attrs) in
		     let score = max 0. (goal_score -. score) in
		       get_score rest (step_count +. score)
		   with Not_found ->
		     failwith "Apsp.get_pro_steps: can't find score for a state"
     in
       get_score l 0.)


let get_heu_value atoms =
  match (!Args.p_alg,!Args.obj) with
      (Args.PROGRESSION, Args.MAKESPAN) -> get_pro_mk atoms
    | (Args.PROGRESSION, Args.STEPS) -> get_pro_steps atoms
    | (Args.POP, Args.MAKESPAN) -> get_reg_mk atoms
    | (Args.POP, Args.STEPS) -> get_reg_steps atoms



(***** setup when new goals come, mostly for progression planner ****)

let process_locs_newgoal ng =
  (** when the new goals come, setup the correct start/end locations
    for different materials *)
  let mats = List.map (fun obj -> obj.Domain.label) ng.Domain.objects in
    set_curr_mats mats;
    let find_loc_attrs mat =
      let start_lit =
	List.find (fun l -> (l.Domain.pred = "at") &&
		     (List.hd l.Domain.args).Domain.label = mat)
	  ng.Domain.init
      and end_lit =
	List.find (fun l -> (l.Domain.pred = "at") &&
		     (List.hd l.Domain.args).Domain.label = mat)
	  ng.Domain.goal
      and attrs =
	List.map (fun lit -> (Wrlist.last lit.Domain.args).Domain.label)
	  (List.find_all (fun l -> (l.Domain.pred = "has_attribute") &&
			    (List.hd l.Domain.args).Domain.label = mat)
	     ng.Domain.goal) in
      let start_loc = (Wrlist.last start_lit.Domain.args).Domain.label
      and end_loc = (Wrlist.last end_lit.Domain.args).Domain.label in
      let score = get_score start_loc (end_loc,attrs) in
	add_locs mat start_loc end_loc score;
	if (score = infinity) then
	  Verb.pe 3 "Goals for material: %s unachievable\n" mat;
	score
    in
      List.fold_left (fun score mat -> max (find_loc_attrs mat) score) 0. mats


(****** debug ****)

let print_debug scores =
  let print init_loc single_score =
    Wrutils.pe "Init Loc: %s\n" init_loc;
    Hashtbl.iter (fun s score -> Wrutils.pe "<state:%s | score: %.2f>" (state_str s) score)
      single_score
  in
    Hashtbl.iter print scores


(* EOF *)
