(* $Id: ground.ml,v 1.1 2005/04/06 18:55:27 ruml Exp ruml $

   grounded representation of a planning domain
*)


open Domain


(************* ground atoms from literals **************)


type ground_atom = int


let is_constant term =
  term.ttype = Constant


let ensure_ground lit =
  if not (List.for_all is_constant lit.args) then
    failwith ("not ground: " ^ (lit_str lit))


let intern_atom, intern_exist, lookup_atom, num_atoms =
  (** grounding literals and maitain the mapping between original literals
    and the atoms correspond to that literal *)
(*
  let tbl = Coatcheck.create_with { valence = true;
				    pred = "";
				    args = []; } 1000
*)
  let tbl = Coatcheck.create 1000
  and count = ref 0 in
    (fun lit ->
       (** returns UNIQUE and REPEATABLE integer for ground literal *)
       ensure_ground lit;
       Coatcheck.intern tbl lit),
    (fun lits ->
       (** intern lits that should have been interned *)
       count := Coatcheck.count tbl;
       let atoms =
	 List.map (fun lit ->
		     ensure_ground lit;
		     (* debug *)
		     let atom = Coatcheck.intern tbl lit in
		       if !count < (Coatcheck.count tbl) then
			 failwith (Wrutils.str "Ground.ml: try to intern new lit %s that should already be interned" (lit_str lit))
		     else atom)
	   lits
       in
	   atoms),
    (fun atom ->
       Coatcheck.retrieve tbl atom),
    (fun () ->
       Coatcheck.count tbl)


let all_atoms () =
  Wrutils.map_n Fn.identity ((num_atoms ()) - 1)


let atom_str a =
  (** convert an atom to a string using [lit_str] in Domain.ml *)
  Wrutils.str "%d:%s" a (lit_str (lookup_atom a))


let print_atoms ch list =
  List.iter (fun a -> Wrutils.pf ch "  %s\n" (atom_str a)) list



let intern_lits lits =
  List.map (fun l ->
	      assert l.valence;
	      intern_atom l)
    lits


let sat_equality_pre lits =
  (** if [lit] is equality/in-equality condition, then check if
    the parameters of [lit] satisfy that condition
    Example: (not (= ?loc1 ?loc2)) -> T if ?loc1 != ?loc2 *)
  let violate_lit lit =
    if lit.Domain.pred = "equal" then
      let dup = (Wrlist.dups_p (fun arg -> arg.Domain.label) lit.args) in
	if lit.Domain.valence then (not dup)
	else dup
    else false
  in let rec check_lits lits =
      match lits with
	  [] -> true
	| lit::others ->
	    if (violate_lit lit) then false
	    else check_lits others
  in
    check_lits lits


let substitute_terms pairs lits =
  (** replaces the arguments of [lit], presumably to constants in
    preparation for grounding *)
  List.map (fun lit ->
	      { lit with args = Wrlist.replace_alist pairs lit.args; })
    lits


let all_of_type, clear_objtype_cache =
  (** get all constants of certain type *)
  let cache = Hashtbl.create 100 in
    (fun types constants t ->
       (** returns all constants of type [t], caching results *)
       Wrht.find_or_compute cache
       (fun t ->
	  match List.filter (is_type types t) constants with
	    [] -> Verb.pe 3 "there are no constants of type %s\n" t;
	      []
	  | list -> list)
       t),
    (fun () -> Hashtbl.clear cache)


(************* ground actions *************)


type ground_action = {
  mutable id: int;
  printout : string;
  pre : ground_atom list;
  add : ground_atom list;
  delete : ground_atom list;
  dur : float;
}

let dummy_act =
  { id = -1;
    printout =  "dummy";
    pre = [];
    add = [];
    delete = [];
    dur = 0.;}

let create_act id name pre add delete dur =
  { id = id;
    printout = name;
    pre = pre;
    add = add;
    delete = delete;
    dur = dur; }

let ground_action schema params =
  (** takes a schema and constants for each parameter *)
  assert (List.for_all is_constant params);
  let label = Wrutils.str "(%s %s)" schema.name
		(String.concat " " (List.map (fun t -> t.label) params))
  and pairs = List.map2 Fn.gather2 schema.parameters params in
  let pre = substitute_terms pairs schema.preconditions in
  let pre_equal_check, pre =
    List.partition (fun l -> l.Domain.pred = "equal") pre
  in
    if not (sat_equality_pre pre_equal_check) then
      []
    else
      let eff = substitute_terms pairs schema.effects in
      let add,del = List.partition (fun l -> l.valence) eff in
	[ { id = 0;
	    printout = label;
	    pre = intern_lits pre;
	    add = intern_lits add;
	    delete = intern_lits (List.map negate del);
	    dur = schema.duration; } ]



let ground_actions types constants schema =
  (** returns list of ground action instantiations of schema *)
  let rec subst_arg so_far remaining =
    match remaining with
      [] -> ground_action schema (List.rev so_far)
    | param::rest ->
	Wrlist.mapcan (fun c -> subst_arg (c::so_far) rest)
	  (all_of_type types constants param.typename)
  in
    subst_arg [] schema.parameters


let action_str a =
  let pres = String.concat " " (List.map atom_str a.pre)
  and adds = String.concat " " (List.map atom_str a.add)
  and dels = String.concat " " (List.map atom_str a.delete) in
    Wrutils.str "%d:%s\n\t dur: %f\n\t pre: %s\n\t add: %s\n\t delete: %s\n"
      a.id a.printout a.dur pres adds dels


(**** post-process the grounded actions/atoms ****)


let deact_atom, deactivated =
  (** check if an atom is deactivated (can never be true). Thus, not
    in the initial state or any other action's effect *)
  let t = Hashtbl.create (num_atoms ()) in
    (fun atom -> Hashtbl.add t atom ()),
    (fun atom -> Hashtbl.mem t atom)


let filter actions init =
  (** filter out actions that has precondition that can never be true:
    i) not in the initial state; ii) not in any other action's effect
    (e.g. fly(cityA,cityB) and A and B are not connected in the problem
    definition) *)
  let nF = num_atoms () and changed = ref true in
  let a = Array.make nF false in
  let clear_a () =
    for i = 0 to (nF-1) do
      a.(i) <- false
    done in
  let relevant_atoms acts =
    clear_a ();
    List.iter (fun atom -> a.(atom) <- true) init;
    List.iter (fun act -> List.iter (fun i -> a.(i) <- true) act.add) acts in
  let bad_act act = List.exists (fun f -> not a.(f)) act.pre in
  let rel_acts acts =
    (* relevant actions *)
    let bads,goods = List.partition (fun act -> bad_act act) acts in
      (if List.length bads = 0 then
	 changed := false
       else changed := true);
      goods in
  let rec filter_acts acts =
    if not (!changed) then acts
    else
      (relevant_atoms acts;
       filter_acts (rel_acts acts)) in
  let good_acts = filter_acts actions and count = ref 0 in
    for i = 0 to (nF-1) do
      if not a.(i) then deact_atom i
      else incr count
    done;
    good_acts, !count



(************ fully-grounded problems: partially-grounded + new goal *********)


type ground_problem = {
  atoms : ground_atom list;
  actions : ground_action list;
  (* actions that have an atom as an add effect *)
  establishers : ground_atom -> ground_action list;
  (* actions that have an atom as a precondition *)
  supportees : ground_atom -> ground_action list;
  in_initial : ground_atom -> bool;
  (* if a state subsume the goals *)
  satisfy_goal : ground_atom list -> bool;
  goal_atoms : ground_atom list;
}


let index_achievers actions =
  (** return a function that return a set of achievers for a given atom *)
  let t = Hashtbl.create (num_atoms ()) in
    List.iter (fun action ->
		 List.iter (fun atom ->
			      Wrht.push t atom action)
		 action.add)
      actions;
    (fun atom ->
       try
	 Hashtbl.find t atom
       with Not_found -> [])


let index_supportees actions =
  (** return a function that return a set of actions supported by a
    given atom *)
  let t = Hashtbl.create (num_atoms ()) in
    List.iter (fun action ->
		 List.iter (fun atom -> Wrht.push t atom action) action.pre)
      actions;
    (fun atom ->
       try
	 Hashtbl.find t atom
       with Not_found -> [])


let evolve init plan time =
  (** apply actions in (global) [plan] to [init] (progression) until
    [time]. For plan format refer to Progression.state (grouped by end
    time, latest first --> got reversed before applying here). NOTE:
    deletes happen at start and adds happen at the end *)
  let del atoms acts =
    List.fold_left (fun l act -> Wrlist.subtractq l act.delete) atoms acts in
  let add atoms acts =
    List.fold_left (fun prev action ->
		      List.fold_left (fun prev this ->
					if List.memq this prev then
					  prev
					else
					  this::prev)
		      prev action.add)
      atoms acts in
  let find_acts p time =
    (* find action start before [time] *)
    List.fold_left (fun accu (acts,et) ->
		      (List.filter (fun act -> (et -. act.dur) < time) acts) @ accu)
      [] p in
  let rec progress atoms p =
    match p with
	[] -> init
      | (acts,t)::rest ->
	  if t > time then
	    (* look for actions started before cutoff [time] but end
	       after [time]. Assert their deletes*)
	    del atoms (find_acts rest time)
	  else
	    (* asserts the delete + add effects *)
	    progress (add (del atoms acts) acts) rest
  in
    progress init plan


let make_initial init_lits plan time =
  (** The initial states in [init_lits] and progress it according to
    the gloabal [plan] until current time [time], then merge with the
    new initial state [new_lits] that comes with the new goal. Return
    function that check whether an atom belong to the initial state *)
  let init = intern_exist init_lits in
  let current_init = evolve init (List.rev plan) time in
  let a = Array.make (num_atoms ()) false in
    List.iter (fun atom -> a.(atom) <- true) current_init;
    (fun atom -> a.(atom))


let make_goal goal_lits =
  (** function checking if goals is subsumed by some atom set *)
  let goals = intern_exist goal_lits in
    goals,
    (fun atoms ->
       List.for_all (fun goal -> List.memq goal atoms) goals)


let ground_problem domain prob plan p_time =
  (** grounding the planning problem based on: (i) domain; (ii)
    initial problem; (iii) problem associated with new goal; (iv) previous plan;
    (v) online plan start time for the new plan *)
  Time.start_timer ();
  clear_objtype_cache ();
  let constants = domain.Domain.constants @ prob.Domain.objects in
    Verb.pe 4 "Constants: %s\n" (String.concat " " (List.map term_str constants));
  let actions = Wrlist.mapcan (ground_actions domain.Domain.types constants)
		  domain.Domain.actions in
    Verb.pe 4 "Generated %d ground actions, %d ground atoms.\n"
      (List.length actions) (num_atoms ());
    let actions, nF = filter actions (intern_exist prob.Domain.init) in
      Verb.pe 4 "After filtering: %d ground actions, %d ground atoms.\n"
	(List.length actions) (nF);
      ignore (List.fold_left (fun i act -> act.id <- i; i+1) 0 actions);
      Verb.pe 4 "Grounding finished in %f secs\n" (Time.elapsed_time ());
      flush_all ();
      let init = make_initial prob.Domain.init plan p_time
      and goals, sat_goals = make_goal prob.Domain.goal in
	{ atoms = all_atoms ();
	  actions = actions;
	  establishers = index_achievers actions;
	  supportees = index_supportees actions;
	  in_initial = init;
	  satisfy_goal = sat_goals;
	  goal_atoms = goals; }


let gproblem_str p =
  let atoms = String.concat " " (List.map atom_str p.atoms)
  and actions = String.concat "\n" (List.map action_str p.actions)
  and init = String.concat " " (List.map atom_str (List.filter p.in_initial p.atoms))
  and goal = String.concat " " (List.map atom_str p.goal_atoms) in
    Wrutils.str "atoms: %s\nactions:\n%s\ninit: %s\ngoal: %s\n"
      atoms actions init goal



(**** utility functions used by other files ****)

let compatible_with a b =
  (** a doesn't delete a precondition or add of b. don't forget to check
    the other way around as well! *)
  not ((Wrlist.intersects_q a.delete b.pre) ||
       (Wrlist.intersects_q a.delete b.add))


let compatible a b =
  (compatible_with a b) && (compatible_with b a)


let init_atoms p =
  List.filter p.in_initial p.atoms

(* EOF *)
