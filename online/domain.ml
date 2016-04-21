(* $Id: domain.ml,v 1.1 2005/04/06 00:38:38 ruml Exp ruml $

   planning domains for tplan
*)


(**************** types **************)


type term_type = Constant | Variable


let bogus_type = "###"


type tlist = (string list * string) list
    (** a structure that arises due to the syntax of pddl.  Used for 1)
      declaring new types as subtypes of existing types and 2) declaring
      constants of particular types.  Only used during parsing. *)


let make_types tlist =
  Wrlist.mapcan (fun (types,supertype) ->
		   List.map (fun t -> t, supertype) types)
    tlist


(**************** terms **************)


type term = {
  label : string;
  typename : string;
  ttype : term_type;
}


let term ttype name =
  { label = name;
    typename = bogus_type;
    ttype = ttype; }


let make_terms tlist ttype =
  Wrlist.mapcan (fun (names,typename) ->
		   List.map (fun n ->
			       { label = n;
				 typename = typename;
				 ttype = ttype; })
		   names)
    tlist


let assert_vars lits =
  assert (List.for_all (fun l -> l.ttype = Variable) lits)


let is_bogus term =
  term.typename == bogus_type


let is_type types t term =
  (** is [term] of type [t] (or a subtype of [t] according to [types])? *)
  (term.typename = t) ||
  (let rec check_super subtype =
     try
       let super = List.assoc subtype types in
	 if super = t then
	   true
	 else
	   check_super super
     with Not_found -> false in
     check_super term.typename)


let term_str t =
  Wrutils.str "%s%s:%s" (match t.ttype with Constant -> "" | Variable -> "?")
    t.label t.typename

(**************** literals **************)


type literal = {
  valence : bool;
  pred : string;
  args : term list;
}


let make_literal name args =
  { valence = true;
    pred = name;
    args = args; }


let negate lit =
  { lit with valence = not lit.valence; }


let lit_str lit =
  Wrutils.str "%s(%s %s)%s" (if lit.valence then "" else "(not ")
    lit.pred (String.concat " " (List.map term_str lit.args))
    (if lit.valence then "" else ")")


let short_lit_str lit =
  Wrutils.str "(%s %s)" lit.pred
  (String.concat " " (List.map (fun t -> t.label) lit.args))

(****************** action schemata *******************)
(*
  We assume TGP-style semantics: All preconditions must hold over the
   action's entire duration and all effects happen at some point before
   end of the action.  Any other action that deletes this action's
   preconditions or relies on its effects must not overlap with it.
*)

type schema = {
  name : string;
  parameters : term list;
  preconditions : literal list;
  effects : literal list;
  duration : float;
}


let check_parameters vars lits =
  (** NEED TO REWRITE *)
  Verb.pe 4 "Check parameters!\n%!"


let  make_schema name parameters pre eff dur =
  check_parameters parameters pre;
  check_parameters parameters eff;
  { name = name;
    parameters = parameters;
    preconditions = pre;
    effects = eff;
    duration = dur; }


let check_schema types constants predicates s =
  (** types and predicate definitions aren't available until after the
    schema is parsed. NEED TO REWRITE *)
  Verb.pe 4 "Check schema\n%!"


let get_typed constants parameters term =
  assert (is_bogus term);
  let choices = (match term.ttype with
		   Constant -> constants
		 | Variable -> parameters) in
    try
      List.find (fun p -> p.label = term.label) choices
    with Not_found ->
      failwith (Wrutils.str "couldn't find %s in %s" (term_str term)
		  (String.concat ", " (List.map term_str choices)))


let substitute_typed constants params lits =
  assert (not (List.exists is_bogus constants));
  List.map (fun lit ->
	      { lit with
		  args = List.map (get_typed constants params) lit.args })
    lits


let correct_types constants s =
  assert (not (List.exists is_bogus constants));
  let sub x = substitute_typed constants s.parameters x in
    { s with
	preconditions = sub s.preconditions;
	effects = sub s.effects; }


let schema_str s =
  let paras = String.concat " " (List.map term_str s.parameters)
  and pres = String.concat " " (List.map lit_str s.preconditions)
  and effs = String.concat " " (List.map lit_str s.effects) in
    Wrutils.str "schema: %s\n\t para: %s\n\t pre:%s\n\t eff:%s\n\t dur: %f"
      s.name paras pres effs s.duration


(********************** domain ************************)


type domain = {
  title : string;
  (* name, supertype *)
  types : (string * string) list;
  (* constants that are always available *)
  constants : term list;
  actions : schema list;
}


let check_requirements requirements =
  List.iter (fun r ->
	       if not (List.mem r [":strips";
				   ":typing";
				   ":negative-preconditions";
				   ":durative-actions";
				   (* ":equality"; *)
				  ]) then
		 failwith ("unhandled requirement: " ^ r))
    requirements


let make_domain name requirements types constants predicates actions =
  check_requirements requirements;
  let actions = List.map (correct_types constants) actions in
    List.iter (check_schema types constants predicates) actions;
    { title = name;
      types = types;
      constants = constants;
      actions = actions; }


let domain_str d =
  let types = String.concat " "
		(List.map (fun (t1,t2) -> Wrutils.str "(%s - %s)" t1 t2) d.types)
  and consts = String.concat " " (List.map term_str d.constants)
  and acts = String.concat "\n" (List.map schema_str d.actions) in
    Wrutils.str "domain: %s\n\t types: %s\n\t consts: %s\n\t acts: %s\n"
      d.title types consts acts


(******************** PROBLEM *******************)
(* keep the initial problem format the same with the offline PDDL-based planner
   even though the initial state may not be fully specified in the online planner *)

type problem = {
  (* instance-specific constants *)
  pname : string;
  objects : term list;
  init : literal list;
  goal : literal list;
}


let make_problem name objects init goal =
  { pname = name;
    objects = objects;
    init = init;
    goal = goal; }


let combine p1 p2 =
  (** used for online planning where new problems keep comming *)
  { pname = String.concat ":" [p1.pname;p2.pname];
    objects = p1.objects @ p2.objects;
    init = p1.init @ p2.init;
    goal = p1.goal @ p2.goal; }


let correct_problem d p =
  (** initially parsed literals without types -> assign correct types
    for objects in the literais in the read problem *)
  assert (not (List.exists is_bogus p.objects));
  let constants = d.constants @ p.objects in
    (* initial and goal states shouldn't contain variables *)
  let sub x = substitute_typed constants [] x in
    { p with
	init = sub p.init;
	goal = sub p.goal; }


let problem_str p =
  let objs = String.concat " " (List.map term_str p.objects)
  and init = String.concat " " (List.map lit_str p.init)
  and goal = String.concat " " (List.map lit_str p.goal) in
    Wrutils.str "problem: %s\n\t objs: %s\n\t init: %s\n\t goal: %s\n"
      p.pname objs init goal


(* EOF *)
