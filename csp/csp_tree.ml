(* $Id: functions.ml,v 1.1 2003/09/27 01:13:33 ruml Exp ruml $

   tree-structured search space for CSPs

   all the action happens during leaf_p.  children are created during
   value choice (because many value choice heuristics need to look at
   them anyway).

   during search, the saved version of a leaf is the same (rather than
   just the assignment).  This makes it easy to check whether a
   satisfiable solution was found after the search exits (in case the
   algorithm consists of multiple stages).


   possible improvements:

   1. index nogoods by variable and value

   2. try removing nongoods that become obsolete (when checking for
   unit propagation)

*)


(************* data structures ***************)


(* var, val - as in csp.ml *)
type nogood = Csp.nogood

let unassigned = Csp.unassigned


(* score, domains, assignment *)
type child_info = float * ((int list) array) * (int array)

type node_kind = Unknown | Unsat | Sat | Branch of child_info list

type node = {
  (* the only ones that vary between nodes are domains, assignment,
     and kind.  kinds is always updated on a node after creation, the
     other two can be when only one child is viable (see determine_node) *)
  mutable kind : node_kind;
  mutable assignment : int array;
  mutable domains : (int list) array;
  nogoods : (nogood list) array;
  var_choice : node -> int option;
  (* given variable, returns sorted children (storing real nodes would
     store entire searched space!) *)
  val_choice : node -> int -> child_info list;
}


exception Unsatisfiable


(************ basic info ***************)


let num_vars n =
  Array.length n.domains


let max_domain_size p =
  snd (Wrarray.max_by List.length p.domains)


let sol = function
    None -> failwith "never reached a leaf!"
  | Some n -> n.assignment


let pr_kind n p =
  Verb.pr Verb.always "Kind is %s at %s.\n"
    (match n.kind with
       Unknown -> "unknown"
     | Sat -> "sat"
     | Unsat -> "unsat"
     | Branch c -> Wrutils.str "%d-way branch" (List.length c))
    p


(******* determining node type *******)


let rec determine_node n =
  n.kind <- (match n.var_choice n with
	       None -> Sat
	     | Some var ->
		 match (n.val_choice n var) with
		   [] -> Unsat
		 | (_, d, a)::[] ->
		     (* only one feasible value *)
		     n.domains <- d;
		     n.assignment <- a;
		     (determine_node n).kind
		 | children -> Branch children);
  n


let rec leaf_p n =
  match n.kind with
    Unknown -> leaf_p (determine_node n)
  | Unsat -> true
  | Sat -> true
  | Branch _ -> false


let satisfied n =
  match n.kind with
    Unsat -> false
  | Sat -> true
  | _ -> invalid_arg "satisfied called on a non-leaf!"


let better_p newer older =
  (satisfied newer) && (not (satisfied older))


let num_unsassigned n =
  match n.kind with
    Unsat | Sat ->
      let a = n.assignment
      and n = ref 0 in
	for i = 0 to (Array.length a) - 1 do
	  if a.(i) = unassigned then incr n
	done;
	float !n
  | _ -> invalid_arg "num_unsassigned called on a non-leaf!"


(******* generating children *******)


let num_children n =
  match n.kind with
    Branch children -> List.length children
  | _ -> failwith "num_children called on a non-branch"


let first_of_3 (v,_,_) =
  v


let child_costs n =
  match n.kind with
    Branch children -> List.map first_of_3 children
  | _ -> failwith "child_costs called on a non-branch"


let gen_child n children i =
  let _, d, a =  List.nth children i in
    { n with
	kind = Unknown;
	assignment = a;
	domains = d; }


let get_opt_child n i =
  match n.kind with
    Branch children -> (try Some (gen_child n children i)
			with Failure _ -> None)
  | _ -> failwith "get_opt_child called on a non-branch"


let get_child n i =
  match n.kind with
    Branch children -> gen_child n children i
  | _ -> failwith "get_child called on a non-branch"


(******* variable choice *******)


let smallest_domains n =
  let ties = ref []
  and min_domain = ref max_int
  and d = n.domains in
    Array.iteri (fun var value ->
		   if value == unassigned
		   then
		     let this = List.length d.(var) in
		       if this < !min_domain
		       then (ties := [var];
			     min_domain := this)
		       else if this == !min_domain
		       then ties := var::!ties)
      n.assignment;
    !ties


let ng_active_p a ng =
  (** a constraint is active if it is still relevant - no variables in
    it have been assigned values different than those referred to by the
    constraint *)
  List.for_all (fun (var, value) ->
		  let c = a.(var) in
		    (c == unassigned) || (c == value))
    ng


let most_constrained vars n =
  fst (Wrlist.maxes_by (fun var ->
			  Wrlist.count (ng_active_p n.assignment)
			  n.nogoods.(var))
	 vars)


let most_constrained_variable n =
  (** brelaz suggests `choose vertex adjacent to the most colors,
    breaking ties on larger degree'.  Another way to say this is `most
    constrained variable (smallest domain), breaking ties using most
    constraining variable (participates in the most active
    constraints).'  Ties are NOT broken randomly, to ensure the same
    tree every time. *)
  match smallest_domains n with
    [] -> None
  | var::[] -> Some var
      (* just take first among those that tie on constrainingness *)
  | vars -> Some (List.hd (most_constrained vars n))


(******* unit propagation *******)


exception Irrelevant
let no_such_var = -1


let rec unit_propagate d a nogoods var =
  (** Eg, forward checking.  a nogood is unit if all variables are
    assigned to the relevant values except for one, which is unassigned.

    might modify domains and assignment

    raises Unsatisfiable if an entire nogood matches *)
  List.iter (fun ng ->
	       try
		 let forced = ref no_such_var
		 and bad_val = ref no_such_var in
		   List.iter (fun (var, value) ->
				let curr = a.(var) in
				  if curr == unassigned then
				    (if (!forced == no_such_var) then
				       (forced := var;
					bad_val := value)
				     else
				       raise Irrelevant)
				  else
				    if curr != value then raise Irrelevant)
		     ng;
		   if !forced == no_such_var then
		     (* matched nogood *)
		     raise Unsatisfiable
		   else
		     (* was unit clause *)
		     (match Wrlist.remove_first !bad_val d.(!forced) with
			[] -> raise Unsatisfiable
		      | v::[] ->
			  a.(!forced) <- v;
			  unit_propagate d a nogoods !forced
		      | l -> d.(!forced) <- l)
	       with Irrelevant -> ())
    nogoods.(var)


(******* value choice *******)


let log_promise d a =
  let prod = ref 0. in
    for var = 0 to (Array.length a) - 1 do
      if a.(var) == unassigned
	(* domain must be >= 2, so log10 will be >= 0.3 *)
      then prod := !prod +. (log10 (float_of_int (List.length d.(var))))
    done;
    if !prod = 0.
      (* no violations and no variables unassigned - very promising! *)
    then log10 max_float
    else !prod


let log_promise_order n var =
  (** Geelen (ECAI-92): promise = product of future domains after
    forward checking = upper bound of possible solutions.  Choose value
    with max promise.

    returns list of values *)
  let pairs = Wrlist.map_opt (fun value ->
				let d = Array.copy n.domains
				and a = Array.copy n.assignment in
				  a.(var) <- value;
				  try
				    unit_propagate d a n.nogoods var;
				    (* -promise to sort largest first *)
				    Some (-. (log_promise d a), d, a)
				  with Unsatisfiable -> None)
		n.domains.(var)
  in
    Wrlist.sort_on first_of_3 pairs


let most_constraining n var =
  (** most occurrences in active constraints *)
  let vals = n.domains.(var) in
  let counts = Array.make (List.length vals) 0
  and a = n.assignment in
    List.iter (fun ng ->
		 try
		   let value = ref unassigned in
		     List.iter (fun (vr,vl) ->
				  let c = a.(vr) in
				    if ((c == unassigned) ||
					(c == vl))
				    then (if vr == var
					  then value := vl)
				    else raise Irrelevant)
		       ng;
		     counts.(!value) <- counts.(!value) + 1
		 with Irrelevant -> ())
      n.nogoods.(var);
    Wrlist.sort_on snd
      (List.map (fun x -> x, float_of_int (- (counts.(x))))
	 vals)


(******** initial state *******)


let initial_assignment d ngs =
  (** returns fresh assignment and whether the problem is clearly
    unsatisfiabile.  checks for empty or unit domains.  processes unit
    domains but does not propagate.  doesn't remove irrelevant
    nogoods. *)
  (*** PASS PROPAGATOR AS ARG.  pure literals, arc consistency ***)
  let a = Array.make (Array.length d) unassigned in
  let empty_domain = Wrarray.existsi (fun var -> function
					  [] -> true
					| value::[] ->
					    a.(var) <- value;
					    false
					| _ -> false)
		       d
		       (********** SHOULD PROPAGATE?? ***************)
  and empty_nogood = List.exists (function [] -> true | _ -> false) ngs in
    a, (empty_domain || empty_nogood)


let index_nogoods n ngs =
  let i = Array.make n [] in
    List.iter (fun ng ->
		 List.iter (fun (var,_) ->
			      i.(var) <- ng::i.(var))
		 ng)
      ngs;
    i


let initial_state var_choice val_choice p =
  let d, ngs = Csp.reduce_domains p in
  let a, unsat = initial_assignment d ngs in
    { domains = d;
      nogoods = index_nogoods (Array.length d) ngs;
      assignment = a;
      kind = if unsat then Unsat else Unknown;
      var_choice = var_choice;
      val_choice = val_choice; }


(* EOF *)
