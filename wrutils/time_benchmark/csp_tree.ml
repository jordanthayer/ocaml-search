(* $Id: functions.ml,v 1.1 2003/09/27 01:13:33 ruml Exp ruml $

   tree-structured search space for CSPs

   all the action happens during leaf_p.  children are created during
   value choice (because many value choice heuristics need to look at
   them anyway).

   during search, the saved version of a leaf is the same (rather than
   just the assignment).  This makes it easy to check whether a
   satisfiable solution was found after the search exits (in case the
   algorithm consists of multiple stages).
*)


(************* data structures ***************)


type nogood = (int * int) list


type node = {
  (* the only ones that vary between nodes are domains, assignment,
     and kind.  kinds is always updated on a node after creation, the
     other two can be when only one child is viable (see determine_node) *)
  mutable domains : (int list) array;
  nogoods : (nogood list) array;
  mutable assignment : int array;
  mutable kind : node_kind;
  var_choice : node -> int option;
  val_choice : node -> int -> (node * float) list;
}

and node_kind = Unknown | Unsat | Sat | Branch of (node * float) list

    
let unassigned = -999999


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
  Printf.printf "Kind is %s at %s.\n"
    (match n.kind with
       Unknown -> "unknown"
     | Sat -> "sat"
     | Unsat -> "unsat"
     | Branch c -> Utils.str "%d-way branch" (List.length c))
    p
    

(******* determining node type *******)


let rec determine_node n =
  n.kind <- (match n.var_choice n with
	       None -> Sat
	     | Some var ->
		 match (n.val_choice n var) with
		   [] -> Unsat
		 | (child, _)::[] ->
		     (* only one feasible value *)
		     n.domains <- child.domains;
		     n.assignment <- child.assignment;
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
  | _ -> failwith "satisfied called on a non-Sat/Unsat node!"


let better_p newer older =
  not (satisfied older)
      

(******* generating children *******)


let copy_node n =
  { n with
      domains = Array.copy n.domains;
      assignment = Array.copy n.assignment }


exception Irrelevant
let no_such_var = -1
		    
  
let rec unit_propagate n var =
  (* a nogood is unit if all variables are assigned to the relevant
     values except for one, which is unassigned.

     might modify domains and assignment

     raises Unsatisfiable if an entire nogood matches *)
  let a = n.assignment
  and d = n.domains in
    List.iter (fun ng ->
		 try
		   let forced = ref no_such_var
		   and bad_val = ref no_such_var in
		     List.iter (fun (var, value) ->
				  let curr = a.(var) in
				    if curr == unassigned
				    then (if (!forced == no_such_var)
					  then (forced := var;
						bad_val := value)
					  else raise Irrelevant)
				    else if curr != value
				    then raise Irrelevant)
		       ng;
		     if !forced == no_such_var
		     then raise Unsatisfiable
		     else
		       (* was unit *)
		       match Wrlist.remove_first !bad_val d.(!forced) with
			 v::[] ->
			   a.(!forced) <- v;
			   unit_propagate n !forced
		       | l -> d.(!forced) <- l
		 with Irrelevant -> ())
      n.nogoods.(var)
  

let num_children n =
  match n.kind with
    Branch children -> List.length children
  | _ -> failwith "num_children called on a non-branch"
      

let child_costs n =
  match n.kind with
    Branch children -> List.map snd children
  | _ -> failwith "child_costs called on a non-branch"
    

let get_opt_child n i =
  match n.kind with
    Branch children -> (try
			  Some (fst (List.nth children i))
			with Failure _ -> None)
  | _ -> failwith "get_child called on a non-branch"


let get_child n i =
  match n.kind with
    Branch children -> fst (List.nth children i)
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
  (* a constraint is active if it is still relevant - no variables in
     it have been assigned values different than those referred to by the
     constraint *)
  List.for_all (fun (var, value) ->
		  let c = a.(var) in
		    (c == unassigned) || (c == value))
    ng


let most_constrained vars n =
  let ties, _ = Wrlist.maxes_by (fun var ->
				   Wrlist.count (ng_active_p n.assignment)
				   n.nogoods.(var))
		  vars
  in
    ties
				   

let most_constrained_variable n =
  (* brelaz suggests `choose vertex adjacent to the most colors,
     breaking ties on larger degree'.  Another way to say this is `most
     constrained variable (smallest domain), breaking ties using most
     constraining variable (participates in the most active constraints),
     breaking ties randomly' *)
  match smallest_domains n with
    [] -> None
  | var::[] -> Some var
  | vars ->
      match most_constrained vars n with
	var::[] -> Some var
      | vars -> Some (Wrlist.random_elt vars)


(******* value choice *******)


let log_promise n =
  let d = n.domains
  and a = n.assignment
  and prod = ref 0. in
    for var = 0 to ((num_vars n) - 1) do
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
				let child = copy_node n in
				  child.assignment.(var) <- value;
				  try
				    unit_propagate child var;
				    Some (child, -. (log_promise child))
				  with Unsatisfiable -> None)
		n.domains.(var)
  in
      Wrlist.sort_on snd pairs


let most_constraining n var =
  (* most occurrences in active constraints *)
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


let reduce_domains d ngs =
  (* remove duplicates from d and ngs, process unary constraints and
     unit domains *)
  let d = Array.map Wrlist.remove_duplicates d in
  let ngs = List.filter (function
			     (var, value)::[] ->
			       d.(var) <- Wrlist.remove_first value d.(var);
			       false
			   | _ -> true)
	      (Wrlist.remove_duplicates ngs)
  in
  let a = Array.make (Array.length d) unassigned in
    Array.iteri (fun var -> function
		     value::[] -> a.(var) <- value
		   | _ -> ())
      d;		     
    d, ngs, a

      
let index_nogoods n ngs =
  let i = Array.make n [] in
    List.iter (fun ng ->
		 List.iter (fun (var,_) ->
			      i.(var) <- ng::i.(var))
		 ng)
      ngs;
    i
      

let check_domains_p d =
  (* check for empty domain or unassigned as a potential value *)
  Wrarray.for_all (function
		       [] -> false
		     | l ->
			 assert (not (List.mem unassigned l));
			 true)
    d

      
let initial_state var_choice val_choice p =
  let d, ngs, a = reduce_domains (Csp.domains p) (Csp.nogoods p) in
    { domains = d;
      nogoods = index_nogoods (Array.length d) ngs;
      assignment = a;
      kind = if check_domains_p d then Unknown else Unsat;
      var_choice = var_choice;
      val_choice = val_choice; }
    

(* EOF *)
