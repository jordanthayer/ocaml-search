(** SAS planning description.  Parsing, expanding states, etc...

    @author eaburns
    @since 2009-07-24
*)

type t = {
  nvariables : int;
  (* Number of variables *)

  vdomains : int array;
  (* The domain size for each variable. *)

  operators : operator list;
  (* All of the possible actions. *)

  op_tree : operator Cond_tree.node;
  (* Decision tree used to find applicable operators. *)

  props : int array array;
  (* The propositionalized form of each assignment.  First indexed on
     variable then on value. *)

  prop_invs : (int * int) array;
  (* The inverse of the props function.  Gets the (variable, value)
     assignment for the given proposition. *)

  axioms : axiom list array;
  (* Array with an element corresponding to each layer.  Each entry is
     a list of axioms that effect the given layer. *) }

and state = {
  s_vars : int array;
  (* Value of each variable.*)
}

and partial_state = {
  s_assigns : assignment list;
  (* Assignments for some of the variables (var, value). *)
}

and operator = {
  o_name : string;
  (* Operator name. *)

  o_id : int;
  (* A unique ID for the operator.  Operator IDs are small ints
     starting at zero so that they can be used for indexind an
     array. *)

  o_cost : float;
  (* Operator cost. *)

  o_conds : assignment list;
  (* Operator perconditions. *)

  o_uncond_effs : assignment list;
  (* Unconditional effects. *)

  o_cond_effs : (assignment list * assignment) list;
  (* List of all of this operator's conditional effects. *)

(* NOTE: in the SAS format, each effect may contain an extra operator
   pre-condition... this is rolled into the conditions in the
   operator and is not stored with the effect. *)
}

and axiom = {
  a_conds : assignment list;
  (* Conditions for the axiom to fire. *)

  a_var : int;
  (* The variable effected by the axiom. *)

  a_val : int;
  (* The value to set the variable to if the axiom fires. *)
}

and assignment = int * int

let propositionalize vdomains =
  (** [propositionalize vdomains] propositionalize the valid
      assignments.  The result is tuple (P, P^{-1}) where P is a 2d
      array from variable and value to proposition and P^{-1} is an
      array that is the inverse of P. *)
  let nprops = Array.fold_left (+) 0 vdomains in
  let next_p = ref 0 in
  let prop_invs = Array.create nprops (~-1, ~-1) in
  let props =
    Array.mapi (fun vr nvls ->
		  (Array.init nvls (fun vl ->
				      let p = !next_p in
					incr next_p;
					prop_invs.(p) <- vr, vl;
					p)))
      vdomains
  in props, prop_invs


let rec conditions_met s conds =
  (** [conditions_met s conds] test if the list of conditions
      [conds] hold for the given state [s]. *)
  match conds with
    | [] -> true
    | (vr, vl) :: tl -> (s.s_vars.(vr) = vl) && (conditions_met s tl)


let apply_axioms axioms initial state =
  (** [apply_axioms domain initial state] apply the axioms from domain
      [domain] on state [state].  The result is [state].

      TODO: find some way to test that the axioms are being applied
      correctly. *)
  let apply_layer lst =
    let rec do_pass = function
      | [] -> false
      | hd :: tl ->
	  if conditions_met state hd.a_conds
	  then begin
	    state.s_vars.(hd.a_var) <- hd.a_val;
	    ignore (do_pass tl);
	    true
	  end else do_pass tl
    in while do_pass lst do () done
  in
    (* initialize to default levels. *)
    Array.iter
      (List.iter (fun a -> state.s_vars.(a.a_var) <- initial.s_vars.(a.a_var)))
      axioms;
    (* apply the axioms at each layer. *)
    Array.iter apply_layer axioms;
    state


let apply_operator domain initial state op =
  (** [apply_operator state op] apply the operator [op] to state
      [state].  This gives the resulting state.  This should only be
      called if the operator conditions apply. *)
  assert (conditions_met state op.o_conds);
  let vars' = Array.copy state.s_vars in
  let new_state = { s_vars = vars' } in
    List.iter (fun (vr, vl) -> vars'.(vr) <- vl) op.o_uncond_effs;
    List.iter
      (fun (cond, (vr, vl)) ->
	 if conditions_met state cond then vars'.(vr) <- vl)
      op.o_cond_effs;
    apply_axioms domain initial new_state


let make_expand1 domain initial =
  (** [make_expand domain initial state] builds an expand function.
      The expand function takes a state and returns a list of (child,
      operator) pairs for each generated child and the operator used
      to generate the child. *)
  (fun state ->
     let ops =
       List.filter (fun o -> conditions_met state o.o_conds)
	 domain.operators
     in
       List.map
	 (fun o -> apply_operator domain.axioms initial state o, o)
	 ops)


let make_expand2 domain initial =
  (** [make_expand domain initial state] builds an expand function.
      The expand function takes a state and returns a list of (child,
      operator) pairs for each generated child and the operator used
      to generate the child. *)
  let op_tree = domain.op_tree in
  let app = apply_operator domain.axioms initial in
    (fun state ->
       let succs = ref [] in
	 Cond_tree.for_all_applicable state.s_vars
	   (fun o ->
	      begin try assert (conditions_met state o.o_conds);
	      with _ ->
		Printf.printf "state:\n";
		Array.iteri (Printf.printf " %d=%d\n") state.s_vars;
		Printf.printf "\nop conditions:\n";
		List.iter (fun (vr, vl) -> Printf.printf " %d=%d\n" vr vl)
		  o.o_conds;
		Printf.printf "\n";
	      end;
	      succs := (app state o, o) :: !succs)
	   op_tree;
(*
	 begin
	   let ops =
	     List.filter (fun o -> conditions_met state o.o_conds)
	       domain.operators
	   in
	     assert ((List.length ops) = (List.length !succs));
	     assert (List.for_all (fun (_, o) -> List.mem o ops) !succs);
	 end;
*)
	 !succs)


let make_expand = make_expand1


let make_is_goal goal =
  (** [make_is_goal goal] builds a goal test function. *)
  (fun state -> conditions_met state goal.s_assigns)


let make_goal_lits_heuristic goal =
  (** [make_goal_lits_heuristic goal] creates a heuristic function
      that sums the number of goal literals not met in the current
      state. *)
  (fun state ->
     List.fold_left (fun sum assign ->
		       if not (conditions_met state [assign])
		       then sum +. 1.
		       else sum)
       0. goal.s_assigns)
