(** Building a planning graph.

    @author eaburns
    @since 2009-09-24
*)

open Printf

let apply_axioms axiom_applies apply axioms =
  (** [apply_axioms axiom_applies apply axioms] applies the axioms to the
      axioms for which the [axiom_applies] predicate is true.  Axioms are
      applied with the [apply] function. *)
  let apply_layer lst =
    let rec do_pass = function
      | [] -> false
      | ax :: axs ->
	  if axiom_applies ax
	  then begin
	    let vr = ax.Sas.a_var and vl = ax.Sas.a_val in
	      apply vr vl;
	      ignore (do_pass axs);
	      true
	  end else (do_pass axs)
    in while do_pass lst do () done
  in Array.iter apply_layer axioms


let apply_effects conds_are_met apply ops =
  (** [apply_effects conds_are_met apply ops] applies the effects of
      the given operators for which the [effect_applies] predicate is
      true using the [apply] function. *)
  List.iter
    (fun o ->
       List.iter (fun (vr, vl) -> apply vr vl) o.Sas.o_uncond_effs;
       List.iter (fun (conds, (vr, vl)) ->
		    if conds_are_met conds then apply vr vl)
	 o.Sas.o_cond_effs)
    ops

(** {1 Simple planning graph with no mutexes} ********************)

module No_mutex = struct

  type t =
    | Nil
    | Layer of t * Sas.operator list * facts
	(* Previous layer, operators that were applied at the previous
	   layer and the facts that are true at the current layer. *)

  and facts = Bitset.t array
      (* Set of facts that could be true in a layer of the graph. *)


  let copy_facts facts =
    (** [copy_facts facts] returns a copy of [facts]. *)
    Array.init (Array.length facts) (fun i -> Bitset.copy facts.(i))


  let facts_of_state domain s =
    (** [facts_of_state s] gets a bitset for each variable that
	represents the variable's domain. *)
    Array.mapi (fun i v ->
		  let s = Bitset.create domain.Sas.vdomains.(i) in
		    Bitset.insert s v;
		    s)
      s.Sas.s_vars


  let rec conditions_met facts conds =
    (** [conditions_met facts conds] tests if the given conditions are
	met. *)
    match conds with
      | [] -> true
      | (vr, vl) :: tl ->
	  (Bitset.mem facts.(vr) vl) && (conditions_met facts tl)


  let operator_applies facts op =
    (** [operator_applies facts op] tests if the given operator is
	applicable. *)
    conditions_met facts op.Sas.o_conds


  let axiom_applies facts ax =
    (** [axiom_applies facts ax] tests if the given axiom is
	applicable. *)
    conditions_met facts ax.Sas.a_conds


  let apply_axioms facts axioms =
    (** [apply_axioms facts axioms] applies the axioms to the fact set. *)
    let applies = axiom_applies facts in
    let apply vr vl = Bitset.insert facts.(vr) vl in
      apply_axioms applies apply axioms

  let apply_effects facts ops =
    let conds_are_met = conditions_met facts in
    let apply vr vl = Bitset.insert facts.(vr) vl in
      apply_effects conds_are_met apply ops


  let rec build_graph axioms ops goal prev =
    (** [build_graph axiioms ops goal prev] builds the planning graph. *)
    let goal_achieved facts =
      List.for_all (fun (vr, vl) -> Bitset.mem facts.(vr) vl) goal.Sas.s_assigns
    in
      match prev with
	| Nil -> invalid_arg (sprintf "Planning_graph.build_graph: Nil prev")
	| Layer (last_prev, last_ops, last_facts) ->
	    if goal_achieved last_facts
	    then prev
	    else begin
	      let app_ops =
		List.filter (operator_applies last_facts) ops
	      in
		if ops = []
		then Nil
		else begin
		  let facts = copy_facts last_facts in
		    apply_effects facts app_ops;
		    apply_axioms facts axioms;
		    build_graph axioms ops goal (Layer (prev, app_ops, facts))
		end
	    end


  let rec output_graph chan = function
      (** [output_graph chan g] outputs the planning graph to the given
	  channel. *)
    | Nil -> ()
    | Layer (prev, ops, facts) ->
	output_graph chan prev;
	Printf.printf "--------------------\noperators\n";
	List.iter (fun op -> Printf.printf "%s\n" op.Sas.o_name) ops;
	Printf.printf "facts\n";
	Array.iteri (fun vr vls ->
		       for i = 0 to Bitset.max vls do
			 if Bitset.mem vls i then Printf.printf "%d=%d\n" vr i;
		       done)
	  facts


  let make_create domain initial goal =
    (** [make_create domain initial goal] creates a planning graph
	function. *)
    (fun state ->
       let facts = facts_of_state domain state in
       let axioms = domain.Sas.axioms and operators = domain.Sas.operators in
	 (* set initial axiom values. *)
	 Array.iter
	   (List.iter (fun a ->
			 let vr = a.Sas.a_var in
			 let vl = initial.Sas.s_vars.(vr) in
			   Bitset.insert facts.(vr) vl))
	   domain.Sas.axioms;
	 (* build the graph *)
	 let g = build_graph axioms operators goal (Layer (Nil, [], facts)) in
	   (*
	     output_graph stdout g;
	   *)
	   g
    )


  let rec number_of_layers = function
    | Nil -> 0.
    | Layer (prev, _, _) -> 1. +. number_of_layers prev
end

(** {1 Counting achieved preconditions} ********************)

module Effect_activation = struct
  (* Counting the number of conditions for each operator and
     conditional effect that are currently met.  This allows easily
     tracking the active effects when building a planning graph. *)

  type effect = {
    op : Sas.operator;
    eff : bool;
    assigns : Sas.assignment list;
    conds : Sas.assignment list;
    n_conds : int;
    mutable cur : int;
  }

  type t = {
    counts : effect list array array;
    mutable active : effect list;
  }

  let create vdomains ops =
    (** [create vdomains ops] creates an object to track which effects
	are active. *)
    let nvars = Array.length vdomains in
    let m =
      Array.init nvars (fun vr -> Array.init vdomains.(vr) (fun _ -> []))
    in
      List.iter
	(fun op ->
	   let conds = op.Sas.o_conds in
	   let n_conds = List.length conds in
	   let op_count = { op = op;
			    eff = false;
			    assigns = op.Sas.o_uncond_effs;
			    conds = conds;
			    n_conds = n_conds;
			    cur = 0 }
	   in
	     List.iter (fun (vr, vl) ->
			  m.(vr).(vl) <- op_count :: m.(vr).(vl);)
	       conds;
	     List.iter
	       (fun (eff_conds, (vr, vl)) ->
		  let n_conds = n_conds + (List.length eff_conds) in
		  let eff_count = { op = op;
				    eff = true;
				    assigns = [ vr, vl ];
				    conds = eff_conds @ conds;
				    n_conds = n_conds;
				    cur = 0; }
		  in List.iter (fun (vr, vl) ->
				  m.(vr).(vl) <- eff_count :: m.(vr).(vl))
		       eff_conds)
	       op.Sas.o_cond_effs)
	ops;
      { counts = m; active = [] }


  let new_assignment t (vr, vl) =
    (** [new_assignment t (vr, vl)] adds all effects activated by the
	latest assignment. *)
    List.iter
      (fun eff ->
	 eff.cur <- eff.cur + 1;
	 assert (eff.cur <= eff.n_conds);
	 if eff.cur = eff.n_conds then t.active <- eff :: t.active)
      t.counts.(vr).(vl)


  let reset t =
    (** [reset t] resets all active effects. *)
    Array.iter (Array.iter (List.iter (fun e -> e.cur <- 0))) t.counts;
    t.active <- []


  let iter_active t f =
    (** [iter_active t f] iterate f over the active effects. *)
    List.iter (fun e -> f e.op e.conds e.assigns) t.active

end

(** {1 Planning graph with binary mutexes} ********************)

module Two_mutex = struct

  (*
    From http://planning.cs.uiuc.edu/node66.html

    operators:
    Inconsistent effects: An effect of o is the negated literal of an effect of o'.
    Interference: An effect of o is the negated literal of a precondition of o'.
    Competing needs: A pair of preconditions, one from each of o and o', are mutex in L_i.

    facts:
    Negated literals: l and l' form a complementary pair.
    Inconsistent support: Every pair of operators, o,o' \in O_{i-1}, that achieve l and l' is mutex. In this case, one operator must achieve l, and the other must achieve l'. If there exists an operator that achieves both, then this condition is false, regardless of the other pairs of operators.
  *)


  (*
    Ignore conditional effects when finding mutex relations!
  *)

  type operator = {
    op : Sas.operator option;
    preconds : assignment list;
    o_mutexes : (operator, bool) Hashtbl.t;
  }


  and assignment = {
    assign : (int * int);
    mutable achievers : operator list;
    a_mutexes : (assignment, bool) Hashtbl.t;
  }


  type fact_layer = {
    depth : int;
    prev : fact_layer;

    assigns : ((int * int), assignment) Hashtbl.t;
    (* achievable assignments. *)
  }


  let tbl_size = 10
    (** The initial size of hash tables that are used all over the place. *)


  let rec non_mutex = function
      (** [non_mutex conds] tests if the conditions are non-mutex in
	  the current layer. *)
    | _ :: [] | [] -> true
    | a :: rest ->
	(List.for_all (fun a' -> not (Hashtbl.mem a.a_mutexes a')) rest)
	&& non_mutex rest


  let achieve active layer achiever a =
    (** [achieve active layer achiever a] achieve the given assignment. *)
    let assigns = layer.assigns in
      try
	let asn = Hashtbl.find assigns a in
	  asn.achievers <- achiever :: asn.achievers;
      with Not_found ->
	let asn = { assign = a;
		    achievers = [ achiever ];
		    a_mutexes = Hashtbl.create tbl_size;
		  }
	in
	  Effect_activation.new_assignment active a;
	  Hashtbl.add assigns a asn


  let no_op asn =
    (** [no_op asn] creates a no-op. *)
    let pre = Hashtbl.create tbl_size in
      Hashtbl.add pre (fst asn.assign) asn;
      { op = None;
	o_mutexes = Hashtbl.create tbl_size;
	preconds = [ asn ];
      }


  let add_operator_mutex a b =
    (** [add_operator_mutex a b] adds a mutex between operators [a]
	and [b]. *)
    Hashtbl.replace a.o_mutexes b true;
    Hashtbl.replace b.o_mutexes a true


  let track_preconditions preconds op =
    (** [track_preconditions preconds op] tracks the preconditions of
	the operator and adds competing needs mutexes where
	appropriate. *)
    List.iter (fun a ->
		 let vr, vl = a.assign in
		   List.iter (fun (vl', op') ->
				if vl <> vl' then add_operator_mutex op op')
		     (Hashtbl.find_all preconds vr);
		   Hashtbl.add preconds vr (vl, op))
      op.preconds


  let track_effects preconds effects op (vr, vl) =
    (** [track_effects preconds effects op assign] tracks the effect
	of the given operator. *)
    List.iter
      (fun (vl', op') -> if vl <> vl' then add_operator_mutex op op')
      (Hashtbl.find_all effects vr);
    List.iter
      (fun (vl', op') -> if vl <> vl' then add_operator_mutex op op')
      (Hashtbl.find_all preconds vr);
    Hashtbl.add effects vr (vl, op)


  let add_no_ops active preconds effects ops layer layer' =
    (** [add_no_ops active preconds effects ops layer layer'] adds the
	no-ops to the new layer. *)
    Hashtbl.iter (fun _ a ->
		    let op = no_op a in
		      ops := op :: !ops;
		      track_preconditions preconds op;
		      track_effects preconds effects op a.assign;
		      achieve active layer' op a.assign;)
      layer.assigns


  let handle_active_effects active preconds effects ops layer layer' =
    (** [handle_active_effects active preconds effects ops layer
	layer'] adds the active active (without mutex preconditions)
	to the new layer. *)
    Effect_activation.iter_active active
      (fun op conds assigns ->
	 let conds = List.map (Hashtbl.find layer.assigns) conds in
	   if non_mutex conds;
	   then begin
	     let op = { op = Some op;
			o_mutexes = Hashtbl.create tbl_size;
			preconds = conds; }
	     in
	       ops := op :: !ops;
	       track_preconditions preconds op;
	       List.iter (fun a ->
			    track_effects preconds effects op a;
			    achieve active layer' op a)
		 assigns
	   end)


  let add_assignment_mutexes assigns =
    (** [add_assignment_mutexes assigns] add mutexes between
	the given assignments. *)
    let add_assignment_mutex a b =
      Hashtbl.replace a.a_mutexes b true;
      Hashtbl.replace b.a_mutexes a true;
    in
    let mutex_with a b =
      if (fst a.assign) = (fst b.assign) && (snd a.assign) <> (snd b.assign)
      then add_assignment_mutex a b
      else if (List.for_all (fun op' ->
			       List.for_all
				 (fun op -> Hashtbl.mem op.o_mutexes op')
				 a.achievers)
		 b.achievers)
      then add_assignment_mutex a b
    in
      ignore (Hashtbl.fold (fun (vr, vl) a rest ->
			      List.iter (mutex_with a) rest;
			      a :: rest)
		assigns [])


  let handle_operators active layer layer' =
    (** [handle_operators active layer layer'] get the assignments
	for all operators that are active/non-mutex.  Also, add the
	mutexes between operators. *)
    let preconds = Hashtbl.create tbl_size in
    let effects = Hashtbl.create tbl_size in
    let ops = ref [] in
      add_no_ops active preconds effects ops layer layer';
      handle_active_effects active preconds effects ops layer layer';
      add_assignment_mutexes layer'.assigns


  let rec next_layer active layer =
    (** [next_layer active layer] builds the next layer of the
	graph. *)
    let layer' = { depth = layer.depth + 1;
		   prev = layer;
		   assigns = Hashtbl.create tbl_size; }
    in
      handle_operators active layer layer';
      layer'


  let make_build vdomains ops =
    (** [make_build vdomains ops] makes a function that builds a
	planning graph for the given state. *)
    let active = Effect_activation.create vdomains ops in
    let n_vars = Array.length vdomains in
      (fun state ->
	 let assigns = Hashtbl.create tbl_size in
	 let rec init_layer = { depth = 0;
				prev = init_layer;
				assigns = assigns;
			      }
	 in
	   Effect_activation.reset active;
	   for vr = 0 to n_vars - 1 do
	     let achiever = { op = None;
			      o_mutexes = Hashtbl.create 0;
			      preconds = []; }
	     in achieve active init_layer achiever (vr, state.(vr))
	   done;
	   next_layer active init_layer;)
end
