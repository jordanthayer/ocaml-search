(** A simple parser for the SAS+ language output by the translator
    from the fast downward planner.

    @author eaburns
    @since 2009-07-30
*)

open Printf
open Scanf
open Sas

exception Parse_error of string

let expect_failure got expected =
  (** [expect_failure got expected] raises a parse error because we
      got a string [got] but we wanted to get [expected]. *)
  raise (Parse_error (sprintf "Expected '%s' got '%s'" expected got))


let parse_line b =
  (** [parse_line b] read a line from [b]. *)
  let line = bscanf b "%[^\n]\n" (fun s -> s) in
    Verb.pe Verb.debug "got line: '%s'\n" line;
    line


let header b expected =
  (** [header b expected] expects to read the string [expected] from
      [b] and fails if it doesn't. *)
  let header = parse_line b in
    Verb.pe Verb.debug "got header '%s'\n" header;
    if header <> expected
    then expect_failure header expected


let footer b expected vl =
  (** [footer b expected vl] expects to read the string [expected]
      from [b] and fails if it doesn't, but if it does [vl] is
      returned. *)
  let footer = parse_line b in
    Verb.pe Verb.debug "got footer '%s'\n" footer;
    if footer <> expected
    then expect_failure footer expected
    else vl


let valid_assign fname vdomains vr vl =
  (** [valid_assign vdomains vr vl] test if [vl] is a valid value for
      variable [vr] given [vdomains] the array of domain values. *)
  let nvars = Array.length vdomains in
    if vr >= nvars then
      invalid_arg (sprintf "%s: Variable var%d does not exist" fname vr);
    if vl >= vdomains.(vr) then
      invalid_arg (sprintf
		     "%s: Value %d is not in the domain of var%d"
		     fname vl vr)


let parse_metric b =
  (** [parse_metric b] parses the metric description from the given
      b. *)
  let _ = header b "begin_metric" in
  let metric = bscanf b "%d\n" (fun d -> d) in
    Verb.pe Verb.debug "metric: %d\n" metric;
    footer b "end_metric" metric


let parse_variables b =
  (** [parse_variables b] parses the variable description from the
      given b. *)
  let _ = header b "begin_variables" in
  let n = bscanf b "%d\n" (fun d -> d) in
  let domains = Array.make n ~-1 in
  let axiom_layers = Array.make n ~-1 in
    for i = 0 to n - 1 do
      bscanf b "var%d %d %d\n"
	(fun vnum dsize layer ->
	   Verb.pe Verb.debug "var%d: domain size=%d\n" i dsize;
	   domains.(i) <- dsize;
	   axiom_layers.(i) <- layer)
    done;
    footer b "end_variables" (domains, axiom_layers)


let parse_initial vdomains b =
  (** [parse_initial vdomains b] parses the initial state
      configuration from [b]. [vdomains] is an array with the domain
      of each variable. *)
  let nvars = Array.length vdomains in
  let inits = Array.make nvars ~-1 in
  let _ = header b "begin_state" in
    for i = 0 to nvars - 1 do
      bscanf b "%d\n"
	(fun v ->
	   if v >= vdomains.(i)
	   then invalid_arg
	     (sprintf "Initial value for var%d is outside of domain" i);
	   inits.(i) <- v)
    done;
    footer b "end_state" { s_vars = inits;  }


let parse_goal vdomains b =
  (** [parse_goal vdomains b] parses goal from [b].  [vdomains] is
      the array of the domain size for each variable. *)
  let rec do_parse ?(accum=[]) = function
    | 0 -> accum
    | left ->
	bscanf b "%d %d\n"
	  (fun vr vl ->
	     valid_assign "parse_goal" vdomains vr vl;
	     if List.exists (fun (x, _) -> x = vr) accum
	     then invalid_arg
	       (sprintf "Goal variable var%d specified twice" vr);
	     do_parse ~accum:((vr, vl)::accum) (left - 1))
  in
  let _ = header b "begin_goal" in
  let n = bscanf b "%d\n" (fun d -> d) in
    footer b "end_goal" { s_assigns = (do_parse n); }


let parse_preconds vdomains b =
  (** [parse_preconds vdomains b] parses the precondition list from
      an operator description. *)
  let rec do_parse ?(accum=[]) = function
    | 0 -> accum
    | left ->
	bscanf b "%d %d\n" (fun vr vl ->
			      valid_assign "parse_preconds" vdomains vr vl;
			      do_parse ~accum:((vr, vl)::accum) (left - 1))
  in bscanf b "%d\n" (fun n -> do_parse n)


let parse_effect vdomains b =
  (** [parse_effect vdomains b] parses a single effect. *)
  let rec parse_conds ?(accum=[]) = function
    | 0 -> accum
    | left ->
	bscanf b " %d %d"
	  (fun vr vl ->
	     valid_assign "parse_effect" vdomains vr vl;
	     parse_conds ~accum:((vr, vl)::accum) (left - 1))
  in
  let ncond = bscanf b "%d" (fun d -> d) in
  let conds = parse_conds ncond in
    bscanf b " %d %d %d\n" (fun vr req vl ->
			      valid_assign "parse_effect" vdomains vr vl;
			      valid_assign "parse_effect" vdomains vr req;
			      let e = conds, (vr, vl) in
			      let p = (if req = -1 then [] else [(vr, req)])
			      in e, p)


let parse_effects vdomains b =
  (** [parse_effects vdomains b] parses the effects of an operator.
      The result is the effects and a list of more preconditions for
      the operator. *)
  let rec do_parse ?(uncond=[])  ?(cond=[]) ?(pres=[]) = function
    | 0 -> uncond, cond, pres
    | left ->
	let ((cs, a) as eff), pre = parse_effect vdomains b in
	let cond' = if cs = [] then cond else eff :: cond
	and uncond' = if cs = [] then a :: uncond else uncond
	in do_parse ~uncond:uncond' ~cond:cond' ~pres:(pre @ pres) (left - 1)
  in bscanf b "%d\n" (fun n -> do_parse n)


let parse_operator vdomains next_id b =
  (** [parse_operator vdomains next_id b] parses a single operator
      from [b]. *)
  let _ = header b "begin_operator" in
  let name = parse_line b in
  let pres = parse_preconds vdomains b in
  let uncond_effs, cond_effs, more_pres = parse_effects vdomains b in
  let cost = bscanf b "%f\n" (fun f -> f) in
    Verb.pe Verb.debug "operator '%s'\n" name;
    footer b "end_operator"
      { o_name = name;
	o_id = next_id;
	o_cost = cost;
	o_conds = pres @ more_pres;
	o_uncond_effs = uncond_effs;
	o_cond_effs = cond_effs; }


let parse_operators vdomains b =
  (** [parse_operators vdomains b] parse the operators from
      [b]. [vdomains] is an array of the domain size for each
      variable. *)
  let next_id = ref 0 in
  let rec do_parse ?(accum=[]) = function
    | 0 -> accum
    | left ->
	let id = !next_id in
	  incr next_id;
	  do_parse ~accum:((parse_operator vdomains id b) :: accum) (left - 1)
  in bscanf b "%d\n" (fun n -> do_parse n)


let parse_axiom vdomains b =
  (** [parse_axiom vdomains b] parses a single axiom. *)
  let rec parse_conds ?(accum=[]) = function
    | 0 -> accum
    | left ->
	bscanf b " %d %d\n"
	  (fun vr vl ->
	     valid_assign "parse_axiom" vdomains vr vl;
	     parse_conds ~accum:((vr, vl)::accum) (left - 1))
  in
  let _ = header b "begin_rule" in
  let n = bscanf b "%d\n" (fun d -> d) in
  let conds = parse_conds n in
  let vr, pre, post = bscanf b "%d %d %d\n" (fun a b c -> a, b, c) in
    valid_assign "parse_axiom" vdomains vr pre;
    valid_assign "parse_axiom" vdomains vr post;
    footer b "end_rule"
      { a_conds = (vr, pre) :: conds;
	(* variable required value is the same as a condition. *)
	a_var = vr;
	a_val = post; }


let parse_axioms vdomains axiom_layers b =
  (** [parse_axioms vdomains b] parse the axioms. *)
  let n = bscanf b "%d\n" (fun n -> n) in
  let axioms = Array.make n [] in
    for i = 0 to n - 1 do
      let axiom = parse_axiom vdomains b in
      let layer = axiom_layers.(axiom.a_var) in
	axioms.(layer) <- axiom :: axioms.(layer);
    done;
    axioms


let parse ch =
  (** [parse ch] parses an SAS planning description from the given
      channel. *)
  let b = Scanf.Scanning.from_channel ch in
    Verb.pe Verb.debug "begin parsing\n";
    let _ = parse_metric b in
      Verb.pe Verb.debug "parsed metric\n";
      let vdomains, axiom_layers = parse_variables b in
	Verb.pe Verb.debug "parsed %d variables\n" (Array.length vdomains);
	let initial = parse_initial vdomains b in
	  Verb.pe Verb.debug "parsed initial state\n";
	  let goal = parse_goal vdomains b in
	    Verb.pe Verb.debug "parsed goal state\n";
	    let operators = parse_operators vdomains b in
	      Verb.pe Verb.debug "parsed %d operators\n"
		(List.length operators);
	      let axioms = parse_axioms vdomains axiom_layers b in
		Verb.pe Verb.debug "parsed %d axioms\n" (Array.length axioms);
		Verb.pe Verb.debug "end parsing\n";
		let props, prop_invs = Sas.propositionalize vdomains in
		let domain =
		  { nvariables = Array.length vdomains;
		    vdomains = vdomains;
		    operators = operators;
		    op_tree =
		      Cond_tree.build vdomains
			(List.map (fun o -> o.o_conds, o) operators);
		    props = props;
		    prop_invs = prop_invs;
		    axioms = axioms; }
		in domain, initial, goal
