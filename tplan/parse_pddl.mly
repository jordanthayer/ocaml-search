/* $Id: parse.mly,v 1.1 2005/04/06 00:38:44 ruml Exp ruml $

  parser definition for tplan

  attempts simplified version of durative actions (level 3?) from PDDL 2.1
*/


%{
%}


/* single chars */
%token CP OP Dash Eq
/* no colon */
%token And At Define Domain Not Over Problem
/* colon */
%token Action Condition Constants PDomain Duration Durative Effect Goal Init
%token Metric Objects Parameters Precondition Predicates Requirements Types
/* have semantic value */
%token <string> Name Variable
%token <float> Number

%start domain problem
%type <Tpl_domain.domain> domain
%type <Tpl_domain.problem> problem


%%


/**************************** domain ***********************/


domain :
   OP Define OP Domain Name CP
      opt_require
      opt_type_defs
      opt_constants
      opt_predicates
      action_star CP
   {
     let name = $5
     and requirements = $7
     and types = $8
     and constants = $9
     and predicates = $10
     and actions = $11 in
       Verb.pe 5 "got domain \"%s\" (%d actions)..."
	 name (List.length actions);
       Tpl_domain.make_domain name requirements types constants predicates actions
   }
;

opt_require :
  OP Requirements Name name_star CP { $3::$4 }
  | { [":strips"] }
;

opt_type_defs :
  OP Types typed_name_list CP { Tpl_domain.make_types $3 }
  | { [] }
;

/* not handling "either" for now ***/
typed_name_list :
  name_star { [$1,"object"] }
  | Name name_star Dash Name typed_name_list
      { (($1::$2), $4)::$5 }
;

name_star :
  Name name_star { $1::$2 }
  | { [] }
;

opt_constants :
  OP Constants typed_name_list CP { Tpl_domain.make_terms $3 Tpl_domain.Constant }
  | { [] }
;

opt_predicates :
  OP Predicates aform_skel aform_skel_star CP { Some ($3::$4) }
  | { None }
;

aform_skel_star :
  aform_skel aform_skel_star { $1::$2 }
  | { [] }
;

aform_skel :
  OP Name typed_var_list CP { Tpl_domain.make_literal $2 $3 }
  | OP At typed_var_list CP { Tpl_domain.make_literal "at" $3 }
;

typed_var_list :
  var_star { Tpl_domain.make_terms [$1,"object"] Tpl_domain.Variable }
  | Variable var_star Dash Name typed_var_list
      { (Tpl_domain.make_terms [($1::$2),$4] Tpl_domain.Variable) @ $5 }
;

var_star :
  Variable var_star { $1::$2 }
  | { [] }
;

action_star :
  action action_star { $1::$2 }
  | { [] }
;

action :
  OP Action Name
     Parameters OP typed_var_list CP
     Precondition goal_desc
     Effect effect CP
     {
       let name = $3
       and params = $6
       and pre = $9
       and eff = $11 in
	 Tpl_domain.make_schema name params pre eff 1.
     }
  | OP Durative Name
     Parameters OP typed_var_list CP
     Duration OP Eq Variable Number CP
     Condition da_goal_desc
     Effect da_effect CP
  {
    assert ($11 = "duration");
    Tpl_domain.make_schema $3 $6 $15 $17 $12
  }
;

goal_desc :
  literal_term { [$1] }
  | OP And goal_desc_star CP { $3 }
;

goal_desc_star :
  goal_desc goal_desc_star { $1 @ $2 }
  | { [] };

literal_term :
  aform_term { $1 }
  | OP Not aform_term CP { Tpl_domain.negate $3 }
;

aform_term :
  OP Name term_star CP { Tpl_domain.make_literal $2 $3 }
  | OP At term_star CP { Tpl_domain.make_literal "at" $3 }
  | OP Eq term_star CP { Tpl_domain.make_literal "equal" $3 }
;

term_star :
  term term_star { $1::$2 }
  | { [] }
;

term :
  Name { Tpl_domain.term Tpl_domain.Constant $1 }
  | Variable { Tpl_domain.term Tpl_domain.Variable $1 }
;

effect :
  aeffect { $1 }
  | OP And effect effect_star CP { $3 @ $4 }
;

effect_star :
  effect effect_star { $1 @ $2 } | { [] }
;

aeffect :
  OP And peffect peffect_star CP { $3 @ $4 }
  | peffect { $1 }
;

peffect_star :
  peffect peffect_star { $1 @ $2 }
  | { [] }
;

peffect :
  OP Not aform_term CP { [Tpl_domain.negate $3] }
  | aform_term { [$1] }
;

da_goal_desc :
  timed_goal_desc { $1 }
  | OP And timed_goal_desc timed_goal_desc_star CP { $3 @ $4 }
;

timed_goal_desc_star :
  timed_goal_desc timed_goal_desc_star { $1 @ $2 }
  | { [] }
;

timed_goal_desc :
  OP At Name goal_desc CP
  { Verb.pe 4 "Note: ignoring precondition time `%s' - ensuring preconditions until end.\n" $3;
    $4 }
  | OP Over Name goal_desc CP
      { assert ($3 = "all");
	$4 }
;

da_effect :
  OP And da_effect_star CP { $3 }
  | timed_effect { $1 }
;

da_effect_star :
  da_effect da_effect_star { $1 @ $2 }
  | { [] }
;

timed_effect :
  OP At Name aeffect CP
  { let time = $3
    and lits = $4 in
      if time <> "end" then
	Verb.pe 4 "Note: ignoring effect time `%s' - using effects only after action.\n" time;
      lits }
;


/**************************** problem ************************/



problem :
  OP Define OP Problem Name CP
    OP PDomain Name CP
    opt_objects
    init
    goal
    opt_metric CP
  {
    let gname = $5
    (* and dname = $9 *)
    and objects = $11
    and init = $12
    and goal = $13 in
      Verb.pe 5 "got problem \"%s\"..." gname;
      Tpl_domain.make_problem objects init goal
  }
;

opt_objects :
  OP Objects typed_name_list CP { Tpl_domain.make_terms $3 Tpl_domain.Constant }
  | { [] }
;

init :
  OP Init initel initel_star CP { $3::$4 }
;

initel_star :
  initel initel_star { $1::$2 }
  | { [] }
;

initel :
  literal_name { $1 }
;

literal_name :
  aform_name { $1 }
  | OP Not aform_name CP { Tpl_domain.negate $3 }
;

aform_name :
  OP Name name_star CP
  { Tpl_domain.make_literal $2 (List.map (Tpl_domain.term Tpl_domain.Constant) $3) }
  | OP At name_star CP
  { Tpl_domain.make_literal "at" (List.map (Tpl_domain.term Tpl_domain.Constant) $3) }
;

goal :
  OP Goal goal_desc CP { $3 }
;

opt_metric :
  OP Metric Name Name CP { assert ($3 = "minimize");
			   assert ($4 = "total-time") }
  | OP Metric Name OP Name CP CP { assert ($3 = "minimize");
			           assert ($5 = "total-time") }
  | {}
;


%%


(* EOF *)
