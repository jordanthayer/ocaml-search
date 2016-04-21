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
%token Action Condition Constants PDomain PProblem Duration Durative Effect Goal Init
%token Metric Objects Parameters Precondition Predicates Requirements Types
/* online message */
%token Newgoals Commitments Until Within Disable Enable Quit
/* have semantic value */
%token <string> Name Variable
%token <float> Number

%start domain problem message
%type <Domain.domain> domain
%type <Domain.problem> problem
%type <Message.message> message


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
       Domain.make_domain name requirements types constants predicates actions
   }
;

opt_require :
  OP Requirements Name name_star CP { $3::$4 }
  | { [":strips"] }
;

opt_type_defs :
  OP Types typed_name_list CP { Domain.make_types $3 }
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
  OP Constants typed_name_list CP { Domain.make_terms $3 Domain.Constant }
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
  OP Name typed_var_list CP { Domain.make_literal $2 $3 }
  | OP At typed_var_list CP { Domain.make_literal "at" $3 }
;

typed_var_list :
  var_star { Domain.make_terms [$1,"object"] Domain.Variable }
  | Variable var_star Dash Name typed_var_list
      { (Domain.make_terms [($1::$2),$4] Domain.Variable) @ $5 }
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
	 Domain.make_schema name params pre eff 1.
     }
  | OP Durative Name
     Parameters OP typed_var_list CP
     Duration OP Eq Variable Number CP
     Condition da_goal_desc
     Effect da_effect CP
  {
    assert ($11 = "duration");
    Domain.make_schema $3 $6 $15 $17 $12
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
  | OP Not aform_term CP { Domain.negate $3 }
;

aform_term :
  OP Name term_star CP { Domain.make_literal $2 $3 }
  | OP At term_star CP { Domain.make_literal "at" $3 }
  | OP Eq term_star CP { Domain.make_literal "equal" $3 }
;

term_star :
  term term_star { $1::$2 }
  | { [] }
;

term :
  Name { Domain.term Domain.Constant $1 }
  | Variable { Domain.term Domain.Variable $1 }
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
  OP Not aform_term CP { [Domain.negate $3] }
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
      Domain.make_problem gname objects init goal
  }
;



opt_objects :
  OP Objects typed_name_list CP { Domain.make_terms $3 Domain.Constant }
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
  | OP Not aform_name CP { Domain.negate $3 }
;

aform_name :
  OP Name name_star CP
  { Domain.make_literal $2 (List.map (Domain.term Domain.Constant) $3) }
  | OP At name_star CP
  { Domain.make_literal "at" (List.map (Domain.term Domain.Constant) $3) }
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

/************************ online message ***********************/


message:
  newgoals { $1 }
  | quit { $1 }
;
/*   | futureevents { $1 }
  | toggles { $1 } */



/*** new goals ***/
newgoals :
  OP Newgoals
  OP PDomain Name CP
  OP PProblem Name CP
  opt_objects
  init
  goal
  CP
  {
    Message.NewGoals (Domain.make_problem $9 $11 $12 $13)
  }
;

quit:
  {
    Message.Quit
  }
;

/*** toggles **
toggles :
  toggle_star { $1 }
;

toggle_star :
  toggle toggle_star { $1::$2 }
  | { [] }
;

toggle :
  OP At Number OP Enable Name CP CP
  { Message.Toggles (Message.make_toggle $3 Message.Enable $6)}
  | OP At Number OP Disable Name CP CP
  { Message.Toggles (Message.make_toggle $3 Message.Diable $6)}
;
**/

/**** future events **
futureevents :
  OP Commitments commitment_star CP
  {
    FutureEvents $3
  }
;

commitment_star :
  commitment commitment_star { $1 @ $2 }
  | { [] }
;

commitment :
  OP At Number event_star CP
  {
    Message.make_commitment $3 $4
  }
;

event_star :
  event event_star { $1 @ $2 }
  | { [] }
;

event :
  OP Until initel Number CP
  { Message.make_event Message.Until $3 $4}
  | OP Within initel Number CP
  { Message.make_event Message.Within $3 $4}
  | OP And event_star CP { $3 }
;
**/   


%%


(* EOF *)
