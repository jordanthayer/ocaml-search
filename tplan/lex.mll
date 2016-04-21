(* $Id: lex.mll,v 1.1 2005/04/06 00:38:49 ruml Exp ruml $
   
   lexer for tplan

   In PDDL, names begin with letter and contain only
   letters, digits, hyphens, and underscores.  Case is not significant.
*)

{
  open Lexing
   (* parse_pddl.mli (from parse_pddl.mly) defines the type token *)
  open Parse_pddl

  let kw_table = Wrht.of_pairs
		   [ "and", And;
		     "at", At;
		     "define", Define;
		     "domain", Domain;
		     "not", Not;
		     "over", Over;
		     "problem", Problem;
		     ":action", Action;
		     ":condition", Condition;
		     ":constants", Constants;
		     ":domain", PDomain;
		     ":duration", Duration;
		     ":durative-action", Durative;
		     ":effect", Effect;
		     ":goal", Goal;
		     ":init", Init;
		     ":metric", Metric;
		     ":objects", Objects;
		     ":parameters", Parameters;
		     ":precondition", Precondition;
		     ":predicates", Predicates;
		     ":requirements", Requirements;
		     ":types", Types; ]

  let get_text buf =
    String.lowercase (lexeme buf)
}

let newline = ['\n' '\012' '\r']
let not_newline = [^ '\n' '\012' '\r']
let whitespace = ( [' ' '\t'] | newline )*
let letter = ['a'-'z' 'A'-'Z']
let digit = ['0'-'9']
(* PDDL specs only - and _, but no reason we can't accept more *)
let punct = ['-' '_' ':']
let stuff = ( letter | digit | punct )
		   
rule lexer = parse
    whitespace
      { lexer lexbuf }
  (* comments go until end of line *)
  | ';' ( not_newline*)
      { lexer lexbuf }
  | '('
      { OP }
  | ')'
      { CP }
  | '-'
      { Dash }
  | '='
      { Eq }
  | digit* ('.' digit*)?
      { Number (float_of_string (get_text lexbuf)) }
  | '?' stuff+
      { Variable (Wrstr.peel (get_text lexbuf)) }
  | stuff*
      {
	let text = get_text lexbuf in
	  try
	    Hashtbl.find kw_table text
	  with Not_found -> Name text
      }
  (* other single character rules occur earlier and will be preferred if
     they can match *)
  | eof { failwith "got eof while still trying to read!" }
  | _ { failwith (Printf.sprintf
		    "lexing problem: bad character '%s' at input position %d?" (lexeme lexbuf) (lexeme_start lexbuf)) }    


(* EOF *)
