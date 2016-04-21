(* operations on lexbufs

*)


let lexbuf_file str =
  (** given an input channel, returns a lexbuf for use by a lexer *)
  Lexing.from_channel (open_in str)


let parse_error nonterm buf =
  (** gives a moderately informative error msg when parsing fails on
    lexbuf [buf].  [nonterm] should be the name of the kind of thing we
    were trying to parse. *)
  failwith (Wrutils.str
	      "error parsing %s detected at token \"%s\" ending at input position %d." nonterm (Lexing.lexeme buf) (Lexing.lexeme_end buf))


let parse_verb v ch nonterm_name parser_func buf =
  Verb.force v (lazy (Wrutils.pf ch "Reading %s..." nonterm_name; flush ch));
  try
    let d = parser_func buf in
      Verb.force v (lazy (Wrutils.pf ch "done.\n"; flush ch)) ;
      Parsing.clear_parser ();
      d
  with Parsing.Parse_error -> parse_error nonterm_name buf


(* EOF *)
