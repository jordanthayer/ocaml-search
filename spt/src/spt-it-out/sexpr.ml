(** Parsing (and lexing) the simple s-expressions for the input file.

    @author eaburns
    @since 2010-05-14
*)

open Printf

module Lexer = struct

  type t = {
    mutable line_no : int;
    stream : char Stream.t;
  }

  type token =
    | Open_paren of int
    | Close_paren of int
    | Ident of int * string
    | String of int * string
    | Number of int * float

  let string_of_token = function
    | Open_paren _ -> "("
    | Close_paren _ -> ")"
    | Ident (_, s) -> s
    | String (_, s) -> s
    | Number (_, vl) -> sprintf "%f" vl

  let lineno_of_token = function
    | Open_paren l
    | Close_paren l
    | Ident (l, _)
    | String (l, _)
    | Number (l, _) -> l


  let rec lex_string ?(buf=Buffer.create 10) l =
    try
      match Stream.next l.stream with
	| '"' -> String (l.line_no, Buffer.contents buf)
	| '\\' ->
	    begin match Stream.next l.stream with
	      | 'n' ->
		  Buffer.add_char buf '\n';
		  lex_string ~buf l
	      | 't' ->
		  Buffer.add_char buf '\t';
		  lex_string ~buf l
	      | '\\' ->
		  Buffer.add_char buf '\\';
		  lex_string ~buf l
	      | '"' ->
		  Buffer.add_char buf '"';
		  lex_string ~buf l
	      | c ->
		  failwith (sprintf "line %d: Bad escape string '\\%c'"
			      l.line_no c)
	    end
	| c ->
	    if c = '\n' then l.line_no <- l.line_no + 1;
	    Buffer.add_char buf c;
	    lex_string ~buf l
    with Stream.Failure ->
      failwith (sprintf "End of string not found on line %d" l.line_no)


  let rec lex_ident ?(buf=Buffer.create 10) l =
    match Stream.peek l.stream with
      | None -> Ident (l.line_no, Buffer.contents buf)
      | Some c -> match c with
	  | '\n' ->
	      Stream.junk l.stream;
	      l.line_no <- l.line_no + 1;
	      Ident (l.line_no - 1, Buffer.contents buf)
	  | '(' | ')'| ' ' | '\t' | ';' ->
	      Ident (l.line_no, Buffer.contents buf)
	  | c ->
	      Stream.junk l.stream;
	      Buffer.add_char buf c;
	      lex_ident ~buf l


  let rec lex_number ?(buf=Buffer.create 10) l =
    match Stream.peek l.stream with
      | None -> Number (l.line_no - 1, float_of_string (Buffer.contents buf))
      | Some c -> match c with
	  | '\n' ->
	      Stream.junk l.stream;
	      l.line_no <- l.line_no + 1;
	      Number (l.line_no - 1, float_of_string (Buffer.contents buf))
	  | '-' | '.' | '0' .. '9' ->
	      Stream.junk l.stream;
	      Buffer.add_char buf c;
	      lex_number ~buf l
	  | x -> Number (l.line_no, float_of_string (Buffer.contents buf))


  let rec junk_comment l =
    try
      match Stream.next l.stream with
	| '\n' -> l.line_no <- l.line_no + 1
	| _ -> junk_comment l
    with Stream.Failure -> ()


  let rec token l =
    (** [token l] gets the next token from the stream. *)
    match Stream.peek l.stream with
      | None -> raise End_of_file
      | Some c -> match c with
	  | '\n' ->
	      Stream.junk l.stream;
	      l.line_no <- l.line_no + 1;
	      token l
	  | '(' ->
	      Stream.junk l.stream;
	      Open_paren l.line_no
	  | ')' -> Stream.junk l.stream; Close_paren l.line_no
	  | ';' -> junk_comment l; token l
	  | '"' -> Stream.junk l.stream; lex_string l
	  | ':' | 'a' .. 'z' | 'A' .. 'Z' -> lex_ident l
	  | '-' | '0' .. '9' -> lex_number l
	  | ' ' | '\t' -> Stream.junk l.stream; token l
	  | c -> failwith (sprintf
			     "line %d: Unknown character '%c' in input stream"
			     l.line_no c)
end


type sexpr =
  | List of int * sexpr list
  | String of int * string
  | Ident of int * string
  | Number of int * float


let line_number = function
    (** [line_number e] gets the line number for the beginning of the
    given expression. *)
  | List (l, _)
  | String (l, _)
  | Ident (l, _)
  | Number (l, _) -> l


let rec parse_list ?(accum=[]) l =
  match Lexer.token l with
    | Lexer.Open_paren line ->
	parse_list ~accum:((List (line, parse_list l)) :: accum) l
    | Lexer.Ident (line, name) ->
	parse_list ~accum:((Ident (line, name)) :: accum) l
    | Lexer.String (line, txt) ->
	parse_list ~accum:((String (line, txt)) :: accum) l
    | Lexer.Number (line, vl) ->
	parse_list ~accum:((Number (line, vl)) :: accum) l
    | Lexer.Close_paren _ ->
	List.rev accum


let parse strm =
  (** [parse strm] parses the s-expression from the given stream. *)
  let l = { Lexer.line_no = 1; Lexer.stream = strm } in
    match Lexer.token l with
      | Lexer.Open_paren line -> List (line, parse_list l)
      | t ->
	  failwith (sprintf "Expected '(', but got %s at line %d"
		      (Lexer.string_of_token t)
		      (Lexer.lineno_of_token t))
