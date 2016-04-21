(** Lexcial analysis for OCM.

    @author eaburns
    @since 2010-08-20
*)
{
  open Printf
  open Parse

  let system_keyword_list = [
    "Dotfiles", Dotfiles;
    "Systems", Systems;
    "Files", Files;
    "Libraries", Libraries;
    "CLibraries", CLibraries;
    "Result", Result;
    "Extensions", Extensions;

    "using", Using;
    "threads", Threads;
    "cmp-flags", Cmp_flags;
    "lnk-flags", Lnk_flags;
    "pp", Pp;
    "in", In;

    "Dirs", Dirs;
  ]
  let system_keywords = Hashtbl.create 100

  let config_keyword_list = [ "Dirs", Dirs; ]
  let config_keywords = Hashtbl.create 100

  let _ =
    List.iter (fun (k, t) -> Hashtbl.add system_keywords k t)
      system_keyword_list;
    List.iter (fun (k, t) -> Hashtbl.add config_keywords k t)
      config_keyword_list

  let unknown_char lexbuf c =
    (** [unknown_char lexbuf c] fail because there is an unknown
	character in the input. *)
    let pos = lexbuf.Lexing.lex_start_p in
    let line = pos.Lexing.pos_lnum in
    let bol = pos.Lexing.pos_bol in
    let col = pos.Lexing.pos_cnum - bol in
      failwith (sprintf "line %d col %d: Unknown character %c" line col c)


  let init_lexbuf lexbuf =
    (** [init_lexbuf lexbuf] initializes the lex buffer to be used for
	parsing. *)
    let cur = lexbuf.Lexing.lex_curr_p in
    let cur' = { cur with Lexing.pos_lnum = 1; Lexing.pos_bol = 0; } in
      lexbuf.Lexing.lex_curr_p <- cur';
      lexbuf.Lexing.lex_start_p <- cur';
      lexbuf
}

let whitespace = [ ' ' '\t' ]

let newline = ['\n' '\r'] | "\r\n"

let ident = ['A'-'Z' 'a'-'z'] ['A'-'Z' 'a'-'z' '0'-'9' '-' '_' ]*

rule token config = parse
  | whitespace               { token config lexbuf }
  | newline                  { Lexing.new_line lexbuf; token config lexbuf }
  | ':'                      { Colon }
  | ','                      { Comma }
  | ident as id              {
                               try
				 if config then
				   Hashtbl.find config_keywords id
				 else
				   Hashtbl.find system_keywords id
                               with Not_found -> Ident id
                             }
  | '"' ([^'"']* as str) '"' { String str }
  | '(' '*'                  { token config (comment 0 lexbuf) }
  | eof                      { Eof }
  | _ as c                   { unknown_char lexbuf c }

and comment n = parse
  | newline                  { Lexing.new_line lexbuf; comment n lexbuf }
  | '(' '*'                  { comment (n + 1) lexbuf }
  | '*' ')'                  {
                               if n > 0
                               then comment (n - 1) lexbuf
                               else lexbuf
                             }
  | _                        { comment n lexbuf }


{
  let system_token = token false
  let config_token = token true
}
