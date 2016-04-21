type token =
  | Colon
  | Comma
  | Dirs
  | Using
  | Threads
  | Cmp_flags
  | Lnk_flags
  | Pp
  | In
  | Dotfiles
  | Systems
  | Files
  | Libraries
  | CLibraries
  | Result
  | Extensions
  | Ident of (string)
  | String of (string)
  | Eof

val system :
  (Lexing.lexbuf  -> token) -> Lexing.lexbuf -> Types.system
val config :
  (Lexing.lexbuf  -> token) -> Lexing.lexbuf -> unit
