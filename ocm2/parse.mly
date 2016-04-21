/** Parsing for OCM

    @author eaburns
    @since 2010-08-20
*/

%{

  open Conf
  open Verb
  open Types

  let error_count = ref 0

  let syntax_error str loc ret =
    let pos = Parsing.rhs_start_pos loc in
    let line = pos.Lexing.pos_lnum in
    let col = pos.Lexing.pos_cnum - pos.Lexing.pos_bol in
      Printf.eprintf "line %d col %d: %s\n%!" line col str;
      incr error_count;
      ret
%}

%token Colon Comma
%token Dirs
%token Using Threads Cmp_flags Lnk_flags Pp In
%token Dotfiles Systems Files Libraries CLibraries Result Extensions
%token <string> Ident
%token <string> String
%token Eof

/* System files */
%type <Types.system> system
%type <string list> dotfiles
%type <string list> systems
%type <Types.file list> files
%type <(string * string) list> libraries
%type <(string * string) list> clibraries
%type <Types.result list> results
%type <Types.result list> result_list
%type <Types.result> result
%type <Types.result_kind> result_kind
%type <Types.file list> file_list
%type <Types.file> file
%type <bool * string list> using
%type <string list * string list * string list> file_flags
%start system

/* Config files */
%type <unit> config
%type <unit> binaries
%type <unit> binary_list
%type <unit> dirs
%start config

%type <string list> string_list
%type <string list> comma_string_list


%%

system:
  dotfiles systems files extensions libraries clibraries results Eof
    {
      if !error_count > 0 then
	failwith (Printf.sprintf "%d errors" !error_count);
      { dummy_sys with
	  sys_dotfiles = $1;
	  sys_systems = $2;
	  sys_files = $3;
	  sys_extensions = $4;
	  sys_libs = $5;
	  sys_clibs = $6;
	  sys_results = $7;
      }
    }
  | error
      {
	syntax_error "Malformed system file" 1 ();
	failwith (Printf.sprintf "%d errors" !error_count);
      }
;

dotfiles:
    Dotfiles Colon string_list { $3 }
  | Dotfiles Colon { [] }
  | { [] }
;

systems:
    Systems Colon string_list { $3 }
  | Systems Colon { [] }
  | { [] }
;

files:
    Files Colon file_list { $3 }
  | { [] }
;

extensions:
    Extensions Colon file_list { $3 }
  | { [] }
;

libraries:
    Libraries Colon library_list { $3 }
  | { [] }
;

clibraries:
    CLibraries Colon library_list { $3 }
  | { [] }
;

library:
  String
    {
      (* A hack to get reverse-compatibility with ocm which would
	 compile these libraries assuming they were installed in
	 lib/ocaml. *)
      match $1 with
	| "threads" -> "threads", "+threads"
	| "lacaml" -> "lacaml", "+lacaml"
	| "lablgtk" -> "lablgtk", "+lablgtk2"
	| "cairo" -> "cairo", "+cairo"
	| "cairo_lablgtk" -> "cairo_lablgtk", "+cairo"
	| _ -> $1, ""
    }
  | String In String { $1, $3 }
;

library_list:
  library library_list { $1 :: $2 }
  | { [] }
;

results:
    result_list { $1 }
  | { [] }
;

result_list:
    result result_list { $1 :: $2 }
  | result { [ $1 ] }
;

result:
    Result Colon String result_kind file_list
      {
	{ res_name = $3;
	  res_files = $5;
	  res_kind = $4; }
      }
  | Result Colon error
      {
	syntax_error "Malformed result" 3 dummy_res
      }
;

result_kind:
    Ident { Result.kind_of_string $1 }
;


file_list:
    file file_list { $1 :: $2 }
  | { [] }
;


file:
    String { File.make $1 }
  | String error
      {
	syntax_error "Malformed file description" 2 dummy_file
      }
  | String using
      {
	let threads, pp = $2 in
	let thread_flags = if threads then [ "-thread" ] else [] in
	  File.make ~cmp_flags:thread_flags ~lnk_flags:[] ~pp $1
      }
  | String file_flags
      {
	let cf, lf, pp = $2 in
	  File.make ~cmp_flags:cf ~lnk_flags:lf ~pp $1
      }
;

file_flags:
    Cmp_flags comma_string_list file_flags
      {
	let cf, lf, pp = $3 in $2 @ cf, lf, pp
      }
  | Cmp_flags comma_string_list
      {
	$2, [], []
      }
  | Lnk_flags comma_string_list file_flags
      {
	let cf, lf, pp = $3 in cf, $2 @ lf, pp
      }
  | Lnk_flags comma_string_list
      {
	[], $2, []
      }
  | Pp comma_string_list file_flags
      {
	let cf, lf, pp = $3 in cf, lf, $2
      }
  | Pp comma_string_list
      {
	[], [], $2
      }

using:
    Using Threads Comma String { true, [ $4 ] }
  | Using String Comma Threads { true, [ $2 ] }
  | Using String { false, [$2 ] }
  | Using Threads { true, [] }
;

/************************************************************/

config:
    error
      {
	syntax_error "Malformed config file" 1 ();
	failwith (Printf.sprintf "%d errors" !error_count);
      }
  | binaries dirs Eof
      {
	if !error_count > 0 then
	  failwith (Printf.sprintf "%d errors" !error_count);
      }
;

binaries:
    binary_list { $1 }
  | { () }
;


binary_list:
    Ident Colon String binary_list
      {
	try set_value $3 $1
	with Invalid_argument x -> syntax_error ("Unknown binary: " ^ x) 1 ()
      }
  | Ident Colon String
      {
	try set_value $3 $1
	with Invalid_argument x -> syntax_error ("Unknown binary: " ^ x) 1 ()
      }
;

dirs:
  Dirs Colon dir_list
    {
      List.iter
	(fun p ->
	   let paths = Fname.expand_simple_stars p in
	     roots := Fname.Set.union paths !roots)
	$3
    }
  | Dirs Colon error
    {
      syntax_error "Malformed file list" 3 ();
    }
  | { () }
;

dir_list:
  string_list { $1 }
  | { [] }
;

/************************************************************/

string_list:
    String string_list { $1 :: $2 }
  | String { [ $1 ] }
  | String error { syntax_error "Malformed string" 2 [] }
;

comma_string_list:
    String Comma comma_string_list { $1 :: $3 }
  | String { [ $1 ] }
  | String error { syntax_error "Malformed string" 2 [] }
;

%%
