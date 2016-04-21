(* $Id: ocm.ml,v 1.8 2003/11/13 21:22:54 ruml Exp ruml $

   my personal make tool for OCaml

   Wheeler Ruml

   To compile this tool to portable bytecode:

   ocamlc -o ocm -I ../wr_utils -pp camlp4o ocm.ml

*)


open Genlex


exception Make_Failure of string

let makefail str =
  raise (Make_Failure str)


(**************** important globals **************)


(* assumes these are in the shell's path.  ref because .ocmrc might
   change them *)

let ocamlyacc_binary = ref "ocamlyacc"

let ocamllex_binary = ref "ocamllex.opt"

let ocamldep_binary = ref "ocamldep"

let ocamlc_binary = ref "ocamlc.opt"

let ocamlopt_binary = ref "ocamlopt.opt"


let force_action = ref false


(**************** system definition **************)


type result_kind =
  | Interface
  | Noexec
  | Bytedebug
  | Nativeprof
  | Native

let noexec_names = ["use"; "noexec"]


type file = {
  path : string;
  threads : bool;
  parsing : string option;
}


type result = {
  name : string;
  kind : result_kind;
  (* just for this result *)
  special_files : file list;
}


type system = {
  title : string;
  systems : string list;
  (* file pathnames will be fully specified, all files will be listed *)
  base_files : file list;
  libraries : string list;
  clibraries: string list;
  base_dir : string;
  results : result list;
}


(*************** registry of system files ****************)


let system_files =
  (** an alist from system names to filenames *)
  ref []


let sys_def_name x =
  Filename.check_suffix x ".system"


let sys_files d =
  Unix.handle_unix_error
    (fun () ->
       let files, _ = Wrfname.dir_contents d in
         List.filter sys_def_name files)
    ()


let add_system_files dir =
  (** fails if multiple definitions *)
  let files = List.map (fun f ->
                          (* Wrutils.pr "Found definition for %s.\n"
                             (Wrfname.stem f); *)
                          (Wrfname.stem f),f)
    (sys_files dir)
  in
    (* prune duplicate names that refer to same file (diff name to
       same file is ok) *)
    system_files := (Wrlist.remove_dups (fun (n, f) ->
                                           n, (Wrfname.file_id f))
                       (files @ !system_files));
    (* any remaining duplicate names are bad *)
    match Wrlist.duplicates (List.map fst !system_files) with
	[] -> ()
      | dups -> makefail ("duplicated system names: " ^ (String.concat " " dups))


let notice_system_files () =
  (** [notice_system_files ()] scans backwards up the directory tree
      looking for a directory that has system files. *)
  let dir = ref (Sys.getcwd ()) in
    while !system_files = [] && (String.length !dir) > 1 do
      add_system_files !dir;
      dir := Filename.dirname !dir;
    done


(*********** camlp4 ************)

(* since very different under windows, need to translate commands *)


let parser_table = ref []


let add_parser_rule oldstr newstr =
  try
    let _ = List.assoc oldstr !parser_table in
      failwith (Wrutils.str "already have parser rule for \"%s\"" oldstr)
  with Not_found ->
    parser_table := (oldstr, newstr)::!parser_table


let translate_parser str =
  try
    List.assoc str !parser_table
  with Not_found -> str


(*********** .ocmrc file ************)


let dot_file = ".ocmrc"
let master_dot_file = Filename.concat (Wrfname.homedir ()) dot_file


(* -- sample .ocm file --
   No .opt binaries.

   Dirs:
   ~/projects/blah
   ~/projects/blech/
   ~/projects/blech/*
*)

let dot_lexer = Wrstrm.make_lexer []


let rec parse_opt_bin_name = parser
    [< 'Ident "ocamlyacc:"; 'String s; foo = parse_opt_bin_name >] ->
      ignore foo;
      ocamlyacc_binary := s
  | [< 'Ident "ocamllex:"; 'String s; foo = parse_opt_bin_name >] ->
      ignore foo;
      ocamllex_binary := s
  | [< 'Ident "ocamldep:"; 'String s; foo = parse_opt_bin_name >] ->
      ignore foo;
      ocamldep_binary := s
  | [< 'Ident "ocamlc:"; 'String s; foo = parse_opt_bin_name >] ->
      ignore foo;
      ocamlc_binary := s
  | [< 'Ident "ocamlopt:"; 'String s; foo = parse_opt_bin_name >] ->
      ignore foo;
      ocamlopt_binary := s
  | [< 'Ident "parser:"; 'String oldstr; 'Ident "="; 'String newstr;
       foo = parse_opt_bin_name >] ->
      ignore foo;
      add_parser_rule oldstr newstr
  | [< >] -> ()


let rec parse_strings = parser
    (** returns a list of all and any strings available at the front of strm *)
    [< 'Genlex.String s ; more = parse_strings >] -> s::more
  | [< >] -> []

let parse_dot_file = parser
    [< foo = parse_opt_bin_name;
       'Ident "Dirs:"; dirs = parse_strings >] ->
      dirs
  | [< >] -> []


let read_dot_file f =
  (* handle No .opt option *)
  flush_all ();
  Wrutils.pr "Reading %s..." f;
  let dirs = Wrio.with_infile f
    (fun s -> parse_dot_file (dot_lexer (Stream.of_channel s)))
  in
    Wrutils.pr "done.\n";
    let dirs = Wrlist.mapcan (fun d ->
				Wrfname.expand_stars
				  (Wrfname.expand_home d))
      dirs
    in
      List.iter add_system_files dirs


let try_dot_file f =
  if Sys.file_exists f
  then read_dot_file f


let read_dot_files () =
  try_dot_file dot_file;
  try_dot_file master_dot_file


(**** parsing system defs ****)


let sys_lexer = Wrstrm.make_lexer []


let parse_file_options2 s = parser
    [< 'String p >] -> { path = s;
                         threads = false;
                         parsing = Some p }
  | [< 'Ident "threads" >] -> { path = s;
				threads = true;
				parsing = None }
  | [< 'Ident "threads,"; 'String p >] -> { path = s;
                                            threads = true;
                                            parsing = Some p }


let parse_file_options s = parser
    [< 'Ident "using"; res = parse_file_options2 s >] -> res
  | [< >] -> { path = s;
               threads = false;
               parsing = None }


let rec parse_files = parser
    [< 'String s;
       res = parse_file_options s;
       more = parse_files >] -> res::more
  | [< >] -> []


let is_mli f =
  Filename.check_suffix f ".mli"

let is_ml f =
  Filename.check_suffix f ".ml"


let check_variant f ext =
  let f = { f with path = Wrfname.new_extension f.path ext } in
    if Sys.file_exists f.path
    then [ f ] else []


let demand_variant f ext =
  match check_variant f ext with
      [] -> makefail ("can't find corresponding " ^ ext ^ " file for" ^ f.path)
    | l -> l


let related_files dir f =
  (** expand [f] to be fully specified, check for .mli or .ml files,
      return list of files to be added to system def *)
  let name = f.path in
  let f = { f with path = Filename.concat dir name } in
    if ((Filename.check_suffix name ".mly") ||
	  (Filename.check_suffix name ".mll"))
    then [ f ]
    else if is_ml name
    then f::(check_variant f ".mli")
    else if is_mli name
    then f::(demand_variant f ".ml")
    else if (String.contains name '.')
    then makefail ("don't know what to do with file: " ^ name)
    else
      (check_variant f ".mli") @
	(demand_variant f ".ml")


let parse_opt_dot_files = parser
    [< 'Ident "Dotfiles:"; files = parse_strings >] ->
      files
  | [< >] -> []


let rec parse_results dir = parser
    [< 'Ident "Result:"; 'String s; 'Ident t; files = parse_files;
       more = parse_results dir >] ->
      { name = s;
	kind = (match t with
                    "bytedebug" -> Bytedebug
		  | "nativeprof" -> Nativeprof
		  | "native" -> Native
		  | s -> makefail ("unknown result type: " ^ s));
	special_files = Wrlist.mapcan (related_files dir) files }::more
  | [< >] -> []

let parse_clibs = parser
  | [< 'Ident "CLibraries:";
       clibs = parse_strings; >] -> clibs
  | [< >] -> []

let parse_system_def dir title = parser
    [< dot_files = parse_opt_dot_files ;
       'Ident "Systems:";
       systems = parse_strings ;
       'Ident "Files:" ?? "expecting `Files:'";
       files = parse_files ;
       'Ident "Libraries:" ?? "expecting `Libraries:'";
       libs = parse_strings;
       clibs = parse_clibs;
       results = parse_results dir >] ->
      let sys = { title = title;
                  systems = systems;
                  base_files = Wrlist.mapcan (related_files dir) files;
                  libraries = libs;
                  clibraries = clibs;
                  base_dir = dir;
                  results = results; } in
	sys, dot_files
  | [< >] -> makefail "stream should start with `Systems:'"


let read_system_def dir title inch =
  try
    parse_system_def dir title (sys_lexer (Stream.of_channel inch))
  with (Stream.Error _) as e ->
    Wrutils.pr "\nTrouble at position %d when reading system def:\n"
      (pos_in inch);
    flush_all ();
    raise e


let load_system_def filename =
  flush_all ();
  Wrutils.pr "Loading definition of %s..." (Wrfname.stem filename);
  flush_all ();
  let s, fs = Wrio.with_infile filename (read_system_def
                                           (Filename.dirname filename)
                                           (Wrfname.stem filename))
  in
    Wrutils.pr "done.\n";
    List.iter try_dot_file fs;
    flush_all ();
    s


let test_load_system () =
  load_system_def "/tilde/ruml/library/code/ocaml/ocm/sample.system"


(*************** registry of loaded system definitions ****************)


let systems =
  (** an alist from system names to definitions *)
  ref []


let get_system name =
  try
    List.assoc name !systems
  with Not_found ->
    let def = load_system_def (try
                                 List.assoc name !system_files
                               with Not_found ->
                                 makefail ("unknown system: " ^ name))
    in
      systems := (name, def)::!systems;
      def


let mapcan_systems f s =
  let o = Stack.create () in
  let c = Hashtbl.create 100 in
  let lst = ref [] in
    Stack.push s o;
    while not (Stack.is_empty o) do
      let s = Stack.pop o in
	if not (Hashtbl.mem c s)
	then begin
	  Hashtbl.add c s true;
	  List.iter (fun t -> Stack.push (get_system t) o) s.systems;
	  lst := !lst @ f s
	end
    done;
    !lst


(*
  let rec mapcan_systems f s =
  (f s) @ (Wrlist.mapcan (fun n ->
  mapcan_systems f (get_system n))
  s.systems)
*)

let accumulate_libs s =
  (* NOT RESISTENT TO LOOPS IN LIB DEPENDENCIES! *)
  let libs = mapcan_systems (fun s -> s.libraries) s in
    Wrlist.remove_duplicates libs


let accumulate_clibs s =
  let clibs = mapcan_systems (fun s -> s.clibraries) s in
    Wrlist.remove_duplicates clibs



let filenames files =
  List.map (fun f -> f.path) files


let accumulate_files r s =
  let files = mapcan_systems (fun s -> s.base_files) s in
  let files = files @ r.special_files in
    (* allow multiple inclusions of same system *)
  let files = Wrlist.remove_duplicates files in
    (* don't allow different paths with same basename *)
    match Wrlist.dups Filename.basename (filenames files) with
	[] -> files
      | dups -> makefail ("duplicate names: " ^ (String.concat " "
                                                   (List.map List.hd dups)))


let get_result r s =
  try
    List.find (fun x -> x.name = r)
      s.results
  with Not_found ->
    if List.mem r noexec_names
    then { name = "bytecode object files";
           kind = Noexec;
           special_files = []; }
    else
      if (Wrfname.get_extension r) = ".mli"
      then { name = r;
	     kind = Interface;
	     special_files = []; }
      else
	makefail
	  (Wrutils.str
	     "can't find result %s in system %s"
	     r s.title)


(*********** gathering files *************)


type job = {
  exec_name : string;
  exec_type : result_kind;
  dirs : string list;
  libs : string list;
  clibs : string list;
  files : file list;
}


let make_path dir base extension =
  Wrfname.new_extension (Filename.concat dir base) extension


let make_exec_name result system =
  let dir = system.base_dir
  and base = result.name in
    match result.kind with
      | Interface -> Filename.concat dir base
      | Noexec -> Filename.concat dir "use.ml"
      | Bytedebug ->
	  (match Sys.os_type with
               "Win32" | "Cygwin" -> make_path dir base "exe"
             | _ -> Filename.concat dir base)
      | Nativeprof | Native ->
	  (make_path
             dir
             (base ^ (match result.kind with
			  Nativeprof -> "_prof" | _ -> ""))
             (match Sys.os_type with
		  "Win32" | "Cygwin" -> "exe"
		| "Unix" ->
		    (try Sys.getenv "OSTYPE"
		     with Not_found -> "unix_unknown")
		| "MacOS" -> "macos9"
		| _ -> failwith "don't recognize this os_type"))


let assemble_job result system =
  let result = get_result result system in
  let libs = accumulate_libs system
  and clibs = accumulate_clibs system
  and files = accumulate_files result system in
  let dirs = Wrlist.remove_duplicates (List.map Filename.dirname
                                         (filenames files)) in
    { exec_name = make_exec_name result system;
      exec_type = result.kind;
      dirs = dirs;
      libs = libs;
      clibs = clibs;
      files = files; }


(*********** generating source files  ***********)


let partition_suffix suffix list =
  List.partition (fun f ->
                    Filename.check_suffix f.path suffix)
    list


let exists_and_newer a b =
  (** a exists and is newer than b *)
  (Sys.file_exists a) &&
    ((Wrfname.file_mod_time a) > (Wrfname.file_mod_time b))


let run_external cmd =
  flush_all ();
  Wrutils.pr "%s\n" cmd;
  flush stdout;
  match Sys.command cmd with
      0 -> ()
    | n -> makefail (Wrutils.str "got return code %d" n)


let ensure_ocamlyacc file =
  let orig = file.path in
  let mli = Wrfname.new_extension orig ".mli"
  and ml = Wrfname.new_extension orig ".ml" in
    if (!force_action ||
	  not ((exists_and_newer mli orig) && (exists_and_newer ml orig)))
    then run_external (!ocamlyacc_binary ^ " " ^ orig);
    [ { file with path = mli };
      { file with path = ml } ]


let ensure_ocamllex file =
  let ml = Wrfname.new_extension file.path ".ml" in
    if (!force_action || not (exists_and_newer ml file.path))
    then run_external (!ocamllex_binary ^ " " ^ file.path);
    [ { file with path = ml } ]


let generate_source job =
  (* would be nice to handle .idl files *)
  let yacc, rest = partition_suffix ".mly" job.files in
  let lex, rest = partition_suffix ".mll" rest in
  let res1 = Wrlist.mapcan ensure_ocamlyacc yacc
  and res2 = Wrlist.mapcan ensure_ocamllex lex in
    { job with files = res1 @ res2 @ rest }


(*********** TAGS ************)

(* catags concatenates tag files and fixes names to be relative to cwd *)


let generate_tags job =
  Wrutils.write_this ()


(*********** file dependencies  ***********)


(* Dependencies are stored in terms of object files stored next to the
   source file.  Perhaps not coincidentally, this is also how they are
   printed by ocamldep.

   Dependencies of object files on their source files are not
   explicitly represented.  They are tested in [needs_making].
*)


let rec new_exts alist f =
  match alist with
      [] -> failwith ("unrecognized extension: " ^ f)
    | (target, replacement)::rest ->
	if Filename.check_suffix f target
	then Wrfname.new_extension f replacement
	else new_exts rest f


let obj_dir job =
  let os_type = match Sys.os_type with
      "Win32" | "Cygwin" -> "windows"
    | "Unix" ->
	(try Sys.getenv "OSTYPE"
	 with Not_found -> "unix_unknown")
    | "MacOS" -> "macos9"
    | _ -> failwith "don't recognize this os_type"
  in
    match job.exec_type with
      | Interface -> invalid_arg "No object directory for interface results"
      | Noexec | Bytedebug -> "bytecode_" ^ os_type
      | Nativeprof | Native -> os_type


let add_subdir dir f =
  Wrfname.filename_from_list [Filename.dirname f; dir; Filename.basename f]


let add_objdir job fname =
  add_subdir (obj_dir job) fname


(*** translations to/from "dep name" ***)


let dep_name f =
  (** conversion from src filename to name as listed in dependencies *)
  new_exts [".mli", ".cmi"; ".ml", ".cmo"] f


let dep_src_file f =
  (** conversion from dep name to src name *)
  new_exts [".mli", ".ml"; ".cmi", ".mli"; ".cmo", ".ml"] f


let deps_file f =
  (** from dep name to name of file storing dependencies *)
  add_subdir "depends" ((dep_src_file f) ^ ".d")


let dep_comp_obj_file job f =
  (** conversion from dep name to obj file output by compiler *)
  if Filename.check_suffix f ".cmi"
  then f
  else if Filename.check_suffix f ".cmo"
  then
    match job.exec_type with
      | Interface -> invalid_arg "No dependencies for interface results"
      | Noexec | Bytedebug -> f
      | Nativeprof | Native -> Wrfname.new_extension f ".cmx"
  else failwith ("unknown extension: " ^ f)


let dep_obj_file job f =
  (** conversion from dep name to obj file in its final true location *)
  if Filename.check_suffix f ".cmi"
  then f
  else if Filename.check_suffix f ".cmo"
  then
    let f = add_objdir job f in
      match job.exec_type with
	| Interface -> invalid_arg "No dependencies for interface results"
	| Noexec | Bytedebug -> f
	| Nativeprof -> Wrfname.new_extension f ".p.cmx"
	| Native -> Wrfname.new_extension f ".cmx"
  else failwith ("unknown extension: " ^ f)


let dep_o_file f =
  (** dep name to place where ocamlopt puts .o file *)
  new_exts [".cmo", ".o"] f


(*** processing ***)


let dir_args basedir job =
  let dirs = job.dirs in
    (*let dirs = List.map (Wrfname.make_relative basedir) job.dirs in *)
    (* leave out [basedir] *)
  let dirs = Wrlist.remove Filename.current_dir_name dirs in
    Wrstr.concat_prefix " -I " dirs


let parsing_arg file =
  match file.parsing with
      None -> " "
    | Some p -> Wrutils.str " -pp \"%s\" " (translate_parser p)


let ensure_dependency job file =
  (** run ocamldep if file doesn't exist or is out of date *)
  let f = dep_name file.path in
  let dep = deps_file f
  and src = dep_src_file f in
    if (!force_action || not (exists_and_newer dep src)) then
      (let dir = Filename.dirname src in
         Wrfname.ensure_path dep;
         let src = Filename.basename src
         and dep = Wrfname.make_relative dir dep in
           Wrsys.with_cwd dir
             (fun () ->
		run_external (!ocamldep_binary ^ " -slash" ^
				(dir_args dir job) ^
				(parsing_arg file) ^ src ^ " > " ^ dep)));
    dep, f


let rec flatten_lines = function
    line::next::rest ->
      if Wrstr.ends_with '\\' line then
	let line = (Wrstr.chop line) ^ next in
          flatten_lines (line::rest)
      else
	line::(flatten_lines (next::rest))
  | other -> other


let filter_lines lines =
  List.filter (fun l -> not (Wrstr.contains ".cmx" l))
    lines


let parse_line line =
  (* assumes filenames do not contain whitespace.  I guess Caml
     assumes this too! *)
  match Wrstr.split_white line with
      file::deps -> (Wrstr.chop file), deps
    | [] -> failwith (Wrutils.str "bad line from ocamldep: \"%s\"" line)


let dep_lines file =
  filter_lines (flatten_lines (Wrio.undos (Wrio.file_lines file)))


let read_dependency (file,orig) =
  match dep_lines file with
      [] -> []
    | line::[] ->
	let f, deps = parse_line line in
	  if ((not Wrsys.windows_p) &&
		((Filename.basename f) <> (Filename.basename orig)))
	  then failwith (Wrutils.str "got filename \"%s\" instead of \"%s\"."
                           f orig);
	  let dir = Filename.dirname (dep_src_file orig) in
            List.map (Wrfname.canonicalize ~cwd:dir) deps
    | lines ->
	failwith (Wrutils.str "got multiple dependency lines in %s for %s:%s"
                    file orig (Wrstr.concat_prefix "\nline=>" lines))


let control_file job f =
  (** do we know about the source file needed to generate [f]? *)
  let src = dep_src_file f in
    List.exists (fun f -> f.path = src) job.files


let get_file job f =
  let src = dep_src_file f in
    try
      List.find (fun x -> x.path = src)
	job.files
    with Not_found -> failwith (Wrutils.str "file not found: %s" src)


(* using a DAG allows us to catch circularities.  just building an
   ordered list wouldn't support that.

   arc from X to Y <-> Y must be older than/built before X, X must be
   newer than/built after Y.  If Y doesn't exist or is
   newer than X, then build X.  (Presumably we tried to
   build Y earlier.)
*)

let safe_add_arc g a b =
  Dag.ensure_vertex g a;
  Dag.ensure_vertex g b;
  try
    Dag.add_arc g a b
  with Dag.Circularity ->
    makefail (Wrutils.str "circular dependency through %s and %s" a b)


let check_cmo_arc job g f d =
  (** If appropriate, adds dependency between [f] and the .cmo for [d].
      This is appropriate when [f] depends on a .cmi file and the corresponding
      .cmo file is going to be linked in.  It must appear on the linker line
      before [f]. *)
  if Filename.check_suffix d ".cmi" then
    let d = Wrfname.new_extension d ".cmo" in
      if (d <> f) && (control_file job d) then
	(* want the .cmo earlier in link list *)
	safe_add_arc g f d


let check_cmi_arc job g f =
  (** If [f] is a .cmo, adds dependency to its ".cmi" (only listed by
      ocamldep if .mli is explicit) *)
  if Filename.check_suffix f ".cmo" then
    safe_add_arc g f (Wrfname.new_extension f ".cmi")


let read_dependencies job =
  let g = Dag.create () in
    List.iter (fun file ->
                 let f = dep_name file.path in
                   check_cmi_arc job g f;
                   List.iter (fun d ->
				safe_add_arc g f d;
				check_cmo_arc job g f d)
                     (read_dependency (ensure_dependency job file)))
      job.files;
    g


let print_dependencies g =
  Dag.iter_bottom_up g (fun f ->
                          Wrutils.pr "%s:\n   %s\n" f
                            (String.concat "\n   " (Dag.out_neighbors g f)))


(********** compiling **********)


let dep_files job g f =
  (** files that [f] must be newer than *)
  let deps = List.map (dep_obj_file job)
    (Dag.out_neighbors g f)
  and src = dep_src_file f in
    src::deps


let needs_compiling job g f =
  let obj = dep_obj_file job f in
    (* doesn't exist *)
    (not (Sys.file_exists obj)) ||
      let ftime = Wrfname.file_mod_time obj in
	(* src or other referenced module is newer *)
	(List.exists (fun d ->
			(Sys.file_exists d) &&
			  ((Wrfname.file_mod_time d) > ftime))
           (dep_files job g f))


let rec move_obj job f o_p =
  let obj = dep_comp_obj_file job f
  and dest = dep_obj_file job f in
    Wrfname.ensure_path dest;
    Wrsys.rename_clobbering obj dest;
    if o_p then
      Wrsys.rename_clobbering (dep_o_file f)
	(Wrfname.new_extension dest ".o")


let compile job path =
  let file = get_file job path in
  let src = dep_src_file path in
  let dir = Filename.dirname src
  and src = Filename.basename src in
  let suffix = (* (if file.threads then " -thread" else "") *)
    " -thread" ^
      (dir_args dir job) ^ (parsing_arg file) ^ "-c " ^ src in
    Wrsys.with_cwd dir
      (fun () ->
         if is_mli src
         then run_external (!ocamlc_binary ^ suffix)
         else if is_ml src
         then
           match job.exec_type with
	     | Interface ->
		 let comp_str =
		   Wrutils.str
		     "%s -i %s > %s"
		     !ocamlc_binary
		     suffix
		     (Wrfname.new_extension src ".mli")
		 in
		 run_external comp_str
             | Noexec | Bytedebug ->
		 run_external (!ocamlc_binary ^ " -g" ^ suffix);
		 move_obj job path false
             | Nativeprof ->
		 run_external (!ocamlopt_binary ^ " -p" ^ suffix);
		 move_obj job path true
             | Native ->
		 run_external (!ocamlopt_binary ^ suffix);
		 move_obj job path true
           else failwith ("unknown extension: " ^ path))


let ensure_object job g file =
  (** check if [file] doesn't exist or is obsolete, (re)creating it if
      necessary. *)
  if (control_file job file) &&
    (!force_action || (needs_compiling job g file))
  then compile job file;
  if is_mli (dep_src_file file)
  then []
  else
    let f = (try
               get_file job file
             with Not_found -> { path = file;
                                 threads = false;
                                 parsing = None })
    in
      [ { f with path = dep_obj_file job file } ]


let generate_object job g =
  (** returns a fresh job with object files in reasonable order *)
  (* print_dependencies g; *)
  let objs = ref [] in
  let success = Dag.bottom_up g (fun f ->
                                   try
                                     let os = ensure_object job g f in
                                       objs := os @ !objs;
                                       true
                                   with Make_Failure _ -> false)
  in
    if not success
    then makefail "error during compilation";
    { job with files = List.rev !objs }


(******************** .mli files ********************)


let generate_interface job path = compile job path


(*********** use.ml ************)


let write_use_prefix path ch =
  Wrutils.pf ch "(* use file

		      Automatically generated by ocm.
  This file should not be mentioned in a system definition
  After compiling all files to bytecode, enter the following at the toplevel:

  #use \"%s\";;

  (Need absolute path because there may be others in active directories.)
*)\n" (String.escaped path)


let write_use dirs files ch =
  Wrutils.pf ch "\n\n";
  List.iter (fun d ->
               Wrutils.pf ch "#directory \"%s\";;\n" (String.escaped d))
    dirs;
  Wrutils.pf ch "\n\n";
  List.iter (fun f -> Wrutils.pf ch "#load \"%s\";;\n" f) files;
  Wrutils.pf ch "\n\n(* EOF *)\n"


let generate_use job =
  (** files in job are object files, in dependency-consistent order.
      assume that all should be listed in the use file *)
  let files = filenames job.files in
  let dirs = Wrlist.remove_duplicates (List.map Filename.dirname files) in
    (* must have /mli directory too! *)
  let dirs = Wrlist.mapcan (fun d -> [d; Filename.dirname d]) dirs in
  let files = List.map Filename.basename files
  and path = job.exec_name in
    Wrutils.fs "Writing use.ml file...";
    Wrio.with_outfile path
      (fun ch ->
         write_use_prefix path ch;
         write_use dirs files ch);
    Wrutils.fs "done.\n"


(********** linking **********)


let lib_args job =
  let libs = List.map (fun l ->
                         Wrfname.new_extension l
                           (match job.exec_type with
			      | Interface ->
				  invalid_arg
				    "who cares about libs for interfaces?"
			      | Noexec ->
				  invalid_arg
				    "who cares about libs for noexec?"
                              | Bytedebug -> ".cma"
                              | Nativeprof | Native -> ".cmxa"))
    job.libs in
    Wrstr.concat_prefix " " libs


let clib_args job =
  let clibs = List.map (fun clib ->
                          (match job.exec_type with
			      | Interface ->
				  invalid_arg
				    "who cares about libs for interfaces?"
                             | Noexec ->
				 invalid_arg
				   "who cares about libs for noexec?"
                             | Bytedebug | Nativeprof | Native ->
				 "-cclib -l" ^ clib))
    job.clibs
  in Wrstr.concat_prefix (match job.exec_type with
			    | Bytedebug -> " -custom "
			    | _ -> " ") clibs



let generate_result job =
  (* don't bother checking times unless we want to check that all
     libraries are up to date *)
  let dir = Filename.dirname job.exec_name
  and result = Filename.basename job.exec_name in
  let suffix = (clib_args job) ^ " -o " ^ result ^
    (if (List.exists (fun f -> f.threads) job.files)
     then " -thread" else "") ^
    (dir_args dir job) ^ (lib_args job) ^
    (Wrstr.concat_prefix " "
       (List.map (Wrfname.make_relative dir)
          (filenames job.files)))
  in
    Wrsys.with_cwd dir
      (fun () ->
         match job.exec_type with
           | Noexec -> invalid_arg "no result for Noexec"
           | Interface -> invalid_arg "no result for Interface"
           | Bytedebug -> run_external (!ocamlc_binary ^ " -g" ^ suffix)
           | Nativeprof -> run_external (!ocamlopt_binary ^ " -p" ^ suffix)
           | Native -> run_external (!ocamlopt_binary ^ suffix))


(*********** command-line args ************)


let default_result system_name =
  match (get_system system_name).results with
      [] -> makefail "No results are specified in the system file!"
    | x::_ -> x.name


let default_system_name () =
  let dir = ref (Sys.getcwd ()) in
    while (sys_files !dir) = [] && (String.length !dir) > 1 do
      dir := Filename.dirname !dir;
    done;
    if (String.length !dir) <= 1
    then
      makefail "No system file in the current or previous directories."
    else
      match sys_files !dir with
	| [] -> assert false;
	| f::[] -> Wrfname.stem f
	| _ -> makefail "Multiple system files found in the current directory."


let usage =
  "Command line arguments:
    -nothing-      use system file in current directory and build first result
    <result>       result to build (from .system file in cwd)
    <file.system>  system file to use (build first result)
    <result> <file> built result in file (can omit .system)
"


let get_args () =
  (** returns (system, result) *)
  let args = ref [] in
    Arg.parse ["-force", Arg.Set force_action, "force recompilation"]
      (fun s -> args := s::!args)
      usage;
    let raw_s, r = (match !args with
			[raw_s; r] -> raw_s, r
                      | [n] ->
			  if (sys_def_name n)
			  then n, default_result n
			  else default_system_name (), n
                      | [] ->
			  let s = default_system_name () in
                            s, default_result s
                      | _ -> print_string usage; exit 128)
    in
      (Wrfname.stem raw_s), r


(*********** top level ************)


let build res_name sys_name =
  Wrutils.pr "Building %s in system %s.\n" res_name sys_name;
  try
    let sys = get_system sys_name in
    let job = assemble_job res_name sys in
      (* generate .mli and .ml files from .mll and .mly *)
      (let job = generate_source job in
         generate_tags job;
         (* generate and load .d files *)
         let g = read_dependencies job in
           (* generate .cmi, .cmo/.cmx or the .mli interface*)
         let job = match job.exec_type with
	   | Interface -> job
	       (* the interface skips generating the object files. *)
	   | _ -> generate_object job g
	 in
           match job.exec_type with
	     | Interface ->
		 generate_interface
		   job
		   (Filename.concat sys.base_dir res_name)
             | Noexec -> generate_use job;
             | _ -> generate_result job);
      Wrutils.pr "Done building %s in system %s.\n" res_name sys_name
  with Make_Failure e ->
    flush_all ();
    Wrutils.pr "Build of %s in system %s failed: %s\n" res_name sys_name e;
    exit 1



let main () =
  notice_system_files ();
  read_dot_files ();
  let sys_file, res_name = get_args () in
    build res_name sys_file


let _ = main ()


(* EOF *)
