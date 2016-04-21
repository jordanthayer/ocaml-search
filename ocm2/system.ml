(** An OCM system file.  This is mainly the AST of a system file.

    @author eaburns
    @since 2010-08-20
*)

open Verb
open Printf
open Types

let sys_build_dir sys =
  Filename.concat (Filename.dirname sys.sys_path) !Conf.build_dir

(** Get the build directory for the given file. *)
let file_build_dir p =
  let dir_path = Filename.dirname p in
  let dir_name = Filename.basename dir_path in
    if dir_name = Filename.basename !Conf.build_dir then
      dir_path
    else
      Filename.concat dir_path !Conf.build_dir


(** Get the build path of the given file. *)
let file_build_path p =
  Filename.concat (file_build_dir p) (Filename.basename p)


module S = Set.Make(String)

(************************************************************)
(** {1 Printing the system} *)


(************************************************************)
(** {1 Loading systems} *)

(** Normalize the file names for a result file.  This should remove
    duplicate files, add .ml and .mli if needed and adds [root] as the
    directory. *)
let normalize_results root rs =
  List.map (fun r -> { r with res_files = File.normalize root r.res_files }) rs


(** Parses the given system file and normalizes its contents. *)
let parse_system_file parse_system file =
  let inch = open_in file in
  let lb = Lexing.from_channel inch in
    try
      let sys = parse_system lb in
      let path = Fname.realpath file in
      let dir = Filename.dirname path in
      let file = Fname.realpath file in
	close_in inch;
	vprintf ~lvl:verb_optional "Loaded system: %s\n" file;
	{ sys with
	    sys_name = Filename.chop_extension (Filename.basename file);
	    sys_path = file;
	    sys_files = File.normalize dir sys.sys_files;
	    sys_extensions = File.normalize dir sys.sys_extensions;
	    sys_results = normalize_results dir sys.sys_results;
	}
    with Failure s ->
      close_in inch;
      failwith (sprintf "%s: while parsing %s\n" s file)


(** Gets the system and a (possibly new) configuration file. *)
let find_system load_conf parse name =
  let find file =
    match Fname.find_files_named !Conf.roots file with
      | file :: [] -> file
      | [] -> failwith ("No " ^ file ^ " system found")
      | _ -> failwith ("Multiple " ^ file ^ " systems found")
  in
  let fname = name ^ ".system" in
  let path = find fname in
  let sys = parse path in
    List.iter load_conf sys.sys_dotfiles;
    sys

(** Gets a list of unique elements by merging the elements from each
    of the systems.  This attempts to preserve order. *)
let union_elms key elms syss =
  let h = Hashtbl.create 149 in
  let uniq e =
    let k = key e in
      if not (Hashtbl.mem h k) then begin
	Hashtbl.add h k true;
	true
      end else
	false
  in
    List.fold_left (fun lst s -> lst @ (List.filter uniq (elms s))) [] syss


let files syss =
  union_elms (fun f -> f.file_path) (fun s -> s.sys_files) syss

let extensions syss =
  union_elms (fun f -> f.file_path) (fun s -> s.sys_extensions) syss

let libs syss =
  union_elms (fun (l, _) -> l) (fun s -> s.sys_libs) syss

let clibs syss =
  union_elms (fun (l, _) -> l) (fun s -> s.sys_clibs) syss


(** Loads the given system file and all of the systems that it depends
    on.  The result is as tuple: (main system * all systems). *)
let load load_conf parse_system file =
  let parse = parse_system_file parse_system in
  let find = find_system load_conf parse in
  let rec deps seen syss cur =
    List.fold_left (fun ((syss, seen) as accum) name ->
		      if S.mem name seen then
			accum
		      else
			deps (S.add name seen) syss (find name))
      (cur :: syss, seen)
      cur.sys_systems
  in
  let main = parse file in
    List.iter load_conf main.sys_dotfiles;
    let seen = S.singleton main.sys_name in
    let syss, _ = deps seen [] main in
    let sys = { main with
		  sys_files = files syss;
		  sys_extensions = extensions syss;
		  sys_libs = libs syss;
		  sys_clibs = clibs syss; }
    in
      sys


(** [find_main ?dir ()] finds the main system for the current
    directory.  This routine peals back directories looking for one
    that contains a .system file. *)
let rec find_main ?(dir=Fname.realpath ".") () =
  let ents = Array.to_list (Sys.readdir dir) in
  let is_system s = (Fname.extension s) = ".system" in
    match List.filter is_system ents with
      | s :: ss ->
	  let sys = Filename.concat dir s in
	    if ss <> [] then
	      vprintf "Multiple system files found, using %s\n" s;
	    vprintf ~lvl:verb_debug "Found system file %s\n" sys;
	    sys
      | [] ->
	  if not (Fname.outter_most_dir dir) then
	    find_main ~dir:(Filename.dirname dir) ()
	  else
	    raise Not_found

(************************************************************)
(** {1 Cleaning} *)

(** Remove the file if it exists and print a message if verbosity is
    high enough. *)
let try_rm file =
  if Sys.file_exists file then begin
    vprintf ~lvl:verb_debug "Removing %s\n" file;
    Sys.remove file
  end

(** Remove the directory and all of its contents. *)
let rec rmdir ?(depth=0) dir =
  if Sys.file_exists dir && Sys.is_directory dir then begin
    if depth = 0 then vprintf "Removing directory %s\n" dir;
    let ents = Sys.readdir dir in
      Array.iter (function f when f <> "." && f <> ".." ->
		    let path = Filename.concat dir f in
		      vprintf ~lvl:verb_debug "Removing %s\n" path;
		      if Sys.is_directory path then
			rmdir ~depth:(depth + 1) path
		      else
			try_rm path
		    | _ -> ())
	ents;
      Unix.rmdir dir;
  end

(** Check for and remove the annotation file if it was copied. *)
let clean_annot_file f =
  if File.is_ml f then
    let annot_file = (Filename.chop_extension f.file_path) ^ ".annot" in
      try_rm annot_file

(** Remove generated source files from source code generators like
    ocamllex and ocamlyacc (menhir). *)
let clean_generated_source f =
  let rm_ml ?(mli=false) path =
    let no_ext = Filename.chop_extension path in
    let ml = no_ext ^ ".ml" in
      if mli then begin
	let mli = no_ext ^ ".mli" in try_rm mli;
      end;
      try_rm ml
  in
    match Fname.extension f.file_path with
      | ".mly" -> rm_ml ~mli:true f.file_path
      | ".mll" -> rm_ml f.file_path
      | _ -> ()

(** Clean the build files for the given system. *)
let clean sys =
  let dirs =
    List.fold_left (fun s f -> S.add (file_build_dir f.file_path) s)
      S.empty sys.sys_files
  in
    S.iter rmdir dirs;
    List.iter (fun f ->
		 clean_annot_file f;
		 clean_generated_source f;)
      sys.sys_files
