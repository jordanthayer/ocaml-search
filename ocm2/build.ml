(** Building of results.

   @author eaburns
   @since 2010-12-08
*)

open Printf
open Verb
open Types

(** Build a list of objects.  Times the build and also gets the count
    of the number of objects that were actually re-built. *)
let build_objects nbuilt time objs =
  let start_time = Unix.gettimeofday () in
  let n = Object.ensure objs in
    time := !time +. (Unix.gettimeofday () -. start_time);
    nbuilt := !nbuilt + n;
    objs


(************************************************************)
(** {1 Copying the source files} *)
module Source_files = struct

  let nbuilt = ref 0 and time = ref 0.

  (** Copies file [a] to [b].  The string [prefix] is optionally added
      to the top of the output file. *)
  let copy_file ?prefix a b () =
    let block_size = 4096 in
    let buf = String.create block_size in
    let st = Unix.stat a in
    let left = ref st.Unix.st_size in
    let ain = open_in a in
    let bout = open_out b in
      begin match prefix with
	| Some s -> output bout s 0 (String.length s)
	| None -> ()
      end;
      try
	while !left > 0 do
	  let nread = min !left block_size in
	  let n = input ain buf 0 nread in
	    if n > 0 then output bout buf 0 n;
	    left := !left - n
	done;
	close_in ain;
	close_out bout;
      with e ->
	close_in ain;
	close_out bout;
	raise e


  (** If the given file is the source for a compiler-compiler then
      this function will create an object for the actual source code
      which depends on the compiler-compiler source, otherwise it just
      passes through.  Basically, this function will ensure that
      ocamlyacc, ocamllex, etc. are called when copying the source
      code to the build directory. *)
  let build_and_copy (os, fs) f =
    let path = f.file_path in
    let fname = Filename.basename path in
    let no_ext = Filename.chop_extension path in
    let bdir = System.file_build_dir path in
      match Fname.extension path with
	| ".mly" ->
	    (* ocamlyacc *)
	    let ml_p = no_ext ^ ".ml" in
	    let mli_p = no_ext ^ ".mli" in
	    let yacc = Object.Cmd (sprintf "%s %s" !Conf.ocamlyacc path) in
	    let ml_o = Object.make ml_p [path] yacc [] in
	    let mli_o = Object.make mli_p [path] yacc [] in
	      (* copy ocamlyacc output *)
	    let ml_copy = Filename.concat bdir (Filename.basename ml_p) in
	    let mli_copy = Filename.concat bdir (Filename.basename mli_p) in
	    let copy_ml = Object.Fun (copy_file ml_p ml_copy) in
	    let copy_mli = Object.Fun (copy_file mli_p mli_copy) in
	    let ml_oobj = Object.make ml_copy [ml_p] copy_ml [ml_o] in
	    let mli_oobj = Object.make mli_copy [mli_p] copy_mli [mli_o] in
	      (ml_oobj :: mli_oobj :: ml_o :: mli_o :: os,
	       { f with file_path = ml_copy } ::
		 { f with file_path = mli_copy } :: fs)
	| ".mll" ->
	    (* ocamllex *)
	    let ml_p = no_ext ^ ".ml" in
	    let lex = Object.Cmd (sprintf "%s %s" !Conf.ocamllex path) in
	    let ml_o = Object.make ml_p [path] lex [] in
	      (* copy ocamllex output *)
	    let ml_copy = Filename.concat bdir (Filename.basename ml_p) in
	    let copy = Object.Fun (copy_file ml_p ml_copy) in
	    let ml_oobj = Object.make ml_copy [ml_p] copy [ml_o] in
	      ml_oobj :: ml_o :: os, { f with file_path = ml_copy } :: fs
	| _ ->
	    (* just copy *)
	    let line_annot = sprintf "# 1 \"%s\"\n" path in
	    let opath = Filename.concat bdir fname in
	    let act = Object.Fun (copy_file ~prefix:line_annot path opath) in
	    let obj = Object.make opath [path] act [] in
	    let f = { f with file_path = opath } in
	      obj :: os, f :: fs


  (** Copies the source files from into their system build directory.
      Builds any source files that may need to be built with a parser
      or lexer generator.  The result is a new system with these files
      re-located. *)
  let build sys files =
    vprintf ~lvl:verb_debug "\n\nBuilding source files:\n";
    let objs, files' = List.fold_left build_and_copy ([], []) files in
      ignore (build_objects nbuilt time objs);
      files'

end


(************************************************************)
(** {1 Building dependency files} *)

module Dep_files = struct

  let nbuilt = ref 0 and time = ref 0.

  (** Gets the dependency file for the given source file. *)
  let obj_file file =
    let path = file.file_path in
    let base = (Filename.basename path) ^ ".d" in
      System.file_build_path base


  (** Gets object wrapping a dependency file. *)
  let obj incl file =
    let src = file.file_path in
    let pp = Flags.parser_flags incl file in
    let obj_file = (Filename.basename src) ^ ".d" in
    let obj = Filename.concat (System.file_build_dir src) obj_file in
    let cmd = sprintf "%s %s %s%s > %s" !Conf.ocamldep incl pp src obj_file in
      Object.make ~obj ~srcs:[src] ~action:(Object.Cmd cmd) []


  (** Build all of the dependency files. *)
  let build sys files =
    vprintf ~lvl:verb_debug "\n\nBuilding depfiles:\n";
    let incl = Flags.ml_include_flags sys files in
    let objs = List.map (obj incl) files in
      build_objects nbuilt time objs

end

(** {1 Object files} *)

module Object_files = struct

  let nbuilt = ref 0 and time = ref 0.

  (** [file_objects incl byte f] gets the object files for the given
      file. *)
  let file_objects sys files =
    let ml_incl = Flags.ml_include_flags sys files in
    let c_incl = Flags.c_include_flags sys in
    let get_objs byte f =
      let path = f.file_path in
      let src = Filename.basename path in
      let no_ext = Filename.chop_extension src in
      let dir = Filename.dirname f.file_path in
	match Fname.extension path with
	  | ".ml" ->
	      let mli = Filename.concat dir (no_ext ^ ".mli") in
	      let srcs = src :: if Sys.file_exists mli then [mli] else [] in
	      let ocamlc = if byte then !Conf.ocamlc else !Conf.ocamlopt in
	      let ext = if byte then ".cmo" else ".cmx" in
	      let obj = Filename.concat dir (no_ext ^ ext) in
	      let flags = Flags.cmp_flags ml_incl f in
	      let cmd = sprintf "%s %s -c %s" ocamlc flags src in
		[ Object.make ~obj ~srcs ~action:(Object.Cmd cmd) [] ]
	  | ".mli" ->
	      let obj = Filename.concat dir (no_ext ^ ".cmi") in
	      let flags = Flags.cmp_flags ml_incl f in
	      let cmd = sprintf "%s %s -c %s" !Conf.ocamlc flags src in
		[ Object.make ~obj ~srcs:[src] ~action:(Object.Cmd cmd) [] ]
	  | ".c" ->
	      let obj = Filename.concat dir (no_ext ^ ".o") in
	      let flags = Flags.cmp_flags c_incl f in
	      let cmd = sprintf "%s %s -c %s" !Conf.cc flags src in
		[ Object.make ~obj ~srcs:[src] ~action:(Object.Cmd cmd) [] ]
	  | e -> failwith ("Unknown file extension: " ^ e)
    in get_objs


  (** This is a bit of a lame hack. *)
  let rec fix_dep byte f d =
    let dirname f d = match Filename.dirname d with
      | "." -> Filename.concat (Filename.dirname f) d
      | _ -> d in
    let d' = dirname f d in
      match Fname.extension d' with
	| ".cmi" when Filename.chop_extension d <> Filename.chop_extension f ->
	    (* Sometimes ocamldep makes a dependent depend on a .cmi, but not
	       the .cmo.  We 'fix' this by making the dependency the .cmo
	       instead.  We want to avoid this, however, if the dependent is
	       the .cmo itself.  *)
	    fix_dep byte f (Filename.chop_extension d' ^ ".cmo")
	| ".cmo" when not byte ->
	    (* In native mode, .cmi files will depend on the .cmo
	       version instead of the .cmx... *)
	    fix_dep byte f (Filename.chop_extension d' ^ ".cmx")
	| _ -> d'

  (** Link the dependencies by iterating through the dependency
      files. *)
  let link_deps byte os deps =
    let see_dep f d =
      let d = fix_dep byte f d in
	try
	  let obj = Hashtbl.find os f and dep = Hashtbl.find os d in
	    vprintf ~lvl:verb_debug "%s -> %s\n" f d;
	    obj.Object.deps <- dep :: obj.Object.deps;
	with Not_found ->
	  let nf o = vprintf ~lvl:verb_debug "object %s not found\n" o in
	    if Hashtbl.mem os f then nf d else nf f
    in
      List.iter
	(fun d ->
	   let o = d.Object.obj in
	     vprintf ~lvl:verb_debug "Loading depfile: %s\n" o;
	     Deps.load ignore see_dep o)
	deps

  (** Create a dependency graph of the object files. *)
  let build byte deps sys files =
    vprintf ~lvl:verb_debug "\n\nBuilding objects:\n";
    let file_objects = file_objects sys files in
    let objs =
      List.fold_left (fun l f -> file_objects byte f @ l) [] files in
    let os = Hashtbl.create 149 in
      List.iter (fun o -> Hashtbl.replace os o.Object.obj o) objs;
      link_deps byte os deps;
      Object.sorted (build_objects nbuilt time objs)

end

(************************************************************)
(** {1 Extensions} *)

module Extension_files = struct

  let nbuilt = ref 0 and time = ref 0.

  let build sys files exts =
    vprintf ~lvl:verb_debug "\n\nBuilding extensions:\n";
    let file_objects = Object_files.file_objects sys (files @ exts) in
    let objs = List.fold_left (fun l f -> file_objects true f @ l) [] exts in
      build_objects nbuilt time objs

end

(************************************************************)
(** {1 Annotation files} check if a .annot file was emmitted from
    compilation and, if it was, copy it back to the directory with its
    original source file. *)

module Annot_files = struct
  let nbuilt = ref 0 and time = ref 0.

  (** Builds an object structure that tests for the annotation file
      associated with the given source file and copies it back to the
      directory of its source file it it existed. *)
  let add_annot_object lst f =
    let bpath = System.file_build_path f.file_path in
    let annot_file = (Filename.chop_extension bpath) ^ ".annot" in
    let annot_file' = (Filename.chop_extension f.file_path) ^ ".annot" in
      if Sys.file_exists annot_file then
	let copy = Source_files.copy_file annot_file annot_file' in
	  Object.make ~obj:annot_file' ~srcs:[annot_file]
	    ~action:(Object.Fun copy) [] :: lst
      else
	lst

  (** This expects to be given the {b original} source file list,
      not the copied source file list. *)
  let find_and_copy files =
    vprintf ~lvl:verb_debug "\n\nHandling .annot files:\n";
    let mls = List.filter (File.is_ml ~mli:false) files in
    let objs = List.fold_left add_annot_object [] mls in
      build_objects nbuilt time objs
end

(************************************************************)
(** {1 The main build driver} *)


(** Do each of the following for files that have been updated:

    - copy source to the build directory

    - build source files

    - build camlp4 extension modules

    - build dependency files

    - build object files

    The result is a topologically sorted list of all of the object
    files.

    @param byte Determines if the OCaml objects will be built as
    bytecode or native code. *)
let objects byte sys files =
  let module Os = Hashtbl.Make(Object) in
  let files' = Source_files.build sys files in
  let exts = Source_files.build sys sys.sys_extensions in
  let _ = Extension_files.build sys files' exts in
  let deps = Dep_files.build sys files' in
  let objs = Object_files.build byte deps sys files' in
  let _ = Annot_files.find_and_copy files in
    files', objs
