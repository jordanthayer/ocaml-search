(** Results of a build.

    @author eaburns
    @since 2011-02-09
*)

open Printf
open Types
open Verb

(* Get the kind from a string in a system file.  Some result kinds are
   not listed here because they can't be listed in the system file. *)
let kind_of_string = function
  | "bytedebug" -> Byte_debug
  | "native" -> Native
  | "nativedebug" -> Native_debug
  | "nativeprof" -> Native_prof
  | "nativeunsafe" -> Native_unsafe
  | k -> raise Parsing.Parse_error


let string_of_kind = function
  | Use -> "<use>"
  | Tags -> "<tags>"
  | Byte_debug -> "bytedebug"
  | Native -> "native"
  | Native_debug -> "nativedebug"
  | Native_prof -> "nativeprof"
  | Native_unsafe -> "nativeunsafe"


(** Get the result object from the system given the command-line
    result argument.  There are a few synthetic results too that don't
    appear in the system such as 'use', 'tags', etc. *)
let get_result sys = function
  | "use" ->
      { dummy_res with res_kind = Use }
  | "tags" ->
      { dummy_res with res_kind = Tags }
  | res_name ->
      try
	List.find (fun r -> r.res_name = res_name) sys.sys_results
      with Not_found ->
	failwith (sprintf "Result %s not found" res_name)


(** Link together all of the objects into a library.  This way, you
    can #load the library and it will pull in all of the OCaml *and* C
    code. *)
let library byte sys =
  let _, objs = Build.objects byte sys sys.sys_files in
  let b = Buffer.create 100 in
  let obj o = match Fname.extension o with
    | ".cmo" | ".cma" | ".cmx" | ".cmxa" | ".o" ->
	if Buffer.length b > 0 then Buffer.add_char b ' ';
	Buffer.add_string b o;
    | _ -> ()
  in
  let lib_prefix = "system_" ^ sys.sys_name in
    Buffer.add_string b !Conf.ocamlmklib;
    Buffer.add_string b " -o ";
    Buffer.add_string b lib_prefix;
    Buffer.add_char b ' ';
    Buffer.add_string b (Flags.lnk_flags byte false sys);
    List.iter (fun o -> obj o.Object.obj) objs;
    let cwd = Sys.getcwd () in
      Sys.chdir (System.sys_build_dir sys);
      vprintf ~lvl:verb_optional "%s\n" (Buffer.contents b);
      ignore (Sys.command (Buffer.contents b));
      Sys.chdir cwd;
      lib_prefix


(************************************************************)
(** {1 Use} There are two types of use.ml files: 1) loads a bunch of
    individual .cmo files; 2) loads a single .cma library.  The first
    type is used if all of the source files are .ml files and the
    second type is required if any of the source was built from .c
    files. *)

let use_byte_debug () =
  Conf.build_dir := Filename.concat !Conf.build_dir "byte_debug";
  Conf.ocamlc := !Conf.ocamlc ^ " -g"

let use_header b =
  Buffer.add_string b
    "(* This file was created automatically with ocm2. *)\n"

(** Build a use.ml file that includes a library.  This is used when
    some of the object files are built from .c source files. *)
let use_library_result sys =
  let lib_prefix = library true sys in
  let b = Buffer.create 100 in
  let dirs = Hashtbl.create 149 in
  let dir d =
    if not (Hashtbl.mem dirs d) then begin
      Hashtbl.add dirs d true;
      Buffer.add_string b "#directory \"";
      Buffer.add_string b d;
      Buffer.add_string b "\";;\n"
    end
  in
    use_header b;
    dir (System.sys_build_dir sys);
    List.iter (fun f -> dir (System.file_build_dir f.file_path)) sys.sys_files;
    Buffer.add_string b "#load \"";
    Buffer.add_string b lib_prefix;
    Buffer.add_string b ".cma\";;\n";
    let dir = Filename.dirname sys.sys_path in
    let use = Filename.concat dir "use.ml" in
    let f = open_out use in
      try Buffer.output_buffer f b; close_out f with e -> close_out f; raise e


(** Builds a use.ml file that pulls in each individual OCaml object
    file.  This is used if the system does not include any object
    files built from .c source files. *)
let use_objects_result sys =
  let _, objs = Build.objects true sys sys.sys_files in
  let b = Buffer.create 100 in
  let dirs = Hashtbl.create 149 in
  let dir d =
    if not (Hashtbl.mem dirs d) then begin
      Hashtbl.add dirs d true;
      Buffer.add_string b "#directory \"";
      Buffer.add_string b d;
      Buffer.add_string b "\";;\n"
    end
  in
  let load f = match Fname.extension f with
    | ".cmo" | ".cma" ->
	dir (System.file_build_dir f);
	Buffer.add_string b "#load \"";
	Buffer.add_string b f;
	Buffer.add_string b "\";;\n";
    | _ -> ()
  in
    use_header b;
    dir (System.sys_build_dir sys);
    List.iter (fun f -> load f.Object.obj) objs;
    let dir = Filename.dirname sys.sys_path in
    let use = Filename.concat dir "use.ml" in
    let f = open_out use in
      try Buffer.output_buffer f b; close_out f with e -> close_out f; raise e


(** Build a use.ml file. *)
let use_result sys =
  let has_c_source fs =
    List.exists (fun f -> (Fname.extension f.file_path) = ".c") fs
  in
    use_byte_debug ();
    if has_c_source sys.sys_files then
      use_library_result sys
    else
      use_objects_result sys

(************************************************************)
(** {1 Tags} *)

let tags_file = "TAGS"

let tag_file tcmd f =
  let cmd = sprintf "%s %s" tcmd f.file_path in
    vprintf "%s\n" cmd;
    let status = Sys.command cmd in
      if status <> 0 then
	failwith (sprintf "Command [%s] failed with status %d\n" cmd status)


let tags_result sys =
  let tf = Filename.concat (Filename.dirname sys.sys_path) tags_file in
  let tcmd = sprintf "%s -a -o %s" !Conf.otags tf in
    if Sys.file_exists tf then begin
      vprintf ~lvl:verb_debug "Removing %s\n" tf;
      Sys.remove tf;
    end;
    List.iter (tag_file tcmd) (List.filter File.is_ml sys.sys_files)


(************************************************************)
(** {1 Binary results} *)

(** Gets a big string containing all object file names. *)
let obj_str objs =
  let b = Buffer.create 4096 in
    List.iter (fun o ->
		 if Buffer.length b > 0 then Buffer.add_char b ' ';
		 Buffer.add_string b o.Object.obj)
      objs;
    Buffer.contents b


(** Gets a string of a bunch of file names. *)
let files_str files =
  let b = Buffer.create 4096 in
    List.iter (fun f ->
		 if Buffer.length b > 0 then Buffer.add_char b ' ';
		 Buffer.add_string b f.file_path)
      files;
    Buffer.contents b


(** Get all linkable objects. *)
let linkable_objects byte sys files =
  let files, objs = Build.objects byte sys files in
    files, List.filter (fun o -> Fname.extension o.Object.obj <> ".cmi") objs


(** Build a result binary. *)
let binary_result sys byte files bin_name =
  let bin_path = Filename.concat (Filename.dirname sys.sys_path) bin_name in
  let files', objs = linkable_objects byte sys (sys.sys_files @ files) in
    if not !Object.build_failures then begin
      let lnk_flags = Flags.lnk_flags byte true sys in
      let inc_flags = Flags.ml_include_flags sys files' in
      let linker = if byte then !Conf.ocamlc else !Conf.ocamlopt in
      let cmd =
	sprintf "%s %s %s %s -o %s\n" linker lnk_flags inc_flags
	  (obj_str objs) bin_path in
      let obj =
	Object.make bin_path (List.map (fun o -> o.Object.obj) objs)
	  (Object.Cmd cmd) []
      in
	ignore (Object.ensure [obj])
    end


(** Build a bytecode binary. *)
let byte_result sys result =
  begin match result.res_kind with
    | Byte_debug ->
	use_byte_debug ()
    | _ -> ()
  end;
(*
  if List.exists File.is_c sys.sys_files then
*)
    Conf.ocamlc := !Conf.ocamlc ^ " -custom";
  let bin_name = result.res_name ^ ".byte" in
    binary_result sys true result.res_files bin_name


(** Build a native binary. *)
let native_result sys result =
  let bin_name = result.res_name ^ "." ^ Sys.os_type in
    begin match result.res_kind with
      | Native_debug ->
	  Conf.build_dir := Filename.concat !Conf.build_dir "native_debug";
	  Conf.ocamlopt := !Conf.ocamlopt ^ " -g"
      | Native_prof ->
	  Conf.build_dir := Filename.concat !Conf.build_dir "native_prof";
	  Conf.ocamlopt := !Conf.ocamlopt ^ " -g -p"
      | Native_unsafe ->
	  let opts = " -unsafe -noassert -fno-PIC -inline 100" in
	    Conf.build_dir := Filename.concat !Conf.build_dir "native_unsafe";
	    Conf.ocamlopt := !Conf.ocamlopt ^ opts
      | Native ->
	  Conf.build_dir := Filename.concat !Conf.build_dir "native";
      | _ -> invalid_arg "native_result: non-native result kind"
    end;
    binary_result sys false result.res_files bin_name

(************************************************************)
(** {1 Result switch} *)

let build sys res_name =
  let result = get_result sys res_name in
    match result.res_kind with
      | Use ->
	  use_result sys
      | Tags ->
	  tags_result sys
      | Byte_debug ->
	  byte_result sys result
      | Native | Native_debug | Native_prof | Native_unsafe ->
	  native_result sys result
(*
      | k ->
	  failwith (sprintf "%s is not yet supported" (string_of_kind k))
*)
