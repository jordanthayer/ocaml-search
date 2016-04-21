(** Dealing with compiler and linker flags.

    @author eaburns
    @since 2011-02-09
*)

open Printf
open Types

module S = Set.Make(String)
module H = Hashtbl.Make(struct
			  type t = string
			  let hash a = Hashtbl.hash a
			  let equal (a:string) b = a = b
			end)

(** Simple utility function to append flags to a buffer. *)
let rec append_flags b = function
  | f :: fs ->
      Buffer.add_string b f;
      Buffer.add_char b ' ';
      append_flags b fs
  | [] -> ()

(************************************************************)
(** {1 Include flags} *)

(** Get the set of directories for all of the given files. *)
let file_dirs files =
  List.fold_left (fun s f ->
		    S.add (Filename.dirname f.file_path) s)
    S.empty
    files


(** Adds the include flags for all systems to the given buffer. *)
let system_include_flags b files =
  let fdirs = file_dirs files in
    S.iter (fun s ->
	      if Buffer.length b > 0 then Buffer.add_char b ' ';
	      Buffer.add_string b "-I ";
	      Buffer.add_string b s)
      fdirs


(** Adds the include flags for libraries to the buffer.

    @param prefix is prepended to the library location.
    @param suffix is appended to the library location.  *)
let library_includes b prefix suffix libs =
  let inc l = prefix ^ l ^ suffix in
  let seen = Hashtbl.create 149 in
    List.iter (function (_, "") -> ()
		 | (_, loc) when not (Hashtbl.mem seen loc) ->
		     Hashtbl.add seen loc true;
		     if Buffer.length b > 0 then Buffer.add_char b ' ';
		     Buffer.add_string b (inc loc)
		 | _ -> ())
      libs

(** The include flags for ML. *)
let ml_include_flags sys files =
  let b = Buffer.create 100 in
    system_include_flags b files;
    library_includes b "-I " "" sys.sys_libs;
    Buffer.contents b


(** The include flags for C source. *)
let c_include_flags sys =
  let b = Buffer.create 100 in
    library_includes b "-ccopt -I" "/include" sys.sys_clibs;
    Buffer.contents b


(************************************************************)
(** {1 Compilation flags} *)

(** Flags for the ocaml parser-pretty-printer. *)
let parser_flags incl f =
  let b = Buffer.create 100 in
    begin match f.file_pp with
      | pp :: parsers ->
	  Buffer.add_string b "-pp \"";
	  Buffer.add_string b pp;
	  if parsers <> [] then begin
	    Buffer.add_char b ' ';
	    Buffer.add_string b incl;
	    List.iter (fun pa ->
			 Buffer.add_char b ' ';
			 Buffer.add_string b pa)
	      parsers;
	  end;
	  Buffer.add_string b "\" ";
      | [] -> ()
    end;
    Buffer.contents b


(** Compilation flags. *)
let cmp_flags incl f =
  let b = Buffer.create 100 in
    Buffer.add_string b (parser_flags incl f);
    append_flags b f.file_cmp_flags;
    Buffer.add_string b incl;
    Buffer.contents b

(************************************************************)
(** {1 Linker flags} This is a bit more complex because order may
    matter for linking!  We need to build a dependency graph on flags
    based on the order in which they were specified.  We also need to
    separate C linking flags from OCaml ones.


    {b For now, linker flags are passed in an arbitrary order...}
*)

type lnk_flag = {
  flag : string;
  mutable deps : lnk_flag list;
}

(** Dependency graph among linker flags. *)
module G = struct
  type node = lnk_flag
  type graph = node list
  let iter objs f = List.iter f objs
  let succs _ obj = obj.deps
  let equal a b = a.flag = b.flag
  let hash = Hashtbl.hash
  let print_node ochan obj = fprintf ochan "%s" obj.flag
end


(** Get or create the linker flag object from the given flag
    string. *)
let lnk_flag flags f =
  try
    H.find flags f
  with Not_found ->
    let lf = { flag = f; deps = [] } in
      H.add flags f lf;
      lf

(** Get the file linker flags along with their dependencies (in the
    order specified in the .system file). *)
let file_lnk_flags flags f =
  let rec scan prev = function
    | f :: fs ->
	let lf = lnk_flag flags f in
	  lf.deps <- prev @ lf.deps;
	  scan (lf :: prev) fs
    | [] -> ()
  in
    scan [] f.file_lnk_flags


(** Get the library linker flags. *)
let lib_lnk_flags flags get_flags libs =
  let rec scan prev = function
    | l :: ls ->
	let add_prev prev f =
	  let lf = lnk_flag flags f in
	    lf.deps <- prev @ lf.deps;
	    lf in
	let lfs, _ =
	  List.fold_left
	    (fun (lfs, p) f -> let lf = add_prev p f in lf :: lfs, lf :: p)
	    ([], prev) (get_flags l)
	in
	  scan (lfs @ prev) ls
    | [] -> ()
  in
    scan [] libs


(** Sort the linker flags based on their dependencies and add them to
    the given buffer.

    @param flags is a hash table containing flag nodes. *)
let sorted_lnk_flags b flags =
  let module G = Graph.Algs(G) in
  let nodes = H.fold (fun _ f l -> f :: l) flags [] in
  let sorted = G.topo_sort ~allow_cycles:true nodes in
    List.iter (fun s ->
		 if Buffer.length b > 0 then Buffer.add_char b ' ';
		 Buffer.add_string b s.flag)
      (List.rev sorted)


(** Get the ML linker flags. *)
let ml_lnk_flags byte ml_libs b sys =
  let flags = H.create 149 in
  let ext = if byte then ".cma" else ".cmxa" in
  let lib_flags (lib, loc) =
    (if loc <> "" then [ "-I " ^ loc ] else []) @ [ lib ^ ext ] in
  let fs = List.filter File.is_ml sys.sys_files in
    if ml_libs then begin
      lib_lnk_flags flags lib_flags sys.sys_libs;
      try
	(* This is a hack for reverse compat with the old ocm.  We
	   need to make sure that threads.cma is dependent on
	   unix.cma. *)
	let t = H.find flags "threads.cma" in
	let u = lnk_flag flags "unix.cma" in
	  t.deps <- u :: t.deps;
	  u.deps <- List.filter (fun f -> f.flag <> "threads.cma") u.deps;
      with Not_found -> ()
    end;
    List.iter (fun f -> file_lnk_flags flags f) fs;
    sorted_lnk_flags b flags


(** Get the C linker flags. *)
let c_lnk_flags b sys =
  let flags = H.create 149 in
  let lib_flags (lib, loc) =
    (if loc <> "" then [ "-ccopt -L" ^ loc ] else []) @ [ "-cclib -l" ^ lib ]
  in
  let fs = List.filter File.is_c sys.sys_files in
    lib_lnk_flags flags lib_flags sys.sys_clibs;
    List.iter (fun f -> file_lnk_flags flags f) fs;
    sorted_lnk_flags b flags


(** Get the linker flags for the given system. *)
let lnk_flags byte ml_libs sys =
  let b = Buffer.create 100 in
    c_lnk_flags b sys;
    ml_lnk_flags byte ml_libs b sys;
    Buffer.contents b
