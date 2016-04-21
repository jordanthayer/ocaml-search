(** Source/object files.

    @author eaburns
    @since 2011-02-09
*)

open Printf
open Verb
open Types

let is_ml ?(mli=true) f = match Fname.extension f.file_path with
  | ".ml" | ".mll" | ".mly" -> true
  | ".mli" -> mli
  | _ -> false

let is_c f = match Fname.extension f.file_path with
  | ".c" | ".h" -> true
  | _ -> false

(** [file ?threads ?pp path] makes a new file structure. *)
let make ?(cmp_flags=[]) ?(lnk_flags=[]) ?(pp=[]) path =
  {
    file_path = path;
    file_cmp_flags = cmp_flags;
    file_lnk_flags = lnk_flags;
    file_pp = pp;
  }


(** Gets the .ml and .mli (if it exists) for the given file that has
    no extension. *)
let ml_and_mli path f =
  let mli_path = path ^ ".mli" in
  let ml_path = path ^ ".ml" in
    if not (Sys.file_exists ml_path) then
      failwith (sprintf "%s does not exist" ml_path);
    let ml = { f with file_path = ml_path } in
      if Sys.file_exists mli_path then
	ml :: [ { f with file_path = mli_path } ]
      else
	[ ml ]


(** Root the given files in [dir] and add '.ml' and '.mli' extensions
    where appropriate and remove duplicates... *)
let rec normalize ?(seen=Hashtbl.create 149) root = function
  | f :: fs when Hashtbl.mem seen f.file_path ->
      normalize root fs
  | f :: fs ->
      let name = f.file_path in
      let path = Filename.concat root name in
	Hashtbl.add seen name true;
	begin match Fname.extension name with
	  | "" -> (ml_and_mli path f) @ normalize ~seen root fs
	  | e -> { f with file_path = path } :: (normalize ~seen root fs)
	end;
  | [] -> []
