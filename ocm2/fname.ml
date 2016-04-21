(** Some file name and path handling.

    @author eaburns
    @since 2010-08-24
*)

open Verb

module Set = struct
  module String_set = Set.Make(String)

  include String_set

  let map f s =
    String_set.fold (fun e s -> String_set.add (f e) s) s String_set.empty

end

type path_set = Set.t

let dir_sep = if Sys.os_type = "Win32" then '\\' else '/'

exception No_user of string

exception No_file of string


(** Tests if the path is for the root directory. *)
let is_root path =
  (Filename.dirname path) = path


(** Gets the user associated with a path beginning with a tilde. *)
let get_tilde_user path len =
  assert (path.[0] = '~');
  if len = 1 || path.[1] = dir_sep
  then begin
    let uid = Unix.geteuid () in
    let pw = Unix.getpwuid uid in
      pw.Unix.pw_name, 2
  end else begin
    let b = Buffer.create 10 in
    let i = ref 1 in
      while !i < len && path.[!i] <> dir_sep do
	Buffer.add_char b path.[!i];
	incr i;
      done;
      Buffer.contents b, (!i + 1)
  end


(** Expands the '~' character at the beginning of a path. *)
let expand_tilde path =
  if path.[0] <> '~'
  then path
  else begin
    let l = String.length path in
    let user, sublen = get_tilde_user path l in
    let pw = try Unix.getpwnam user with Not_found -> raise (No_user user) in
    let home = pw.Unix.pw_dir in
    let path_tail =
      if l - sublen <= 0 then "" else String.sub path sublen (l - sublen)
    in Filename.concat home path_tail
  end


(** Makes the given path absolute by either tacking on the currend
    directory or by expanding a tilde. *)
let make_absolute ?(cwd=Sys.getcwd ()) path =
  if path.[0] = '~'
  then expand_tilde path
  else begin
    if Filename.is_relative path
    then Filename.concat cwd path
    else path
  end


(** Removes '.', '..' and tildes. *)
let normalize ?(cwd=Sys.getcwd ()) path =
  let rec remove_dots path =
    if is_root path
    then path
    else begin
      let dir = Filename.dirname path in
	match Filename.basename path with
	  | b when b = Filename.current_dir_name -> remove_dots dir
	  | b when b = Filename.parent_dir_name ->
	      Filename.dirname (remove_dots dir)
	  | b -> Filename.concat (remove_dots dir) b
    end
  in remove_dots (make_absolute ~cwd path)


(** Normalize the path and follow symbolic links. *)
let realpath path =
  let rec do_realpath path =
    if is_root path
    then path
    else begin
      let ls = Unix.lstat path in
	match ls.Unix.st_kind with
	  | Unix.S_LNK ->
	      let dir = Filename.dirname path in
		do_realpath (normalize ~cwd:dir (Unix.readlink path))
	  | _ ->
	      let dir = Filename.dirname path in
	      let base = Filename.basename path in
		Filename.concat (do_realpath dir) base
    end
  in
    try do_realpath (normalize path)
    with Unix.Unix_error (Unix.ENOENT, _, f) -> raise (No_file f)


(** Expands all simple '*'s in a path name.  A simple '*' is one in
    which the entire component is 'starred'.  This means that paths
    like: ["/home/foo*"] will not be expanded properly, but
    ["/home/*/foo"] will. *)
let expand_simple_stars path =
  let rec do_expand path =
    if is_root path then
      Set.singleton path
    else begin match Filename.basename path with
      | "*" ->
	  Set.fold
	    (fun e acc ->
	       if Sys.is_directory e then
		 let bases =
		   Array.fold_left (fun s e -> Set.add e s)
		     Set.empty (Sys.readdir e)
		 in
		   Set.union
		     (Set.map (Filename.concat e) bases)
		     acc
	       else Set.add e acc)
	    (do_expand (Filename.dirname path)) Set.empty
      | base ->
	  Set.map
	    (fun d -> Filename.concat d base)
	    (do_expand (Filename.dirname path))
    end
  in Set.filter Sys.file_exists (do_expand (normalize path))


(** Finds all files for which [f] returns [true] in the directories
    specified by [roots].  [f]'s first argument is the root directory
    and its second argument is the file within that directory. *)
let find_files roots f =
  let accum = ref [] in
    Set.iter (fun r ->
		if Sys.is_directory r then begin
		  let ents = Sys.readdir r in
		    for i = 0 to (Array.length ents) - 1 do
		      let e = ents.(i) in
		      let path = Filename.concat r e in
			if f r e then accum := path :: !accum
		    done;
		end)
      roots;
    !accum


(** Finds files named [name] within the given set of roots. *)
let find_files_named roots name =
  find_files roots (fun _ n -> n = name)


(** Tests if [f] is the outter most directory in a path. *)
let outter_most_dir f =
  let d = Filename.dirname f in
    d = (Filename.dirname d)


(** Ensures that the given directory exists. *)
let rec ensure_directory f =
  if not (outter_most_dir f) then ensure_directory (Filename.dirname f);
  if not (Sys.file_exists f) then begin
    vprintf ~lvl:verb_debug "Creating directory %s\n" f;
    try Unix.mkdir f 0o777 with Unix.Unix_error (Unix.EEXIST, _, _) -> ()
  end


(** Gets the extension from [f]. *)
let extension f =
  try
    let base = Filename.chop_extension f in
    let len = String.length f in
    let base_len = String.length base in
      String.sub f base_len (len - base_len)
  with Invalid_argument _ -> ""
