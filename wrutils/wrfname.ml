(* $Id: wrfname.ml,v 1.2 2003/09/30 18:11:13 ruml Exp ruml $

   operations on file names

   and thus files and directories...
*)


(***** file info *****)


let file_id filename =
  (** unique id for file referred to by [filename] *)
  let s = Unix.stat filename in
    s.Unix.st_dev, s.Unix.st_ino


let samefile f1 f2 =
  (** true iff [f1] and [f2] refer to the same file *)
  (file_id f1) = (file_id f2)


let file_perms name =
  (Unix.stat name).Unix.st_perm


let file_kind name =
  (Unix.stat name).Unix.st_kind


let file_mod_time name =
  (Unix.stat name).Unix.st_mtime


let directory_p name =
  (** true iff name (which must exist) is a directory *)
  match file_kind name with
    Unix.S_DIR -> true
  | _ -> false


let file_p name =
  (** true iff name (which must exist) is a regular file or a symlink *)
  match file_kind name with
    Unix.S_REG | Unix.S_LNK -> true
  | _ -> false


(**** filenames ****)


let get_extension name =
  (** [get_extension name] gets the extension (with the leading '.')
      from the file name.  If there is no extension then the empty
      string is returned. *)
  try
    let base = Filename.chop_extension name in
    let base_len = String.length base
    and len = String.length name in
      String.sub name base_len (len - base_len)
  with Invalid_argument _ -> ""


let stem name =
  (** returns base file name without leading directory or trailing
      extension, if any *)
  let name = Filename.basename name in
    try
      Filename.chop_extension name
    with Invalid_argument _ -> name


let new_extension name ext =
  (** removes any extension from [name] and adds [ext], interposing a
    period if necessary *)
  (try
     Filename.chop_extension name
   with Invalid_argument _ -> name)
  ^ (if ext.[0] = '.'
     then ext
     else ("." ^ ext))


let filename_from_list list =
  List.fold_left Filename.concat "" list


let create_new name =
  (** like touch.  [name] must not already exist *)
  assert (not (Sys.file_exists name));
  close_out (open_out name)


let with_temp_file ?(prefix = "temp-") ?(suffix = "") f =
  let path = Filename.temp_file prefix suffix in
  let result = f path in
    if Sys.file_exists path then
      Sys.remove path;
    result


(***** directories *****)


let dir_contents dir_name =
  (** given a string naming a directory, returns two lists of strings:
    the files/symlinks and the directories.  ignores devices, named
    pipes, and sockets *)
  let d = Unix.opendir dir_name
  and files = ref []
  and dirs = ref [] in
    (try
       while true do
	 match Unix.readdir d with
	   "." | ".." -> ()
	 | base ->
	     let n = Filename.concat dir_name base in
	       try
		 match file_kind n with
		   Unix.S_REG | Unix.S_LNK -> files := n::!files
		 | Unix.S_DIR -> dirs := n::!dirs
		 | _ -> ()
		     (* eg, broken symlinks cannot be stat'ed *)
		with Unix.Unix_error (_, "stat", _) -> ()
       done
     with End_of_file -> ());
    Unix.closedir d;
    (List.rev !files), (List.rev !dirs)


let rec ensure_dir dirname =
  (** ensures that the given directory exists, creating any
    intermediate directories as needed *)
  if not (Sys.file_exists dirname)
  then
    let sub = Filename.dirname dirname in
      ensure_dir sub;
      (* check again to avoid failure of Filename.dirname on "/foo/bar/" *)
      if not (Sys.file_exists dirname)
      then
	Unix.mkdir dirname (file_perms sub)


let ensure_path filename =
  ensure_dir (Filename.dirname filename)


let homedir () =
  match Sys.os_type with
    "Win32" -> Sys.getcwd ()
  | _ -> (Unix.getpwuid (Unix.getuid ())).Unix.pw_dir


let expand_home filename =
  (** under Unix, expand starting ~ if present *)
  if (Sys.os_type = "Unix") ||
    (Sys.os_type = "Cygwin")
  then
    let len = String.length filename in
      if ((len > 0) && (filename.[0] == '~'))
      then
	Filename.concat (homedir ())
	  (Wrstr.peel ~n:(if ((len > 1) && (filename.[1] == '/'))
			     then 2 else 1)
	     filename)
      else filename
  else filename


let windows_p = match Sys.os_type with
    "Win32" | "Cygwin" -> true
  | _ -> false


let split_path filename =
  (** returns a list of strings.  first will be starting directory (eg
    "." or "u:/"), rest will be directory names relative to previous,
    last might be filename *)
  (* tries to be better than (Wrstr.split_bag "/" name) *)
  let filename = (if ((Wrstr.ends_with '/' filename) ||
		      (windows_p && (Wrstr.ends_with '\\' filename)))
		  then Wrstr.chop filename
		  else filename) in
  let rec split prev f =
    let dir = Filename.dirname f
    and base = Filename.basename f in
      if dir = f
      then dir::prev
      else split (base::prev) dir
  in
    split [Filename.basename filename] (Filename.dirname filename)


let split_path_normed dir =
  (** like split_path, but removes components like ".." or "." *)
  let rec process so_far = function
      [] -> List.rev so_far
    | part::rest ->
	if (part = Filename.current_dir_name)
	then process so_far rest
	else if (part = Filename.parent_dir_name)
	then
	  match so_far with
	    prev::other -> process other rest
	  | [] -> failwith "can't back up in filename!"
	else process (part::so_far) rest
  in
  let parts = split_path dir in
    process [] parts


let collapse_path = function
    first::rest -> List.fold_left Filename.concat first rest
  | [] ->
      (* failwith "path with no parts!" *)
      Filename.current_dir_name


let expand_stars name =
  let rec expand so_far = function
      [] -> [ so_far ]
    | part::rest ->
	if part = "*"
	then
	  let _, dirs = dir_contents so_far in
	    Wrlist.mapcan (fun d -> expand d rest)
	      dirs
	else
	  let next = Filename.concat so_far part in
	    if Sys.file_exists next
	    then expand next rest
	    else []
  in
    match split_path name with
      start::rest ->
	expand start rest
    | [] -> failwith ("bad path: \"" ^ name ^ "\"")


let make_absolute cwd dir =
  if Filename.is_relative dir
  then Filename.concat cwd dir
  else dir


let add_root_ext root ext path =
  make_absolute root (new_extension path ext)


let split_canonical cwd path =
  split_path_normed (make_absolute cwd path)


let canonicalize ?(cwd = Sys.getcwd ()) path =
  (** returns absolute path with no ".." or "." *)
  collapse_path (split_canonical cwd path)


let make_relative base path =
  (** any relative argument is interpreted wrt the cwd *)
  let cwd = Sys.getcwd () in
  let base = split_canonical cwd base
  and path = split_canonical cwd path in
  let base, path = Wrlist.remove_common_prefix base path in
    List.fold_left (fun curr _ ->
		      (* back up over base elt *)
		      (Filename.concat Filename.parent_dir_name curr))
      (collapse_path path) base


(* EOF *)
