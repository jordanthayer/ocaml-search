(* $Id: rdb.ml,v 1.1 2003/07/02 20:51:11 ruml Exp ruml $

   results data base

*)



type attrs = (string * string) list


let key_prefix = "KEY="
let key_prefix_len = String.length key_prefix

let unspecified = "UNSPECIFIED"


(*********** processing attrs *************)


let rec desired_val key = function
    (** given attrs, return desired value for [key], otherwise
      unspecified.  also returns remainaing attrs *)
    [] -> unspecified, []
  | ((k,v) as h)::t ->
      if (k = key)
      then v, t
      else
	let v, new_t = desired_val key t in
	  v, (h::new_t)


let bad_filename s =
  (** true if [s] contains characters that would mess up Unix or
    Windows paths *)
  (String.contains s '/') ||
  (String.contains s ':') ||
  (String.contains s '\\')


let assert_attrs attrs =
  List.iter (fun (k, v) ->
	       assert (not (bad_filename k));
	       assert (not (bad_filename v));
	       assert (not (Wrstr.starts_as key_prefix v));
(*	       assert (not (v = unspecified))*))
    attrs


let merge_attrs defaults attrs =
  List.append (List.filter (fun (k,_) ->
			      not (List.mem_assoc k attrs))
		 defaults)
    attrs


let override_attrs first second =
  List.append first
    (List.filter (fun (k,_) ->
		    not (List.mem_assoc k first))
       second)


let filter_attrs ignored_keys attrs =
  List.filter (fun (k,v) ->
		 not (List.mem k ignored_keys))
    attrs


let filtered_values ignored_keys attrs =
  Wrlist.map_opt (fun (k,v) ->
		    if (List.mem k ignored_keys)
		    then None
		    else Some v)
    attrs


(******* string representations ************)


let attrs_str ignored attrs =
  (** string from values in [attrs] *)
  String.concat "_" (filtered_values ignored attrs)


let human_str attrs =
  List.fold_left (fun s (vr, vl) ->
		    if s <> ""
		    then Printf.sprintf "%s, %s=%s" s vr vl
		    else Printf.sprintf "%s=%s" vr vl)
    "" attrs


(********* processing files ***********)


let keyfile_p name =
  (** does file [name] represent a key? *)
  (Wrfname.file_p name) &&
  (Wrstr.starts_as key_prefix (Filename.basename name))


let key name =
  (** the key represented by file [name] *)
  assert (keyfile_p name);
  let n = Filename.basename name in
    Wrstr.peel ~n:key_prefix_len n


let make_key dir key =
  Wrfname.create_new (Filename.concat dir (Wrutils.str "%s%s" key_prefix key))


let look_in dirname =
  (** returns optional key, then files then dirs *)
  let files, dirs = Wrfname.dir_contents dirname in
    match (List.filter keyfile_p files) with
      [] -> None, files, dirs
    | kf::[] -> (Some kf), (Wrlist.remove_first kf files), dirs
    | _ -> failwith (Wrutils.str "multiple key files in %s" dirname)


let value name =
  (** the value represented by file [name] *)
  assert (not (keyfile_p name));
  Filename.basename name


let find_val desired names =
  (** given file/dir names, return the one with value [desired], or None *)
  match List.filter (fun x -> (value x) = desired) names with
    [] -> None
  | n::[] -> Some n
  | _ -> failwith "multiple files or directories with same value!"


let val_file_name dir value =
  Filename.concat dir value


let make_val_dir dir value =
  (** makes directory and returns name *)
  let n = val_file_name dir value in
    Verb.pe Verb.toplvl "Making %s\n%!" n;
    Unix.mkdir n (Wrfname.file_perms dir);
    n


(************* functions for path_for ************)


let rename_unique f =
  let dir = Filename.dirname f
  and base = Filename.basename f
  and count = ref 1 in
  let new_name = Wrutils.eval_until (fun () ->
				     Filename.concat dir
				     (Wrutils.str "%s-temp-%d-%d"
					base (Unix.getpid ()) !count))
		   (fun n -> not (Sys.file_exists n))
  in
    Sys.rename f new_name;
    new_name


let rec descend dir desired =
  (** return the name of a file in or below [dir] that corresponds the
    [desired] attributes *)
  let keyfile, files, dirs = look_in dir in
    match keyfile with
      None ->
	(match desired with
	   [] -> failwith "found a complete match but it is an existing directory with no key!"
	 | _ -> (* pick first, make key.  if more, make dir for value and
		   descend with rest, else done *)
	     let k, v = List.hd desired in
	       make_key dir k;
	       match List.tl desired with
		 [] -> val_file_name dir v
	       | more -> descend (make_val_dir dir v) more)
    | Some k ->
	let v, remaining = desired_val (key k) desired in
	  match find_val v dirs with
	    None ->
	      (match remaining with
		 [] -> (* no more attrs, so done.  don't care if already
			  exists as a file *)
		   val_file_name dir v
	       | _ -> (* more attrs, so we need to make a new dir *)
		   match find_val v files with
		     None -> descend (make_val_dir dir v) remaining
		   | Some f ->
		       (* whoops! need to move existing file into new dir
			  as unspecified *)
		       let moved = rename_unique f in
		       let d = make_val_dir dir v in
			 Sys.rename moved (val_file_name d unspecified);
			 descend d remaining)
	  | Some d -> descend d remaining


let path_for root attrs =
  assert_attrs attrs;
  descend root attrs


let rec attrs_for path =
  (** [attrs_for path] gets the attributes that specify [path]. *)
  if path = ""
  then []
  else begin
    let basename = Filename.basename path
    and dirname = Filename.dirname path in
      if basename = "."
      then attrs_for dirname
      else begin
	match look_in dirname with
	  | Some k, files, dirs ->
	      (key k, basename) :: (attrs_for dirname)
	  | None , _, _ -> []
      end
  end

(************* functions for matching_attrs ************)


let rec matching dir remaining =
  (** returns list of attrs of files in [dir] or below that match
      [remaining]. *)
      let keyfile, files, dirs = look_in dir in
	match keyfile with
	    None ->
	      Wrutils.pr "WARNING: dir %s has no keyfile!\n" dir;
	      (* look in subdirs anyway *)
	      Wrlist.mapcan (fun d -> matching d remaining) dirs
	  | Some k ->
	      let key = key k in
	      let desired, remaining = desired_val key remaining in
		if (desired = unspecified)
		then
		  (* take all existing values *)
		  List.append (match remaining with
				   [] ->
				     List.map (fun f ->
						 [key, (value f)])
				       files
				 | _ -> [])
		    (Wrlist.mapcan (fun d ->
				      List.map (fun attrs ->
						  (key, (value d))::attrs)
					(matching d remaining))
		       dirs)
		else
		  match find_val desired dirs with
		      None ->
			(match find_val desired files with
			     None -> []
			   | Some f -> [[key, (value f)]])
		    | Some d ->
			List.map (fun attrs ->
				    (key, (value d)):: attrs)
			  (matching d remaining)


let matching_attrs root attrs =
  assert_attrs attrs;
  matching root attrs


let matching_paths root attrs =
  List.map (path_for root)
    (matching_attrs root attrs)


let matching_path root attrs =
  let list = matching_attrs root attrs in
    try
      let full_attrs = Wrlist.get_singleton list in
	path_for root full_attrs
    with Not_found -> failwith (Wrutils.str "no match for: %s"
				  (attrs_str [] attrs))
    | Wrlist.Multiple -> failwith (Wrutils.str "multiple matches:\n%s"
				     (String.concat "\n"
					(List.map (attrs_str []) list)))


(* EOF *)
