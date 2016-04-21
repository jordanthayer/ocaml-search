(* $Id: backedht.ml,v 1.1 2003/09/26 23:39:02 ruml Exp ruml $

   hashtables that write to disk.
*)


type ('a, 'b) t = {
  ht : ('a, 'b) Hashtbl.t;
  outch : out_channel;
  locked : bool;
  write : out_channel -> 'a -> 'b -> unit;
  read : in_channel -> ('a * 'b);
}


let read ht read_pair path =
  (** modifies [ht] and returns number of entries in file.  last
    occurence of entry with a particular key obliterates any earlier
    entries. *)
  let pairs_in_file = ref 0 in
    Wrio.with_infile path
      (fun inch ->
	 set_binary_mode_in inch true;
	 try
	   while true do
	     let k, v = read_pair inch in
	       incr pairs_in_file;
	       Hashtbl.replace ht k v
	   done
	 with End_of_file -> ());
    !pairs_in_file


let write ht write_pair path =
  (** write [ht] to [path], destroying any exiting file *)
  Wrio.with_outfile path
    (fun outch ->
       set_binary_mode_out outch true;
       Hashtbl.iter (write_pair outch) ht)


let max_duplication = 1.65


let create read_pair write_pair path create_if_not_exist =
  let ht = Hashtbl.create 1024 in
    (if Sys.file_exists path then
       let pairs = read ht read_pair path in
       let duplication = Math.div pairs (Hashtbl.length ht) in
	 if duplication > max_duplication then
	   (Verb.pr 4 "Duplication is %.2f (%d pairs) - rewriting backedht...%!"
	      duplication pairs;
	    write ht write_pair path;
	    Verb.pr 4 "done.\n%!"));
    let outch = open_out_gen ([ Open_wronly; Open_append; Open_binary] @
			      (if create_if_not_exist
			       then [Open_creat] else []))
		  0o666 (* (Wrsys.curr_perms ()) *)
		  path
    in
      { ht = ht;
	outch = outch;
	locked = false;
	write = write_pair;
	read = read_pair; }


let with_writable f read_pair write_pair path
  create_if_not_exist =
  let t = create read_pair write_pair path create_if_not_exist in
    Wrutils.unwind_protect f t
      close_out t.outch


let open_read_only read_pair path =
  let fail _ _ = failwith
    "Backed_ht.open_read_only: tried to write to read-only backedht!" in
  let ht = Hashtbl.create 1024 in
    ignore (read ht read_pair path);
    { ht = ht;
      outch = stderr;
      locked = true;
      write = fail;
      read = read_pair; }


let compact read_pair write_pair path =
 with_writable (fun _ -> ()) read_pair write_pair path false


let find t key =
  Hashtbl.find t.ht key


let replace t key value =
  if t.locked then
    failwith "Backed_ht.replace: opened read_only"
  else
    (Hashtbl.replace t.ht key value;
     t.write t.outch key value;
     flush t.outch)


let iter f t =
  Hashtbl.iter f t.ht



(* EOF *)
