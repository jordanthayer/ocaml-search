(** Functionality for using external memory based records.

    Currently this is an implementation of the procedure described on
    pg. 12 of "Linear-Time Disk-Based Implicit Graph Search", 2008,
    Richard Korf, JACM.

    @author eaburns
    @since 2010-06-01
*)


(* TODO:

   Change the interface so that iterating reads each externalized
   buffer in-turn and that if the object is sorted first then they
   will read in order.

   Don't copy the array when sorting an output buffer... instead write
   a sort that will sort an array up to a given fill.

   If the output_buffer is not empty at the time of the sort, it is
   flushed to disk then sorted.  It would save some I/O to just leave
   it in memory and work from there...

   If there is only a single externalized buffer just rename it
   instead of sorting.
*)


open Printf


(** {1 File handling} ****************************************)


let rec storage_file ?(prefix="external-memory-") ?(suffix="") root =
  (** [storage_file ?prefix ?suffix root] gets a new file for storage
      of records. *)
  let rnd = Random.int 1_000_000 in
  let fname = sprintf "%s/%s%d%s" root prefix rnd suffix in
    if Sys.file_exists fname
    then storage_file ~prefix ~suffix root
    else fname


(** {1 Low-level output buffers} ****************************************)

type 'record output_buffer = {
  (* A buffer used to hold records that are outgoing to the disk. *)

  out_size : int;
  (* The size of the array to allocate. *)

  mutable out_fill : int;
  (* The fill of the buffer. *)

  mutable out_entries : 'record array;
  (* The entries in the buffer. *)
}


let out_buffer_create size =
  (** [out_buffer_create size dummy] creates a new output buffer of
      the given size. *)
  {
    out_size = size;
    out_fill = 0;
    out_entries = [| |]
  }


let out_buffer_is_full buf =
  (** [out_buffer_is_full buf] tests if the output buffer is full. *)
  buf.out_entries <> [||] && buf.out_fill = (Array.length buf.out_entries)


let out_buffer_is_empty buf = buf.out_fill = 0
  (** [out_buffer_is_empty buf] tests if the output buffer is empty. *)


let out_buffer_clear buf = buf.out_fill <- 0
  (** [out_buffer_clear buf] clear the fill of the output buffer
      making it empty. *)


let out_buffer_add buf r =
  (** [out_buffer_add buf r] adds the given record to the buffer. *)
  if out_buffer_is_full buf
  then invalid_arg "External_memory.out_buffer_add: buffer is full";
  let fill = buf.out_fill in
    if buf.out_entries = [||]
    then begin
      assert (buf.out_fill = 0);
      buf.out_entries <- Array.create buf.out_size r;
      buf.out_fill <- 1;
    end else begin
      buf.out_entries.(fill) <- r;
      buf.out_fill <- fill + 1;
    end


let out_buffer_sort cmp buf =
  (** [out_buffer_sort cmp buf] sorts the entries in the buffer. *)
  let fill = buf.out_fill in
  let ents = Array.sub buf.out_entries 0 fill in
    Array.sort cmp ents;
    Array.blit ents 0 buf.out_entries 0 fill


let out_buffer_sort_and_flush ?(elim_dups=true) write cmp buf =
  (** [out_buffer_sort_and_flush ?elim_dups write cmp buf] sorts the
      buffer and flushes it to the given channel. *)
  assert (not (out_buffer_is_empty buf));
  let fill = buf.out_fill in
  let ents = buf.out_entries in
    out_buffer_sort cmp buf;
    let prev = ref ents.(0) in
      write ents.(0);
      for i = 1 to fill - 1 do
	let e = ents.(i) in
	  assert ((cmp !prev e) <= 0);
	  if not elim_dups || (cmp !prev e) < 0
	  then begin
	    write e;
	    prev := e;
	  end;
      done;
      buf.out_fill <- 0


(** {1 Low-level input buffers} ****************************************)


type 'record input_buffer = {
  (* A buffer used to hold nodes coming in from disk that must be
     sorted. *)

  in_size : int;
  (* The size of the input buffer. *)

  mutable in_entries : 'record array;
  (* The entries coming in from the channel. *)

  mutable in_fill : int;
  (* The number of entries in the [in_entries] array. *)

  mutable in_pos : int;
  (* The position of the next entry in the [in_entries] array. *)
}


let in_buffer_create size =
  (** [in_buffer_create size] creates a new input buffer. *)
  {
    in_size = size;
    in_entries = [| |];
    in_fill = 0;
    in_pos = 0;
  }

let in_buffer_is_empty buf =
  (** [in_buffer_is_empty buf] tests if the buffer is empty. *)
  buf.in_entries = [| |] || buf.in_pos >= buf.in_fill


let in_buffer_fill read buf =
  (** [in_buffer_fill read buf] fills the buffer by reading entries
      from the channel. *)
  if not (in_buffer_is_empty buf)
  then invalid_arg "External_memory.in_buffer_fill: buffer is not empty.";
  let size = buf.in_size in
  let next = ref 0 in
    begin
      try
	if buf.in_entries = [| |]
	then begin
	  buf.in_entries <- Array.create buf.in_size (read ());
	  next := 1;
	end;
	let ents = buf.in_entries in
	  while !next < size do
	    ents.(!next) <- read ();
	    incr next
	  done
      with End_of_file -> ()
    end;
    buf.in_fill <- !next;
    buf.in_pos <- 0


let in_buffer_next read buf =
  (** [in_buffer_next read buf] gets the next entry from the
      input buffer.  If the buffer is empty then this will raise
      End_of_file. *)
  if in_buffer_is_empty buf then in_buffer_fill read buf;
  if in_buffer_is_empty buf then raise End_of_file;
  let pos = buf.in_pos in
  let r = buf.in_entries.(pos) in
    buf.in_pos <- pos + 1;
    r


(** {1 High-level} ****************************************)

let iter_records ?(buff_size=8192) read f file =
  (** [iter_records ?buff_size read f file] iterates over each record
      in [file], calling [f] on each. *)
  let buf = in_buffer_create buff_size in
  let ichan = open_in file in
  let read () = read ichan in
    try while true do f (in_buffer_next read buf) done
    with End_of_file -> close_in ichan


let store_flush_clear ?(buf_size=8192) ?(elim_dups=true) write cmp root =
  (** [store_flush_clear ?buf_size ?elim_dups write cmp root] create a
      store function, a flush function, a clear function and a path
      list reference.

      The store function can be used to store reconds to external
      memory.  Each time the buffer fills, it is written to a single
      file on disk in the directory referenced by [root].  The
      contents of the file are internally sorted in ascending order
      according to [cmp] and may have duplicates eliminated (however
      not across files).

      The flush function flushes all remaining contents in the buffer
      to disk.  This should be used before accessing the files output
      by the store function to ensure that all data is on disk.

      The clear function clears the buffer and removes the files from
      the disk.

      The path list is the list of files containing the output
      records.

      If [elim_dups] is true then duplicates are eliminated in each
      individual file but not across multiple files. *)
  let buf = out_buffer_create buf_size in
  let files = ref [] in
  let write_buf () =
    let fname = storage_file root in
      Verb.pr Verb.debug "Flushing buffer to file %s\n%!" fname;
      Wrio.with_outfile fname
	(fun ochan ->
	   let write = write ochan in
	     out_buffer_sort_and_flush write cmp ~elim_dups buf);
      files := fname :: !files;
  in
  let store r =
    if out_buffer_is_full buf then write_buf ();
    out_buffer_add buf r
  and flush () =
    if not (out_buffer_is_empty buf) then write_buf ();
  and clear () =
    out_buffer_clear buf;
    List.iter Sys.remove !files;
    files := []
  in store, flush, clear, files


let heapify_next heap read close buf =
  (** [heapify_next heap read close buf] tries to get the next element
      from the buffer to put on the heap. *)
  try
    let r = in_buffer_next read buf in
      Dpq.insert heap (r, buf, read, close)
  with End_of_file -> close ()


let do_merge_sort elim_dups write cmp root init =
  (** [do_merge_sort elim_dups write cmp root init] performs
      a merge sort in the input buffer descriptors of type (fst :
      'record, buffer : 'record input_buffer, read : unit ->
      'record, close : unit -> unit).  The result is the name of the
      file containing the sorted records. *)
  let ofile = storage_file root in
  let ochan = open_out ofile in
  let before (a, _, _, _) (b, _, _, _) = (cmp a b) < 1 in
  let heap = Dpq.create_with before (List.hd init) in
    Verb.pr Verb.debug "Sorting to file %s\n%!" ofile;
    List.iter (Dpq.insert heap) init;

    let r, buf, read, close = Dpq.extract_first heap in
    let prev = ref r in
      write ochan r;
      heapify_next heap read close buf;
      while not (Dpq.empty_p heap) do
	let r, buf, read, close = Dpq.extract_first heap in
	  heapify_next heap read close buf;
	  assert ((cmp !prev r) <= 0);
	  if not elim_dups || (cmp !prev r) < 0
	  then begin
	    write ochan r;
	    prev := r;
	  end else assert ((cmp !prev r) = 0);
      done;
      close_out ochan;
      ofile


let rec merge_sort ?(buf_size=8192) ?(nbufs=0) ?(delete=true) ?(elim_dups=true)
    read write cmp root files =
  (** [merge_sort ?buf_size ?nbufs ?delete ?elim_dups read write cmp
      root files] performs an external merge sort on the list of
      files.  The result is a file containing the sorted records.

      [buf_size] is the size (in records) of each input buffer.

      [nbufs] is the number of input buffers to use at a time.  If
      [nbufs < 2] then all of the files are sorted in one pass.

      If [elim_dups] is true than duplicate records are eliminated.

      If [delete] is true than each file in [files] is deleted once it
      is finished being merged.  *)
  let nfiles = List.length files in
    Verb.pr Verb.debug "Sorting %d files\n%!" (List.length files);
    if nfiles = 1
    then List.hd files
    else begin
      let n = if nbufs < 2 then nfiles else nbufs in
      let to_sort, rest = Wrlist.prefix (min n nfiles) files in
      let bufs =
	List.map (fun f ->
		    let ichan = open_in f in
		    let read () = read ichan in
		    let close () =
		      close_in ichan;
		      if delete
		      then begin
			Verb.pr Verb.debug "Deleting file %s\n%!" f;
			Sys.remove f
		      end in
		    let buf = in_buffer_create buf_size
		    in in_buffer_next read buf, buf, read, close)
	  to_sort
      in
      let s = do_merge_sort elim_dups write cmp root bufs in
	if rest <> []
	then
	  merge_sort ~buf_size ~nbufs ~elim_dups ~delete
	    read write cmp root (s :: rest)
	else s
    end


(* Testing ****************************************)

let test_root = "/tmp"

let test0 () =
  Verb.set_default Verb.debug;
  let store, flush, clear, files =
    store_flush_clear ~buf_size:8 output_binary_int compare test_root
  in
    for i = 1 to 1000 do store (Random.int 10000) done;
    clear ();
    for i = 1 to 1000 do store i done;
    flush ();
    let f =
      merge_sort ~buf_size:8 ~nbufs:2
	input_binary_int output_binary_int compare test_root !files
    in
    let count = ref 1 in
      iter_records input_binary_int (fun d ->
				       assert (d = !count);
				       incr count;
				       Printf.printf "%d\n" d) f;
      Sys.remove f;
