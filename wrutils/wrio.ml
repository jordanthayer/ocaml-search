(* $Id: wrio.ml,v 1.4 2007/12/19 16:31:57 ruml Exp ruml $

   I/O functions on channels


   Note that OCaml has channels, streams, formatters, and lexbufs.

   See also wrfilename, wrstream, wrformat, wrlexing.
*)



(****** opening files ***)


let with_infile name f =
  let s = open_in name in
    Wrutils.unwind_protect f s
      close_in s


let with_outfile name f =
  (** truncates and writes over previous file if one exists *)
  let s = open_out name in
    Wrutils.unwind_protect f s
      close_out s


let with_outfile_append name f =
  (** truncates and writes over previous file if one exists *)
  let s = open_out_gen [Open_append; Open_creat] 0o666 name in
    Wrutils.unwind_protect f s
      close_out s


(**** input from a user ****)


let pause () =
  print_string "Press enter to continue: ";
  flush_all ();
  ignore (read_line ())


let rec user_select prompt names =
  (** returns an optional index into [names] *)
  flush_all ();
  Wrutils.pr "%s\n" prompt;
  Wrlist.iteri (fun i n -> Wrutils.pr "   %d: %s\n" i n)
    names;
  Wrutils.pr "Enter the number of your choice, or type return to quit: %!";
  let r = String.lowercase (Wrstr.trim_white (read_line ())) in
    if r = "" then
      None
    else
      let n = int_of_string r in
	if n < List.length names then
	  Some n
	else
	  (Wrutils.pr "Sorry, your selection doesn't seem to be valid!\n\n";
	   user_select prompt names)


let user_select_elt prompt key items =
  (** returns an optional element from list of [items].  [key] provides
    printed representation of item.  [prompt] should be something like
    "Please select a widget:". *)
  match user_select prompt (List.map key items) with
    Some n -> Some (List.nth items n)
  | None -> None


(************* other input ************)


let non_comment_line ?(comment = '#') ch =
  (** returns the first non-whitespace line from [ch] (without the
    newline), skipping lines starting with the [comment] char (defaults
    to '#').  Can raise End_of_file.  *)
  let line = ref (input_line ch) in
    while (try
	     !line.[Wrstr.first_nonwhite !line] == comment
	   with Not_found -> true) do
      line := input_line ch
    done;
    !line


let input_lines ic =
  (** returns a list of lines. modified from a posting by Christian
    Schaller *)
  let lines = ref [] in
    try
      while true do
	Wrutils.push (input_line ic) lines
      done;
      failwith "exited infinite loop without an exception?"
    with End_of_file -> List.rev !lines


let rec after_white ch =
  (** eats whitespace and returns next character *)
  let c = input_char ch in
    if Wrchar.white_p c then
      after_white ch
    else
      c

let read_token ch =
  (** reads a whitespace delimited sequence of non-whitespace
      characters and returns it.  will also eat all whitespace before and
      one whitespace after the token. *)
  (* scanf allocates scanning buffer that reads ahead a lot!
     Scanf.fscanf ch " %s " Wrutils.identity *)
  let b = Buffer.create 10 in
  let rec eat_more () =
    let c = input_char ch in
      if not (Wrchar.white_p c) then
	(Buffer.add_char b c;
	 eat_more ())
  in
    (try
       Buffer.add_char b (after_white ch);
       eat_more ();
     with End_of_file -> ());
    Buffer.contents b


let input_int ch =
  (* scanf allocates scanning buffer that reads ahead a lot!
     Scanf.fscanf ch " %d" Wrutils.identity *)
  int_of_string (read_token ch)


let input_float ch =
  (* scanf allocates scanning buffer that reads ahead a lot!
     Scanf.fscanf ch " %f" Wrutils.identity *)
  float_of_string (read_token ch)


let read_ints ch =
  (** returns a list of ints parsed from the next line of [ch].  To
      get arrays of ints and floats, see wrarray.ml *)
  Wrstr.parse_ints (input_line ch)


let undos lines =
  (** removes Windows line endings from a list of strings *)
  List.map (fun l ->
	      if (Wrstr.ends_with '\013' l)
	      then Wrstr.chop l
	      else l)
    lines


let read_escaped_char ch =
  (** assumes open slash has been read *)
  let c = input_char ch in
    match c with
      '0'..'9' ->
	let second = input_char ch in
	  char_of_int (((Wrchar.int c) * 100) +
		       ((Wrchar.int second) * 10) +
		       (Wrchar.int (input_char ch)))
    | 'n' -> '\n'
    | 'r' -> '\r'
    | 't' -> '\t'
    | 'b' -> '\b'
    | '\\' | '"' | '\'' -> c
    | _ -> failwith "bad escaped character sequence"


let read_string ch =
  (** reads a doublequote-delimited string.  eats preceeding whitespace *)
  assert ((after_white ch) = '"');
  let b = Buffer.create 80
  and finished = ref false in
    while (not !finished) do
      match input_char ch with
	'\\' -> Buffer.add_char b (read_escaped_char ch)
      | '"' -> finished := true
      | c -> Buffer.add_char b c
    done;
    Buffer.contents b


(**** input from files ****)


let map_file f path =
  (** calls [f] repeatedly on an input_channel from [path] until
    End_of_file is raised.  Returns a list of the results. *)
  with_infile path
    (fun ch ->
       let accum = ref [] in
	 try
	   while true do
	     Wrutils.push (f ch) accum
	   done;
	   failwith "the end is nigh!"
	 with End_of_file -> List.rev !accum)


let with_file_lines filename f =
  (** calls [f] repeatedly on lines of [filename] (without newline
    char), returning unit *)
  with_infile filename
    (fun s ->
       try
	 while true do
	   f (input_line s)
	 done
       with End_of_file -> ())


let with_non_comment_file_lines filename f =
  (** calls [f] repeatedly on lines of [filename] (without newline
    char), returning unit *)
  with_infile filename
    (fun s ->
       try
	 while true do
	   f (non_comment_line s)
	 done
       with End_of_file -> ())



let with_file_line filename f line =
  (** calls [f] repeatedly on lines of [filename] (without newline
    char), returning unit *)
  let index = ref 0 in
  with_infile filename
    (fun s ->
       try
	 while !index < line do
	   (ignore (input_line s);
	    index := !index +1)
	 done;
	 f (input_line s)
       with End_of_file -> ())



let map_file_lines f filename =
  let accum = ref [] in
    with_file_lines filename (fun l -> accum := (f l)::!accum);
    List.rev !accum


let file_lines filename =
  map_file_lines Fn.identity filename


let file_contents filename =
  (** returns the contents of [filename] as a string *)
  let inbuflen = 4096 in
    with_infile filename
      (fun ch ->
	 let accum = Buffer.create inbuflen
	 and inbuf = String.create inbuflen in
	   try
	     while true do
	       match input ch inbuf 0 inbuflen with
		 0 -> raise End_of_file
	       | n -> Buffer.add_substring accum inbuf 0 n
	     done;
	     failwith "impossible to get here"
	   with End_of_file -> Buffer.contents accum)


let cat_file filename ch =
  (** prints contents of [filename] to [ch] *)
  let buflen = 4096 in
    with_infile filename (fun s ->
			    let buf = String.create buflen in
			      try
				while true do
				  match input s buf 0 buflen with
				    0 -> raise End_of_file
				  | n -> output ch buf 0 n
				done
			      with End_of_file -> ())


(*** generic input-like functions ***)


let take_through src_func target =
  (** calls [src_func] until suffix matches elements in [target] array
    (according to =) *)
  (* does not restart search from end of match failure, but rather
     from the position after the start of the match *)
  let len = Array.length target in
    (* front of buf is current match hypothesis *)
  let buf = Fixedq.init len (fun _ -> src_func ())
  and pos = ref 0 in
    while !pos != len do
      if (Fixedq.get buf !pos) = target.(!pos)
      then incr pos
      else (pos := 0;
	    Fixedq.shift buf (src_func ()))
    done


let skip_through_str ch text =
  (** throws chars away *)
  (* see also echo_through, below *)
  take_through (fun () -> input_char ch) (Wrstr.to_array text)


let skip_char ch =
  ignore (input_char ch)


let skip_line ch =
  while ((input_char ch) <>'\n') do
    ()
  done


let accum_through_str ch buf text =
  (** modifies [buf] *)
  take_through (fun () ->
		  let c = input_char ch in
		    Buffer.add_char buf c;
		    c)
    (Wrstr.to_array text)


let read_through_str ch text =
  (** returns a string *)
  let buf = Buffer.create 64 in
    accum_through_str ch buf text;
    Buffer.contents buf


let take_through_matching src_func left right =
  (** calls [src_func] until a [right] is returned (tested via =).
    absorbs nested pairs of [left] and [right].  Note that we are
    assuming that the first [left] has already been seen. *)
  let level = ref 1 in
    while !level != 0 do
      let x = src_func () in
	if x = right
	then decr level
	else if x = left
	then incr level
    done


let accum_through_matching ch buf left right =
  (** returns unit. modifies [buf] *)
  take_through_matching (fun () ->
			   let c = input_char ch in
			     Buffer.add_char buf c;
			     c)
    left right


let read_through_matching ch left right =
  (** returns a string *)
  let buf = Buffer.create 64 in
    accum_through_matching ch buf left right;
    Buffer.contents buf


(**** output ****)


let fflushf outch =
  Printf.kprintf (fun s -> output_string outch s ; flush outch ; "")

let flushf fmt =
  fflushf stdout fmt


let output_line ch str =
  (** symmetric with input_line ch str.  doesn't flush *)
  output_string ch str;
  output_char ch '\n'


let output_int ch i =
  output_string ch (string_of_int i)


let output_float ch f =
  (* allocates a string but the output looks nicer *)
  let s = string_of_float f in
    output_string ch s;
    if (Wrstr.ends_with '.' s)
      (* want to retain compatibility with scanf *)
    then output_char ch '0'


let echo_through text in_ch out_ch =
  (** can raise End_of_file *)
  take_through (fun () ->
		  let ch = input_char in_ch in
		    output_char out_ch ch;
		    ch)
    (Wrstr.to_array text)


let test_ett () =
  let s = open_in "/tilde/ruml/.cshrc-personal" in
    echo_through "path" s stdout;
    close_in s


let echo inch outch =
  (** reads from inch, writes to outch.  returns unit at eof *)
  try
    let buflen = 4096 in
    let buf = String.create buflen in
      while (true) do
	match input inch buf 0 buflen with
	  0 -> raise End_of_file
	| n ->
	    output outch buf 0 n;
	    flush outch
    done
  with End_of_file -> ()


let with_output_to_str f =
  (** calls [f] on an output channel.  everything written is returned
      as a string. *)
  Wrfname.with_temp_file (fun path ->
			    with_outfile path f;
			    file_contents path)


(* EOF *)
