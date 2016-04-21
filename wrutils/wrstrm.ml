(* $Id: wrstream.ml,v 1.1 2003/11/18 14:15:11 ruml Exp $

   stream utility code

   streams are heavier than channels - they allow peeking.
   they can be made from channels, strings, or arbitrary generating functions.
*)


let empty_p strm =
  (* odd implementation, but shouldn't need to allocate memory *)
  try
    Stream.empty strm;
    true
  with Stream.Failure -> false


let rec eat_white strm =
  (** eats whitespace: space, tab, linefeed, formfeed, return *)
  match Stream.peek strm with
    Some c ->
      if Wrchar.white_p c
      then (Stream.junk strm;
	    eat_white strm)
      else ()
  | None -> ()


let rec eat_until e strm =
  (** removes elements from [strm] until something equal to [e] is next *)
  match Stream.peek strm with
    Some x ->
      if x = e
      then ()
      else (Stream.junk strm;
	    eat_until e strm)
  | None -> ()


let eat_through e strm =
  (** removes elements from [strm] until something equal to [e] is removed *)
  eat_until e strm;
  Stream.junk strm


let rest_string strm =
  let b = Buffer.create 10 in
    try
      while true do
	Buffer.add_char b (Stream.next strm)
      done;
      failwith "stream never ended?"
    with Stream.Failure ->
      Buffer.contents b


(******************* a generic lexer *****************)


(** like Genlex.make_lexer but doesn't have any hang-ups about
  so-called special characters.  Keywords and identifiers are any
  sequence of whitespace-delimited non-whitespace characters that aren't
  strings or character literals or integers or floats.  Skips nested
  comments.  *)


let rec eat_comment strm =
  (** assumes opening has been read.  handles nested comments *)
  let finished = ref false in
    while (not !finished) do
      try
	match Stream.next strm with
	  '*' -> (match Stream.next strm with
		    ')' -> finished := true
		  | _ -> ())
	| '(' -> (match Stream.next strm with
		    '*' -> eat_comment strm
		  | _ -> ())
	| _ -> ()
	with Stream.Failure -> raise Parsing.Parse_error
    done


let read_char_esc strm =
  (** assumes open slash has been read *)
  try
    let c = Stream.next strm in
      match c with
	'0'..'9' ->
	  let second = (Stream.next strm) in
	    char_of_int (((Wrchar.int c) * 100) +
			 ((Wrchar.int second) * 10) +
			 (Wrchar.int (Stream.next strm)))
      | 'n' -> '\n'
      | 'r' -> '\r'
      | 't' -> '\t'
      | 'b' -> '\b'
      | '\\' | '"' | '\'' -> c
      | _ -> raise Parsing.Parse_error
      with Stream.Failure -> raise Parsing.Parse_error


let read_char strm =
  (** assumes opening quote has been read *)
  try
    match Stream.next strm with
      '\\' -> read_char_esc strm
    | c -> c
    with Stream.Failure -> raise Parsing.Parse_error


let read_string strm =
  (** assumes opening double quote has been read *)
  let b = Buffer.create 80
  and finished = ref false in
    while (not !finished) do
      try
	match Stream.next strm with
	  '\\' -> Buffer.add_char b (read_char_esc strm)
	| '"' -> finished := true
	| c -> Buffer.add_char b c
	with Stream.Failure -> raise Parsing.Parse_error
    done;
    Buffer.contents b


let test_read_string () =
  let s = Wrstr.random 10 in
    Wrutils.pr " string: \"%s\"\n" s;
    let escaped = String.escaped s in
    Wrutils.pr "escaped: \"%s\"\n" escaped;
    let s2 = read_string (Stream.of_string (escaped ^ "\"")) in
      Wrutils.pr "    got: \"%s\"\n" s2;
      if s = s2 then
	Wrutils.pr "matches.\n"
      else
	Wrutils.pr "FAILED!!!.\n"


let read_num ?(neg = false) ?(leading = '0') strm =
  (** [(+-)](0-9)*[.(0-9)*][(eE)[(-+)](0-9)*] *)
(*  let negative = (leading == '-')
  and accum = ref 0 in
    if leading == '+' then
      ()
    else if Wrchar.is_digit leading then

*)
  failwith "numbers not implemented yet"


let read_tok c strm =
  (** read token, starting with [c] *)
  let b = Buffer.create 80
  and finished = ref false in
    Buffer.add_char b c;
    while (not !finished) do
      try
	let c = Stream.next strm in
	  if Wrchar.white_p c
	  then finished := true
	  else Buffer.add_char b c
      with Stream.Failure -> finished := true
    done;
    Buffer.contents b


let make_lexer keys strm =
  (** returns a Genlex.token stream *)
  let ident_or_key char =
    let s = read_tok char strm in
      if List.mem s keys
      then Genlex.Kwd s
      else Genlex.Ident s
  in
  let rec next_token i =
    (** returns an optional Genlex.token *)
    eat_white strm;
    try
      let c = Stream.next strm in
	match c with
	  '\'' -> Some (Genlex.Char (read_char strm))
	| '"' -> Some (Genlex.String (read_string strm))
	| '0'..'9' as c -> Some (read_num ~leading:c strm)
	| '(' -> (match Stream.peek strm with
		    Some '*' ->
		      eat_comment strm;
		      next_token i
		  | _ -> Some (ident_or_key '('))
	| '-' -> Some (match Stream.peek strm with
			 Some x when (match x with
					'0'..'9' -> true
				      | _ -> false) ->
			   read_num ~neg:true strm
		       | _ -> ident_or_key '-')
	| c -> Some (ident_or_key c)
	with Stream.Failure -> None
  in
    Stream.from next_token


(******************* other functions ********************)


let parse_string strm =
  (** given a char stream, eats whitespace then reads an OCaml
    formatted string *)
  eat_white strm;
  assert ((Stream.peek strm) = Some '"');
  Stream.junk strm;
  read_string strm


let parse_strings strm =
  (** given a char stream, returns list of OCaml strings *)
  let rec get_more () =
    eat_white strm;
    if empty_p strm then
      []
    else
      let this = parse_string strm in
	this::(get_more ())
  in
    get_more ()


let rec all_string_tokens strm =
  (** returns a list of all and any strings available at the front of
      the given Genlex.token strm *)
  match Stream.peek strm with
      Some (Genlex.String x) ->
	Stream.junk strm;
	x::(all_string_tokens strm)
    | _ -> []


(*
  let rec parse_strings strm = parser
(** returns a list of all and any strings available at the front of strm *)
    [< 'Genlex.String s ; more = parse_strings strm >] -> s::more
  | [< >] -> []
*)


(* EOF *)
