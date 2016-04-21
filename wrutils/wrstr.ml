(* $Id: wrstr.ml,v 1.2 2003/10/28 18:15:16 ruml Exp ruml $

   string utility code
*)


(********* constructing *********)


let init n f =
  let s = String.create n in
    for i = 0 to n-1 do
      s.[i] <- f i
    done;
    s


let concat_prefix prefix = function
    (** given a prefix and a list of strings, returns the
      concatenation of the strings where each one is preceeded by the
      prefix *)
    [] -> ""
  | strings -> prefix ^ (String.concat prefix strings)


(* see also Wrlist.sprint_list *)


(*********** mapping and iteration ************)


let map f s =
  let n = String.length s in
  let ns = String.create n in
    for i = 0 to (n-1) do
      ns.[i] <- f s.[i]
    done;
    ns


let mapi f s =
  let n = String.length s in
  let ns = String.create n in
    for i = 0 to (n-1) do
      ns.[i] <- f i s.[i]
    done;
    ns


let iteri f s =
  let n = String.length s in
    for i = 0 to (n-1) do
      f i s.[i]
    done


let iter2 f a b =
  let n = String.length a in
    if (String.length b) <> n then failwith "Wrstr.iter2: lengths differ";
    for i = 0 to (n-1) do
      f a.[i] b.[i]
    done


let fold f init s =
  let n = String.length s in
  let rec aux prev i =
    if (i > n)
    then prev
    else aux (f prev s.[i]) (i+1)
  in
    aux init 0


exception Fail

let for_all f s =
  try
    String.iter (fun c ->
		   if not (f c)
		   then raise Fail)
      s;
    true
  with Fail -> false


(********** conversion ************)


let of_char c =
  String.make 1 c


let of_list chars =
  (** given a list of characters *)
  let s = String.create (List.length chars) in
    Wrlist.iteri (fun i c ->
		    s.[i] <- c)
      chars;
    s


let to_array s =
  Array.init (String.length s) (fun i -> s.[i])


let to_list s =
  (** returns a list of characters *)
  let l = ref [] in
    String.iter (fun c ->
		   l := c::!l)
      s;
    List.rev !l


let to_str_list s =
  (** returns list of one-element strings *)
  List.map of_char (to_list s)


(************** testing ************)


let find_index p s =
  (** returns index of first char in [s] that satisfies [p] *)
  let len = String.length s in
    if len == 0
    then raise Not_found
    else
      let i = ref 0 in
	while not (p s.[!i]) do
	  incr i;
	  if !i == len
	  then raise Not_found
	done;
	!i


let find_rindex p s =
  (** returns index of last char in [s] that satisfies [p] *)
  let len = String.length s in
    if len == 0
    then raise Not_found
    else
      let i = ref (len - 1) in
	while not (p s.[!i]) do
	  decr i;
	  if !i == -1
	  then raise Not_found
	done;
	!i


exception Found

let contains substr str =
  (** true iff the second argument contains the first argument as a
    substring *)
  let len = String.length str
  and sublen = String.length substr in
  let max = len - sublen
  and maxsub = sublen - 1 in
    try
      for start = 0 to max do
	try
	  for pos = 0 to maxsub do
	    if not (str.[start + pos] == substr.[pos])
	    then raise Fail
	  done;
	  raise Found
	with Fail -> ()
      done;
      false
    with Found -> true


let matches_from s1 s1start s2 =
  (** true iff s1 starting at i matches all of s2 *)
  ((String.length s1) >= (String.length s2)) &&
  (try
     iteri (fun i c ->
	      if not (c == s1.[s1start + i])
	      then raise Fail)
       s2;
     true
   with Fail -> false)


let matches_regexp exp str =
  (** true iff [str] matches [exp], which is in Emacs syntax, somewhere *)
  try
    ignore (Str.search_forward (Str.regexp exp) str 0);
    true
  with Not_found -> false


let starts_as prefix s =
  ((String.length s) >= (String.length prefix)) &&
  (try
     iteri (fun i c ->
	      if not (s.[i] == c)
	      then raise Fail)
       prefix;
     true
   with Fail -> false)


let ends_as suffix s =
  let len = String.length s
  and len2 = String.length suffix in
    (len >= len2) &&
    (let offset = len - len2 in
       try
	 iteri (fun i c ->
		  if not (s.[i + offset] == c)
		  then raise Fail)
	   suffix;
	 true
       with Fail -> false)


let starts_with ch s =
  ((String.length s) > 0) && (s.[0] == ch)


let ends_with ch s =
  let len = String.length s in
    (len > 0) && (s.[len - 1] == ch)


(************* modifying **************)


let firstn n s =
  String.sub s 0 (min n (String.length s))


let chop ?(n = 1) str =
  (** remove the last N (defaults to 1) chars from STR *)
  String.sub str 0 ((String.length str) - n)


let peel ?(n = 1) str =
  (** remove the first N (defaults to 1) chars from STR *)
  String.sub str n ((String.length str) - n)


let peel_and_chop ?(n = 1) ?(m = 1) str =
  (** [str] without the first [n] and last [m] characters (default to 1) *)
  String.sub str n ((String.length str) - (n + m))


let peel_prefix prefix str =
  (** removes the first part of [str] with the same length as
    [prefix].  Doesn't check that the prefix actually matches! *)
  peel ~n:(String.length prefix) str


let pad_right s n ch =
  let r = String.create n
  and m = String.length s in
    if m > n then failwith "pad_right: string argument already too long!";
    String.blit s 0 r 0 m;
    String.fill r m (n - m) ch;
    r


let whitespace_regexp = Str.regexp "[ \t\n\012\r]+"
let nonwhite_regexp = Str.regexp  "[^ \t\n\012\r]"


let first_white s =
  (** returns index *)
  Str.search_forward whitespace_regexp s 0

let first_nonwhite s =
  (** returns index *)
  Str.search_forward nonwhite_regexp s 0


let trim_white s =
  (** trims both ends of the string [s] *)
  try
    let max = (String.length s) - 1
    and first = first_nonwhite s in
    let last = Str.search_backward nonwhite_regexp s max in
      if ((first != 0) || (last != max))
      then String.sub s first ((last - first) + 1)
      else s
  with Not_found -> s


let trim_white2 s =
  (** another implementation? *)
  let first = find_index Wrchar.non_white_p s
  and last = find_rindex Wrchar.non_white_p s
  and len = String.length s in
    if ((first != 0) || (last != (len - 1)))
    then String.sub s first ((last - first) + 1)
    else s


let split_white line =
  (** breaks line at whitespace (any amount).  whitespace at start or
    end is ignored *)
  Str.split whitespace_regexp line


let split_bag delimiters line =
  (** splits [line] on any char in [delimiters] string.  delimiters at start and end are NOT ignored *)
  let regexp = Str.regexp (String.concat "|" (to_str_list delimiters)) in
    Str.split_delim regexp line


let uppercase_first str =
  (** [uppercase_first str] gets a copy of the string with the first
      letter in Uppercase. *)
  let res = String.copy str in
    res.[0] <- Char.uppercase res.[0];
    res


(**** could form the basis for a REMOVE_BAG function
      let norm s =
(* remove / or : *)
  let chars = ref [] in
    String.iter (function
		     '/' | ':' -> ()
		   | c -> chars := c::!chars)
      s;
    let len = List.length chars in
    let s = String.create len in
      Wrlist.iteri (fun i c ->
		      s.[(len - 1) - i] <- c)
	!chars;
      s


let normalize = function
    [] -> []
  | h::t -> (norm h)::(normalize t)
******)


let replace_strings s list =
  (** global search and replace using the (string, replacement) pairs
    in [list].  Not general regexps, just plain strings. Replacements
    are done sequentially - later targets can match the results of
    previous substitutions. *)
  List.fold_left (fun s (target, rep) ->
		    Str.global_substitute (Str.regexp_string target)
		    (fun _ -> rep) s)
    s list


let unescaped s =
  (** given a string which might have ocaml escape sequences in it,
    returns the intended string.  Intended to be the inverse of
    String.escaped. *)
  (* see wrstream string parsing?  and Wrio.read_string *)
  Wrutils.write_this ()


let format ch s =
  output_string ch (String.escaped s)


(************* parsing ***********)


let next_token s from =
  (** starting at [from] in string [s], return the next chunk of
    non-whitespace and the index of the start of the next token (which
    might be the index just beyond the end of the string).  can raise
    Not_found *)
  let from = Str.search_forward nonwhite_regexp s from in
    try
      let e = Str.search_forward whitespace_regexp s from in
	(String.sub s from (e - from)), (Str.match_end ())
    with Not_found ->
      let e = String.length s in
	(String.sub s from (e - from)), e


let parse_ints s =
  (** returns a list *)
  List.map int_of_string (split_white s)


let better_float_of_string str =
  float_of_string (Str.global_replace (Str.regexp "\"") "" str)

let parse_floats s =
  (** returns a list *)
  List.map better_float_of_string (split_white s)


(***** stream-like behavior ********)


type strm = {
  s : string;
  mutable i : int;
}


let strm str =
  { s = str; i = 0; }


let next strm =
  (** will raise Not_found when string has been exhausted *)
  let t, i = next_token strm.s strm.i in
    strm.i <- i;
    t


(************ unique strings **************)


let make_gensymmer =
  let gensymmers = ref [] in
    fun prefix ->
      (** given a PREFIX, returns a function that generates unique
	strings by appending an integer.  Will return the same unique
	generator for a given prefix *)
      (Wrlist.assoc_default prefix gensymmers
	 (fun () ->
	    let counter = ref (-1) in
	      fun () ->
		incr counter ;
		prefix ^ (string_of_int !counter)))


let suffix_str ?(join = "-") base =
  (make_gensymmer (base ^ join)) ()


let random n =
  init n (fun i -> Wrchar.random ())


(************* pairs of strings *****************)


let interpair_ch = ';'
let interpair_str = of_char interpair_ch
let intrapair_ch = '@'
let intrapair_str = of_char intrapair_ch


let encode_pairs attrs =
  let check s =
    assert (not (String.contains s interpair_ch));
    assert (not (String.contains s intrapair_ch))
  in
    List.iter (fun (k,v) -> check k; check v) attrs;
    String.concat interpair_str (List.map (fun (k,v) -> k ^ intrapair_str ^ v)
			 attrs)


let decode_pairs str =
  List.map (fun s ->
	      match split_bag intrapair_str s with
		[k; v] -> k,v
	      | _ -> failwith "bad string to decode")
    (split_bag interpair_str str)


(* EOF *)
