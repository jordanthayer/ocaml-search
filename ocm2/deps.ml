(** Handling of ocamldep dependency files.

    @author eaburns
    @since 2010-08-24
*)

open Printf
open Scanf

(** {1 Parsing dependency files} *)

let isspace = function
  | ' ' | '\t' | '\r' | '\n' -> true
  | _ -> false


(** [eat_space_not_newline inch] eats all the whitespace that is not
    a newline. *)
let eat_space_not_newline inch =
  let rec do_eat inch = function
    | c when (isspace c) && c <> '\n' -> do_eat inch (input_char inch)
    | c -> c
  in do_eat inch (input_char inch)


(** [next_token inch] gets the next token from the input file. *)
let next_token inch =
  let rec handle_char b inch = function
    | '\n' -> Buffer.contents b, true
    | c when isspace c ->
	Buffer.contents b, false
    | c ->
	Buffer.add_char b c;
	handle_char b inch (input_char inch)
  in
  let b = Buffer.create 10 in
    try handle_char b inch (eat_space_not_newline inch)
    with End_of_file ->
      if (Buffer.length b) = 0
      then raise End_of_file
      else Buffer.contents b, false


(** [read_depends see_depend src inch] reads the dependency lines
    from the file and calls [see_depend] on each dependency. *)
let read_depends see_depend src inch =
  let rec handle_token escaped (t, nl) =
    let len = String.length t in
      if t = "\\"
      then handle_token true (next_token inch)
      else begin
	let escaped' = if len > 0 then false else escaped in
	  if len > 0 then see_depend src t;
	  if not nl || escaped' then handle_token escaped' (next_token inch)
      end
  in try handle_token false (next_token inch) with End_of_file -> ()


(** [read see_node see_depend inch] reads the dependency file and
    calls [see_depend] on each dependency. *)
let read see_node see_depend inch =
  let rec do_read see_depend inch =
    let t, nl = next_token inch in
    let len = String.length t in
      if t.[len - 1] <> ':' then failwith "Malformed dependency file";
      let src = String.sub t 0 (len - 1) in
	see_node src;
	if not nl then read_depends see_depend src inch;
	do_read see_depend inch
  in try do_read see_depend inch with End_of_file -> ()


(** [load see_node see_depend file] loads the given dependency
    file. *)
let load see_node see_depend file =
  let inch = open_in file in
    read see_node see_depend inch;
    close_in inch
