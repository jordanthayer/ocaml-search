(* Logfn from ahutils, Allen Hubbe *)
(* base case log_fn:

   print string list to channel as one line separated by commas
*)
let log ch =
  fun opt_log _ ->
    output_string ch (String.concat "," opt_log);
    output_char ch '\n'
    (* flush ch *)


(* alternative base case log_fn and hdr_fn:

   ignore and do nothing
*)
let no_log _ _ = ()
let no_hdr = no_log


(* build on a log function:

   a_of_b - unwrap 'b node as 'a node
   b_log - prepend values for 'b node to a string list
   a_log_fn - log function for 'a nodes
   
   returns - log function for 'b nodes
   opt_log - values of later columns
   b - 'b node
*)
let make_log_fn a_of_b b_log a_log_fn =
  fun opt_log b ->
    a_log_fn (b_log opt_log b) (a_of_b b)


(* build on a header function:
   (* uses format of log_fn with unit instead of nodes *)

   b_hdr - prepend headers for 'b domain to a string list
   a_hdr_fn - header function for 'a domain

   returns - header function for 'b domain
   opt_hdr - headers of later columns
*)
let make_hdr_fn b_hdr a_hdr_fn =
  fun opt_hdr () ->
    a_hdr_fn (b_hdr opt_hdr ()) ()



