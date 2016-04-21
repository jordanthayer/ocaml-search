(**
   A simple stand-alone program that will build a path for the given
   set of keys.
*)


let usage () =
  (* Print the usage string. *)
  Verb.pe Verb.always 
    "Usage:\nrdb_get_path <root> <key=value> [<key=value>...]\n";
  exit 1


let rec get_attrs ?(accum=[]) ary i =
  (* Get the key=value attributes from the given argument array. *)
  if Array.length ary > i then
    match Str.split (Str.regexp "=") ary.(i) with
      | key::vl::[] -> get_attrs ~accum:((key, vl)::accum) ary (i + 1)
      | _ -> failwith ("Bad arugment: " ^ ary.(i))
  else
    List.rev accum


let main () =
  if (Array.length Sys.argv) < 2 then
    usage ()
  else begin
    let root = Sys.argv.(1) and attrs = get_attrs Sys.argv 2 in
    let path = Rdb.path_for root attrs in
      Verb.pe Verb.always "path: %s\n" path
  end


let _ = main ()
