(* $Id: rdb.mli,v 1.1 2003/07/02 20:51:05 ruml Exp ruml $

   results data base

   keys and vals can't contain : or / and values can't start with "KEY="
*)


type attrs = (string * string) list


(****** files ******)


val path_for : string -> attrs -> string
  (** given a root directory and unnormalized attrs, returns a
    pathname.  works even if no file with those attrs exists. *)


val attrs_for : string -> attrs
  (** [attrs_for path] gets the attributes that specify [path]. *)


val matching_attrs : string -> attrs -> attrs list
  (** given a root directory and unnormalized attrs, returns a list of
    the attrs of all extant files that match the given attrs *)


val matching_paths : string -> attrs -> string list
  (** like [matching_attrs] but returns paths *)


val matching_path : string -> attrs -> string
  (** ensure only one return result.  can raise Not_found or failure
    with multiple matches *)


(****** attrs *******)


val filter_attrs : string list -> attrs -> attrs
  (** copy without the given keys *)


val merge_attrs : attrs -> attrs -> attrs
  (** given defaults and attrs, appends attrs to those defaults not
    already present *)


val override_attrs : attrs -> attrs -> attrs
  (** given [a] and [b], appends those attrs in [b] that aren't in [a] to
    [a] *)


val attrs_str : string list -> attrs -> string
  (** given a list of keys to ignore, returns a string representation
    of the values of all the other keys *)

val human_str : attrs -> string
  (** [human_str attrs] gets a human readable string of the attributes. *)


(* EOF *)
