(** For verbose printing.

    This should be written with the expectation that it will be
    opened.

    @author eaburns
    @since 2010-05-26
*)

open Printf

(** The verbosity level for debugging.

    Convension:  The following things are [verb_optional]:

    - Things that no one really cares about seeing unless there is a
    bug.
*)
let verb_debug = 3

(** The verbosity level for optional text.  This is turned on
    automatically when running from the top level.

    Convension:  The following things are [verb_optional]:

    - short messages that contain numbers in plot coordinates.
    - information about output formats.
*)
let verb_optional = 2

(** The verbosity level for normal printing.

    Convension: The following things are [verb_normal]:

    - short messages that contain data coordinates.
    - other short and useful messages to the user.
*)
let verb_normal = 1

(** This will *always* print. *)
let verb_critical = 0


module Verb_level : sig
  val set : int -> unit
  val get : unit -> int
end = struct
  (* This module exists to hide [verb_level] from the user so that
     they must set it through the [set] function. *)

  (** The current verbosity level. *)
  let verb_level =
    ref (if !Sys.interactive then verb_optional else verb_normal)

  let set l =
    if l < verb_critical
    then printf "Warning: Attempting to set verbosity less than critical\n";
    if l > verb_debug
    then printf "Warning: Attempting to set verbosity greater than debug\n";
    verb_level := max (min l verb_debug) verb_critical

  let get () = !verb_level
end

(** [vprintf lvl fmt] for verbosity printing *)
let vprintf lvl fmt =
  let dp s =
    if Verb_level.get () >= lvl then output_string stderr s; flush stderr
  in kprintf dp fmt


(** [verb_eval lvl f] evaluates the unit function [f] if the level is
    sufficient. *)
let verb_eval lvl f = if Verb_level.get () >= lvl then f ()
