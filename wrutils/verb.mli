(* $Id: verb.mli,v 1.2 2003/11/20 17:39:57 ruml Exp $
   
   printing debugging messages with varying verbosity

   verbosity is 1 through 5, with 5 being most verbose.

   A given message will be printed when curr verbosity is >= the
   message verbosity.  So verbosity 5 prints all messages and a 1
   message is very important and is always printed.
*)

val always : int
val often : int
val optional : int
val toplvl : int
val debug : int
val never : int

  
val with_level : int -> (unit -> 'a) -> 'a
  (** evaluates the function with the given verbosity level in force *)

val pf : int -> out_channel -> ('a, unit, string, unit) format4 -> 'a
  (** Prints to the given out_channel iff the verbosity warrants. The
    returned 'a is the closure to be applied to the arguments specified
    in the format string.  The full application returns unit. *)

val pr : int -> ('a, unit, string, unit) format4 -> 'a
  (** Like [pf] but prints to stdout *)
  
val pe : int -> ('a, unit, string, unit) format4 -> 'a
  (** Like [pf] but prints to stderr *)

val echo : int -> out_channel -> ('a, unit, string, unit) format4 -> 'a
  (** Prints to the given out_channel, and also to stdout iff the
      verbosity warrants. The returned 'a is the closure to be applied
      to the arguments specified in the format string.  The full
      application returns unit. *)
  
val force : int -> unit Lazy.t -> unit
  (** evaluate iff the verbosity warrants *)
  
val level : int -> bool
  (** would verbosity justify printing at this level?  *)


  
val set_default : int -> unit
  (** Meant for use from the toplevel.  Please don't call this from an
    executable.  The default level is 3.  *)


(* EOF *)
