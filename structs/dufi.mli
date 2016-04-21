(* $Id: dufi.mli,v 1.1 2006/08/12 14:11:55 ruml Exp ruml $

   destructive union find for dense sets of small integers
*)


type t
  

val make : int -> t
  (** specify num elements (possible elements thus 0 - (num-1)).  starts
    empty. *)
  
val init : t -> int -> unit
  (** adds a single-element set.  No exception raised if you re-init
    something.  In fact, if a set of elements are re-inited at the same time,
    they are reset in such a way that future queries on just those elements
    will see a consistent structure.  So the data structure can be
    re-used. *)

val same_p : t -> int -> int -> bool
  (** asks if two elements are in the same set.  Sometimes raise
      Not_found if queried about a non-added object, but no guarantee,
      so don't ask about something you didn't init! *)

val join : t -> int -> int -> unit
  (** joins the sets that the two elements are in. Sometimes raise
      Not_found if queried about a non-added object, but no guarantee,
      so don't ask about something you didn't init! *)


(* EOF *)
