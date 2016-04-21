(** $Id: stn.mli,v 1.2 2004/11/11 17:53:31 ruml Exp $
  
  simple temporal networks.  destructive operations, only arc
  consistency.  copying is O(#points).
*)


type t

type id

type edge_constraint


val zero : id
  (** id of the point representing a time of 0. *)

val dummy_point : id
  
val create : unit -> t
  (** given a base time and a precision, returns STN that contains only the
    zero point.  The base time should be similar in value to the constraints
    that will be posted against the zero point.  It is used internally when
    converting constraints to integers to keep values within the range of
    31-bit intergers.  (Integers are used internally rather than floating
    point to gain speed and avoid numerical errors.)  Precision should be the
    smallest difference worth preserving between two time points.  For
    instance, base might be the result of Unix.gettimeofday and precision
    might be 0.0001.  *)

val copy : t -> t

val add_point : t -> id
  (** returns id of new point *)

exception Inconsistent

val add_constraint : t -> edge_constraint -> unit
  (** can raise Inconsistent *)

val add_constraints : t -> edge_constraint list -> unit
  (** can raise Inconsistent *)

val constrain_order : t -> id -> id -> unit
  (** can raise Inconsistent *)

val fixed_constraint : id -> id -> int -> edge_constraint

val lower_constraint : id -> id -> int -> edge_constraint

val upper_constraint : id -> id -> int -> edge_constraint

val range_constraint : id -> id -> int -> int -> edge_constraint
  
val earliest : t -> id -> int

val latest : t -> id -> int

val print : t -> unit

val print_id : id -> unit

val stats : t -> int * int
  (** number of points and number of constraints *)
  
val done_p : t -> id -> int -> bool
  (** check if a time point is "done" (frozen and before current time) *)

val cleanup : t -> id list -> unit
  (** clean up the obsolete ids and their related constraints *)
  
(**** outside of usual interface ****)

  
(** testing and debugging 
val to_int_time : (id * id * int) -> t -> int
val interactive : unit -> unit
val test1 : unit -> unit
val test2 : unit -> unit
val test3 : unit -> unit *)


(* EOF *)
