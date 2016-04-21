(* $Id: geq.mli,v 1.2 2006/08/14 18:59:53 ruml Exp ruml $

   "good enough queue".  destructive.  designed for A*_epsilon, but meant
   to be general-purpose.
*)


type 'a entry

type 'a t


val make_dummy_entry : unit -> 'a entry

val create_with : ?equals:('a -> 'a -> bool) ->
  ?pr:(out_channel -> 'a -> unit) ->
  ('a -> 'a -> bool) -> ('a -> 'a -> bool) -> ('a -> 'a -> bool) ->
  ('a -> int -> unit) -> ('a -> int) -> 'a -> 'a t
  (** first three arguments are quality, convenience, and close_enough.
      quality and convenience should return true if order of arguments is OK.
      elements y that satisfy [close_enough_p x y], where x is the first of all
      elements when sorted by [quality_pred], are sorted by [convenience_pred].
      close_enough_p must not test features beyond those used by quality_pred.
      (this is because the benchmark x for close_enough is updated only when a
      node of that quality is removed.)  [setpos] and [getpos] are needed by
      the geq to keep itself in sync.  [dummy] is an element used for
      initialization that is never garbage collected. *)

val empty_p : 'a t -> bool

val clear : 'a t -> unit

val insert : 'a t -> 'a -> 'a entry
  (** returns a handle that can be used to remove the item later *)

val data : 'a entry -> 'a

val remove : 'a t -> 'a entry -> unit
  (** remove entry (object from the corresponding insertion) *)

val swap : 'a t -> 'a entry -> 'a -> 'a entry

val remove_doset : 'a t -> 'a

val remove_best : 'a t -> 'a

val peek_best : 'a t -> 'a

val peek_doset : 'a t -> 'a

val count : 'a t -> int

val ge_count : 'a t -> int

val update_close_enough : 'a t -> ('a -> 'a -> bool) -> unit

val in_order_iter : ('a -> unit) -> 'a t -> unit
(** an in order iteration **)

val iter : ('a -> unit) -> 'a t -> unit
  (** over all elements.  tolerates any modifications during traversal
      as elements are copied to an intermediate array. *)

val raw_iter : ('a entry -> unit) -> 'a t -> unit
  (** over all elements.  tolerates any modifications during traversal
      as elements are copied to an intermediate array. *)


val ge_iter : ('a -> unit) -> 'a t -> unit
  (** over good enough elements.  tolerates no modifications during traversal *)

val replace_specific :  ('a -> ('a list * 'b)) -> ('a -> 'a entry -> unit) ->
  'a t -> 'a entry -> 'b

val replace_using : ('a -> ('a list * 'b)) -> ('a -> 'a entry -> unit) ->
  'a t -> 'b
  (** remove best, pass it to [f] and insert the results, calling [g] on
      each result and its resulting entry.  returns the second result of [f].
      meant to be more efficient than separate removal and multiple insertions
      (only one sync operation between q and tree). *)


val resort : 'a t -> ('a -> 'a entry -> unit) -> ('a -> unit) -> unit
  (** resorts the underlying geq *)


val print : 'a t -> unit

(* EOF *)
