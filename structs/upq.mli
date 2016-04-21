(* $Id: upq.mli,v 1.3 2004/08/20 20:18:15 ruml Exp ruml $
   
   update-able destructive priority queues

   based on dpq.ml.  Should probably only be used for convenience, now that
   Dpq takes a notifier function.
*)


type ('a, 'b) t
  (** priority q of 'a.  elements must be unique when considered as 'b *)

  
(** basic interface, as in Dpq *)

  
val create_with : ('a -> 'a -> bool) -> ('a -> 'b) -> int -> 'a -> ('a, 'b) t
  (** predicate should be true iff elements are in the desired order
    (including equality!).  takes key for hash indexing, and guess for
    initial size.  initial element is not formally added to the q.  duplicate
    keys will raise Failure.  Note: this initial element will never be
    garbage-collected unless another never-to-be-collected element is
    specified via set_garbage.  Sorry, but array must be filled with
    something! *)

val set_garbage : ('a, 'b) t -> 'a -> unit
  (** under usual operation, initial element will never be garbage
    collected. *)
  
val insert : ('a, 'b) t ->  'a -> unit
 
val count : ('a, 'b) t -> int
  (** number of objects in q.  constant time.  *)

(* clear *)
  
val empty_p : ('a, 'b) t -> bool
  
exception Empty
  
val extract_first : ('a, 'b) t -> 'a
  (** removes and returns an object at the beginning of the ordering
    defined by the queue's predicate.  may raise Empty *)

val peek_first : ('a, 'b) t -> 'a
  (** returns but does not remove the object at the beginning of the
    ordering defined by the queue's predicate.  may raise Empty *)

val iter : ('a -> unit) -> ('a, 'b) t -> unit
  (** traverses elements of q (as it exists at the time of the call)
    in heap order, which is not necessarily best-first.  copies
    contents, so q can be arbitrarily modified during the traverse. *)

val iter_unsafe : ('a -> bool) -> ('a, 'b) t -> unit
  (** traverses heap of q in heap order (not necessarily best-first).
    moves to next index when [f] returns true.  does not copy contents, so
    changes to q can affect the traverse. *)

  
(** updatable interface *)

  
val mem : ('a, 'b) t -> 'a -> bool
  (** tests whether obj with same key is already in q *)

val mem_key : ('a, 'b) t -> 'b -> bool
  (** tests whether obj with given key is already in q *)

(* val find : ('a, 'b) t -> 'a -> 'a
  (** returns obj already in q with same key value or raises Not_found *) *)

(* val find_key : ('a, 'b) t -> 'b -> 'a
  (** returns obj already in q with given key value or raises Not_found *) *)

val remove : ('a, 'b) t -> 'a -> unit
  (** remove given object from queue.  Can raise Not_found. *)

(* val swap : ('a, 'b) t -> 'a -> 'a -> unit
  (** remove first obj and add second.  slightly faster than doing a remove
    and an insert.  Doesn't assume new belongs in same place.  Can raise
    Not_found. *) *)

val see_update : ('a, 'b) t -> 'a -> unit
  (** must be called whenever an object's behavior with regard to the
    ordering predicate might have changed.  Can raise Not_found. *)

  
(** other extras *)

  
val of_list : ('a -> 'a -> bool) -> ('a -> 'b) -> 'a -> 'a list -> ('a, 'b) t

(* val make_iterator_unsafe : ('a, 'b) t -> (unit -> 'a option) * (unit -> unit)
  (** returns a function that returns elements in best-first order and a
    function that resets the iterator to the beginning.  All bets are off if
    queue is modified during iterating (OK just before reset, though). *) *)
  

(** outside of usual interface *****)

  
val check_index  : ('a, 'b) t -> unit
  
val test1 : unit -> unit
  

(* EOF *)
