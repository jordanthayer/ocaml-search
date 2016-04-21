(** fixed-size queue with constant-time access

  destructive operations
*)


type 'a t

val init : int -> (int -> 'a) -> 'a t
  (** like Array.init *)

val get : 'a t -> int -> 'a
  (** constant-time access.  index is from the front, starting at 0.
    can raise Invalid_argument "Fixedq.get" *)
  
val shift : 'a t -> 'a -> unit
  (** adds element to end of the queue, removing the item at the front
    if necessary to make room *)

(* below here not necessary yet
  
exception Full
  
val add : 'a t -> 'a -> unit
  (** adds element to end of the queue.  can raise Full *)

val create : int -> 'a -> 'a t

val take : 'a t -> 'a

val length : 'a t -> int

val is_empty : 'a t -> bool
*)
  
(* EOF *)
