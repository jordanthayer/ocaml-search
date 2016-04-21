(* destructive sets

   insertion is O(1).

   currently implemented using hash tables
*)


type 'a t

val create : int -> 'a t
  
val add : 'a -> 'a t -> unit
  
val add_new_p : 'a -> 'a t -> bool
  (* returns true iff element wasn't already in set *)
    
val count : 'a t -> int

val mem : 'a -> 'a t -> bool

val fold : ('a -> 'b -> 'a) -> 'a -> 'b t -> 'a

val iter : ('a -> unit) -> 'a t -> unit

val to_list : 'a t -> 'a list
  

(* EOF *)
