(** union/find data structure for disjoint sets
  
  updates are functional.
*)


module type ORDERED_TYPE =
  (** input sig of Fdsets.Make *)
sig
  type t
  val compare : t -> t -> int
end


module type FDSET =
  (** output sig of Fdsets.Make *)
sig
  type obj
    (** type of object in the sets *)
    
  type t
    (** type of an fdsets holding objects of type [obj] *)
    
  val empty : t
    
  val same_set_p : obj -> obj -> t -> bool
    (** true iff the first two arguments are in the same set *)
    
  val join_sets : obj -> obj -> t -> t
    (** [join_sets a b s] returns a new set forest in which [a] and
      [b] are guaranteed to be in the same set *)

(* could support merge by iterating through one Map, forcing (in the
   other Map) each child into the same set with its parent *)
    
end

  
module Make (Ord : ORDERED_TYPE) : FDSET with type obj = Ord.t


(* EOF *)
