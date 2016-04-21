(* $Id: dag.mli,v 1.1 2003/07/02 17:40:15 ruml Exp $
   
   directed acyclic graphs

   destructive updates   
*)

type 'a t

exception Duplicate

exception Circularity
  
val create : unit -> 'a t

val add_vertex : 'a t -> 'a -> unit
  (** can raise Duplicate *)
  
val ensure_vertex : 'a t -> 'a -> unit
  (** doesn't raise Duplicate *)

val add_arc : 'a t -> 'a -> 'a -> unit
  (** can take linear time - checks for possible cycle *)

val out_neighbors : 'a t -> 'a -> 'a list

val bottom_up : 'a t -> ('a -> bool) -> bool
  (** evaluates [f] on vertices all of whose out_neighbors satisfied
    [f].  return true iff all vertices satisfied [f].  Takes O(V) time. *)

val iter_bottom_up : 'a t -> ('a -> unit) -> unit
  
(* EOF *)
