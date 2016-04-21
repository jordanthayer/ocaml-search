(* $Id: graph.mli,v 1.1 2003/07/02 17:40:07 ruml Exp ruml $
   
   directed graphs

   intended to be general-purpose and not necessarily optimized.  can
   add vertices at any time.  destructive updates.  see dag for an
   extension.  see igraph.mli for efficient fixed-size graphs on ints.
   
*)

type 'a t

exception Duplicate

val create : unit -> 'a t

val from_edges : ('a * 'a) list -> 'a t

val add_vertex : 'a t -> 'a -> unit
  (** can raise duplicate *)

val ensure_vertex : 'a t -> 'a -> unit

val num_vertices : 'a t -> int
  
val add_arc : 'a t -> 'a -> 'a -> unit

val add_edge : 'a t -> 'a -> 'a -> unit

val out_neighbors : 'a t -> 'a -> 'a list

val exists_path : 'a t -> 'a -> 'a -> bool
  (** path from src to dest? *)

val fold_vertices : 'a t -> ('b -> 'a -> 'b) -> 'b -> 'b
  (** vertices aren't in any particular order *)
  
(* EOF *)
