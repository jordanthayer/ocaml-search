(* directed graph with destructive updates

   simple representation for large graphs and fast operations
*)

type t

val create : int -> t

val add_arc : t -> int -> int -> unit

val add_edge??

val out_neighbors : t -> int -> int list

(* reverse - returns graph with all edges reversed? *)

  
(* EOF *)
