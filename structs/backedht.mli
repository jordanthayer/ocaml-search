(* $Id: backedht.mli,v 1.1 2003/09/26 23:38:58 ruml Exp $
   
   hashtables that write to disk.
*)


type ('a, 'b) t

  
val with_writable : (('a, 'b) t -> 'c) ->
  (in_channel -> ('a * 'b)) -> (out_channel -> 'a -> 'b -> unit) ->
  string -> bool -> 'c
  (** ensures that file gets closed.  bool arg is create_if_not_exist. *)

val open_read_only : (in_channel -> ('a * 'b)) -> string -> ('a, 'b) t
  (** raises Failure on [replace] operations.  no file is left open. *)

  
val compact : (in_channel -> ('a * 'b)) -> (out_channel -> 'a -> 'b -> unit) ->
  string -> unit
  (** if there's a lot of duplication, rewrites file *)

  
val find : ('a, 'b) t -> 'a -> 'b
  (** can raise Not_found *)

val replace : ('a, 'b) t -> 'a -> 'b -> unit

  
val iter : ('a -> 'b -> unit) -> ('a, 'b) t -> unit
  

(* EOF *)
