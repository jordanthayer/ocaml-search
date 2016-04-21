(* $Id: dblq.mli,v 1.3 2003/09/27 01:12:01 ruml Exp ruml $
   
   simple destructive set with a very limited API

   Created by Jordan, Dec 07.
   Tweaked by Wheeler, Jan 08.
*)
  
type 'a t
    
val create : unit -> 'a t
  
val empty_p : 'a t -> bool
  
val insert : 'a -> 'a t -> 'a t
  
val swap : 'a -> 'a t -> 'a t
  (* swaps in new content for the given cell *)
  
val remove_arbitrary : 'a t -> 'a
  (* removes an arbitrary element from the set and returns it, or
  raises Not_Found *)


(* EOF *)
