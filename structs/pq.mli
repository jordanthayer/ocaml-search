(* $Id: pq.mli,v 1.1 2003/07/02 17:40:32 ruml Exp $
   
   simple functional priority queues

   taken from the OCaml manual

   Insertion and extraction should be log n in average space and time.

   Balance is not strictly enforced, so WORST CASE TIME IS LINEAR!
   
   Purely functional.

   For balanced destrucive version, see dpq.mli.
   It would be worth implementing a truly balanced functional version.
*)


type value = float
    
type 'a t

val empty : 'a t
  
val insert : 'a -> value -> 'a t -> 'a t

val is_empty : 'a t -> bool
  
exception Empty
  
val extract_min : 'a t -> 'a * value * 'a t
  (** removes and returns a lowest valued object *)
  
val extract_mins : 'a t -> 'a list * value * 'a t
  (** removes and returns list of all objects with lowest value *)
  

(* EOF *)
