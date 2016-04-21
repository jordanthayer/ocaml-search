(* $Id: backedht.ml,v 1.1 2003/09/26 23:39:02 ruml Exp ruml $
   
   associative arrays

   maps keys to values like alists but easier to modify destructively.
*)


type ('a * 'b) t = ('a array) * ('b array)


let init_keys f keys =
  (** given list of keys that will be used, init values by mapping
    using [f] *)
  let a = Array.of_list keys in
  let b = Array.map f a in
    a, b


let copy (a, b) =
  (Array.copy a), (Array.copy b)
    

let get (a, b) key =
  (** can raise Not_found *)
  b.(Wrarray.index a key)


let set (a, b) key value =
  (** can raise Not_found *)
  b.(Wrarray.index a key) <- value
    
    
(* EOF *)
