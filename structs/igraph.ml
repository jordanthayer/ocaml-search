(* simple directed graph with destructive updates

*)


(* list of outgoing edges *)
type t = (int list) array
    

let create int =
  Array.make int []


let out_neighbors g v =
  g.(v)
    
      
let add_edge g src dest =
  g.(src) <- dest::g.(src)
    

(* EOF *)
