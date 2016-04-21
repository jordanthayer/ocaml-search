(* fixed-size destructive queues
*)


type 'a t = {
  contents : 'a array;
  mutable start : int;
}


let length q =
  Array.length q.contents
    

let init len f =
  { contents = Array.init len f;
    start = 0; }


let get q i =
  q.contents.((q.start + i) mod (length q))


let shift q x =
  let start = q.start in
    q.contents.(start) <- x;
    q.start <- (start + 1) mod (length q)


(* EOF *)
