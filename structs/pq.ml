(* $Id: pq.ml,v 1.1 2003/07/02 17:40:26 ruml Exp $
   
   simple functional priority queues

 *)


type value = float

type 'a t = Leaf | Node of value * 'a * 'a t * 'a t

    
let empty = Leaf


(* always pushes larger-pri element down to the left, but swaps the left
   and right branches to give the effect of pushing new elements into
   alternating sides of the tree
 *)
let rec insert elt pri queue  =
  match queue with
    Leaf -> Node (pri, elt, Leaf, Leaf)
  | Node (p, e, left, right) ->
      if pri <= p
	  (* children of every touched branch are swapped *)
      then Node (pri, elt, insert e p right, left)
      else Node (p, e, insert elt pri right, left)


let is_empty q =
  q = empty
	
    
exception Empty

  
let rec remove_min = function
    Leaf -> raise Empty
  | Node( pri, elt, left, Leaf) -> left
  | Node (pri, elt, Leaf, right) -> right
  | Node (pri, elt, (Node (lpri, lelt, _, _) as left),
         (Node (rpri, relt, _, _) as right)) ->
	   (* elt quietly disappears and lesser child recursively percolates
	      up *)
           if lpri <= rpri
           then Node (lpri, lelt, remove_min left, right)
           else Node (rpri, relt, left, remove_min right)

	       
let extract_min = function
    Leaf -> raise Empty
  | Node (pri, elt, _, _) as queue -> (elt, pri, remove_min queue)


let rec extract_mins q =
  let (e, p, q) = extract_min q in
    match q with
      Node (next_p, _, _, _) when (next_p = p) ->
	let (rest, p, q) = extract_mins q in
	  (e::rest), p, q
    | _ -> [e], p, q
  


(* EOF *)
