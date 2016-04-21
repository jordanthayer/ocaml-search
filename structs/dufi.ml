(* $Id: dufi.ml,v 1.1 2006/08/12 14:11:57 ruml Exp ruml $

   destructive union find for integers.

   following Sedgewick p446, except we store total # of nodes in each tree,
   not just # of descendants (allows 0 to be a valid parent).  The version
   in CLR looks a little slower and both are pretty clear.
*)


type t = int array
    (* index of parent, or for roots, the negative of # nodes in tree *)
    
    
let not_present = min_int


let make n =
  Array.make n not_present

    
let init t x =
  t.(x) <- -1


let find_root t x =
  (** returns index of representative of x's set *)
  let i = ref x in
    while t.(!i) >= 0 do
      i := t.(!i)
    done;
    (* t.(!i) is now negative, !i is a root *)
    (* path compression *)
    (let j = ref x in
       while t.(!j) >= 0 do
	 let parent = t.(!j) in
	   t.(!j) <- !i;
	   j := parent
       done);
    !i
      

let same_p t x y =
  if ((t.(x) == not_present) ||
      (t.(y) == not_present)) then raise Not_found;
  (find_root t x) == (find_root t y)


let join t x y =
  if ((t.(x) == not_present) ||
      (t.(y) == not_present)) then raise Not_found;
  let x = find_root t x
  and y = find_root t y in
    if t.(x) < t.(y) then
      (* x's tree is larger, put y under it *)
      (t.(x) <- t.(x) + t.(y);
       t.(y) <- x)
    else
      (t.(y) <- t.(y) + t.(x);
       t.(x) <- y)


(* EOF *)
