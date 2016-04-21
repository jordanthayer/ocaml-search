(* $Id: dblq.mli,v 1.3 2003/09/27 01:12:01 ruml Exp ruml $
   
   The set is a singly linked list, with a pointer to the object
   contained by the item

   Created by Jordan, Dec 07.
   Tweaked by Wheeler, Jan 08.

   want destructive insertion.
   
*)

  
(* first cell is always dummy.  nil is point-to-self *)
type 'a t = {
  mutable contents: 'a;
  mutable next: 'a t;
}

    
let create () =
  let rec c = { contents = Obj.magic 0;
		next = c; } in
    c


(**************** basics *****************)


let empty_p s =
  s.next == s


let insert x s =
  (* put after dummy *)
  if s.next == s then
    (* empty, so terminate *)
    let rec n = { contents = x;
		  next = n; } in
      s.next <- n;
      n
  else
    let n = { contents = x;
	      next = s.next; } in
      s.next <- n;
      n


let swap x c =
  c.contents <- x;
  c

    
let remove_arbitrary s =
  let n = s.next in
    if n == s then
      raise Not_found
    else if n.next == n then
      (* n is last - terminate dummy *)
      (s.next <- s;
       n.contents)
    else
      (* plenty more *)
      (s.next <- n.next;
       n.contents)
	

(* EOF *)
