(* $Id: astar.ml,v 1.3 2004/08/25 23:15:06 ruml Exp ruml $

   very simple set that can return a random element in constant time.
*)


type 'a t = {
  (* a.(fp) exists and is garbage element. *)
  mutable fp : int;
  mutable a : 'a array;
}


let create_with ?(initial_len = 128) garbage =
  Wrutils.pr "Rset module is NOT YET TESTED!\n";
  assert (initial_len > 0);
  { fp = 0;
    a = Array.make initial_len garbage; }


let count s =
  a.fp


let add s x =
  let a = s.a in
  let l = Array.length a
  and fp = s.fp in
  let next = fp + 1 in
    if next = l then
      (s.a <- Wrarray.extend a l a.(fp);
       s.a.(fp) <- x;
       s.fp <- next)
    else
      (a.(fp) <- x;
       s.fp <- next)


exception Empty


let random_elt s =
  let fp = s.fp in
    if fp = 0 then raise Empty;
    let a = s.a in
    let i = Random.int fp in
    let x = a.(i)
    and prev = fp - 1 in
      a.(i) <- a.(prev);
      a.(prev) <- a.(fp);
      s.fp <- prev;
      x


let iter f s =
  let a = s.a in
    for i = 0 to s.fp - 1 do
      f a.(i)
    done


let test1 () =
  let s = make (-99) in
    s


(* EOF *)
