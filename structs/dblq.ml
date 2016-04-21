(* $Id: dblq.ml,v 1.3 2003/09/27 01:11:52 ruml Exp ruml $

   double-ended queues.  destructive
*)


type 'a cell = 'a Dbl.t

type 'a t = {
  mutable head : 'a cell;
  mutable tail : 'a cell;
}


let create () =
  { head = Dbl.nil;
    tail = Dbl.nil; }


let is_empty q =
  q.head = Dbl.nil


let length q =
  Dbl.length_forward q.head


let clear q =
  q.head <- Dbl.nil;
  q.tail <- Dbl.nil

let data c = Dbl.data c

let in_q c = Dbl.in_list c


(************ adding *************)

let push_front_cons q x =
  if is_empty q then
    let c = Dbl.cons x Dbl.nil Dbl.nil in
      q.head <- c;
      q.tail <- c;
      c
  else
    let c = Dbl.add_before x q.head in
      q.head <- c;
      c


let push_front q x =
  ignore (push_front_cons q x)


let push_rear q x =
  if is_empty q then
    let c = Dbl.cons x Dbl.nil Dbl.nil in
      q.head <- c;
      q.tail <- c
  else
    let c = Dbl.add_after x q.tail in
      q.tail <- c


(*********** removing *********)


exception Empty


let peek_front q =
  if is_empty q then
    raise Empty
  else
    Dbl.data q.head


let pop_front q =
  if is_empty q then
    raise Empty
  else
    let c = q.head in
      q.head <- Dbl.next c;
      Dbl.unlink c;
      if q.tail == c
      then q.tail <- Dbl.nil;
      Dbl.data c


let peek_rear q =
  if is_empty q then
    raise Empty
  else
    Dbl.data q.tail


let pop_rear q =
  if is_empty q then
    raise Empty
  else
    let c = q.tail in
      q.tail <- Dbl.prev c;
      Dbl.unlink c;
      if q.head == c
      then q.head <- Dbl.nil;
      Dbl.data c


let remove q c =
  if is_empty q then
    failwith "Dblq.remove: how can there be a cell in an empty q?"
  else
    (if q.head == c then
       q.head <- Dbl.next c;
     if q.tail == c then
       q.tail <- Dbl.prev c;
     Dbl.unlink c;
     Dbl.data c)

let replace q c x =
  let n = Dbl.cons x (Dbl.prev c) (Dbl.next c) in
    if q.head == c then
      q.head <- n;
    if q.tail == c then
      q.tail <- n;
    ignore (Dbl.add_before x c);
    Dbl.unlink c;
    n


(********* iteration and mapping ********)


let fold_left f start q =
  Dbl.fold_left f start q.head

let fold_right f start q =
  Dbl.fold_right f start q.tail


let iter_left f q =
  Dbl.iter_left f q.head

let iter_right f q =
  Dbl.iter_right f q.tail


let find pred q =
  Dbl.find pred q.head


let exists pred q =
  Dbl.exists pred q.head


(********** derived ***********)


let copy q =
  let c = create () in
  let add x = push_rear c x in
    iter_left add q;
    c


let filter p q =
  fold_left (fun q x ->
	       if p x then
		 (push_rear q x;
		  q)
	       else q)
    (create ())
    q


let matching p q =
  fold_left (fun q x ->
	       if p x then
		 x:: q
	       else q)
    [] q


let partition p q =
  let yes = create ()
  and no = create () in
    iter_left (fun x ->
		 push_rear (if p x then yes else no) x)
      q;
    yes, no


let append a b =
  let q = create () in
  let add x = push_rear q x in
    iter_left add a;
    iter_left add b;
    q


let to_list q =
  fold_right (fun prev this -> this::prev) [] q


(* EOF *)
