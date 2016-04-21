(** Dynamically sizing array, allowing objects in the array to track
    where they are stored.

    @author sna4
    @since 2010-11-28
*)

type 'a t = {
  mutable fp : int;
  mutable objs : 'a array;
  mutable update : 'a -> int -> unit;
}

let no_position = -1

let create update n_items init =
  { fp = 0;
    objs = Array.make n_items init;
    update = update}

let count q =
  q.fp

let empty_p q =
  q.fp > 0

let to_list q =
  Array.to_list q.objs


(************* insertion *************)


let ensure_room q =
  (** doubles array [a] if necessary, filling with last element *)
  let a = q.objs in
  let len = Array.length a in
  let max = len - 1 in
    if q.fp == max then
      (* garbage element is still at end and will get copied *)
      q.objs <- Wrarray.extend a len a.(max)

let insert q e =
  ensure_room q;
  let fp = q.fp in
    q.objs.(fp) <- e;
    q.update e fp;
    q.fp <- q.fp + 1


(************* removal *************)


exception Empty of string


let extract_first q =
  let fp = q.fp in
    if fp == 0
    then raise (Empty "Dynarray.extract_first")
    else
      let a = q.objs
      and last = fp - 1 in
      let e = a.(0) in
	a.(0) <- a.(last);
	a.(last) <- a.(fp);
	q.fp <- last;
	q.update e (no_position);
	e

let get_at q i =
  if ((i < 0) || (i >= q.fp))
  then invalid_arg (Wrutils.str "Dynarray.get_at %d of %d" i q.fp);
  q.objs.(i)

let remove q i =
  if ((i < 0) || (i >= q.fp))
  then invalid_arg (Wrutils.str "Dynarray.remove %d of %d" i q.fp);
  (* added by jtd7, nov 11.  Items being removed should be set to removed *)
  q.update q.objs.(i) (no_position);
  let a = q.objs
  and last = q.fp - 1 in
    a.(i) <- a.(last);    (* a.(i) receives the last element *)
    a.(last) <- a.(q.fp); (* end element gets junk *)
    q.fp <- last;
    if i < last
    then (q.update a.(i) i)

let remove_ret q i =
  if ((i < 0) || (i >= q.fp))
  then invalid_arg (Wrutils.str "Dynarray.remove %d of %d" i q.fp);
  (* added by jtd7, nov 11.  Items being removed should be set to removed *)
  q.update q.objs.(i) (no_position);
  let a = q.objs
  and last = q.fp - 1 in
  let ret = a.(i) in
    a.(i) <- a.(last);    (* a.(i) receives the last element *)
    a.(last) <- a.(q.fp); (* end element gets junk *)
    q.fp <- last;
    if i < last
    then (q.update a.(i) i;
	  ret)


(************** iteration ************)

let iter f q =
  Array.iter f (Array.sub q.objs 0 q.fp)

let iteri f q =
  Array.iteri f (Array.sub q.objs 0 q.fp)

let map f q =
  Array.map f (Array.sub q.objs 0 q.fp)

let mapi f q =
  Array.mapi f (Array.sub q.objs 0 q.fp)

let print q fn =
  iter (fun a -> fn stdout a;
		 Verb.pe Verb.always " ") q
