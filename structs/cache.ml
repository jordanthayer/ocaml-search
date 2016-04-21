(* $Id: cache.ml,v 1.2 2003/09/27 01:11:28 ruml Exp ruml $

   fixed-size LRU cache
*)


type ('a, 'b) t = {
  max : int;
  mutable curr : int;
  (* map from key to cell in q *)
  ht : ('a, ('a * 'b) Dblq.cell) Htable.t;
  (* ordered most-recently-used first. need key to remove last from ht *)
  q : ('a * 'b) Dblq.t;
}


let create ?(eq = (=)) max_size =
  assert (max_size >= 0);
  { max = max_size;
    curr = 0;
    ht = Htable.create Hashtbl.hash eq max_size;
    q = Dblq.create (); }


let find c k =
  let cons = Htable.find c.ht k in
    (* move to front *)
  let (_, v) as pair = Dblq.remove c.q cons in
  let cons = Dblq.push_front_cons c.q pair in
    Htable.replace c.ht k cons;
    v


let mem c k =
  Htable.mem c.ht k


let remove_if_present c k =
  try
    let cons = Htable.find c.ht k in
      ignore (Dblq.remove c.q cons);
      c.curr <- c.curr - 1
  with Not_found -> ()


let insert c k v =
  remove_if_present c k;
  (* uses a fresh cons no matter what *)
  let cons = Dblq.push_front_cons c.q (k, v) in
    c.curr <- c.curr + 1;
    Htable.replace c.ht k cons;
    (* evict if necessary *)
    if c.curr > c.max then
      let k, _ = Dblq.pop_rear c.q in
	Htable.remove c.ht k;
	c.curr <- c.curr - 1


let find_or_compute c f k =
  try
    find c k
  with Not_found ->
    let v = f k in
      insert c k v;
      v


let memoize f size =
  let c = create size in
    (fun a ->
       find_or_compute c f a)


let to_list c =
  Dblq.to_list c.q


(********** testing ***********)


let dump c =
  let i = ref 0 in
    Dblq.iter_left
      (fun (k, v) ->
	 Wrutils.pr "%d: %d -> '%c'\n" !i k v;
	 incr i)
      c.q;
    assert ((Dblq.length c.q) = c.curr);
    assert ((Htable.length c.ht) = c.curr);
    assert (c.curr <= c.max);
    flush_all ()


let test1 () =
  let find_prob = 0.4
  and cache = create 5 in
    Wrutils.pr "created\n"; flush_all ();
    Wrutils.ntimes (fun () ->
		  if Math.true_with_prob find_prob then
		    let (k,_) = Wrlist.random_elt (to_list cache) in
		    let v = find cache k in
		      Wrutils.pr "%d gives '%c'\n" k v;
		      dump cache
		  else
		    let i = (Random.int 26) + (Char.code 'a') in
		    let c = Char.chr i in
		      Wrutils.pr "adding %d -> '%c'\n" i c;
		      insert cache i c;
		      dump cache)
      200


(* EOF *)
