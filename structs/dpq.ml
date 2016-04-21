(* $Id: dpq.ml,v 1.3 2005/12/10 23:25:37 ruml Exp ruml $

   destructive priority queues
*)


type 'a t = {
  (* fill pointer - index of next item *)
  mutable fp : int;
  mutable heap : 'a array;
  mutable pred : 'a -> 'a -> bool;
  update : 'a -> int -> unit;
(*  check_val: 'a -> int -> unit;*)
}


(************* basics *************)

let no_position = -1


let create pred update approx_n init_e =
  let approx_n = max approx_n 8 in
    { fp = 0;
      heap = Array.make approx_n init_e;
      pred = pred;
      update = update;
(*      check_val = get*)
    }


let copy q =
  (** [copy q] makes a copy. *)
  { q with heap = Array.copy q.heap }

let copy_no_notif q =
  (** [copy q] makes a copy. *)
  { q with heap = Array.copy q.heap; update = (fun _ _ -> ()); }


let create_with pred init_e =
  create pred Fn.no_op2 255 init_e


(*
  let from_array pred update init_a init_e =
  let q = { fp = Array.length init_a;
	    heap = init_a;
	    pred = pred;
	    update = update;
	  } in
    push_down from halfway to front
    q
*)


let clear q =
  let fp = q.fp in
    for i = 0 to fp - 1 do
      q.heap.(i) <- q.heap.(fp)
    done;
    q.fp <- 0


let empty_p q =
  q.fp == 0


let count q =
  q.fp


let set_garbage q e =
  let a = q.heap
  and fp = q.fp in
  let len = Array.length a in
    Array.fill a fp (len - fp) e


(************* insertion *************)


let ensure_room q =
  (** doubles array [a] if necessary, filling with last element *)
  let a = q.heap in
  let len = Array.length a in
  let max = len - 1 in
    if q.fp == max then
      (* garbage element is still at end and will get copied *)
      q.heap <- Wrarray.extend a len a.(max)



let check_heap q =
  (** for debugging.  ensures heap property. *)
  let a = q.heap
  and pred = q.pred in
    for i = 1 to q.fp - 1 do
      let p = (i-1)/2 in
      if not (pred a.(p) a.(i)) then
        failwith (Wrutils.str
          "Dpq.check_heap: child %d has higher priority than parent %d"
          i p )
    done


let check_index getpos q =
  (** for debugging.  ensures index consistency. *)
  let a = q.heap in
    for i = 0 to q.fp - 1 do
      let p = getpos a.(i) in
      if p <> i then
        failwith ( Wrutils.str
          "Dpq.check_index: at index %d element shows position %d"
          i p )
    done


let check_index2 getpos q =
  (** for debugging.  ensures index consistency. *)
  Array.iteri
    (fun i n ->
       let posn = getpos n in
	 if (posn < 0 || posn >= (count q))
	     then failwith
	       (Wrutils.str
		  "Dpq.check_index2: Element @ %d thinks loc = %d, max %d"
		  i posn (count q)))
    (Array.sub q.heap 0 q.fp)



let check getpos q =
  check_heap q;
  check_index2 getpos q


let pull_up q e_i =
  (** element at [e_i] might be too low in heap.  moves things around. *)
  let a = q.heap
  and curr_i = ref e_i
  and parent_i = ref ((e_i - 1) / 2) in
  let elt = a.(e_i)
  and parent = ref a.(!parent_i)
  and pred = q.pred
  and update = q.update in
    (* need to give objs to predicate in order, as they may be identical *)
    while (!curr_i != 0) && (not (pred !parent elt)) do
      (* Wrutils.pr "trying parent %d vs current %d\n" !parent_i !curr_i; *)
      a.(!curr_i) <- !parent;
      update !parent !curr_i;
      curr_i := !parent_i;
      parent_i := (!parent_i - 1) / 2;
      parent := a.(!parent_i);
    done;
    a.(!curr_i) <- elt;
    !curr_i


let insert q e =
  ensure_room q;
  let fp = q.fp in
    q.heap.(fp) <- e;
    q.update e (pull_up q fp);
    q.fp <- fp + 1


let resort q p =
  q.pred <- p;
  let update = q.update
  and a = q.heap in
  for i = 1 to q.fp - 1 do
    let j = pull_up q i in
    update a.(j) j
  done


(************* removal *************)


let push_down q e_i =
  (** element at [e_i] might be too high in the heap.  moves things around. *)
  let a = q.heap
  and fp = q.fp
  and pred = q.pred
  and update = q.update in
  let e = a.(e_i) in
  let rec try_push curr =
    (** returns correct index to use for e *)
    let child_i = ref ((curr * 2) + 1) in
      if (!child_i < fp)
      then
	let child = ref a.(!child_i) in
	  (* check if right child is larger *)
	  (let right_i = !child_i + 1 in
	     if (right_i < fp)
	     then
	       let right = a.(right_i) in
		 if not (pred !child right)
		 then
		   (child_i := right_i;
		    child := right));
	  (* check if we can go over largest child *)
	  if (pred e !child)
	  then curr
	  else (a.(curr) <- !child;
		update !child curr;
		try_push !child_i)
      else
	curr
  in
  let i = try_push e_i in
    a.(i) <- e;
    update e i

let update_all q f =
  let a = q.heap in
  for i = 0 to q.fp - 1 do f a.(i) done;
  if q.fp > 0 then
    for i = (q.fp - 1) / 2 downto 0 do
      ignore (push_down q i)
    done

exception Empty of string


let extract_first q =
  let fp = q.fp in
    if fp == 0
    then raise (Empty "Dpq.extract_first")
    else
      let a = q.heap
      and last = fp - 1 in
      let e = a.(0) in
	a.(0) <- a.(last);
	a.(last) <- a.(fp);
	q.fp <- last;
	if last <> 0 then push_down q 0;
	q.update e (no_position);
	e


let peek_first q =
  if q.fp == 0
  then raise (Empty "Dpq.peek_first")
  else q.heap.(0)


let peek_second q =
  if q.fp == 0 then
    raise (Empty "Dpq.peek_first")
  else if q.fp == 1 then
    raise Not_found
  else if q.fp == 2 then
    q.heap.(1)
  else
    let a = q.heap.(1) and b = q.heap.(2) in
      if q.pred a b then a else b


let see_update q i =
  if ((i < 0) || (i >= q.fp))
  then invalid_arg (Wrutils.str "Dpq.see_update %d of %d" i q.fp);
  let i = pull_up q i in
    push_down q i


(* removes the node at index i from the heap *)
let remove q i =
  if ((i < 0) || (i >= q.fp))
  then invalid_arg (Wrutils.str "Dpq.remove %d of %d" i q.fp);
  (* added by jtd7, nov 11.  Items being removed should be set to removed *)
  q.update q.heap.(i) (no_position);
  let a = q.heap
  and last = q.fp - 1 in
    a.(i) <- a.(last);    (* a.(i) receives the last element *)
    a.(last) <- a.(q.fp); (* end element gets junk *)
    q.fp <- last;
    if i < last
    then (q.update a.(i) i;
    (* incorporates Allen's fix *)
	  push_down q (pull_up q i))


(* removes the node at index i from the heap and returns it *)
let remove_ret q i =
  if ((i < 0) || (i >= q.fp))
  then invalid_arg (Wrutils.str "Dpq.remove %d of %d" i q.fp);
  (* added by jtd7, nov 11.  Items being removed should be set to removed *)
  let n = q.heap.(i) in
    q.update n (no_position);
    let a = q.heap
    and last = q.fp - 1 in
      a.(i) <- a.(last);    (* a.(i) receives the last element *)
      a.(last) <- a.(q.fp); (* end element gets junk *)
      q.fp <- last;
      if i < last
      then (q.update a.(i) i;
	    (* incorporates Allen's fix *)
	    push_down q (pull_up q i));
      n



let swap q remove_i add =
  if ((remove_i < 0) || (remove_i >= q.fp))
  then invalid_arg (Wrutils.str "Dpq.swap %d of %d" remove_i q.fp);
  let a = q.heap in
    a.(remove_i) <- add;
    (* setpos happens in push down *)
    see_update q remove_i


let get_at q i =
  if ((i < 0) || (i >= q.fp))
  then invalid_arg (Wrutils.str "Dpq.get_at %d of %d" i q.fp);
  q.heap.(i)


(************** iteration ************)


let iter f q =
  Array.iter f (Array.sub q.heap 0 q.fp)

let iteri f q =
  Array.iteri f (Array.sub q.heap 0 q.fp)


let map f q =
  Array.map f (Array.sub q.heap 0 q.fp)

let mapi f q =
  Array.mapi f (Array.sub q.heap 0 q.fp)


let iter_unsafe f q =
  let a = q.heap
  and i = ref 0 in
    (* recheck fp each time in case [f] causes changes! *)
    while !i < q.fp do
      if f a.(!i)
      then incr i
      else i := q.fp
    done


let make_iterator_unsafe q =
  (* do best-first search within the heap, using predicate to order
     frontier.  Don't cache much state about [q], since its fp may change and
     its heap may be enlarged. *)
  let p = q.pred in
  let in_order i j =
    let a = q.heap in
      p a.(i) a.(j)
  in
  let indices = create in_order Fn.no_op2 (Array.length q.heap) (-99) in
  let get_elt () =
    try
      let head = extract_first indices in
      let left = (head * 2) + 1
      and fp = q.fp in
	if left < fp then
	  (insert indices left;
	   let right = left + 1 in
	     if right < fp then insert indices right);
	Some q.heap.(head)
    with (Empty _ )-> None
  and reset () =
    clear indices;
    if q.fp > 0 then insert indices 0
  in
    if q.fp > 0 then insert indices 0;
    get_elt, reset


let resort_old q =
  resort q q.pred


let self_map q mf = 
  (**applies mf to the elements in q.*)
  for i = 0 to q.fp
  do
    q.heap.(i) <- mf q.heap.(i)
  done


let print q fn =
  iter (fun a -> fn stdout a;
		 Verb.pe Verb.always " ") q


(********** testing ***********)
type 'a test_struct = {
  data : 'a;
  mutable index : int
}

let dump q =
  for i = 0 to (q.fp - 1) do
    Wrutils.pr "%d: %d\n" i q.heap.(i).data
  done;
  flush_all ()


let test1 do_dump num_times =
  let q = (create
	     (fun a b -> a.data <= b.data)
	     (fun n i -> n.index <- i)
(*	     (fun n i -> if n.index <> i then failwith "indicies hosed.")*)
	     1
	     {data = 0;
	      index = -99} )in
    Wrutils.pr "created\n"; flush_all ();
    let remove_prob = 0.25 in
      Wrutils.ntimes (fun () ->
		      if ((Math.true_with_prob remove_prob) &&
			    (not (empty_p q)))
		      then Wrutils.pr "removing %d\n" (extract_first q).data
		      else (let i = Random.int 10 in
			    let i = i - 5 in
			      Wrutils.pr "adding %d\n" i;
			      insert q {data = i;
					index = -99});
		      if do_dump then dump q;
		      check (fun n -> n.index) q)
	num_times


let size t =
  t.fp


(* EOF *)
