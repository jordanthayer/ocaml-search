(* $Id: upq.ml,v 1.4 2005/12/10 23:26:03 ruml Exp ruml $

   update-able destructive priority queues
*)


type ('a, 'b) t = {
  q : 'a Dpq.t;
  key : 'a -> 'b;
  index : ('b, int) Hashtbl.t;
}


let create_with pred key approx_n init_e =
  let index = Hashtbl.create approx_n in
  let update = (fun e i -> Hashtbl.replace index (key e) i) in
    { q = Dpq.create pred update approx_n init_e;
      key = key;
      index = index;
    }


let empty_p q =
  Dpq.empty_p q.q


let count q =
  Dpq.count q.q

(*
  let get_garbage q =
  q.heap.(q.fp)
*)

let set_garbage q e =
  Dpq.set_garbage q.q e


let insert q e =
  if Hashtbl.mem q.index (q.key e) then
    failwith "Upq.insert: element already in priority queue";
  Dpq.insert q.q e


let of_list pred key init_e list =
  let q = create_with pred key (List.length list) init_e in
    List.iter (insert q) list;
    q


exception Empty


let extract_first q =
  try
    let e = Dpq.extract_first q.q in
      Hashtbl.remove q.index (q.key e);
      e
  with Dpq.Empty _ -> raise Empty


let peek_first q =
  try
    Dpq.peek_first q.q
  with Dpq.Empty _ -> raise Empty


let see_update q e =
  Dpq.see_update q.q (Hashtbl.find q.index (q.key e))


let mem q e =
  Hashtbl.mem q.index (q.key e)


let mem_key q e_k =
  Hashtbl.mem q.index e_k

(*
let find q e =
  q.heap.(Hashtbl.find q.index (q.key e))
*)

let find_key q e_k =
  Dpq.get_at q.q (Hashtbl.find q.index e_k)


let remove q e =
  let ht = q.index
  and e_k = q.key e in
    Dpq.remove q.q (Hashtbl.find ht e_k);
    Hashtbl.remove ht e_k


let swap q remove add =
  let ht = q.index in
  let rem_k = q.key remove in
  let rem_i = Hashtbl.find ht rem_k in
    Hashtbl.remove ht rem_k;
    if Hashtbl.mem ht (q.key add) then
      failwith "Upq.swap: element already present";
    Dpq.swap q.q rem_i add


let iter f q =
  Dpq.iter f q.q


let iter_unsafe f q =
  Dpq.iter_unsafe f q.q


(* let make_iterator_unsafe q =
  Dpq.make_iterator_unsafe q.q *)


(********** testing ***********)


let iteri f q =
  let i = ref 0 in
    Dpq.iter_unsafe (fun e ->
		       f !i e;
		       incr i;
		       true)
      q.q


let check_index q =
  (** ensure index is accurate *)
  iteri (fun i e ->
	   try
	     let j = Hashtbl.find q.index (q.key e) in
	       if (i <> j) then
		 failwith
		   (Wrutils.str
		      "Upq.check_index: obj at %d was thought to be at %d" i j)
	   with Not_found -> failwith
	     (Wrutils.str
		"Upq.check_index: obj at %d not in index" i))
    q;
  let l = Hashtbl.length q.index
  and m = Dpq.count q.q in
    if l <> m then
      failwith (Wrutils.str
		  "Upq.check_index: index has %d elts while q has %d" l m)


let dump q =
  iteri (fun i e ->
	   Wrutils.pr "%d: %d\n" i e)
    q;
  flush_all ();
  check_index q


let test1 () =
  let q = create_with (<=) Fn.identity 5 0 in
    Wrutils.pr "created\n"; flush_all ();
    let remove_prob = 0.25 in
      Wrutils.ntimes (fun () ->
		      (if ((Math.true_with_prob remove_prob) &&
			   (not (empty_p q))) then
			 Wrutils.pr "removing %d\n" (extract_first q)
		       else
			 let i = Wrutils.eval_until (fun () -> Random.int 10)
				   (fun x -> not (mem q x)) in
			   Wrutils.pr "adding %d\n" i;
			   insert q i);
		      dump q)
	20


(* EOF *)
