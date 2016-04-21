(**
   Very simplified version of the geq, guaranteed not to break -
   Jordan
*)

type 'a t = {
  q : 'a Dpq.t;
  close_enough : ('a -> 'a -> bool);
  quality_pred : ('a -> 'a -> bool);
  focal_pred :('a -> 'a -> bool);
  get_pos : 'a -> int;
  set_pos : 'a -> int -> unit;
}


type 'a entry = 'a


(************ creation **************)


let create_with quality_pred convenience_pred close_nuf setpos getpos dummy =
  { q = Dpq.create quality_pred setpos 500 dummy;
    close_enough = close_nuf;
    quality_pred = quality_pred;
    focal_pred = convenience_pred;
    get_pos = getpos;
    set_pos = setpos;
  }

let data n =
  n


let count t =
  Dpq.count t.q

let ge_count t =
  let to_ret = ref 0
  and best = Dpq.peek_first t.q  in
    Dpq.iter
      (fun n -> if t.close_enough best n then to_ret := !to_ret + 1) t.q;
    !to_ret


let ge_iter f t =
  let best = Dpq.peek_first t.q  in
    Dpq.iter (fun n -> if t.close_enough best n then f n) t.q


let empty_p t =
  Dpq.empty_p t.q


let validate t =
  ()


let pos_validate t p =
  ()

(********************* insert ****************)


(* Iteraties over the collection in order *)
let in_order_iter f t =
  Dpq.iter f t.q


let iter f t =
  in_order_iter f t


let insert t data =
  Dpq.insert t.q data;
  data

(*********** remove *****************)
let remove t n =
  Dpq.remove t.q (t.get_pos n)


let peek_best t =
  let best = Dpq.peek_first t.q
  and to_ret = ref (Dpq.peek_first t.q) in
    Dpq.iter (fun n -> if t.close_enough best n && not (t.focal_pred !to_ret n)
	      then to_ret := n) t.q;
    !to_ret


let peek_doset t =
  Dpq.peek_first t.q


let remove_best t =
  let to_ret = peek_best t in
    Dpq.remove t.q (t.get_pos to_ret);
    to_ret


let remove_doset t =
    Dpq.extract_first t.q


(**************** exchange ******************)

let replace_specific process_fated process_entry t f =
  failwith "not implemented in safegeq"

let replace_using process_min process_entry t =
  failwith "not implemented in safegeq"

let clear geq =
  Dpq.clear geq.q


let rec to_list geq =
  if (count geq) = 0
  then (Verb.pe Verb.debug "Geq converted to list\n"; clear geq;[])
  else ((*Verb.pe Verb.debug "Geq not empty\n";
	Verb.pe Verb.debug "size before remove %i\n" (count geq);*)
	let v = remove_best geq in
(*	  Verb.pe Verb.debug "size after remove %i\n" (count geq);*)
	   v :: to_list geq)


let resort geq =
  let rec fill_geq lst =
    match lst with
	[] -> ()
      | hd::tl -> (ignore(insert geq hd);
		   fill_geq tl)
  in
    Verb.pe Verb.debug "resorting geq of size %i\n" (count geq);
    let lst = to_list geq in
      fill_geq lst;
      Verb.pe Verb.debug "resorted geq of size %i\n" (count geq)


(* EOF *)
