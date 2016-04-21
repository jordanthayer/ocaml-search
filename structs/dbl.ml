(* $Id: dbl.ml,v 1.1 2003/06/24 17:57:54 ruml Exp ruml $

   doubly-linked lists.  destructive.
*)

type 'a cell =  {
  data : 'a;
  mutable next : 'a t;
  mutable prev : 'a t;
}

and  'a t = ('a cell) option

let nil = None

exception Nil

let fake = Obj.magic "uidfakenode"

let cons x prev next =
  Some { data = x;
	 next = next;
	 prev = prev;
       }


let is_nil = function
    None -> true
  | _ -> false


let data = function
    None -> raise Nil
  | Some { data = x} -> x


let next = function
    None -> None
  | Some { next = n} -> n


let prev = function
    None -> None
  | Some { prev = p} -> p

let in_list = function
    None -> raise Nil
  | (Some {prev = p; next = n}) -> not ((n = (Some fake)) &&
					  (p = (Some fake)))


let length_forward c =
  let rec aux so_far = function
      None -> so_far
    | Some c -> aux (so_far + 1) c.next
  in
    aux 0 c


(****** destructive modification *****)


let add_before x c =
  let n = Some { data = x;
		 next = c;
		 prev = prev c; } in
    (match c with
       None -> ()
     | Some c ->
	 (match c.prev with
	    None -> ()
	  | Some p -> p.next <- n);
	 c.prev <- n);
    n


let add_after x c =
  let n = Some { data = x;
		 next = next c;
		 prev = c; } in
    (match c with
       None -> ()
     | Some c ->
	 (match c.next with
	    None -> ()
	  | Some p -> p.prev <- n);
	 c.next <- n);
    n

let unlink = function
    None -> ()
  | Some c ->
      (match c.next with
	   None -> ()
	 | Some n -> n.prev <- c.prev);
      (match c.prev with
	   None -> ()
	 | Some n -> n.next <- c.next);
      c.next <- (Some fake);
      c.prev <- (Some fake)


(****** mapping and iteration *****)


let rec fold_left f init = function
    None -> init
  | Some c ->
      fold_left f (f init c.data) c.next

let rec fold_right f init = function
    None -> init
  | Some c ->
      fold_right f (f init c.data) c.prev


let iter_left f l =
  fold_left (fun () x -> f x) () l

let iter_right f l =
  fold_right (fun () x -> f x) () l


(****** searching *****)


let rec find pred = function
    None -> raise Not_found
  | Some c ->
      if pred c.data then
	c.data
      else
	find pred c.next


let rec exists pred = function
    None -> false
  | Some c ->
      if pred c.data then
	true
      else
	exists pred c.next


(* EOF *)
