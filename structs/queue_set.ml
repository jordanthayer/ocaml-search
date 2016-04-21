(** A queue that doesn't allow for duplicates.

    @author eaburns
    @since 2010-02-15
*)

type 'a t = {
  q : 'a Queue.t;
  on_q : ('a, bool) Hashtbl.t;
}

let create ?(hash_size=100) () =
  (** [create ?hash_size=100 ()] creates a new queue. *)
  { q = Queue.create (); on_q = Hashtbl.create hash_size }


let push q elm =
  (** [push q elm] pushes an element onto the queue if it is not there
      already. *)
  if not (Hashtbl.mem q.on_q elm) then Queue.push elm q.q


let push_all q elms = List.iter (push q) elms
  (** [push_all q elms] pushes a list of elements onto the queue. *)


let of_list elms =
  (** [of_list elms] creates a queue populated with the given list of
      elements. *)
  let q = create () in
    push_all q elms;
    q


let take q =
  (** [take q] takes an element off of the front of the queue. *)
  let elm = Queue.take q.q in
    Hashtbl.remove q.on_q elm;
    elm


let on_q q elm = Hashtbl.mem q.on_q elm
  (** [on_q q elm] tests if the given element is on the queue. *)


let is_empty q = Queue.is_empty q.q
  (** [is_empty q] tests if the queue is empty. *)
