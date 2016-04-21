(* $Id: sharedq.ml,v 1.1 2003/06/20 16:30:46 ruml Exp $
   
   shared queues
*)


type 'a t = {
  q : 'a Queue.t;
  (* the right to access or modify q *)
  mod_rights : Mutex.t;
  (* signaled when something is added to q *)
  addition : Condition.t;
}


let create () =
  { mod_rights = Mutex.create ();
    q = Queue.create ();
    addition = Condition.create (); }


let mutexing = Wrthreads.with_mutex
		 

let add q x =
  mutexing q.mod_rights (fun () -> Queue.add x q.q);
  Condition.signal q.addition


let is_empty q =
  mutexing q.mod_rights (fun () -> Queue.is_empty q.q)


let iter f q =
  mutexing q.mod_rights (fun () -> Queue.iter f q.q)


let fold f a q =
  mutexing q.mod_rights (fun () -> Queue.fold f a q.q)
    
    
exception Empty


let take q =
  mutexing q.mod_rights (fun () ->
			   try
			     Queue.take q.q
			   with Queue.Empty -> raise Empty)


let take_next q =
  mutexing q.mod_rights (fun () ->
			   while Queue.is_empty q.q do
			     (* unlocks mutex and blocks *)
			     Condition.wait q.addition q.mod_rights
			       (* mutex now locked again *)
			   done;
			   Queue.take q.q)


(* EOF *)
