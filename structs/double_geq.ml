(* $Id: geq.ml,v 1.4 2006/08/14 18:59:27 ruml Exp ruml $

   "good enough queue" for A*_epsilon

   a combination of a balanced binary tree (sorted by f and tracking the
   worst f within epsilon of the best f) and a synchronized priority queue
   sorted by d.  any node in the binary tree that falls within the f bound
   is also in the priority queue.

   this makes it fast to insert a node (add to queue also if below bound,
   easy to update bound) and remove the appropriate node (structure in
   queue must point into tree for easy removal)

   possible speed-up: remember node at boundary?  Nah - too complicated.
*)

let in_doset_not_dpq = -42
and being_deleted = -17

type 'a t = {
  tree : 'a Doset.t;
  q : ('a Doset.node) Dpq.t;
  preferred : ('a Doset.node) Dpq.t;
  (* track best in tree (ie, quality, not necessarily best in q) *)
  mutable best : 'a option;
  mutable close_enough : ('a -> 'a -> bool);
  quality_pred : ('a -> 'a -> bool);
  equals : ('a -> 'a -> bool);
  (* for the q *)
  get_pos : ('a Doset.node -> int);
  set_pos : ('a Doset.node -> int -> unit);
  is_preferred : ('a Doset.node -> bool);
  printer : (out_channel -> 'a -> unit);
}


type 'a entry = 'a Doset.node

(************ Illusions *************)

let make_dummy_entry () =
  let t = Doset.make_with
    (fun _ _ -> failwith "Geq.make_dummy_entry") 0 in
  Obj.magic (Doset.insert_node t 0)


(************ creation **************)


let create_with ?(equals = (fun a b -> a == b)) ?(pr = (fun _ _ -> ()))
    quality_pred convenience_pred close_nuf setpos getpos
    is_preferred dummy =
  let tree = Doset.make_with quality_pred dummy
  and c_nodes_pred a b = convenience_pred (Doset.data a) (Doset.data b)
  and notifier n i = setpos (Doset.data n) i in
  let dummy_node = Doset.insert_node tree dummy in
    Doset.delete tree dummy_node;
    { tree = tree;
      q = Dpq.create c_nodes_pred notifier 510 dummy_node;
      preferred = Dpq.create c_nodes_pred notifier 510 dummy_node;
      best = None;
      close_enough = close_nuf;
      quality_pred = quality_pred;
      equals = equals;
      get_pos = (fun n -> getpos (Doset.data n));
      set_pos = (fun n i -> setpos (Doset.data n) i);
      is_preferred = (fun n -> is_preferred (Doset.data n));
      printer = pr;}


let clear t =
  Doset.clear t.tree;
  Dpq.clear t.q

let data n =
  Doset.data n


let print geq =
  Verb.pe Verb.always "Dpq(%i):\t" (Dpq.count geq.q);
  Dpq.print geq.q (fun ch n -> (geq.printer ch (data n)));
  Verb.pe Verb.always "\nDoset(%i)\n"(Doset.count geq.tree);
  Doset.print_tree geq.tree geq.printer;
  Verb.pe Verb.always "\n"


let count t =
  Doset.count t.tree

let ge_count t =
  Dpq.count t.q


let ge_iter f t =
  let iterator,reset = Dpq.make_iterator_unsafe t.q in
  let rec iterate ele =
    match ele with
	Some e -> (f (data e);
		   iterate (iterator ()))
      | None -> () in
    iterate (iterator ())


let ge_iter2 f t =
  let best = Doset.data (Doset.min t.tree) in
    Doset.visit_interval (fun _ -> true) (fun n -> t.close_enough best n)
      f t.tree


let empty_p t = Doset.empty_p t.tree


(* some validation tests *)

let validate t =
  match t.best with
      None -> ()
    | Some min ->
	Dpq.iter (fun b ->
		    if not (t.close_enough min (data b))
		    then failwith
		      "Geq.validate: node not good enough in dpq") t.q;

	Doset.visit_interval
	  (fun a -> true)
	  (fun a -> true)
	  (fun b ->
	     if (t.close_enough min (data b)) & ((t.get_pos b) < 0)
	     then failwith "Geq.validate: good enough node not in dpq")
	  t.tree



let pos_validate t p =
  try validate t
  with Failure str ->
    failwith (Wrutils.str "Geq.pos_validate- %s: %s" p str)

(********************* insert ****************)


let adjust_for_new_best t old_best new_best new_better =
  (** given new best data, add or remove nodes from q.  on entry, t.best
      has obsolete value. *)
  t.best <- Some new_best;
  let close_p = t.close_enough in
  let close_new = close_p new_best
  and close_old = close_p old_best in
    if new_better
      (* new is better then old (eg, new node inserted).  remove nodes
	 that are close_enough to old but not to new. *)
    then(Doset.visit_interval
	   (fun x -> not (close_new x)) (* sats_lower *)
    	   (fun x -> close_old x)       (* sats_upper *)
	   (* we know it is in the q *)
	   (fun n ->
	      let i = t.get_pos n in
	      let pref = t.is_preferred n in
		if i >= 0
		then (if pref
		      then Dpq.remove t.preferred i
		      else Dpq.remove t.q i;
		      t.set_pos n in_doset_not_dpq))
	   t.tree)
      (* new worse than old (eg, best was removed).  add nodes that
	 are close_enough to new but not to old. *)
    else
      (Doset.visit_interval
	 (*(fun _ -> true)*)
	 (fun x -> not (close_old x)) (* sats lower *)
	 (fun x -> close_new x) (* this is the problem *)
	 (fun n ->
	    let i = t.get_pos n in
	    if i < 0
	    then (if t.is_preferred n
		  then Dpq.insert t.preferred n
		  else Dpq.insert t.q n))
	 t.tree)

(* Iteraties over the collection in order *)
let in_order_iter f t =
  Doset.unsafe_iter (fun n -> f n) t.tree


let iter f t =
  Doset.iter f t.tree

let raw_iter f t =
  Doset.raw_iter f t.tree


let insert t data =
  (*Verb.pe Verb.debug "Inserting into doset\n";*)
  let node = Doset.insert_node t.tree data in
    (*Verb.pe Verb.debug "Setting Position\n";*)
    t.set_pos node in_doset_not_dpq;
    (*Verb.pe Verb.debug "Considering Insertion into Queue.\n";*)
    (match t.best with
	 None -> (if t.is_preferred node
		  then Dpq.insert t.preferred node
		  else Dpq.insert t.q node;
		  t.best <- Some data)
       | Some b -> (if t.close_enough b data
		    then (if t.is_preferred node
			  then Dpq.insert t.preferred node
			  else Dpq.insert t.q node;
			  (if not (t.quality_pred b data)
			   then (if (t.quality_pred data b)
				 then adjust_for_new_best t b data true)))));
    node


let insert_raw t node =
  t.set_pos node in_doset_not_dpq;
  (match t.best with
       None ->
	 (if t.is_preferred node
	  then Dpq.insert t.preferred node
	  else Dpq.insert t.q node;
	  t.best <- Some data)
     | Some b ->
	 if t.close_enough b data
	 then (if t.is_preferred node
	       then Dpq.insert t.preferred node
	       else Dpq.insert t.q node;
	       (if not (t.quality_pred b data)
		then (if (t.quality_pred data b)
		      then adjust_for_new_best t b data true))));
  node


let update_close_enough geq pred =
  (* Assumes that the new predicate is tighter! *)
  geq.close_enough <- pred;
  match geq.best with
      Some best ->
	(Dpq.iter (fun ele -> geq.set_pos ele in_doset_not_dpq) geq.q;
	 Dpq.iter (fun ele -> geq.set_pos ele in_doset_not_dpq) geq.preferred;
	 Dpq.clear geq.q;
	 Dpq.clear geq.preferred;
	 Doset.visit_interval
	   (fun _ -> true)
	   (fun x -> pred best x)
	   (fun n -> if geq.is_preferred n
	    then Dpq.insert geq.preferred n
	    else Dpq.insert geq.q n) geq.tree)
    | _ -> assert (empty_p geq)

(*********** remove *****************)

let how_many node geq =
  let to_ret = ref 0 in
    Doset.iter
      (fun rbn -> if (geq.equals rbn (data node)) then to_ret := !to_ret + 1)
      geq.tree;
    !to_ret


let disp_doset_node t n =
  t.printer stderr (data n);
  Verb.pe Verb.debug "\tlc: ";
  t.printer stderr (data (Doset.get_left_child n));
  Verb.pe Verb.debug "\trc: ";
  t.printer stderr (data (Doset.get_right_child n));
  Verb.pe Verb.debug "\tp: ";
  t.printer stderr (data (Doset.get_parent n));
  Verb.pe Verb.debug " :\n"


let remove_from_tree t n =
  (** assumes already removed from q as necessary *)
  (*Verb.pe Verb.debug "Checking before remove from tree\n";
  Doset.check_no_count t.tree;*)
  Doset.delete t.tree n;
  (*Verb.pe Verb.debug "Checking After remove_from_tree delete\n%!";
  Doset.check_no_count t.tree;*)
  match t.best with
      None -> failwith "Geq.remove: had already lost track of best"
    | Some b ->
	if (data n) == b
	then ((*Verb.pe Verb.debug "Removing best from tree\n";*)
	      if Doset.empty_p t.tree
	      then t.best <- None
	      else
		(let md = data (Doset.min t.tree) in
		   if ((not (t.quality_pred b md)) &&
			 (not (t.quality_pred md b)))
		   then t.best <- Some md
		   else adjust_for_new_best t b md false))
	(*else Verb.pe Verb.debug "not removing best from tree\n"*)


let remove t n =
  let q = t.q
  and p = t.preferred
  and i = t.get_pos n in
    match t.best with
      | None -> failwith "Removing from empty geq"
      | Some best ->
	  (* remove from q if it's in q *)
	  (if i >= 0
	   then (if t.is_preferred n
		 then (assert ((Dpq.get_at p i) == n);
		       Dpq.remove p i)
		 else (assert ((Dpq.get_at q i) == n);
		       Dpq.remove q i));
	   t.set_pos n being_deleted;
	   remove_from_tree t n)


let swap t old next =
  remove t old;
  insert t next


let peek_best t =
  if not (Dpq.empty_p t.preferred)
  then data (Dpq.peek_first t.preferred)
  else data (Dpq.peek_first t.q)

let peek_doset t =
  match t.best with
    | None -> failwith "empty"
    | Some v -> v


let remove_doset t =
  let n = Doset.min t.tree in
    (* Being the minimum element of the doset guarantees this *)
    if t.is_preferred n
    then Dpq.remove t.preferred (t.get_pos n)
    else Dpq.remove t.q (t.get_pos n);
    t.set_pos n being_deleted;
    remove_from_tree t n;
    (*Verb.pe Verb.debug "Checking after remove best\n";
    Doset.check_no_count t.tree;*)
    data n


let remove_best t =
  let n = (if not (Dpq.empty_p t.preferred)
	   then Dpq.peek_first t.preferred
	   else Dpq.peek_first t.q) in
    remove t n;
    data n


let remove_best_raw t =
  let n = Dpq.extract_first t.q in
    remove_from_tree t n;
    n


let clear geq =
  Doset.clear geq.tree;
  Dpq.clear geq.q;
  geq.best <- None


let rec to_list geq updater =
  if (count geq) = 0
  then (clear geq;[])
  else ((*Verb.pe Verb.debug "Geq not empty\n";
	  Verb.pe Verb.debug "size before remove %i\n" (count geq);*)
    let v = remove_best geq in
      updater v;
      (*	  Verb.pe Verb.debug "size after remove %i\n" (count geq);*)
      v :: to_list geq updater)


let resort geq see_insert updater =
  let rec fill_geq lst =
    match lst with
	[] -> ()
      | hd::tl -> (see_insert hd (insert geq hd);
		   fill_geq tl)
  in
    (*Verb.pe Verb.debug "resorting geq of size %i\n" (count geq);*)
    let lst = to_list geq updater in
      fill_geq lst
      (*Verb.pe Verb.debug "resorted geq of size %i\n" (count geq);
	Verb.pe Verb.debug "Doset was correct after resort\n"*)


(* EOF *)
