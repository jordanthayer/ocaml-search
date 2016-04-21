(** A doubly-linked skiplist.  Has order of operations that are the
    same as a balanced binary tree (in expectation), except for
    removal which is in constant time.

    @author eaburns
    @since 2010-06-22
*)

open Printf

type ('key, 'data) t = {
  mutable head : ('key, 'data) entry option;
  mutable tail : ('key, 'data) entry option;
  mutable size : int;

  cmp : 'key -> 'key -> int;
  (* comparison function on keys. *)

  mutable next_id : int;
  (* used to assign id's to entries for debugging printing. *)
}

and ('key, 'data) entry = {
  mutable height : int;
  (* height only changes for the head and tail entries. *)

  mutable prev : ('key, 'data) entry array;
  mutable next : ('key, 'data) entry array;
  mutable key : 'key;
  data : 'data;

  id : int;
  (* used for debugging printing. *)
}

exception Empty


let random_height () =
  (** [random_height ()] get a random height. *)
  let bits = ref (Random.bits ()) and h = ref 1 in
    while (!bits land 0x1) = 1 do
      if (!h mod 30) == 0 then bits := Random.bits ();
      bits := !bits lsr 1;
      incr h;
    done;
    !h


let grow_entry e prev_ent next_ent h =
  (** [grow_entry e prev_ent next_ent h] grows the given entry of the
      list. *)
  assert (h > e.height);
  let prev = e.prev and next = e.next in
  let p = if prev <> [||] then Array.create h prev_ent else prev
  and n = if next <> [||] then Array.create h next_ent else next in
    for i = 0 to e.height - 1 do
      if p <> [||] then p.(i) <- prev.(i);
      if n <> [||] then n.(i) <- next.(i);
    done;
    e.prev <- p;
    e.next <- n;
    e.height <- h


let consider_growing sl h =
  (** [consider_growing sl h] considers growing the height of the head
      and tail entries of the skiplist to height [h]. *)
  match sl.head, sl.tail with
    | Some head, Some tail ->
	if head.height < h
	then begin
	  grow_entry head head tail h;
	  grow_entry tail head tail h;
	end
    | _, _ -> invalid_arg "consider_growing: no head or tail entries"


let rec find_previous cmp head tail first key h =
  (** [find_previous cmp head tail first key h] finds the entry that
      comes before [key] in the skiplist at height [h] beginning with
      entry [first]. *)
  assert (h >= 0);
  let next = first.next.(h) in
    if next == tail || (cmp key next.key) <= 0
    then first
    else find_previous cmp head tail next key h


let remove_entry e =
  (** [remove_entry e] removes the given entry from the skiplist. *)
  let prev = e.prev and next = e.next in
    for h = 0 to e.height - 1 do
      let p = prev.(h) and n = next.(h) in
	p.next.(h) <- n;
	n.prev.(h) <- p;
    done


let insert_entry cmp head tail e =
  (** [insert_entry cmp head tail e] inserts the given entry into the
      skiplist. *)
  let p = ref head in
  let key = e.key and height = e.height in
  let prev = e.prev and next = e.next in
    for h = head.height - 1 downto 0 do
      p := find_previous cmp head tail !p key h;
      if h < height
      then begin
	let p = !p in
	let pnext = p.next in
	next.(h) <- pnext.(h);
	prev.(h) <- p;
	pnext.(h).prev.(h) <- e;
	pnext.(h) <- e;
      end
    done


let create cmp =
  (** [create cmp] creates a new skiplist that orders elements in
      increasing order of [cmp]. *)
  {
    head = None;
    tail = None;
    size = 0;
    cmp = cmp;
    next_id = 0;
  }


let size sl = sl.size
  (** [size sl] gets the size of the skiplist. *)


let is_empty sl = sl.size = 0
  (** [is_empty sl] tests if the skiplist is empty. *)


let head_tail sl h key data =
  (** [head_tail sl h key data] creates a head and tail entry
      if the do not already exist.  These 'dummy' nodes will be
      created using [key] as their key and [data] as their data.
      These fields of the head and tail nodes are never used but this
      makes it so the user doesn't have to pass in an 'initial key'
      and 'initial data' upon structure creation. *)
  match sl.head, sl.tail with
    | None, None ->
	let head = { key = key;
		     data = data;
		     height = h;
		     prev = [||];
		     next = [||];
		     id = ~-1;
		   } in
	let tail = { key = key;
		     data = data;
		     height = h;
		     prev = [||];
		     next = [||];
		     id = ~-2;
		   }
	in
	  head.next <- Array.create h tail;
	  tail.prev <- Array.create h head;
	  sl.head <- Some head;
	  sl.tail <- Some tail;
	  head, tail
    | Some head, Some tail -> head, tail
    | _, _ -> invalid_arg "head_tail: one of head or tail exists but not both"


let prev_entry e =
  (** [prev_entry e] gets the previous entry on the list. *)
  e.prev.(0)


let next_entry e =
  (** [next_entry e] gets the next entry on the list. *)
  e.next.(0)


let find_entry_or_next sl key =
  (** [find_entry_or_next sl key] returns the data and entry for the
      first entry matching [key] or the next entry after the location
      that [key] would belong if there are no entries matching [key].

      raise [Not_found] if the list has yet to have an element
      inserted into it yet.  *)
  let rec do_find cmp head tail first h =
    if h < 0
    then tail
    else begin
      let p = find_previous cmp head tail first key h in
	if p.next.(h) == tail
	then do_find cmp head tail first (h - 1)
	else begin
	  let c = cmp p.next.(h).key key in
	    if c = 0 || (h = 0 && c > 0)
	    then p.next.(h)
	    else do_find cmp head tail first (h - 1)
	end
    end
  in
    match sl.head, sl.tail with
      | Some head, Some tail when sl.size > 0 ->
	  do_find sl.cmp head tail head (head.height - 1)
      | _, _ -> raise Not_found


let prev_entry e =
  (** [prev_entry e] gets the previous entry on the list. *)
  e.prev.(0)


let next_entry e =
  (** [next_entry e] gets the next entry on the list. *)
  e.next.(0)


let find_entry_or_next sl key =
  (** [find_entry_or_next sl key] returns the data and entry for the
      first entry matching [key] or the next entry after the location
      that [key] would belong if there are no entries matching [key].

      raise [Not_found] if the list has yet to have an element
      inserted into it yet.  *)
  let rec do_find cmp head tail first h =
    if h < 0
    then tail
    else begin
      let p = find_previous cmp head tail first key h in
	if p.next.(h) == tail
	then do_find cmp head tail first (h - 1)
	else begin
	  let c = cmp p.next.(h).key key in
	    if c = 0 || (h = 0 && c > 0)
	    then p.next.(h)
	    else do_find cmp head tail first (h - 1)
	end
    end
  in
    match sl.head, sl.tail with
      | Some head, Some tail when sl.size > 0 ->
	  do_find sl.cmp head tail head (head.height - 1)
      | _, _ -> raise Not_found


let prev_entry e =
  (** [prev_entry e] gets the previous entry on the list. *)
  e.prev.(0)


let next_entry e =
  (** [next_entry e] gets the next entry on the list. *)
  e.next.(0)


let find_entry_or_next sl key =
  (** [find_entry_or_next sl key] returns the data and entry for the
      first entry matching [key] or the next entry after the location
      that [key] would belong if there are no entries matching [key].

      raise [Not_found] if the list has yet to have an element
      inserted into it yet.  *)
  let rec do_find cmp head tail first h =
    if h < 0
    then tail
    else begin
      let p = find_previous cmp head tail first key h in
	if p.next.(h) == tail
	then do_find cmp head tail first (h - 1)
	else begin
	  let c = cmp p.next.(h).key key in
	    if c = 0 || (h = 0 && c > 0)
	    then p.next.(h)
	    else do_find cmp head tail first (h - 1)
	end
    end
  in
    match sl.head, sl.tail with
      | Some head, Some tail when sl.size > 0 ->
	  do_find sl.cmp head tail head (head.height - 1)
      | _, _ -> raise Not_found


let prev_entry e =
  (** [prev_entry e] gets the previous entry on the list. *)
  e.prev.(0)


let next_entry e =
  (** [next_entry e] gets the next entry on the list. *)
  e.next.(0)


let find_entry_or_next sl key =
  (** [find_entry_or_next sl key] returns the data and entry for the
      first entry matching [key] or the next entry after the location
      that [key] would belong if there are no entries matching [key].

      raise [Not_found] if the list has yet to have an element
      inserted into it yet.  *)
  let rec do_find cmp head tail first h =
    if h < 0
    then tail
    else begin
      let p = find_previous cmp head tail first key h in
	if p.next.(h) == tail
	then do_find cmp head tail first (h - 1)
	else begin
	  let c = cmp p.next.(h).key key in
	    if c = 0 || (h = 0 && c > 0)
	    then p.next.(h)
	    else do_find cmp head tail first (h - 1)
	end
    end
  in
    match sl.head, sl.tail with
      | Some head, Some tail when sl.size > 0 ->
	  do_find sl.cmp head tail head (head.height - 1)
      | _, _ -> raise Not_found


let insert sl key data =
  (** [insert sl key data] inserts [data] into the skiplist with key
      [key]. *)
  let h = random_height () in
  let head, tail = head_tail sl h key data in
  let e = { key = key;
	    data = data;
	    height = h;
	    prev = Array.create h head;
	    next = Array.create h tail;
	    id = sl.next_id;
	  }
  in
    sl.next_id <- sl.next_id + 1;
    consider_growing sl h;
    insert_entry sl.cmp head tail e;
    sl.size <- sl.size + 1;
    e


let remove sl e =
  (** [remove sl e] removes an entry from the skiplist. *)
  remove_entry e;
  sl.size <- sl.size - 1


let take_first sl =
  (** [take_first sl] removes the first entry in the list and
      returns its (key, data).  Raises [Empty] if the skiplist is
      empty. *)
  match sl.head, sl.tail with
    | Some head, Some tail when sl.size > 0 ->
	let e = head.next.(0) in
	  assert (e != tail);
	  remove_entry e;
	  sl.size <- sl.size - 1;
	  e.key, e.data
    | _, _ -> raise Empty


let update_key sl e new_key =
  (** [update_key sl e new_key] updates the key for entry [e].  This
      puts [e] in the correct position for the new key. *)
  let head, tail = head_tail sl e.height new_key e.data in
    remove_entry e;
    e.key <- new_key;
    insert_entry sl.cmp head tail e


let find sl key =
  (** [find sl key] returns the data and entry for the first entry
      matching [key].  Raises Not_found if the entry is not found. *)
  let e = find_entry_or_next sl key in
    if (sl.cmp e.key key) = 0
    then e, e.data
    else raise Not_found


let iter f sl =
  (** [iter f sl] iterates over the skiplist in increasing order of
      keys calling [f] with each key/data pair. *)
  match sl.head, sl.tail with
    | Some head, Some tail when sl.size > 0 ->
	let p = ref head.next.(0) in
	  while !p != tail do
	    f !p.key !p.data;
	    p := !p.next.(0);
	  done
    | _, _ -> ()


let iter_rev f sl =
  (** [iter_rev f sl] iterates over the skiplist in decreasing order
      of keys calling [f] with each key/data pair. *)
  match sl.head, sl.tail with
    | Some head, Some tail when sl.size > 0 ->
	let p = ref tail.prev.(0) in
	  while !p != head do
	    f !p.key !p.data;
	    p := !p.prev.(0);
	  done
    | _, _ -> ()


let iter_from f sl e =
  (** [iter_from f sl e] iterates [f] forward from [e]. *)
  match sl.head, sl.tail with
    | Some head, Some tail when sl.size > 0 ->
	let p = ref (if e == head then e.next.(0) else e) in
	  while !p != tail do
	    f !p.key !p.data;
	    p := !p.next.(0);
	  done
    | _, _ -> ()


let iter_from_rev f sl e =
  (** [iter_from_rev f sl e] iterates [f] backward from [e]. *)
  match sl.head, sl.tail with
    | Some head, Some tail when sl.size > 0 ->
	let p = ref (if e == tail then e.prev.(0) else e) in
	  while !p != head do
	    f !p.key !p.data;
	    p := !p.prev.(0);
	  done
    | _, _ -> ()


let fold f sl init =
  (** [fold f sl init] folds [f] across the skiplist evaluating on
      each element in increasing order. *)
  let v = ref init in
    iter (fun key data -> v := f init key data) sl;
    !v


let print sl key_str outchan =
  (** [print sl key_str outchan] prints the skiplist to the given
      output channel (for debugging). *)
  match sl.head, sl.tail with
    | Some head, Some tail when sl.size > 0 ->
	let ent = ref head in
	let break = ref false in
	  while not !break do
	    let e = !ent in
	      if e.prev <> [||]
	      then begin
		fprintf outchan "<- %4d (%s) " e.id (key_str e.key);
		for h = 0 to e.height - 1 do
		  fprintf outchan " %4d" e.prev.(h).id;
		done;
		fprintf outchan "\n"
	      end;
	      if e.next <> [||]
	      then begin
		fprintf outchan "-> %4d (%s) " e.id (key_str e.key);
		for h = 0 to e.height - 1 do
		  fprintf outchan " %4d" e.next.(h).id;
		done;
		fprintf outchan "\n"
	      end;
	      if e == tail
	      then break := true
	      else ent := e.next.(0);
	  done
    | _, _ -> fprintf outchan "<empty>"
