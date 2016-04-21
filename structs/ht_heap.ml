(**

   hybridization of a hash table and a binary heap.  

   uses the hash table to find items quickly and the binary heap to
   retrieve certain items in quality order quickly.  Backed by a picky
   queue to allow maximum flexibility.

*)

type 'c node =
{
  item : 'c;
  quality: float array;
  mutable stored_quality : float;
  mutable index: int;
}

type ('a,'key) pq_var = 
{
  heap: 'a node Picky_queue.picky_queue;
  hash: ('key,'a node) Htable.t;
  hash_key : 'a -> 'key;
  mutable metric: float array -> float;
}


let wrap f = 
  (fun n -> f n.item)

let wrap2 f = 
  (fun n1 n2 -> f n1.item n2.item)

let make_comparator metric = 
  fun n1 n2 -> (metric n1.quality) <= (metric n2.quality)

(*

  create

  push - adds something
  pop - removes one thing

  remove_item - removes one particular something
  member - checks to see if something is a member

*)

let member ht item = 
  Htable.mem ht.hash (ht.hash_key item)


let has_more ht = 
  not(Picky_queue.empty_p ht.heap)


let for_all ht eval = 
  Picky_queue.for_all ht.heap (wrap eval)


let check_index ht = 
  Picky_queue.check_index ht.heap (fun n -> n.index)


let check ht = 
  let c1 = (Htable.length ht.hash) = (Picky_queue.count ht.heap) in
  let c2 = Picky_queue.for_all ht.heap (fun n -> Htable.mem ht.hash
					  (ht.hash_key n.item)) in
  let c4 = check_index ht in
  let c3 = ref true in
    Htable.iter (fun _ n -> 
		   if(n.index > (Picky_queue.count ht.heap)) then
		     failwith (Printf.sprintf 
				 "index %d heap count %d ht ct %d" 
				 n.index 
				 (Picky_queue.count ht.heap)
				 (Htable.length ht.hash));
		   if(n == (Picky_queue.access_at ht.heap n.index))
		   then ()
		   else c3 := false
		) ht.hash;
    
    c2 && c1 && !c3 && c4

let create 
    ?(resize = true) ?(pickiness = None)
    (default: 'a) 
    (hash_function: 'b -> int) 
    (hash_equal: 'b -> 'b -> bool) 
    initial_size 
    (metric: float array -> float)
    (hash_key: 'a -> 'b) =
  {
    heap = (Picky_queue.create 
	      ~update_function:(fun n index -> n.index <- index)
	      ~resize:resize ~pickiness:pickiness
	      (make_comparator metric) (fun a -> metric a.quality) 
	      initial_size
	      {item = default;
	       quality = [|-1.0|];
	       stored_quality= (-1.0);
	       index = (-1); });
    hash = Htable.create (hash_function)
      (hash_equal) initial_size;
    hash_key = hash_key;
    metric = metric;
  }

let push (ht:('a,'b) pq_var) (item:'a) (quality:float array) =
  let new_node = {item = item;
		  quality = quality;
		  stored_quality = ht.metric quality;
		  index = (-1)} in
(*
  let insert_thing () = 
    (
      let ihtc = Htable.length ht.hash in
      let ihc = Picky_queue.count ht.heap in
	assert(ihtc = ihc);
	Htable.replace ht.hash (ht.hash_key new_node.item) new_node;
	assert(Picky_queue.insert ht.heap new_node = []);
	let htc = Htable.length ht.hash in
	let hc = Picky_queue.count ht.heap in
	  if(ihtc != htc - 1) then
	    failwith (Printf.sprintf "old ht size %d new ht size %d" ihtc htc);
	  if(ihc != hc - 1) then
	    failwith (Printf.sprintf "old heap size %d new ht size %d" ihc hc);

	  if(htc != hc) then
	    failwith (Printf.sprintf "ht size %d heap size %d" htc hc);
    ) in 
*)
    if(Htable.mem ht.hash (ht.hash_key item)) then
      (if((ht.metric new_node.quality) < 
	    (ht.metric (Htable.find ht.hash (ht.hash_key item)).quality)) then
	 (
	   (*find the old one*)
	   let old_node = (Htable.find ht.hash (ht.hash_key item)) in
	     Htable.replace ht.hash (ht.hash_key new_node.item) new_node;
	     let removed_nodes = (Picky_queue.replace_at ht.heap new_node
				    old_node.index) in
	       assert (removed_nodes = []);
	 )
       else 
	 ())
    else
      (
	Htable.replace ht.hash (ht.hash_key new_node.item) new_node;
	let removed_nodes = (Picky_queue.insert ht.heap new_node) in
	  List.iter 
	    (fun a -> Htable.remove ht.hash (ht.hash_key a.item)) 
	    removed_nodes;
      )

let pop ht = 
  let to_remove = Picky_queue.extract_first ht.heap in
    Htable.remove ht.hash (ht.hash_key to_remove.item);
    to_remove.item

let remove_item ht item = 
  try (
    let a = Htable.find ht.hash (ht.hash_key item) in
      Htable.remove ht.hash (ht.hash_key item);
      assert((Picky_queue.count ht.heap) >= a.index);
      ignore(Picky_queue.remove_at ht.heap a.index);)
  with
      Not_found -> ()


let cheap_iter ht process_to_do = 
  Htable.iter (fun _ b -> (wrap process_to_do) b) ht.hash


