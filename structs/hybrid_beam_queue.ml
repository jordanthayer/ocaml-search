(**
   queue that dispenses items one at a time, giving out the best
   item.  Allows items to be stratified according to an integer.
*)


type 'a layer = 
{
  data : 'a; (**)
  level: int;
  mutable exp_count : int; (*tracks how many have been pulled from
			     this level*)
}


type 'a hybrid_beam_queue = 
{
  default : 'a;
  queue : (('a Mmh.pq_var) layer) Mmh.pq_var; (* the queue from which items
					  are removed*)
  level_max : int; (* how many are allowed in each level *)
  mutable current_min : int; (*if some levels are sealed off, prunes
			       the nodes in them*)
  comparator: 'a -> 'a -> bool;
  update : 'a -> int -> unit;
  mutable heap_count: int;
  depth_map : int Garray.t; (* array used to find specific elements.
			   Where are the nodes at depth X? *)
  reduce_capacity : bool;
}


let empty_p hbq = 
  hbq.heap_count = 0


let make_layer update_function comparator capacity default level = 
  {
    data = Mmh.create ~update_function:update_function comparator
      capacity default;
    level = level;
    exp_count = 0;
  }


let create 
    ?(update_function = Fn.no_op2) 
    ?(reduce_capacity = true)
    comparator capacity default
    = 
  let initial_depth = 10 in
  let (initial:'a layer) = make_layer update_function comparator 
    capacity default (-1) in
  let cmp = (fun a b -> 
	       if((Mmh.empty_p a.data) && (Mmh.empty_p b.data)) then true
	       else if ((Mmh.empty_p a.data) && (not (Mmh.empty_p b.data))) then false
	       else if ((not (Mmh.empty_p a.data)) && (Mmh.empty_p b.data)) then true
	       else (
	       let i1 = Mmh.peek_first a.data in
	       let i2 = Mmh.peek_first b.data in
		 comparator i1 i2)) in
  let depth_map = Garray.make ~init_size:initial_depth (-1) in
  let update = (fun layer index -> let depth = layer.level in
		  Garray.set depth_map depth index) in
    {
      default = default;
      queue = (Mmh.create ~update_function:update ~resize:true
		 cmp initial_depth
		 (initial:'a layer));
      current_min = 0;
      level_max = capacity;
      comparator = comparator;
      update = update_function;
      heap_count = 0;
      depth_map= depth_map;
      reduce_capacity = reduce_capacity;
    }


let insert (hbq:'a hybrid_beam_queue) (item:'a) (strata:int) = 
  if (strata < 0) then
    (
      Verb.pe Verb.always "trying to insert into strata %d\n" strata;
      failwith "invalid insert";
    );
  let heap_index = Garray.get ~init_fun:(fun _ -> -1) hbq.depth_map strata in
    (*if the layer is missing, create it.*)
    if (heap_index = -1) then 
      (
	let (to_insert: 'a Mmh.pq_var layer) = 
	  make_layer 
	    hbq.update
	    hbq.comparator
	    hbq.level_max
	    hbq.default
	    strata in
	  (*this list had better be empty*)
	let result = Mmh.insert to_insert.data item in
	  assert (result = []);
	  let second_result = Mmh.insert hbq.queue to_insert in
	    hbq.heap_count <- hbq.heap_count + 1;
	    assert(second_result = []);
	    result;
      )
    else 
      (
	let heap_index = Garray.get ~init_fun:(fun _ -> -1) 
	  hbq.depth_map strata in
	let this_layer = Mmh.access_at hbq.queue heap_index in
	  assert (this_layer.level = strata);
	  let kicked_out = Mmh.insert this_layer.data item in
	    Mmh.update_index hbq.queue heap_index;
	    if(kicked_out != []) then 
	      (
		hbq.heap_count <- 
		  (hbq.heap_count + 1 - (List.length kicked_out));
	      )
	    else 
	      (hbq.heap_count <- hbq.heap_count + 1;);
	    (kicked_out: 'a list))


let extract_first (hbq:'a hybrid_beam_queue) = 
  (*peek at the first item
    make sure it isn't empty
    pop the item out of it
    push it down the heap
  *)
  if(hbq.heap_count = 0) then failwith "hybrid beam queue underflow";
  if(Mmh.empty_p hbq.queue) then failwith "no layers in the hybrid beam queue";
  let this_layer = Mmh.peek_first hbq.queue in
    assert (not (Mmh.empty_p this_layer.data));
    let to_return = Mmh.extract_first this_layer.data in
      if (hbq.reduce_capacity) then 
	Mmh.reduce_capacity this_layer.data;

      this_layer.exp_count <- this_layer.exp_count + 1;
      Mmh.update_index hbq.queue 1;
      hbq.heap_count <- hbq.heap_count - 1;
      to_return


let remove_at (hbq:'a hybrid_beam_queue) (strata:int)
    (heap_position:int) = 
  (**given a strata and a heap position, finds the item there and
     removes it, returning the removed item.

     this function might require asserts to fail gracefully.
  *)
  let heap_index = Garray.get ~init_fun:(fun _ -> -1)
    hbq.depth_map strata in 
    hbq.heap_count <- hbq.heap_count - 1;
    assert (heap_index != (-1));
    let this_level = Mmh.access_at hbq.queue heap_index in
    let to_remove = Mmh.remove_at this_level.data heap_position in
      Mmh.update_index hbq.queue heap_index;
      to_remove


let rec check_major_index_helper (hbq:'a hybrid_beam_queue) index = 
  let mmh_level = (Mmh.safe_access_at hbq.queue index) in
    match mmh_level with
	None -> true
      | Some t -> (
	  let mh_strata = t.level in
	    if(mh_strata = -1) then true 
	    else
	      (
		let ga_val = (Garray.get hbq.depth_map mh_strata) in
		  if(ga_val != index) then 
		    (
		      failwith (Printf.sprintf "ga value %d heap value %d at index %d"
				  ga_val t.level index))
		  else 
		    check_major_index_helper hbq (index+1)
	      )
	)

let check_major_index (hbq:'a hybrid_beam_queue) = 
  check_major_index_helper hbq 1 

let check_minor_index (hbq:'a hybrid_beam_queue)
    (get_minor_index:'a->int) = 
  if (Mmh.for_all (hbq.queue) (fun a -> Mmh.check_index a.data
    get_minor_index))
  then true
  else failwith "heap is corrupted"

let check_counts (hbq:'a hybrid_beam_queue) = 
  let verified_counts = ref 0 in
  let items_in_queue = Mmh.count hbq.queue in
    for i = 1 to items_in_queue 
    do
      verified_counts := !verified_counts + 
	(Mmh.count (Mmh.access_at hbq.queue i).data);
    done;
    (!verified_counts = hbq.heap_count)

(*
  #use "use.ml";;

  let compare_tuple t1 t2 = 
  let value1,_ = t1 in
  let value2,_ = t2 in
  value1 < value2


  let update_tuple t new_index= 
  let _,index = t in
  index := new_index


let t = Hybrid_beam_queue.create (<) 3 0;;





  for i = 0 to 100 do
  let r1 = (Random.int 10) + 1 
  and r2 = (Random.int 20) + 1 in
  ignore(Hybrid_beam_queue.check_major_index t);
  Printf.fprintf stderr "%d %d\n" r1 r2;
  flush stderr;
  ignore (Hybrid_beam_queue.insert t r1 r2);
  ignore(Hybrid_beam_queue.check_major_index t);
  done;;





Hybrid_beam_queue.insert t 10 1;;
Hybrid_beam_queue.insert t 10 1;;
Hybrid_beam_queue.insert t 10 1;;
Hybrid_beam_queue.insert t 1 1;;
Hybrid_beam_queue.insert t 2 1;;
Hybrid_beam_queue.insert t 3 1;;

Hybrid_beam_queue.insert t 5 2;;
Hybrid_beam_queue.insert t 6 2;;
Hybrid_beam_queue.insert t 7 2;;
Hybrid_beam_queue.insert t 10 2;;
Hybrid_beam_queue.insert t 6 2;;
Hybrid_beam_queue.insert t 7 2;;


  Hybrid_beam_queue.extract_first t;;
  Hybrid_beam_queue.extract_first t;;
  Hybrid_beam_queue.extract_first t;;
  Hybrid_beam_queue.extract_first t;;
  Hybrid_beam_queue.extract_first t;;
  Hybrid_beam_queue.extract_first t;;



*)
