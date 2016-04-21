(*

  implementation of a min-max heap.  Provides constant time access to
  both the smallest and largest elements in the heap.  Logrithmic
  deletion for smallest and largest elements and for adding new
  elements (adding new elements is actually .5 log(n)).  This one is
  leaky and refuses to put things in that are too big once it reaches
  its capacity.

  Probably should be used with a capacity of at least three, I didn't
  do much to handle the degenerate cases, and they may cause the
  algorithm to break down.

*)


let log2 i = int_of_float ((log (float_of_int i)) /. log 2.)
let junk_function a b = ()

exception CapacityTooLow


type 'a pq_var = {
  mutable heap : 'a array;
  mutable heap_size: int;
  mutable heap_count: int;
  comparator: 'a -> 'a -> bool;
  update: 'a -> int -> unit;
  resize: bool;
}


let count mmh =
  mmh.heap_count


let reduce_capacity mmh =
  assert (mmh.heap_size > mmh.heap_count);
  mmh.heap_size <- mmh.heap_size - 1


let swap mh n1 n2 =
  let temp_node = mh.heap.(n1) in
    mh.heap.(n1) <- mh.heap.(n2);
    mh.heap.(n2) <- temp_node;
    mh.update mh.heap.(n1) n1;
    mh.update mh.heap.(n2) n2

(*
let verify_heap mh =
  let rec check_node node_index =
*)

exception EmptyList

let create ?(update_function = fun a b -> ()) ?(resize = false)
    comparator capacity default =
  {
    heap = Array.make (capacity +2) default;
    heap_size = capacity;
    heap_count = 0;
    comparator = comparator;
    update = update_function;
    resize = resize;
  }

let peek_first mh =
  assert (mh.heap_count > 0);
  mh.heap.(1)

let count mh = mh.heap_count

let empty_p mh = (mh.heap_count = 0)

let peek_largest mh =
  assert (mh.heap_count > 0);
    if (mh.heap_count = 1) then mh.heap.(1)
    else if (mh.heap_count = 2) then mh.heap.(2)
    else if (mh.comparator mh.heap.(2) mh.heap.(3)) then mh.heap.(3)
    else mh.heap.(2)


let get_descendents mh i =
  assert (mh.heap_count >= (i * 2));
  (** brings back the smallest child or grand child index *)
  let list_of_elements = (
    if (mh.heap_count >= i * 4 + 3) then
      (i*2)::
	(i*2+1)::
	(i*4)::
	(i*4+1)::
	(i*4+2)::
	(i*4+3)::
	[]
    else if (mh.heap_count = i * 4 + 2) then
      (i*2)::
	(i*2+1)::
	(i*4)::
	(i*4+1)::
	(i*4+2)::
	[]
    else if (mh.heap_count = i * 4 + 1) then
      (i*2)::
	(i*2+1)::
	(i*4)::
	(i*4+1)::
	[]
    else if (mh.heap_count = i * 4) then
      (i*2)::
	(i*2+1)::
	(i*4)::
	[]
    else if (mh.heap_count >= i * 2 + 1) then
      (i*2)::
	(i*2+1)::
	[]
    else
      (i*2)::
	[]
  ) in list_of_elements


let get_smallest mh i =
  let rec find_min_in_list lst =
    match lst with
	[] -> raise EmptyList
      | [v1] -> v1
      | hd::tl -> let mt = find_min_in_list tl in
	  if (mh.comparator mh.heap.(hd) mh.heap.(mt)) then hd
	  else mt
  in find_min_in_list (get_descendents mh i)



let get_largest mh i =
  let rec find_min_in_list lst =
    match lst with
	[] -> raise EmptyList
      | [v1] -> v1
      | hd::tl -> let mt = find_min_in_list tl in
	  if (mh.comparator mh.heap.(mt) mh.heap.(hd)) then hd
	  else mt
  in find_min_in_list (get_descendents mh i)


let rec trickle_down_min mh i =
  if (2 * i <= mh.heap_count) then
    (
      let m = get_smallest mh i in
	if (m > 2 * i + 1) (* m is a grand child *) then (
	  if (mh.comparator mh.heap.(m) mh.heap.(i)) then
	    (
	      swap mh i m;
	      if (mh.comparator mh.heap.(m/2) mh.heap.(m)) then
		(
		  swap mh m (m/2));
	      trickle_down_min mh m;
	    )
	)
	else (* m is a child *)
	  (
	    if (mh.comparator mh.heap.(m) mh.heap.(i)) then (
	      swap mh i m
	    )
	  )
    )


let rec trickle_down_max mh i =
  if (2 * i <= mh.heap_count) then
    (
      let m = get_largest mh i in
	if (m > 2 * i + 1) (* m is a grand child *) then (
	  if (mh.comparator mh.heap.(i) mh.heap.(m)) then
	    (
	      swap mh i m;
	      if (mh.comparator mh.heap.(m) mh.heap.(m/2)) then
		(
		  swap mh m (m/2));
	      trickle_down_max mh m;
	    )
	)
	else (* m is a child *)
	  (
	    if (mh.comparator mh.heap.(i) mh.heap.(m)) then (
	      swap mh i m
	    )
	  )
    )


let trickle_down mh i =
  if ((log2 i) mod 2 = 0) then trickle_down_min mh i
  else trickle_down_max mh i


let rec bubble_up_min mh i =
  if (i/4 != 0) then
    (
      if (mh.comparator mh.heap.(i) mh.heap.(i/4)) then
	(
	  swap mh i (i/4);
	  bubble_up_min mh (i/4);
	)
    )


let rec bubble_up_max mh i =
  if (i/4 != 0) then
    (
      if (mh.comparator mh.heap.(i/4) mh.heap.(i)) then
	(
	  swap mh i (i/4);
	  bubble_up_min mh (i/4);
	)
    )


let bubble_up mh i =
  if ((log2 i) mod 2 = 0) (* i is a min row *) then
    (
      if (i != 1 && mh.comparator mh.heap.(i/2) mh.heap.(i)) then
	(
	  swap mh i (i/2);
	  bubble_up_max mh (i/2);
	)
      else bubble_up_min mh i
    )
  else (* i is on a max row *)
    (
      if (i != 1 && mh.comparator mh.heap.(i) mh.heap.(i/2)) then
	(
	  swap mh (i) (i/2);
	  bubble_up_min mh (i/2);
	)
      else bubble_up_max mh i
    )


let update_index mh index =
  bubble_up mh index;
  trickle_down mh index


let rec insert mh item =
  (* there's room for the thing to be added *)
  if (mh.heap_count < mh.heap_size) then (
    mh.heap_count <- mh.heap_count + 1;
    mh.heap.(mh.heap_count) <- item;
    mh.update mh.heap.(mh.heap_count) mh.heap_count;
    bubble_up mh mh.heap_count;
    [];)
    (* there's no more room *)
  else if (mh.resize) then (
    (* if there is no more room and it is in resize mode, resize. *)
    let new_array = Array.make (mh.heap_size * 2 + 2) mh.heap.(0) in
      Array.blit mh.heap 0 new_array 0 (mh.heap_size + 2);
      mh.heap <- new_array;
      mh.heap_size <- mh.heap_size * 2;
      insert mh item;
  )
  else if (mh.heap_size = 0) then
    [item]
  else if (mh.comparator (peek_largest mh) item) then
    [item]
  else if (mh.heap_size = 1) then
    (
      if(mh.comparator mh.heap.(1) item) then
	[item]
      else
	(
	  let item_to_return = mh.heap.(1) in
	    mh.heap.(1) <- item;
	    mh.update mh.heap.(1) 1;
	    mh.update item_to_return (-1);
	    [item_to_return]
	)
    )
  else if (mh.heap_size = 2) then
    (
      if(mh.comparator mh.heap.(2) item) then
	[item]
      else
	(
	  let item_to_return = mh.heap.(2) in
	    mh.heap.(2) <- item;
	    mh.update mh.heap.(2) 2;
	    mh.update item_to_return (-1);
	    update_index mh 2;
	    [item_to_return]
	)
    )
  else (
    let index_to_kick = (if (mh.comparator mh.heap.(2) mh.heap.(3))
			 then 3 else 2) in
      (* have to replace the element in index 1 *)

      (*This does not look correct, must revisit this code.
	Does not looks like it adjusts the index of the item to be removed.*)
      if (mh.comparator item mh.heap.(1)) then(
	mh.update mh.heap.(index_to_kick) (-1);
	let kicked_out = mh.heap.(index_to_kick) in
	  mh.heap.(index_to_kick) <- mh.heap.(1);
	  mh.update mh.heap.(index_to_kick) index_to_kick;
	  mh.heap.(1) <- item;
	  mh.update mh.heap.(1) 1;
	  trickle_down mh index_to_kick;
	  [kicked_out];
      )
	(* dont have to replace the element in index 1 *)
      else (
	mh.update mh.heap.(index_to_kick) (-1);
	let kicked_out = mh.heap.(index_to_kick) in
	  mh.heap.(index_to_kick) <- item;
	  mh.update mh.heap.(index_to_kick) index_to_kick;
	  trickle_down mh index_to_kick;
	  [kicked_out];
      )
  )



let replace_at mh item index =
  assert (index <= mh.heap_count);
  assert (index > 0);
  mh.update mh.heap.(index) (-1);
  mh.heap.(index) <- item;
  mh.update mh.heap.(index) index;
  update_index mh index;
  []


let access_at mh index =
  assert (index <= mh.heap_count);
  assert (index > 0);
  mh.heap.(index)


let safe_access_at mh index =
  if(index < 0) then
    None
  else if (index >= Array.length mh.heap) then
    None
  else
    Some (mh.heap.(index))


let pop_any mh =
  assert (mh.heap_count > 0);
  let to_return = mh.heap.(mh.heap_count) in
    mh.heap.(mh.heap_count) <- mh.heap.(0);
    mh.heap_count <- mh.heap_count - 1;
    mh.update to_return (-1);
    to_return


let remove_at mh index =
  if(index > mh.heap_count) then
    failwith (Printf.sprintf "illegal pop %d %d" index mh.heap_count);
  assert (index <= mh.heap_count);
  assert (index > 0);
  if(index = mh.heap_count) then
    pop_any mh
  else (
    mh.update mh.heap.(index) (-1);
    let to_remove = mh.heap.(index) in
      mh.heap.(index) <- mh.heap.(mh.heap_count);
      mh.update mh.heap.(index) index;
      mh.heap.(mh.heap_count) <- mh.heap.(0);
      mh.heap_count <- mh.heap_count - 1;
      if(mh.heap_count > 3) then
	(
	  bubble_up mh index;
	  trickle_down mh index
	);
      to_remove)



let extract_first mh =
  assert (mh.heap_count > 0);
  let item_to_return = mh.heap.(1) in
    mh.heap.(1) <- mh.heap.(mh.heap_count);
    mh.update mh.heap.(1) 1;
    mh.heap.(mh.heap_count) <- mh.heap.(0);
    mh.heap_count <- mh.heap_count -1;
    trickle_down mh 1;
    mh.update item_to_return (-1);
    item_to_return


let extract_worst mh =
  assert (mh.heap_count > 0);
  let index_to_return =
    if (mh.heap_count = 1) then 1
    else if (mh.heap_count = 2) then 2
    else if (mh.comparator mh.heap.(2) mh.heap.(3)) then 3
    else 2 in
    assert (mh.heap_count >= index_to_return);
    remove_at mh index_to_return




let print_heap mh pf =
  Verb.pf Verb.always stdout "\n***** Printing Heap *****\n";
  for i = 1 to mh.heap_count do
    pf mh.heap.(i);
  done;
  Verb.pf Verb.always stdout "\n";;


let worst_item_quality _ =
  failwith "Mmh does not have a concept of quality"
let best_item_quality _ =
  failwith "Mmh does not have a concept of quality"


let for_all mh eval =
  let all_okay = ref true in
    for i = 1 to (mh.heap_count - 1) do
      if(eval mh.heap.(i)) then ()
      else all_okay := false
    done;
    !all_okay




let check_index mh get_index =
  let all_okay = ref true in
    for i = 1 to mh.heap_count do
      if(i = (get_index mh.heap.(i))) then ()
      else
	(
	  all_okay := false;
	  failwith (Printf.sprintf "heap index %d contains value %d"
		      i (get_index mh.heap.(i)));
	)
    done;
    !all_okay


(*
(*
  useful little print integer function
*)
let print_int i = Verb.pf Verb.always stdout " %d " i


(*
  testing main used to vertfy heap functionality.
*)
let _ = (
  let depq = create (fun x y -> x < y) 5 0 in
    insert depq 1;
    insert depq 10;
    insert depq 5;
    insert depq 3;
    insert depq 12;
    insert depq 103;
    insert depq 9;
    print_heap depq print_int;
    print_int (extract_first depq);
    print_heap depq print_int;
    print_int (extract_first depq);
    print_heap depq print_int;
)
*)


(*

#use "use.ml";;

type ht = {
  i1 : int;
  i2 : int;
  mutable t_index: int;
};;

let compare_t t1 t2 =
  if(t1.i1 < t2.i1) then true
  else if (t1.i1 > t2.i1) then false
  else (t1.i2 < t2.i2);;


let uf thing ni = thing.t_index <- ni;;


let th = Mmh.create ~update_function:uf compare_t 7 {i1=0;i2=0;t_index = 0};;

  Mmh.insert th {i1=1;i2=6;t_index = 0};;
Mmh.insert th {i1=2;i2=6;t_index = 0};;
Mmh.insert th {i1=3;i2=6;t_index = 0};;
Mmh.insert th {i1=4;i2=6;t_index = 0};;
Mmh.insert th {i1=5;i2=6;t_index = 0};;

  Mmh.extract_first th;
  th;;

Mmh.insert th {i1=19;i2=6;t_index = 0};;

*)
