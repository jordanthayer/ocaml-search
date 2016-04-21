(*

  this is a leaky queue.  It takes in items until it is full, and when
  it is full, it only allows in items that are of superior quality
  to those already inside it.  Since it doesn't allow in more than
  its size for the number of items, it doesn't have to ever resize
  itself, and its initial size is very important.  Items come out in
  reverse order of quality, so when removing items from it, the worst
  one will come out first.  

*)



type 'a pq_var = {
  heap : 'a array;
  heap_size: int;
  mutable heap_count: int;
  comparator: 'a -> 'a -> bool;
  update: 'a -> int -> unit
}

let junk_func a b = ()


let create ?(update_function = junk_func) comparator capacity default = {
  heap = Array.make (capacity+2) default;
  heap_size = capacity;
  heap_count = 0;
  comparator = comparator;
  update = update_function;
}


exception Index_out_of_bounds of string


let peek_first lq = 
  assert (lq.heap_count > 0);
  lq.heap.(1)


let count lq = lq.heap_count


let empty_p lq = (lq.heap_count = 0)




(*
  this lets items move around in the heap, but I don't think they need
  to do this, so this is commented out, but was included in case this
  ability seems needed for something.
*)

let rec bubble_up bh node_index =
  if node_index*2 > (bh.heap_size - 1) then ()
  else if bh.comparator bh.heap.(node_index*2) bh.heap.(node_index)
  then
    (
      let tmp = bh.heap.(node_index) in
	bh.heap.(node_index) <- bh.heap.(node_index*2);
	bh.heap.(node_index*2) <- tmp;
	bh.update bh.heap.(node_index) node_index;
	bh.update bh.heap.(node_index*2) (node_index*2);
	bubble_up bh (node_index*2);
    )
  else if ((node_index * 2) > bh.heap_size) then () 
  else if bh.comparator bh.heap.(node_index*2+1) bh.heap.(node_index)
  then
    (
      let tmp = bh.heap.(node_index) in
	bh.heap.(node_index) <- bh.heap.(node_index*2+1);
	bh.heap.(node_index*2+1) <- tmp;
	bh.update bh.heap.(node_index) node_index;
	bh.update bh.heap.(node_index*2+1) (node_index*2+1);
	bubble_up bh (node_index*2+1)
    )


(*
  This makes a poor quality iterator.  It is poor quality becuase it
  doesn't iterate in order and it doesn't react to changes in the
  underlying data structure.  
*)
let make_iterator_unsafe lq = 
  let index = ref 0 in
    (fun () -> 
       index := !index + 1;
       if(!index <= lq.heap_count) then 
	 (Some (lq.heap.(!index)))
       else 
	 None
    ),
  (
    fun () -> index := 0;
  )


let unsafe_iter lq f = 
  for i=1 to lq.heap_count do
    f lq.heap.(i)
  done 

let rec fix_heap bh hole =
  let tmp = bh.heap.(hole) in
    if (hole*2 = bh.heap_count && (bh.comparator bh.heap.(hole)
				     bh.heap.(hole*2))) then ()
    else if (hole*2 = bh.heap_count) then (
      bh.heap.(hole) <- bh.heap.(hole*2);
      bh.heap.(hole*2) <- tmp;
      bh.update bh.heap.(hole) hole;
      bh.update bh.heap.(hole*2) (hole*2);
    )
    else if ((hole*2) > bh.heap_count) then ()
    else if bh.comparator (bh.heap.(hole))
      (bh.heap.(hole*2)) &&
      bh.comparator (bh.heap.(hole))
      (bh.heap.(hole*2+1))
    then ()
    else if (bh.comparator bh.heap.(hole*2) bh.heap.(hole) && 
		bh.comparator bh.heap.(hole*2) bh.heap.(hole*2+1)) then (
      bh.heap.(hole) <- bh.heap.(hole*2);
      bh.heap.(hole*2) <- tmp;

      bh.update bh.heap.(hole) hole;
      bh.update bh.heap.(hole*2) (hole*2);

      fix_heap bh (hole*2) )
    else (
      bh.heap.(hole) <- bh.heap.(hole*2 + 1);
      bh.heap.(hole*2 + 1) <- tmp;

      bh.update bh.heap.(hole) hole;
      bh.update bh.heap.(hole*2+1) (hole*2+1);

      fix_heap bh (hole*2+1)
    );;


let insert bh item =
  assert (bh.heap_count-1 < bh.heap_size);
  (*
    this is the case where the item to be added is so bad it gets kicked
    out of the heap.
  *)
  if ((bh.heap_size = bh.heap_count) && 
	not (bh.comparator bh.heap.(1) item)) then
    (
      [item];
    )
      (* room for the new item.
      *)
  else if (bh.heap_size > bh.heap_count) then
    (
      bh.heap_count <- bh.heap_count + 1;
      let hole = ref bh.heap_count in
	while bh.comparator item (bh.heap.(!hole/2)) && !hole > 1 do
	  bh.heap.(!hole) <- bh.heap.( !hole/2 );
	  bh.update bh.heap.(!hole) !hole;
	  hole := !hole / 2;
	done;
	bh.heap.(!hole) <- item;
	bh.update bh.heap.(!hole) !hole;
	[];
    )
      (* this is the case where the worst item currently in the heap
	 is going to get thrown out, and replaced with the new item,
	 then the heap is fixed *)
  else (
    let kicked_out = bh.heap.(1) in 
      bh.heap.(1) <- item;
      bh.update bh.heap.(1) 1;
      fix_heap bh 1;
      bh.update kicked_out (-1);
      [kicked_out];
  );;


let extract_first bh =
  assert (bh.heap_count > 0);

  if bh.heap_count = 0 then 
    (
      bh.update bh.heap.(1) (-1);
      bh.heap.(1))
  else (
    let min_node = bh.heap.(1) in
      bh.heap.(1) <- bh.heap.(bh.heap_count);
      bh.heap.(bh.heap_count) <- bh.heap.(bh.heap_size - 1);
      bh.heap_count <- (bh.heap_count - 1);
      (* print_node_verbose gw min_node ;*)
      fix_heap bh 1;
      bh.update min_node (-1);
      min_node;)


    
let print_heap bh pf = 

  Verb.pf Verb.always stderr "\n***** Printing Heap *****\n";

  for i = 1 to (bh.heap_count) do
    Verb.pf Verb.always stderr "heap location: %d!" i;
    pf bh.heap.(i);
    Verb.pf Verb.always stderr "\n!";
  done;
  flush stderr



let insert_index (leaky_queue:'a pq_var) (index:int) (item:'a)=
  if index > leaky_queue.heap_size then 
    raise (Index_out_of_bounds "Lq.insert_index");
    leaky_queue.heap.(index)<-item;
    leaky_queue.update leaky_queue.heap.(index) index;
    bubble_up leaky_queue index;
    []


let replace_at (leaky_queue:'a pq_var)  (item:'a) (index:int)= 
  insert_index leaky_queue index item


let remove_at _ =
  failwith "cannot remove a specific item from a leaky queue"

let worst_item_quality _ =
  failwith "leaky queues have no concept of quality"

let best_item_quality _ = 
  failwith "leaky queues have no concept of quality"

let peek_largest _ =
  failwith "cannot access worst item in a leaky queue"


let access_at (leaky_queue:'a pq_var) (index:int) =
  if index > leaky_queue.heap_size then 
    raise (Index_out_of_bounds "Lq.insert_index");
  leaky_queue.heap.(index)

let print_things (leaky_queue: 'a pq_var) (print_function: 'a -> string) 
    (delimiter: string)= 
  for i = 1 to (leaky_queue.heap_count) do
    Printf.fprintf stderr "%s%s" (print_function leaky_queue.heap.(i)) 
      delimiter;
  done;


(*

let test_heap () = 
  Random.init 1000;
  let th = create
    (fun i1  i2 -> (i1 >= i2))
    7
    0 in   
  let i = (fun x ->  Lq.insert th x; Dpq.insert wq x)in

    i 10;
    i 14;
      i 20;
      i 889;
      i 3893;
      i 3737;
      i 1722;  
      i 0;
      i 299;
      i 1;


(*
    ignore (extract_first th);
    ignore (extract_first th);
    ignore (extract_first th);    ignore (extract_first th);
    ignore (extract_first th);
*)



    print_heap th (fun x -> Verb.pf Verb.always stdout "%d," x)
;;

*)

(*
    let _ = test_heap ();
*)



(*eof*)


(*

  testing stuff

type node = {
contents:int;
mutable index:int;
};;

let a = Lq.create ~update_function:(fun a b -> a.index <- b)
(fun a b -> a.contents > b.contents) 3
{
contents = 0;
index = 0;
};;

Lq.insert a 
{
contents=100;
index=(-1);
};;


for i = 1 to a.Lq.heap_size do
ignore(
Lq.insert a 
{
contents=Random.int 10000;
index=(-1);
});
done


let add_replace () = 
(
ignore(
Lq.insert a 
{
contents=Random.int 10000;
index=(-1);
});

let junk = Random.int 10000 in 
let junk2 = Random.int a.Lq.heap_size in 
if(junk <= (a.Lq.heap.(junk2)).contents) then 
Lq.insert_index a junk2 
{
contents=junk;
index=(-1);
}
)
;;


let add_one () = 
ignore (Lq.insert a 
{
contents=Random.int 10000;
index=(-1);
});
a;;

*)
