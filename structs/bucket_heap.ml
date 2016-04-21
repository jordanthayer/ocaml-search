(**

   Bucketed heap.  Just takes in floating point quality items then
   arranges them into buckets.  Gives back items in ascending order,
   the smallest one comes out first.  Insertion time depends somewhat
   on the number of buckets and whether or not the bucket is new or
   not, sicne the bucket indices are stored in a binary heap.

*)

type 'a bucket = {
  mutable b: 'a array;
  mutable last: int;
  mutable tracked: bool;
}

type 'a bucket_heap = {
  mutable heap: 'a bucket array;
  delta: float;
  mutable floor : float;
  mutable count : int;
  mutable occupied : int Dpq.t;
  update : 'a -> int -> unit;
  prototype: 'a;
}

let round f = 
  if(f >= 0.0) then int_of_float f
  else (int_of_float (f)) - 1

let no_pos = (-1)

let make_bucket ?(size=10) prototype = 
  {
    b = Array.make size prototype;
    last = no_pos;
    tracked = false;
  }

let is_empty bucket = 
  bucket.last = 0

let remove_from_bucket buck prototype= 
  (**utility function to pull something out of a bucket.*)
  assert(not (is_empty buck));
  buck.last <- buck.last - 1;
  let to_return = buck.b.(buck.last) in
    buck.b.(buck.last) <- prototype;

    if(buck.last = 0) then 
      buck.tracked <- false;
    to_return


let bucket_has_room buck = 
  (*tells if the specified bucket has any room left in it.*)
  buck.last < ((Array.length buck.b) - 1) && buck.last != (0)


let make_bigger_bucket buck prototype = 
  (**copies buck into a bigger one and returns the bigger one.*)
  let old_length = (Array.length buck.b) in
    if(old_length = 0) then 
      {
	b = Array.make 10 prototype;
	last = 0;
	tracked = false;
      }
    else 
      (
	let new_ary = Array.init (old_length * 2) 
	  (
	    fun ix ->
	      if(ix < old_length) then buck.b.(ix)
	      else
		prototype
	  ) in
	  {
	    b = new_ary;
	    last = buck.last;
	    tracked = buck.tracked;
	  }
      )

let make_empty_bucket () =
  {
    b = [||];
    last = 0;
    tracked = false;
  }


let fix_index bh ix = 
  (*Check the bucket to see if it is a new one*)
  if(not bh.heap.(ix).tracked) then 
    (
      Dpq.insert bh.occupied ix;
      bh.heap.(ix).tracked <- true;
    )

let put_in_bucket update buck item = 
  (**Just puts an item into the bucket, no bounds checks.*)
  buck.b.(buck.last) <- item;
  update item buck.last;
  buck.last <- buck.last + 1


let make_more_room bh index = 
  (**expands the bucket at index to double whatever its size was.*)
  bh.heap.(index) <- make_bigger_bucket bh.heap.(index) bh.prototype


let create ?(update = fun _ _ -> ()) delta initial_max initial_min
    prototype = 
  (**
     update function tracks the indices, so if you want to remove
     something it knows where it is.  This is the index in the minor
     array, if you want to remove something you have to know what
     quality the item came in with in order to remove it.  

     [delta] is how big each bucket should be.

     [initial_max] is the initial maximum bucket.  If you add a bigger
     item this will grow, but this is the default.

     [initial_min] is the initial minimum bucket.  At the moment the
     array refuses to grow negative, but this will change at some
     point.

     [prototype] is a prototype element that is used to fill empty
     slots.

  *)
  let length = round ((initial_max -. initial_min) /. delta) in
    {
      heap = Array.make length (make_empty_bucket ());
      delta = delta;
      floor = initial_min;
      count = 0;
      occupied = Dpq.create_with (<) (-1);
      update = update;
      prototype = prototype;
    }

let grow_up bh index = 
  (**grows theb ucketed heap in the up direction.*)
  let index = if(index < (Array.length bh.heap) * 2) then 
    (Array.length bh.heap) * 2
  else 
    index in
  let new_buckets = 
    Array.init (index + 1) (
      fun ix ->
	if(ix < Array.length bh.heap)
	then bh.heap.(ix)
	else
	  make_empty_bucket ()
    ) in
    bh.heap <- new_buckets


let grow_down bh index = 
  (*is supposed to grow the bucketed heap in the down direction*)
  let index = 
    if((index*(-1)) < (Array.length bh.heap) * 2) then 
      (Array.length bh.heap) * 2
    else 
      (Array.length bh.heap) - index in
  let new_buckets = 
    Array.init (index + (Array.length bh.heap)) (
      fun ix ->
	
	if(ix < index)
	then 
	  (
	    make_empty_bucket ()
	  )
	else 
	  (
	    bh.heap.(ix - index)
	  )
    ) in

    bh.heap <- new_buckets;
    Dpq.self_map bh.occupied (fun i -> i + index);
    bh.floor <- bh.floor -. bh.delta *. (float_of_int index)



let rec insert bh item quality = 
  (**puts item into the bucket heap with the specified quality.

     Recursive because sometimes you modify the heap to make it bigger
     then try again.
  *)
  let item_bucket = round ((quality -. bh.floor) /. bh.delta)
  in
    (*grow down*)
    if(item_bucket < 0) then 
      (
	grow_down bh item_bucket;
	insert bh item quality;
      )
	(*grow up*)
    else if (item_bucket >= Array.length bh.heap) then
      (
	grow_up bh item_bucket;
	insert bh item quality;
      )
	(*no need to grow*)
    else
      (
	bh.count <- bh.count + 1;
	if(bucket_has_room bh.heap.(item_bucket)) then 
	  (
	    put_in_bucket bh.update bh.heap.(item_bucket) item;
	    fix_index bh item_bucket;
	  )
	else
	  (
	    make_more_room bh item_bucket;
	    put_in_bucket bh.update bh.heap.(item_bucket) item;
	    fix_index bh item_bucket;
	  )
      )


let remove bh index quality = 
  (**removes the thing of [quality] at [index].  Doesn't do anything
     with the predecessor, except for reset its index.*) 
  let item_bucket = round ((quality -. bh.floor) /. bh.delta) in
    (*check to make sure the thing actually exists.*)
  let b = bh.heap.(item_bucket) in
    (*pull something out of the last bucket*)
    assert(index < b.last);

    (*removing the last element*)
    if(index+1 = b.last)
    then (
      ignore (remove_from_bucket b bh.prototype)
    )
    else
      (
	let moved = remove_from_bucket b bh.prototype in
	  (*note that the thing getting removed is kicked out.*)
	  bh.update b.b.(index) no_pos;
	  b.b.(index) <- moved;
	  bh.update moved index
      )

let swap bh item index old_quality new_quality = 
  (**Removes the thing with [old_quality] at [index].  Inserts [item]
     into the bucket heap with [new_quality].*)
  remove bh index old_quality;
  insert bh item new_quality


let empty bh =
  (*tells you if the bucket heap is empty*)
 bh.count = 0

let extract_first bh = 
  (**gets the smallest thing in the bucket heap and returns it.  Note
     that "smallest" is defined as within delta of the actual
     smallest, so the element returned may not be the smallest one
     ever seen, but is rather just pretty small.*)
  (*make sure there actually is a first to pull out.*)
  assert(bh.count != 0);
  assert(not (Dpq.empty_p bh.occupied));
  let index = Dpq.peek_first bh.occupied in
  let to_return = remove_from_bucket bh.heap.(index) bh.prototype in
    while(not (Dpq.empty_p bh.occupied) && 
	    not bh.heap.(Dpq.peek_first bh.occupied).tracked)
    do
      ignore (Dpq.extract_first bh.occupied);
    done;
    bh.count <- bh.count - 1;
    bh.update to_return no_pos;
    to_return
