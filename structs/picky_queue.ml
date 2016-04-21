(*

  A picky queue will only allow elements that are within some certain
  percentage of the best element in it.  It can optionally be
  configured to also only allow a certain number of things in it as
  well, so if there are more than X elements trying to get in, 

*)


type 'a picky_queue = 
{
  heap: 'a Mmh.pq_var;
  resize: bool;
  quality_metric: 'a -> float;
  (*this is for checking how good a particular item is.*)
  pickiness:float;
  be_picky: bool;
}

let create ?(update_function = fun a b -> ()) 
    ?(resize = false) ?(pickiness = None)
    comparator qm capacity default =
  (* pickiness less than 1 will really screw things up, nothing will
     be allowed in. *)
  let bp, pn = match pickiness with
      Some f -> true,f
    | None -> false,1.0 in
  if(pn < 1.0) then
    failwith "pickiness must be greater than or equal to 1.0\n";
  {
    heap = Mmh.create ~update_function:update_function 
      ~resize:resize comparator capacity default;
    resize = resize;
    quality_metric = qm;
    pickiness = pn;
    be_picky = bp;
  }


let peek_first pq = 
  Mmh.peek_first pq.heap

let count pq = 
  Mmh.count pq.heap

let empty_p pq = 
  Mmh.empty_p pq.heap

let peek_largest pq = 
  Mmh.peek_largest pq.heap

let best_item_quality pq = 
  pq.quality_metric (peek_first pq)

let worst_item_quality pq = 
  pq.quality_metric (peek_largest pq)

(*
  As items get added to the picky queue, it is possible that the
  definition of the best item will change.  When this change occurs,
  some of the items that were previously allowed in the queue may need
  to be removed.  The purge function performs this function.
*)
let purge (pq) = 
  (*
    compare the quality of the first item to the worst item.  If it is
    within the tolerance, don't have to do anything, otherwise have to
    purge it.
  *)
  if(not pq.be_picky) then [] 
  else
    (
      let purged = ref [] in
	while (((worst_item_quality pq) > ((best_item_quality pq) *. pq.pickiness))
	       && (count pq) >= 1) do
	  (
	    purged := Mmh.extract_worst pq.heap :: !purged;
	  )
	done;
	!purged)


let insert pq (item: 'a) = 
  (* first item *)
  if(Mmh.empty_p pq.heap) then (ignore (Mmh.insert pq.heap item); [];)
    (* item is good enough *)
  else if((pq.quality_metric item) < 
	    ((pq.quality_metric (Mmh.peek_largest pq.heap)) *. pq.pickiness))
  then (
    let to_return = Mmh.insert pq.heap item in 
      to_return @ (purge pq)
  )
  else if(pq.be_picky) then 
    (
      [item]
    )
  else (
    let to_return = Mmh.insert pq.heap item in 
      to_return @ (purge pq)
  )

let replace_at pq item index = 
  let kicked_out = Mmh.replace_at pq.heap item index in
    kicked_out @ (purge pq)

let insert_index pq item index= replace_at pq index item

let access_at pq index = 
  Mmh.access_at pq.heap index

let remove_at pq index = 
  Mmh.remove_at pq.heap index

let extract_first pq = 
  Mmh.extract_first pq.heap

let extract_worst pq = 
  Mmh.extract_worst pq.heap

let pop_any pq = 
  Mmh.pop_any pq.heap


(*
  This makes a poor quality iterator.  It is poor quality becuase it
  doesn't iterate in order and it doesn't react to changes in the
  underlying data structure.  
*)
let make_iterator_unsafe pq = 
  let index = ref 0 in
    (fun () -> 
       index := !index + 1;
       if(!index <= pq.heap.Mmh.heap_count) then 
	 (Some (pq.heap.Mmh.heap.(!index)))
       else 
	 None
    ),
  (
    fun () -> index := 0;
  )

let for_all pq eval = 
  Mmh.for_all pq.heap eval


let check_index pq get_index = 
  Mmh.check_index pq.heap get_index


let iter pq fn = 
  let ct = count pq in
  let to_return = ref [] in
    for i = 1 to ct do
      to_return := (fn (access_at pq i)) :: !to_return;
    done;
    !to_return


(*
  #use "use.ml";;

type test_struct =
{
  n1: float;
  n2: float;
  index: int;
};;

let compare s1 s2 = 
  let fp_delta = 0.00001 in
  let delta = ((s1.n1 +. s1.n2) -. (s2.n1 +. s2.n2)) in
    if(delta < fp_delta) then true
    else false;;

let qm s = s.n1 +. s.n2;;

let tpq = Picky_queue.create compare qm 2.0 5
{
  n1 = -1.0;
  n2 = -1.0;
  index = 0;
};;


  Picky_queue.insert tpq
{
  n1 = 1.1;
  n2 = 1.1;
  index = 0;
};
  Picky_queue.insert tpq
{
  n1 = 2.1;
  n2 = 2.1;
  index = 0;
};;


*)
