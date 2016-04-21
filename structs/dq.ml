
type 'a double_heap = {
  capacity:int;
  mutable current_size:int;
  heap1:'a Dpq.t;
  heap2:'a Dpq.t;
  get_1:'a -> int;
  get_2:'a -> int;
}

let insert dh i = 
  if(dh.current_size < dh.capacity) then
    (
      Dpq.insert dh.heap1 i;
      Dpq.insert dh.heap2 i;
      dh.current_size <- dh.current_size + 1;
      None;
    )
  else 
    (
      let a = Dpq.extract_first dh.heap2 in
	Dpq.remove dh.heap1 (dh.get_1 a);
	Dpq.insert dh.heap1 i;
	Dpq.insert dh.heap2 i;
	(Some a);
    )

exception Empty of string

let extract_first dh = 
  if(dh.current_size == 0)
  then raise (Empty "extracting something from an empty double queue")
  else 
  dh.current_size <- dh.current_size - 1;
  let a = Dpq.extract_first dh.heap1 in
    Dpq.remove dh.heap2 (dh.get_2 a);
    a


let empty_p dq = 
  dq.current_size = 0


let peek_first dq = 
  Dpq.peek_first dq.heap1


let create pred1 pred2 update1 update2 get1 get2 approx_n init_e = 
(**
   pred1 : first ordering predicate
   pred2 : second ordering predicate
   update1 : first update function
   update2 : second update function
   approx_n : number of things to let it hold
   init_e : thing to initialize the structure with
*)
  {
    capacity = approx_n;
    current_size = 0;
    heap1 = Dpq.create pred1 update1 approx_n init_e;
    heap2 = Dpq.create pred2 update2 approx_n init_e;
    get_1 = get1;
    get_2 = get2;
  }

(*
let test_dq = create 
  (fun n1 n2 -> (f_ordered n1 n2)) 
  (fun n1 n2 -> not (h_ordered n1 n2))
  update_index1 update_index2
  9 {
  data = "asdf";
  cost = 1.0;
  incurred_cost = 100.0;
  heap1_index = 0;
  heap2_index = 0;
};;


let test = Array.make_matrix 3 3 {
  data = "asdf";
  cost = 1.0;
  incurred_cost = 100.0;
  heap1_index = 0;
  heap2_index = 0;
};;

*)

(*
for i = 0 to 2 do
  (
    for j = 0 to 2 do
      (
	test.(i).(j) <- 
	  {
	    data = "asdf";
	    cost = (float_of_int i);
	    incurred_cost = (float_of_int j);
	    heap1_index = 0;
	    heap2_index = 0;
	  };
	insert test_dq test.(i).(j);
      )
    done
  )
done
*)
