(**

   Bit Packed pattern database for towers of hanoi

   Christopher Wilt

*)


let is_good f = 
  let f_type = classify_float f in
    match f_type with
	FP_normal | FP_subnormal | FP_zero -> true
      | _ -> false


let make_bp_pdb expand initial key size n_disks= 
  let pdb = Bp_array.make size 1000 1000 in
  let open_list = Queue.create () in
    Queue.add (key initial) open_list;
    Bp_array.set pdb (key initial) (0);
    Printf.printf "first: %d\n"((key initial));
    let expand_node n = 
      (
	let g_val = (Bp_array.get pdb (key n)) in 
	  assert (g_val != 1000);
	  let children = (expand n (float_of_int g_val)) in
	    List.iter (
	      fun (child, child_cost) ->
		(
		  if((Bp_array.get pdb (key child)) != 1000) then 
		    ()
		  else
		    (
		      Queue.add (key child) open_list;
		      Bp_array.set pdb (key child) (int_of_float child_cost);
		    )
		)
	    )
	      children
      ) in
    let rec process_item () = 
      (
	match Queue.is_empty open_list with
	    true -> ()
	  | false -> (
	      let n = Queue.pop open_list in
		if(n<0) then
		  (Printf.printf "%d\n" n;
		   failwith "bad index";);
		assert(n < Bp_array.length pdb);

		let n = Min_hanoi.from_int (n_disks) 4 n in
		  expand_node n;
		  process_item ();
	    )
      ) in
      process_item ();
      pdb

