(**

   Towers of hanoi is a bit packed format.

   Christopher Wilt

   January 15, 2011

*)

let masks = 

  [|1; 3; 7; 15; 31; 63; 127; 255; 511; 1023; 2047; 4095; 8191; 16383; 32767;
    65535; 131071; 262143; 524287; 1048575; 2097151; 4194303; 8388607;
    16777215; 33554431; 67108863; 134217727; 268435455; 536870911; 1073741823;
    2147483647; 4294967295; 8589934591; 17179869183; 34359738367; 68719476735;
    137438953471; 274877906943; 549755813887; 1099511627775; 2199023255551;
    4398046511103; 8796093022207; 17592186044415; 35184372088831;
    70368744177663; 140737488355327; 281474976710655; 562949953421311;
    1125899906842623; 2251799813685247; 4503599627370495; 9007199254740991;
    18014398509481983; 36028797018963967; 72057594037927935;
    144115188075855871; 288230376151711743; 576460752303423487;
    1152921504606846975; 2305843009213693951; 4611686018427387903|]



type cost = 
    Unit 
  | AbstractedFree


let move_cost child cost_function =
  match cost_function with
      Unit -> 1.0 
    | _ -> failwith "only unit cost works with these abstractions"


type min_hanoi = 
{
  pegs: int array;
  last_moved: int;
  depth: int;
}


let make_empty n_pegs = 
{
  pegs = Array.make n_pegs 0;
  last_moved = 255;
  depth = 0;
}


let int_p1 = 65535
let int_p2 = 4294901760
let int_p3 = 281470681743360
let int_p4 = (-281474976710656)


let get_top_bit_basic i = 
  let rec check mask accum = 
    if((i land mask) != 0) then accum
    else if (mask = 0) then 0
    else check (mask asr 1) (accum - 1) in
    check (1 lsl 15) 15


let get_top_bit i = 
  if((i land int_p4) != 0) then
    ((get_top_bit_basic (i asr 48)) + 48)
  else if((i land int_p3) != 0) then
    ((get_top_bit_basic (i asr 32)) + 32)
  else if((i land int_p2) != 0) then
    ((get_top_bit_basic (i asr 16)) + 16)
  else
    get_top_bit_basic i


let set_bit old_int bit_to_set = 
  let mask = 1 lsl (bit_to_set) in 
    mask lor old_int


let unset_bit old_int bit_to_unset = 
  let rev_mask = lnot (1 lsl (bit_to_unset)) in
    rev_mask land old_int


let check_bit i bit_to_check = 
  let mask = 1 lsl bit_to_check in
    (mask land i) != 0


let print_int_bin i = 
  let buf = Buffer.create 80 in
    for ctr = 0 to 62 do
      let to_print = match
	check_bit i ctr 
      with true -> "1" 
	| false -> "0" in
	Buffer.add_string buf (to_print)
    done;
    Buffer.contents buf


let make_from_hanoi h = 
  let peg_count = (Hanoi.n_pegs h) in
  let mh = Array.make peg_count 0 in
    for i = 0 to peg_count - 1 do
      for j = 0 to (Char.code h.Hanoi.top_index.(i)) do
	mh.(i) <- (set_bit mh.(i) ((Char.code h.Hanoi.pegs.(i).(j)) - 1))
      done;
    done;
    {
      pegs = mh;
      last_moved = Char.code h.Hanoi.last_moved;
      depth = h.Hanoi.depth;
    }


let copy_state h = 
  {
    pegs = Array.copy h.pegs;
    last_moved = h.last_moved;
    depth = h.depth;
  }

let hash h = 
  let n_pegs = Array.length h.pegs in
    Hashtbl.hash_param n_pegs n_pegs h.pegs


let hash_compare a b = 
 let rec do_eq (a:int array) b i =
    if i >= (Array.length a) then true
    else a.(i) = b.(i) && do_eq a b (i + 1)
  in do_eq a.pegs b.pegs 0


let n_pegs h = 
  Array.length h.pegs


let print_hanoi h = 
  let buf = Buffer.create 80 in
    for i = 0 to (n_pegs h) - 1 do
      Buffer.add_string buf (print_int_bin h.pegs.(i));
      Buffer.add_string buf "\n";
    done;
    Buffer.add_string buf (string_of_int h.depth);
    Buffer.contents buf


let can_move_peg h start_peg end_peg =
  (**tells if the disk on start_peg can be moved to end_peg in state h*)
  if(start_peg = end_peg) then false
  else if (h.pegs.(start_peg) = 0) then false
  else if (h.pegs.(end_peg) = 0) then true
  else if start_peg = h.last_moved then false
  else
    (get_top_bit h.pegs.(start_peg)) > (get_top_bit h.pegs.(end_peg))
      


let last_moved_int h = 
  (**gets the integer id of the peg that last had something moved
     from it.*)
  h.last_moved


let n_disks h = 
  (**gets how many disks there are*)
  let m_arr = Array.map get_top_bit h.pegs in
    fst (Wrarray.max_by Fn.identity m_arr)


let sol_length h = 
  (**tells how long a solution is.  Does not verify that the node in
     question is a solution though.*)
  h.depth


let is_goal h = 
  Wrarray.for_alli 
    (fun i item -> 
       match i with 
	   0 -> true 
	 | _ -> item = 0
    ) h.pegs

let move_peg h start_peg end_peg = 
  if(can_move_peg h start_peg end_peg)
  then
    (
      let new_state = {(copy_state h) with last_moved = end_peg;
			 depth = h.depth + 1;
		      } in
      let start_top = get_top_bit new_state.pegs.(start_peg) in
	new_state.pegs.(start_peg) <- 
	  unset_bit new_state.pegs.(start_peg) start_top;
	new_state.pegs.(end_peg) <-
	  set_bit new_state.pegs.(end_peg) start_top;
	Some new_state
    )
  else
    None


let expand ?(cost=Unit) h g_value= 
  (**Expands the state h into its legal children.  Works for both
     abstract and normal states.*)
  let children = ref [] in
    (*consider moving the disk on top of each peg*)
    for i = 0 to (n_pegs h) - 1
    do
      (*don't move the disk that was previously moved*)
      if(i = last_moved_int h) then ()
	(*take top disk of peg i and try moving it somewhere else*)
      else 
	(
	  (*consider putting that peg somewhere else*)
	  for j = 0 to (n_pegs h) - 1
	  do
	    (
	      (*this move accomplishes nothing*)
	      if(j = i) then ()
		(*consider moving the top of peg i onto peg j*)
	      else
		(
		  match move_peg h i j with
		      None -> ()
		    | Some child -> children := 
			(child, (g_value +. (move_cost child cost))) 
			:: !children;
		)
	    )
	  done
	)
    done;
    !children


let abstract_state_down n h_state = 
  let n = n-1 in
  let total_disks = n_disks h_state in
  let disks_to_delete = total_disks - n in
  let abstracted_state = copy_state h_state in
    for i = 0 to (Array.length h_state.pegs) - 1
    do
      abstracted_state.pegs.(i) <- (abstracted_state.pegs.(i) asr disks_to_delete);
    done;
    abstracted_state


let abstract_state_up n h_state = 
  let n = n-1 in
  let abstracted_state = copy_state h_state in
    for i = 0 to (Array.length h_state.pegs) - 1
    do
      abstracted_state.pegs.(i) <- (abstracted_state.pegs.(i) land masks.(n));
    done;
    abstracted_state


let make_goal n_disks n_pegs = 
  let temp = Hanoi.make_goal n_disks n_pegs in
    make_from_hanoi temp


let find_disk_peg disk_ix h_state = 
  (**Finds where the specified disk is.*)
  let mask = 1 lsl disk_ix in
  let rec find_peg peg_to_check = 
    if(peg_to_check >= (n_pegs h_state)) then (-1)
    else 
      (
	match (mask land (h_state.pegs.(peg_to_check))) with
	    0 -> find_peg (peg_to_check + 1)
	  | _ -> peg_to_check)
  in find_peg 0


let abstract_state_up_int n h_state = 
  let disks_to_process = n in
  let rec process_disk disk_id output_index remaining_disks accum =
    let disk_location = find_disk_peg disk_id h_state in
    let acc_mask = disk_location lsl output_index in
      if(remaining_disks = 0) then accum
      else 
	process_disk 
	  (disk_id + 1)
	  (output_index + 2)
	  (remaining_disks - 1)
	  (accum lor acc_mask)
  in 
    process_disk 0 0 disks_to_process 0


let abstract_state_down_int n h_state = 
  let total_disks = (n_disks h_state) + 1 in
  let disks_to_process = n in
  let rec process_disk disk_id output_index remaining_disks accum =
    let disk_location = find_disk_peg disk_id h_state in
    let acc_mask = disk_location lsl output_index in
      if(remaining_disks = 0) then accum
      else 
	process_disk 
	  (disk_id + 1)
	  (output_index + 2)
	  (remaining_disks - 1)
	  (accum lor acc_mask)
  in 
    process_disk (total_disks - disks_to_process) 0 disks_to_process 0


let abstract_state_exact start n h_state = 
  let disks_to_process = n in
  let rec process_disk disk_id output_index remaining_disks accum =
    let disk_location = find_disk_peg disk_id h_state in
    let acc_mask = disk_location lsl output_index in
      if(remaining_disks = 0) then accum
      else 
	process_disk 
	  (disk_id + 1)
	  (output_index + 2)
	  (remaining_disks - 1)
	  (accum lor acc_mask)
  in 
    process_disk start 0 disks_to_process 0


let from_int n_disks n_pegs int_state = 
  assert(n_pegs = 4);
  let get_disk_loc disk = 
    (int_state asr (disk * 2)) land 3 in 
  let new_state = make_empty n_pegs in
    for i = 0 to n_disks - 1 do
      let disk_loc = get_disk_loc i in
(*
	Printf.printf "disk %d location %d\n" i disk_loc;
*)
	new_state.pegs.(disk_loc) <- set_bit new_state.pegs.(disk_loc) i
    done;
    new_state



let crap_expand h_state = 
  let children = expand h_state 0.0 in
    fst (List.split children)
