(**

   Towers of Hanoi

   Christopher Wilt November 10, 2010

*)

type cost = 
    Unit 
  | AbstractedFree

type direction = 
    Up
  | Down

type peg = char array


(*
  in the peg array, a value of 255 means the peg has been abstracted
  away.  A value of 0 means the slot is empty.  
*)

let unused = '\000';;
let abstracted = '\255';;


type hanoi = 
{
  pegs : peg array;
  (*Contains the value the top disk in the stack.  If the disk is
    abstracted, it contains the largest value that COULD be the top.*)
  top_disk : char array;
  top_index : char array;
  last_moved : char;
  depth : int;
  (*index of the PEG onto which a disk was last moved*)
}


let copy_state h = 
{
  pegs = Wrarray.copy_matrix h.pegs;
  top_disk = Array.copy h.top_disk;
  top_index = Array.copy h.top_index;
  last_moved = h.last_moved;
  depth = h.depth;
}



let hash h =
  (**takes a hanoi state and hashes it.  Hashing is based upon the
     peg arrays.*)
  let pegs = Array.length h.pegs in
  let disks = Array.length h.pegs.(0) in
    (*the hash function requires pegs + (disks + 1) for that first
      parameter.  I have no idea what the second parameter does.*)
    Hashtbl.hash_param (pegs * (disks + 1)) 100 h.pegs


let hash_compare h1 h2 = 
  (**checks if two states are exactly the same.*)

  (*checking each thing in the array is annoying.  First check the
    small memoized arrays for equality.  If these arrays aren't equal
    there is no way the states are equal.  If these two arrays do
    match, then have to check all the individual arrays.*)
  if((Wrarray.char_equal h1.top_disk h2.top_disk) &&
       (Wrarray.char_equal h1.top_index h2.top_index))
  then 
    (
      Verb.pe Verb.debug "entering secondary compare\n";
      let all_match = ref true in 
      let i = ref 0 in
      let j = ref 0 in
	while (!i < ((Array.length h1.pegs) - 1) && !all_match)
	do
	  while (!j < ((Char.code h1.top_index.(!i)))) && !all_match
	  do
	    Verb.pe Verb.debug 
	      "comparing index %d %d with values (%d %d)\n"
	      !i !j (Char.code h1.pegs.(!i).(!j)) 
	      (Char.code h2.pegs.(!i).(!j));
	    if((Char.compare h1.pegs.(!i).(!j) h2.pegs.(!i).(!j)) != 0)
	    then 
	      all_match := false;
	    j := !j + 1;
	  done;
	  i := !i + 1;
	done;
	!all_match
    )
  else
    false


let print_hanoi h = 
  (**turns h into a string.*)
  let buf = Buffer.create 80 in

  for peg_index=0 to (Array.length h.pegs.(0)) - 1 
  do
    let j = (Array.length h.pegs.(0)) - peg_index - 1 in 
    for i = 0 to (Array.length h.pegs) - 1
    do
      if (h.pegs.(i).(j) = unused) then 
	Buffer.add_string buf "\t"
      else if (h.pegs.(i).(j) = abstracted) then 
	Buffer.add_string buf "\tA"
      else
	Buffer.add_string buf (Printf.sprintf "\t%d" (Char.code h.pegs.(i).(j)));
    done;
     Buffer.add_string buf "\n";
  done;
  (*print out labels for the pegs*)
  for i = 0 to (Array.length h.pegs) - 1
  do
    Buffer.add_string buf (Printf.sprintf "\tPeg %d" i);
  done;
  Buffer.add_string buf "\n";
  for i = 0 to (Array.length h.pegs) - 1
  do
    Buffer.add_string buf (Printf.sprintf "\ttop %d" (Char.code h.top_disk.(i)));
  done;
  Buffer.add_string buf "\n";
  for i = 0 to (Array.length h.pegs) - 1
  do
    Buffer.add_string buf (Printf.sprintf "\tindex %d" (Char.code h.top_index.(i)));
  done;
  Buffer.add_string buf "\n";
  Buffer.contents buf



let pegs_okay (p1:Char.t) (p2:Char.t) = 
(**Given two pegs, checks if the second one can be on top of the first
   one.*)
  if(p1 = abstracted) then true
  else if (p2 = abstracted) then true
  else if (p1 > p2) then true
  else if (p2 = unused) then true
  else false

let check_memoarrays h = 
  (**Checks to see that the memoized arrays contain the correct values.*)
  let okay = ref true in
    for i = 0 to (Array.length h.pegs) - 1 
    do
      if (h.pegs.(i).(Char.code h.top_index.(i)) = h.top_disk.(i)) then
	()
      else 
	okay := false;
    done;
    !okay


let update_top_disk h = 
  (**assuming the top index is correct, changes the value to be
     whatever the top index points to.*)
  for i = 0 to (Array.length h.pegs) - 1
  do
    h.top_disk.(i) <- h.pegs.(i).(Char.code h.top_index.(i));
  done


let verify_hanoi h = 
  (**Invasive check to make sure that the entire state is valid.*)
  let okay = ref true in
    for i = 0 to (Array.length h.pegs) - 1 
    do
      for j = 0 to (Array.length h.pegs.(i)) - 2 
      do
	if(pegs_okay h.pegs.(i).(j) h.pegs.(i).(j+1)) then ()
	else 
	  (
	    Printf.fprintf stderr "Error on peg %d index %d\n" i j;
	    Printf.fprintf stderr "%s" (print_hanoi h);
	    flush stderr;
	    okay := false;
	  )
      done
    done;
    if(check_memoarrays h) then 
      ()
    else
      okay := false;
    !okay


let abstract_state h (abs_map:bool array) = 
  let new_state = copy_state h in
  Array.iteri 
    (
      fun index item ->
	for i = 0 to Char.code h.top_index.(index) do
	  (*if the current code is -1, this means this particular
	    stack is empty, so it can just be skipped.  Have to
	    subtract 1 because the transform array is zero indexed
	    while the disks are 1 indexed.*)
	  let current_code = (Char.code item.(i)) - 1 in
	    if(current_code = -1) then ()
	    else if(abs_map.(current_code)) then
	      item.(i) <- '\255'
	done
    )
    new_state.pegs;
    update_top_disk new_state;
    new_state


let make_empty n_disks n_pegs = 
  (**makes a goal state with n_disks disks and n_pegs pegs.  All disks
     are on peg 0.*)
  let pegs = Array.init n_pegs (fun _ -> (Array.make n_disks '\000')) in
    let top_disk = Array.make n_pegs '\000' in
    let top_index = Array.make n_pegs '\000' in
    {
      pegs = pegs;
      top_disk = top_disk;
      top_index = top_index;
      last_moved = (Char.chr 255);
      depth = 0;
    }



let make_goal n_disks n_pegs = 
  (**makes a goal state with n_disks disks and n_pegs pegs.  All disks
     are on peg 0.*)
  let pegs = Array.make n_pegs (Array.make n_disks (Char.chr 0)) in
    pegs.(0) <- Array.mapi (fun i _ -> Char.chr (n_disks - i)) pegs.(0);
    let top_disk = Array.make n_pegs (Char.chr 0) in
    let top_index = Array.make n_pegs (Char.chr 0) in
      top_disk.(0) <- (Char.chr 1);
      top_index.(0) <- (Char.chr (n_disks - 1));
    {
      pegs = pegs;
      top_disk = top_disk;
      top_index = top_index;
      last_moved = (Char.chr 255);
      depth = 0;
    }


let last_moved_int h = 
  (**gets the integer id of the peg that last had something moved
     from it.*)
  Char.code h.last_moved


let n_pegs h = 
  (**gets how many pegs there are.*)
  Array.length h.pegs


let n_disks h = 
  (**gets how many disks there are*)
  Array.length h.pegs.(0)


let sol_length h = 
  (**tells how long a solution is.  Does not verify that the node in
     question is a solution though.*)
  h.depth

let can_move_peg h (start_peg:char) (end_peg:char) = 
  (**Given a hanoi state h, tells if you can move a peg from the
     start_peg to the end_peg.  Works for both abstract and normal states.*)
  if(h.top_disk.(Char.code start_peg) = unused)
  then 
    false
      (*There are no restrictions on putting something on an unused peg*)
  else if (h.top_disk.(Char.code end_peg) = unused) then
    true
      (*There are no restrictions on moving abstracted disks*)
  else if (h.top_disk.(Char.code start_peg) = abstracted) then
    true
      (*Both disks are real disks*)
      (*abstracted disks look really big, so if the top of the
	destination peg is an abstract peg, we have to look under
	this disk to see if its okay to put something on it.*)
  else if (h.top_disk.(Char.code end_peg) = abstracted) then
    (
      Wrarray.for_all 
	(fun c ->
	   if(c = abstracted) then 
	     true
	   else if(c = unused) then 
	     true
	   else
	     (
	       (Char.code (h.top_disk.(Char.code start_peg))) <
		 (Char.code c)
	     )
	)
	h.pegs.(Char.code end_peg)
    )
  else if (h.top_disk.(Char.code start_peg) > h.top_disk.(Char.code end_peg))
  then 
    false
  else    true 

let move_peg h start_peg end_peg = 
  (**Given the state h and a start and end, attempts to construct a
     child that would be created from that move.  If there is
     something wrong with the request, it will return None.
     Otherwise it will return Some child.*)
  if(can_move_peg h start_peg end_peg) then
    (
      let new_pegs = Array.init (Array.length h.pegs) (fun n -> Array.copy h.pegs.(n)) in
	(*make it so the disk is at its new location*)
      let new_peg_index = 
	if((h.top_index.(Char.code end_peg) = unused) && 
	     (h.top_disk.(Char.code end_peg) = unused)) then
	  0
	else
	  (Char.code h.top_index.(Char.code end_peg)) + 1
      in
	new_pegs.(Char.code end_peg).(new_peg_index) 
	<- h.top_disk.(Char.code start_peg);
	(*make it so the disk is not at its old location*)
	new_pegs.(Char.code start_peg).
	  ((Char.code (h.top_index.(Char.code start_peg)))) <- unused;
	(*update the memoized array datas.*)


	let new_top_index = Array.copy h.top_index in
	let new_top_disk = Array.copy h.top_disk in
	  (*the new top index of the start peg goes down by one*)
	  if(new_top_index.(Char.code start_peg) != unused) then 
	    new_top_index.(Char.code start_peg) <- 
	      Char.chr ((Char.code new_top_index.(Char.code start_peg)) - 1);
	  (*the new top index of the end peg goes up by one*)

	  if((new_top_index.(Char.code end_peg) != unused) || 
	       (new_top_disk.(Char.code end_peg) != unused) 
	    ) then 
	    new_top_index.(Char.code end_peg) <- 
	      Char.chr ((Char.code new_top_index.(Char.code end_peg)) + 1);

	  (*update the top disk for the end peg*)
	  new_top_disk.(Char.code end_peg) <- 
	    new_pegs.
	    (Char.code end_peg).
	    (Char.code (new_top_index.(Char.code end_peg)));

	  (*update the top disk for the start peg*)
	  new_top_disk.(Char.code start_peg) <- 
	    new_pegs.
	    (Char.code start_peg).
	    (Char.code (new_top_index.((Char.code start_peg))));

	  Some {
	    pegs = new_pegs;
	    top_disk = new_top_disk;
	    top_index = new_top_index;
	    last_moved = end_peg;
	    depth = h.depth + 1;
	  }
    )
  else None


let find_last_moved h = 
  let lmi = last_moved_int h in
    if(lmi = 255) then
      failwith "calling find last moved when there is no last moved";
    h.top_disk.(lmi)


let move_cost child cost_function =
  match cost_function with
      Unit -> 1.0 
    | AbstractedFree ->
	(
	  match (find_last_moved child)
	  with 
	      '\255' -> 0.0
	    | _ -> 1.0
	)


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
		  match move_peg h (Char.chr i) (Char.chr j) with
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


let add_disk h index disk_id = 
  let empty_index = 
    if(h.top_index.(index) = '\000') then
      (*if the top index is zero, have to check if there is something
	there.*)
      (
	if(h.top_disk.(index) = '\000') then
	  0
	else 1
      )
    else
      (Char.code (h.top_index.(index))) + 1 in
    h.pegs.(index).(empty_index) <- disk_id;
    h.top_index.(index) <- (Char.chr empty_index);
    h.top_disk.(index) <- disk_id


let read ch = 
  let n_pegs = Wrio.input_int ch in
  let n_disks = Wrio.input_int ch in
  let disk_positions = Wrio.read_ints ch in
  let new_state = make_empty n_disks n_pegs in
    Wrlist.iteri 
      (
	fun disk position ->
	  add_disk new_state position (Char.chr (n_disks - disk))
      )
      disk_positions;
    if(verify_hanoi new_state) then ()
    else
      failwith "the new instance is invalid";
    new_state


let find_disk h disk = 
  Wrarray.find 
    (
      Wrarray.mem disk
    )
    h.pegs


let to_outch h outch = 
  (**sends the hanoi instance to outch in the minimalist format in
     which the instances are stored.*)
  Printf.fprintf outch "%d\n" (n_pegs h);
  Printf.fprintf outch "%d\n" (n_disks h);
  for i = 0 to (n_disks h) - 1
  do
    Printf.fprintf outch "%d " (find_disk h (Char.chr ((n_disks h) - i)));
  done;
  Printf.fprintf outch "\n"
  

let is_goal h =
  (**checks to see if h is the goal.  The goal is when all the disks
     are on peg 0.
     
     The check is accomplished by checking to see that the top disk on
     every peg is unused.  It does not check to see that the pegs on
     the top disk are actually in order, it assumes that the order
     property was maintained throughout the search.  

     An improvement would be to check if the state passes the first
     test, that is all pegs are empty except peg 0, then checking to
     see that the pegs on state 0 are actually in order.  If this test
     fails the state is invalid.
  *)
  let to_return = ref true in
  let index = ref 1 in
    while ((!index < (n_pegs h)) && !to_return)
    do 
      if(h.top_disk.(!index) != '\000') then
	to_return := false;
      index := !index + 1;
    done;
    !to_return


let abstract_state_down n_disks h_state = 
  let abstracted_state = make_empty n_disks (n_pegs h_state) in
    Array.iteri 
      (
	fun index item ->
	  let current_target_index = ref 0 in
	    for i = 0 to Char.code h_state.top_index.(index) do
	      if((Char.code item.(i)) <= n_disks) then
		(
		  abstracted_state.pegs.(index).(!current_target_index) <- 
		    item.(i);
		  abstracted_state.top_index.(index) <-
		    (Char.chr !current_target_index);

		  current_target_index := !current_target_index + 1;
		)
	    done;
      )
      h_state.pegs;
    update_top_disk abstracted_state;
    abstracted_state


let abstract_state_up n_disks_ct h_state = 
  let abstracted_state = make_empty n_disks_ct (n_pegs h_state) in
  let lost_disks = (n_disks h_state) - n_disks_ct in
    Array.iteri 
      (
	fun index item ->
	  let current_target_index = ref 0 in
	    for i = 0 to Char.code h_state.top_index.(index) do
	      if((Char.code item.(i)) > ((n_disks h_state) - n_disks_ct)) then
		(
(*
		  Printf.fprintf stderr "%d %d %d %d %d\n"
		    index !current_target_index i
		    (Char.code item.(i))
		    (((n_disks h_state) - n_disks_ct))
		  ;
		  flush stderr;
*)
		  abstracted_state.pegs.(index).(!current_target_index) <- 
		    (Char.chr ((Char.code item.(i)) - lost_disks));
		  abstracted_state.top_index.(index) <-
		    (Char.chr !current_target_index);
		  current_target_index := !current_target_index + 1;
		)
	    done;
      )
      h_state.pegs;
    update_top_disk abstracted_state;
    abstracted_state

let read_from_file infile_name = 
  let inch = open_in infile_name in
  let state_to_return = 
    read inch in
    close_in inch;
    state_to_return;

