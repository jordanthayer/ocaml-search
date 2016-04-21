(** 

    Topspin puzzle
    
    A representation of the (N,K) top-spin puzzle.
    N tokens are arranged in a ring.
    K elements can be flipped at any time (order reversed) 

    The heuristic is the number of disks that are not adjacent to
    their neighbor, which is inadmissible.

    Christopher Wilt
    
*)




type ts_state = 
    {
      disks: int array;
      last_flip: int;
      depth: int;
    }


let make_child s f = 
  {
    disks = Array.copy s.disks;
    last_flip = f;
    depth = s.depth + 1;
  }


type ts_problem = 
    {
      n_disks: int;
      k_flipper: int;
      initial_configuration: ts_state
    }


let make_goal n_disks = 
  {
    disks = Array.init n_disks Fn.identity;
    last_flip = (-1);
    depth = 0;
  }


let make_initial initial = 
  {
    disks = initial;
    last_flip = (-1);
    depth = 0;
  }



let make_hash tsp = 
  Array_hasher.hash_array_function tsp.n_disks tsp.n_disks


let key s = s.disks


let hash_compare a b = 
 let rec do_eq (a:int array) b i =
    if i >= (Array.length a) then true
    else a.(i) = b.(i) && do_eq a b (i + 1)
  in do_eq a b 0


let to_ch_key outch tss = 
  Wrarray.fprint_array outch string_of_int " " tss.disks;
  Printf.fprintf outch "\n"


let to_string tsk = 
  let buf = Buffer.create 80 in
    for i = 0 to (Array.length tsk) - 1
    do
      Buffer.add_string buf (string_of_int tsk.(i));
      Buffer.add_string buf " ";
    done;
    Buffer.contents buf


let sol_length s = 
  s.depth


exception Notgoal


let is_goal s = 
  try
    let current_value = ref s.disks.(0) in  
      for i = 0 to (Array.length s.disks) - 2
      do
	if(!current_value = (Array.length s.disks) - 1) then
	  current_value := (-1);
	if (!current_value + 1 != s.disks.(i+1)) then
	  raise Notgoal;
	current_value := s.disks.(i+1);
      done;
      true
  with Notgoal -> false 


let cycle_left s = 
  let push_left ix = 
    let new_index = (ix + 1) mod ((Array.length s.disks)) in
      s.disks.(ix) <- s.disks.(new_index) in
  let first_slot = s.disks.(0) in
    for i = 0 to ((Array.length s.disks) - 1)
    do
      push_left i;
    done;
    s.disks.((Array.length s.disks) - 1) <- first_slot


let swap a i1 i2 =
  (**helper function that swaps indices in an array.*)
  let temp = a.(i1) in
    a.(i1)<- a.(i2);
    a.(i2)<- temp


let use_turnstile s p index = 
  assert(index < Array.length s.disks);
  for i = 0 to (p.k_flipper / 2) - 1
  do
    let first_index = (index + i) mod p.n_disks in
    let second_index = (index - i + p.k_flipper - 1) mod p.n_disks in
      swap s.disks first_index second_index
  done


let find_flips total_flips last_flip = 
  List.filter 
    (fun a -> a != last_flip)
    (Wrutils.map_n Fn.identity (total_flips - 1))


let make_expand prob = 
  fun s g -> 
    let flips_to_do = find_flips prob.n_disks s.last_flip in
      List.map 
	(
	  fun flip ->
	    let new_child = make_child s flip in
	      use_turnstile new_child prob flip;
	      new_child, (g +. 1.0)
	)
	flips_to_do


(*

  #use "use.ml";;

  let ts = Topspin.make_goal 10;;

  let tp = 
  {
  Topspin.n_disks = 10;
  Topspin.k_flipper = 5;
  Topspin.initial_configuration = ts;
  };;

  let expand = Topspin.make_expand tp;;
  
*)

(* EOF *)
