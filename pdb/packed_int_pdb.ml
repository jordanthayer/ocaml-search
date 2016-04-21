(** A modified version of uniform cost search which returns a hashtbl
    representing the pattern database -- Jordan *)

(* doop*)

let init_lists max_ele size key initial  =
  let pinfo = Packed_ints.info max_ele in
  let pints = Packed_ints.make_create pinfo size in
  let get = Packed_ints.make_get pinfo pints
  and set = Packed_ints.make_set pinfo pints in
    for i = 0 to (size - 1) do set i (-1) done;
    set (key initial) 0;
    pints, get, set


(*
let get_max size expand key initial equals hash =
  let max = ref 0 in
  let openlist = Dpq.create ordered_p (fun _ _ -> ()) 100 initial in
  let found = Array.create size false in
  let consider_child n =
    let kv = key n in
      if not found.(kv)
      then Dpq.insert openlist n in

  let rec expand_best () =
    if not (Dpq.empty_p openlist)
    then (let n = Dpq.extract_first openlist in
	  let kv = key n in
	    if not found.(kv)
	    then (found.(kv) <- true;
		  max := Math.imax !max n.g;
		  let children = expand n in
		    List.iter consider_child children;
		    expand_best ()))
  in
    Dpq.insert openlist initial;
    expand_best ();
    !max
*)

let rec get_max_dups size expand skey initial  =
  let current_iteration = Queue.create ()
  and next_iteration = Queue.create ()
  and current_g = ref 0
  and generated = Bitset.create size in

  let consider_child (n,g) =
    let kv = skey n in
    if not (Bitset.mem generated kv)
    then (Bitset.insert generated kv;
	  if (truncate g) = !current_g
	  then Queue.add n current_iteration
	  else Queue.add n next_iteration) in

  let rec expand_best () =
    if (Queue.is_empty current_iteration)
    then (if not (Queue.is_empty next_iteration)
	  then (current_g := !current_g + 1;
		while (not (Queue.is_empty next_iteration))
		do
		  Queue.add (Queue.pop next_iteration) current_iteration
		done;
		expand_best ()))
    else
      (let n = Queue.pop current_iteration in
       let children = expand n (float !current_g) in
	 List.iter consider_child children;
	 expand_best ())  in
    Queue.add initial current_iteration;
    expand_best ();
    !current_g



(********************* PDB Generation functions *******************************)
(*
let search max_ele size expand key initial equals hash =
  let openlist, cost, get, set = (init_lists max_ele size ordered_p key
				    initial equals hash) in
  let consider_child n =
    let kv = key n in
      Dpq.insert openlist n;
      set kv n.g  in

  let rec expand_best () =
    if not (Dpq.empty_p openlist) then
      (let n = Dpq.extract_first openlist in
	 let children = expand n in
	   List.iter consider_child children;
	   expand_best ())
  in
    expand_best ();
    cost,max_ele
*)

let search_dups max_ele psize ssize expand pkey skey initial =
  let cost, get, set  = init_lists max_ele psize pkey initial in
  let current_iteration = Queue.create ()
  and next_iteration = Queue.create ()
  and current_g = ref 0 in
  let closed = Bitset.create ssize in

  let consider_child (n,g) =
    let p_state = pkey n
    and gint = truncate g
    and s_state = skey n in
      if (get p_state) > gint
      then set p_state gint;
      if not (Bitset.mem closed s_state)
      then (Bitset.insert closed s_state;
	    if gint = !current_g
	    then Queue.add n current_iteration
	    else Queue.add n next_iteration) in


  let rec expand_best () =
    if (Queue.is_empty current_iteration)
    then (if not (Queue.is_empty next_iteration)
	  then (current_g := !current_g + 1;
		while (not (Queue.is_empty next_iteration))
		do
		  Queue.add (Queue.pop next_iteration) current_iteration
		done;
		expand_best ()))
    else
      (let n = Queue.pop current_iteration in
(*
       let kv = skey n in
*)
       let children = expand n (float !current_g) in
	 List.iter consider_child children;
	 expand_best ())  in

    Queue.add initial current_iteration;
    expand_best ();
    cost,max_ele


(***************************************************************************)
(*
let no_dups size expand key initial equals hash =
  let max_ele = (get_max size (make_expand expand)
		   (wrap key) {data = initial;
			       g = 0;} equals hash) in
    search max_ele size (make_expand expand) (wrap key)
      {data = initial; g = 0;} equals hash
*)

let dups pdb_size ?(state_size = pdb_size) expand pdb_key pdb_unrank
    ?(search_key = pdb_key) ?(search_unrank = pdb_unrank) initial equals hash =
  let max_ele = get_max_dups state_size expand search_key initial in
    Verb.pe Verb.always "Max ele: %i\n%!" max_ele;
    search_dups max_ele pdb_size state_size expand pdb_key search_key
      initial

(* EOF *)
