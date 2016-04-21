(**

   Makes a pattern database that is in a hash table.

*)


(*need a functorial hash table*)

module Pdb_hash_new = Hashtbl.Make(struct type t = Min_hanoi.min_hanoi
					    let equal (a:t) b = Min_hanoi.hash_compare a b
					      let hash = Min_hanoi.hash
						end)

module Pdb_hash = Hashtbl.Make(struct type t = Hanoi.hanoi
					    let equal (a:t) b = Hanoi.hash_compare a b
					      let hash = Hanoi.hash
						end)



let search_dups expand (initial:'a) 
= 
  (**creates a pattern database.  The inital state is assumed to be
     the goal.  The state space is enumerated outwards from this state.*)

  let pdb = Pdb_hash.create 100 in
  let open_list = Queue.create () in
    (*put the initial state into the queue and the hash table.*)
    Pdb_hash.add pdb initial (0.0);
    Queue.add initial open_list;

    let expand_node n = 
      (
	(*should only be expanding nodes for which we already have the
	  correct g value.*)
	assert(Pdb_hash.mem pdb n);
	List.iter (
	  fun (child, child_cost) ->
	    (*if the child is alrady in the hash table, its junk.*)
	    if(Pdb_hash.mem pdb child) then ()
	      (*put the child onto the open list and the hash table
		now that we have its correct g value*)
	    else
	      (
		Queue.add child open_list;
		Pdb_hash.add pdb child (child_cost);
	      )
	) (expand n (Pdb_hash.find pdb n))
      ) in

    let rec process_item () = 
      (
	match Queue.is_empty open_list with
	    true -> ()
	  | false -> 
	      (
		let n = Queue.pop open_list in
		  expand_node n;
		  process_item ();
	      )
      ) in
      process_item ();
      pdb



let search_dups_new expand (initial:'a) 
    = 
  (**creates a pattern database.  The inital state is assumed to be
     the goal.  The state space is enumerated outwards from this state.*)

  let global_child_count = ref 0 in

  let pdb = Pdb_hash_new.create 100 in
  let open_list = Queue.create () in
    (*put the initial state into the queue and the hash table.*)
    Pdb_hash_new.add pdb initial (0.0);
    Queue.add initial open_list;

    let expand_node n = 
      (
	(*should only be expanding nodes for which we already have the
	  correct g value.*)
	assert(Pdb_hash_new.mem pdb n);
	List.iter (
	  fun (child, child_cost) ->
	    global_child_count := !global_child_count + 1;
	    
	    (*if the child is alrady in the hash table, its junk.*)
	    if(Pdb_hash_new.mem pdb child) then 
	      ()
		(*put the child onto the open list and the hash table
		  now that we have its correct g value*)
	    else
	      (
		Queue.add child open_list;
		Pdb_hash_new.add pdb child (child_cost);
	      )
	) (expand n (Pdb_hash_new.find pdb n))
      ) in

    let rec process_item () = 
      (
	match Queue.is_empty open_list with
	    true -> ()
	  | false -> 
	      (
		let n = Queue.pop open_list in
		  expand_node n;
		  process_item ();
	      )
      ) in
      process_item ();
      pdb

