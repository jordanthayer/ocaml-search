(*

  implementation of the bulb style of beam search proposed by david
  furcy and sven koenig in Furcy and Koenig (dammit people need to put
  the date and venue in their publications, preferably in the
  beginning in very large letters)

*)

open Beam
open Search_interface

(*
type 'a bulb_node = {
  data : 'a;
  cost: float;
  mutable incurred_cost: float;
  mutable heap_index: int;
  mutable p_unexpanded_children: float;
}
*)


type 'a slice_packet = {
  slice : 'a node list;
  value : int;
  index : int
}


let generate_new_successors
    sif
    (node_set: 'a node list)
    hash_table
    (expand: 'a node -> 'a node list)
    (ordered_p: 'a node -> 'a node -> bool)
    (key: 'a node -> 'b)
    (limit_container: 'a Beam.node Limit.info)
    (key_printer : 'a Beam.node -> string)
    =
  (**
     thing actually returns an array so the items can be accessed
     randomly, just like specified in the algorithm.  They do need to
     be randomly accessed.
  *)
  let is_duplicate test_node =
    if(Htable.mem hash_table (key test_node)) then
      (Limit.incr_dups limit_container; false;)
    else true;
  in
  let node_lt n1 n2 = if((ordered_p n1 n2)) then -1 else 1 in
  let children_list = ref [] in

    List.iter (fun n ->
		 List.iter (fun child ->
			      if(Limit.halt_p limit_container) then ()
			      else
				(Limit.incr_gen limit_container;
				 if(is_duplicate child) then
				   Wrutils.push child children_list))
		   (Limit.incr_exp limit_container; expand n))
      node_set;
    let children_array = Array.of_list(!children_list) in
      Array.fast_sort node_lt children_array;
      children_array


let next_slice
    sif
    (depth:int)
    (index:int)
    (is_goal: 'a node -> bool)
    (beam_width: int)
    (node_capacity: int)
    (key: 'a node -> 'b)
    hash_table
    (expand: 'a node -> 'a node list)
    (ordered_p: 'a node -> 'a node -> bool)
    (i: 'a Beam.node Limit.info)
    (current_layer : 'a node array)
    (key_printer : 'a node -> string)=
    if (((Array.length current_layer) = 0) or
	  (index = Array.length current_layer)) then
      (
	{
	  slice= [];
	  value= 0;
	  index= max_int
	}
      )
    else if (Wrarray.exists is_goal current_layer) then
      (
	let goal_node_index = Wrarray.find is_goal current_layer in
	let goal_node = current_layer.(goal_node_index) in
	  Limit.new_incumbent i (Limit.Incumbent (0., goal_node));
	    {
	      slice= [];
	      value= depth + 1;
	      index= -1
	    }
      )
    else
      (
	let checked_node_count = ref index in
	let succs_list = ref [] in
	(*
	  start at i and keep looking for baem_width new elements to
	  add to the next list.
	*)
	  while ((List.length !succs_list) < beam_width &&
		   !checked_node_count < Array.length current_layer) do
	    if(not(Htable.mem hash_table
		 (key current_layer.(!checked_node_count)))) then
		   (
		     Htable.add hash_table
		       (key current_layer.(!checked_node_count))
		       (current_layer.(!checked_node_count)) ;
		     succs_list := current_layer.(!checked_node_count)
		     :: !succs_list;
		   );

	    checked_node_count := !checked_node_count + 1;
	  done;

	  (* This thing is supposed to have some way to check if it
	     filled up the hash table, but I'm not sure how to get
	     the hash table's maximum size into the search, other
	     than maybe hard coding it, but that seems to be a bit
	     lame.  Maybe pull it off the limit, if I can find some
	     way to get that piece of information. *)

	  if ((Htable.length hash_table) > node_capacity) then
	    (
	      (* have to remove succs_list from the hash table if its
		 full now, then return an empty slice. *)
	      List.iter (fun (item:'a node) -> Htable.remove
			   hash_table (key item))
		!succs_list;
	      {
		slice= [];
		value= max_int;
		index= -1;
	      }
	    )
	  else
	    (* return a regular old slice *)
	    {
	      slice= (!succs_list);
	      value= -1;
	      index= (!checked_node_count);
	    }
      )


let rec bulb_probe
    ?(print_size = false)
    sif
    (depth: int)
    (discrepencies: int)
    (beam_width: int)
    (node_capacity: int)
    hash_table
    expand
    (i : 'a Beam.node Limit.info)
    (parent_nodes : 'a node list) =

  if(print_size) then
    (
      Datafile.write_pairs stdout 
	["nodes this level",(string_of_int (List.length parent_nodes))];);
  let key_printer =  (fun a -> String.concat " "
			[(sif.key_printer (wrap sif.key a));
			 "f value:";
			 (string_of_float a.incurred_cost);
			 "h value:";
			 (string_of_float a.cost);
			]
		     ) in
  let ordered_p = f_ordered in
  let key = (wrap sif.key) in
  let is_goal = (wrap sif.goal_p) in

  let generate_this_level_nodes () = generate_new_successors sif parent_nodes
    hash_table expand ordered_p key i key_printer in
  let slicelet = ref(next_slice sif depth 0 is_goal beam_width
		       node_capacity key hash_table expand ordered_p i
		       (generate_this_level_nodes ()) key_printer) in

    (* found a solution *)
    if (!slicelet.value >= 0) then ()
      (* another solution check *)
    else if (i.Limit.incumbent != Limit.Nothing) then ()
      (* if its time to quit as defined by the limit, do nothing. *)
    else if (Limit.halt_p i) then ()
    else if (discrepencies = 0) then
      (
	if (Wrlist.empty !slicelet.slice) then ()
	else (
	  bulb_probe
	    ~print_size:print_size
	    sif
	    (depth + 1)
	    0
	    beam_width
	    node_capacity
	    hash_table
	    expand i !slicelet.slice;
	  List.iter (fun item -> Htable.remove hash_table (key item))
	    !slicelet.slice;)
      )
    else (
      if (not (Wrlist.empty !slicelet.slice)) then
	(
	  List.iter (fun item -> Htable.remove
		       hash_table (key item)) !slicelet.slice;
	);
      (* here is where all the less optimal nodes are gonna get
	 processed *)
      let current_index = ref !slicelet.index in
      let rec process_bad_option_list () =
	slicelet := (next_slice sif depth !current_index is_goal beam_width
		       node_capacity key hash_table expand ordered_p i
		       (generate_this_level_nodes ()) key_printer);
	if(!slicelet.value >= 0) then
	  (
	    if(!slicelet.value < max_int) then ()
	  )
	else if(Wrlist.empty !slicelet.slice) then ()
	else
	  (bulb_probe ~print_size:print_size sif (depth + 1) (discrepencies - 1)
	     beam_width node_capacity
	     hash_table expand i !slicelet.slice;
	   List.iter (fun item -> Htable.remove
			hash_table (key item)) !slicelet.slice;
	   current_index := !slicelet.index;
	   (* if you found the goal, return.*)
	   if(i.Limit.incumbent = Limit.Nothing) then
	     (process_bad_option_list ())
	   else ()) in
	process_bad_option_list ();

	slicelet := next_slice sif depth 0 is_goal
	  beam_width node_capacity key hash_table expand ordered_p
	  i (generate_this_level_nodes ()) key_printer;
	if (Wrlist.empty !slicelet.slice) then ()
	else (bulb_probe ~print_size:print_size sif (depth + 1) discrepencies beam_width
		node_capacity hash_table expand i !slicelet.slice;
	      List.iter (fun item -> Htable.remove
			   hash_table (key item)) !slicelet.slice;))



let bulb
    ?(print_size = false)
    sif
    (limit_container: 'a Beam.node Limit.info)
    (beam_width : int)
    (node_capacity: int)
    (initial: 'a node)
    (is_goal: 'a node -> bool)
    (expand: 'a node -> 'a node list)
    (ordered_p: 'a node -> 'a node -> bool)
    (better_p: 'a node -> 'a node -> bool)
    hash_compare
    hash_function
    (key: 'a node -> 'b)
    (key_printer: 'a node -> string) =

  let discrepencies = ref 0 in
  let closed_list = Htable.create hash_function hash_compare
    (node_capacity * 2) in
    Htable.add closed_list (key initial) initial;
    let rec bulb_loop () =
      (
	if(limit_container.Limit.incumbent != Limit.Nothing) then ()
	else if (Limit.halt_p limit_container) then ()
	else (bulb_probe ~print_size:print_size sif 0 !discrepencies 
		beam_width node_capacity
		closed_list
		expand limit_container [initial;];
	      (*
		Printf.fprintf stderr "backtracking %d complete\n" !discrepencies;
	      *)
	      flush stderr;
	      discrepencies := !discrepencies + 1;
	      bulb_loop ();
	     )

      ) in
      bulb_loop ();

      Datafile.write_pairs stdout
	["backtracks",(string_of_int (!discrepencies - 1))];
      Limit.results6 limit_container




let unwrap_sol_node s =
  match s with
      Limit.Nothing -> None
    | Limit.Incumbent (q,n) -> Some (n.data, n.incurred_cost)



let call_bulb sif args =
  let beam_width = Search_args.get_int "Bulb.call_bulb" args 0 in
  let node_capacity = Search_args.get_int "Bulb.call_bulb" args 1 in
  Limit.unwrap_sol6 unwrap_sol_node (
    bulb
      sif
      (Limit.make Limit.Nothing sif.halt_on f_ordered
	 (Limit.make_default_logger (fun n -> n.incurred_cost)
	    (wrap sif.get_sol_length)))
      beam_width
      node_capacity
      (*initial state*)
      {
	data=sif.initial;
	cost=0.0;
	incurred_cost = 0.0;
	indecision = -1.0;
	heap_index = -1;
	p_unexpanded_children = 0.0
      }
      (wrap sif.goal_p)
      (wrap_expand sif.domain_expand sif.h no_record)
      f_ordered
      f_ordered
      sif.equals
      sif.hash
      (wrap sif.key)
      (fun a -> String.concat " "
	 [(sif.key_printer (wrap sif.key a));
	  "f value:";
	  (string_of_float a.incurred_cost);
	  "h value:";
	  (string_of_float a.cost);
	 ]
      )
  )

let call_printing_bulb sif args =
  let beam_width = Search_args.get_int "Bulb.call_bulb" args 0 in
  let node_capacity = Search_args.get_int "Bulb.call_bulb" args 1 in
  Limit.unwrap_sol6 unwrap_sol_node (
    bulb
      ~print_size:true
      sif
      (Limit.make Limit.Nothing sif.halt_on f_ordered
	 (Limit.make_default_logger (fun n -> n.incurred_cost)
	    (wrap sif.get_sol_length)))
      beam_width
      node_capacity
      (*initial state*)
      {
	data=sif.initial;
	cost=0.0;
	incurred_cost = 0.0;
	indecision = -1.0;
	heap_index = -1;
	p_unexpanded_children = 0.0
      }
      (wrap sif.goal_p)
      (wrap_expand sif.domain_expand sif.h no_record)
      f_ordered
      f_ordered
      sif.equals
      sif.hash
      (wrap sif.key)
      (fun a -> String.concat " "
	 [(sif.key_printer (wrap sif.key a));
	  "f value:";
	  (string_of_float a.incurred_cost);
	  "h value:";
	  (string_of_float a.cost);
	 ]
      )
  )




(*eof*)
