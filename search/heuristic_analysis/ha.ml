(**code for doing analysis on the heuristics.  Requires A* to work
   on the problem
*)

type node_type = Child | Root


let nt_to_string n = 
  match n with
      Child -> "Child"
    | Root -> "Root"


let write_h_pair nt h_star h = 
  Datafile.write_pairs stdout 
    [(nt_to_string nt),(Printf.sprintf "%f %f" h_star h)]


let heuristic_analysis (sif:('node,'key,'cost)
			  Search_interface.interface) 
    args = 
  (**does some analysis of the heuristic's quality.*)

  (*
    for each randomly selected initial state I want the following:

    heuristic at the state (h of root), h* for the state (h* of root)

    Run breadth first search out 100 nodes, and see how accurate the
    ordering is for those nodes.
  *)

  let n_children = Search_args.get_int "Ha.heuristic_analysis" args 0 in

  let root = sif.Search_interface.initial in

  let h_root = (sif.Search_interface.h root) in

  let children_ht = 
    Htable.create sif.Search_interface.hash 
      sif.Search_interface.equals
      n_children in

    Htable.add children_ht (sif.Search_interface.key root) root;

    let children_queue = Queue.create () in
      Queue.push root children_queue;

      while((Htable.length children_ht) < n_children)
      do
	let children = sif.Search_interface.domain_expand
	  (Queue.pop children_queue) 0.0 in
	  List.iter (
	    fun ((c:'node),_) -> 
	      (*add the new child to the queue if its new*)
	      Queue.push c children_queue;
	      if((Htable.length children_ht) < n_children) then
		Htable.replace children_ht (sif.Search_interface.key c) c;
	  ) children;
      done;
      
      let order_check_list = ref [] in

	Htable.iter (
	  fun _ c -> 
	    let child_h = (sif.Search_interface.h c) in
	      (*set the child's parent to itself*)
	      sif.Search_interface.parent_update c c;
	      let nsi = Search_interface.alter ~initial:(Some c) sif in
	      let (sol,_,_,_,_,_) = Astar.dups_silent nsi [||] in
		(match sol with
		     None -> failwith "A* failed to find a solution\n"
		   | Some (_,cost) -> (
		       write_h_pair Child cost child_h;);

		       order_check_list := (Order.make_n None cost
					      child_h) ::
			 !order_check_list;
		);
	) children_ht;

	let order_quality = Order.check_order !order_check_list in


	  Datafile.write_pairs stdout 
	    ["Inversion Count",(Printf.sprintf "%d" order_quality)];
	  
	  let (sol,exp,gen,pru,qs,dup) = Astar.dups_silent sif [||] in
	    (match sol with
		 None -> failwith "A* failed to find a solution\n"
	       | Some (_,cost) -> 
		   write_h_pair Root cost h_root;);
	    (sol,exp,gen,pru,qs,dup)
