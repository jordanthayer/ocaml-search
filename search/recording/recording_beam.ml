open Beam
open Search_interface

(*
change this so it writes crap out to the command line
*)
let record_wrap_expand expand h node_record =
  (fun n -> let children = List.map (fun (d, g) -> {data = d;
				     cost = (h d);
				     incurred_cost= g;
				     indecision =  -1.0;
				     heap_index = -1;
	  p_unexpanded_children = 0.0
						}
				    ) (expand n.data n.incurred_cost) in
     node_record n n children;
     children)


let call_f_beam_search_dups_noderec sif args =
  let beam_width = Search_args.get_int
    "Recording_beam.call_f_beam_search_dups_noderec" args 0 in
  let er = Recorders.expansion_recorder
    sif.key_printer
    (fun n -> sif.key n.data)
    (fun n -> n.incurred_cost)
    (fun n -> 0)
    (fun n -> n.cost) in
  let info = Limit.make Limit.Nothing sif.halt_on f_ordered
    (Limit.make_default_logger (fun n -> n.incurred_cost)
       (wrap sif.get_sol_length)) in
    Limit.unwrap_sol6 unwrap_sol_node (
      beam_search_dups
	info
	beam_width 
	(make_initial sif.initial)
	(wrap sif.goal_p)
	(record_wrap_expand sif.domain_expand sif.h (er info))
	f_ordered
	f_ordered
	sif.equals
	sif.hash
	(wrap sif.key)
	(fun a -> "")
    )

let call_beam_search_dups_noderec sif args =
  let beam_width = Search_args.get_int
    "Recording_beam.call_beam_search_dups_noderec" args 0 in
  let er = Recorders.expansion_recorder
    sif.key_printer
    (fun n -> sif.key n.data)
    (fun n -> n.incurred_cost)
    (fun n -> 0)
    (fun n -> n.cost) in
  let info = Limit.make Limit.Nothing sif.halt_on f_ordered
    (Limit.make_default_logger (fun n -> n.incurred_cost)
       (wrap sif.get_sol_length)) in
    Limit.unwrap_sol6 unwrap_sol_node (
      beam_search_dups
	info
	beam_width (make_initial sif.initial)
	(wrap sif.goal_p)
	(record_wrap_expand sif.domain_expand sif.h (er info))
	h_ordered
	h_ordered
	sif.equals
	sif.hash
	(wrap sif.key)
	(fun a -> "")
    )



let call_f_beam_search_dups_queuerec sif args =
  let beam_width = Search_args.get_int
    "Recording_beam.call_f_beam_search_dups_queuerec" args 0 in
  let queue_recorder = Recorders.lq_recorder sif.key_printer
    (fun n -> sif.key n.data) in
    Limit.unwrap_sol6 unwrap_sol_node (
      beam_search_dups
	~queue_record:queue_recorder
	(Limit.make Limit.Nothing sif.halt_on f_ordered
	   (Limit.make_default_logger (fun n -> n.incurred_cost)
	      (wrap sif.get_sol_length)))
	beam_width (make_initial sif.initial)
	(wrap sif.goal_p)
	(wrap_expand sif.domain_expand sif.h no_record)
	f_ordered
	f_ordered
	sif.equals
	sif.hash
	(wrap sif.key)
	(fun a -> "")
    )


let call_beam_search_dups_queuerec sif args =
  let beam_width = Search_args.get_int
    "Recording_beam.call_beam_search_dups_queuerec" args 0 in
  let queue_recorder = Recorders.lq_recorder sif.key_printer
    (fun n -> sif.key n.data) in
    Limit.unwrap_sol6 unwrap_sol_node (
      beam_search_dups
	~queue_record:queue_recorder
	(Limit.make Limit.Nothing sif.halt_on f_ordered
	   (Limit.make_default_logger (fun n -> n.incurred_cost)
	      (wrap sif.get_sol_length)))
	beam_width (make_initial sif.initial)
	(wrap sif.goal_p)
	(wrap_expand sif.domain_expand sif.h no_record)
	h_ordered
	h_ordered
	sif.equals
	sif.hash
	(wrap sif.key)
	(fun a -> "")
    )



let call_f_beam_search_dups_prunerec sif args =
  (*
    have to make something that will take none or some and add the somes
    and ignore the nones.

    also have to make something that can take a Limit and provide a list
    of the pruned nodes.
  *)
  let beam_width = Search_args.get_int
    "Recording_beam.call_f_beam_search_dups_prunerec" args 0 in

  let prune_tracker, prune_printer =
    let pruned = ref [] in
    let pp i =
      match !pruned with [] -> ()
	|	_ -> 
		  (*		  Verb.pe Verb.often "%i" i.Limit.expanded;*)
		  List.iter (fun (a,n) -> Verb.pe 
			       Verb.often "\npruned,%f,%f,%s,replaced:,%f,%f,%s"
			       n.incurred_cost
			       n.cost
			       (sif.key_printer (sif.key n.data))
			       a.incurred_cost
			       a.cost
			       (sif.key_printer (sif.key a.data))

			    )
		    (!pruned);
		  (*		  Verb.pe Verb.often "\n";*)
		  pruned := []
    in
    let pt (newnode,pn) = 
      if(sif.equals (sif.key pn.data) (sif.key newnode.data))
      then ()
      else 
	pruned := (newnode,pn) :: !pruned
    in 
      pt, pp in

    Limit.unwrap_sol6 unwrap_sol_node (
      beam_search_dups
	~prune_tracker:prune_tracker
	~prune_printer:prune_printer
	(Limit.make Limit.Nothing sif.halt_on f_ordered
	   (Limit.make_default_logger (fun n -> n.incurred_cost)
	      (wrap sif.get_sol_length)))
	beam_width (make_initial sif.initial)
	(wrap sif.goal_p)
	(wrap_expand sif.domain_expand sif.h no_record)
	f_ordered
	f_ordered
	sif.equals
	sif.hash
	(wrap sif.key)
	(fun a -> "")
    )

let call_f_beam_search_dups_prunecount sif args =
  (*
    have to make something that will take none or some and add the somes
    and ignore the nones.

    also have to make something that can take a Limit and provide a list
    of the pruned nodes.
  *)
  let beam_width = Search_args.get_int
    "Recording_beam.call_f_beam_search_dups_prunecount" args 0 in
  let prune_tracker, prune_printer =
    let pruned = ref [] in
    let pp i =
      Verb.pe Verb.often "%i" i.Limit.expanded;
      Verb.pe Verb.often "total nodes pruned: %i" (List.length !pruned);
      Verb.pe Verb.often "\n";
      pruned := []; in
    let pt (newnode,n) = 
      pruned := (newnode,n) :: !pruned in
      pt, pp in
    Limit.unwrap_sol6 unwrap_sol_node (
      beam_search_dups
	~prune_tracker:prune_tracker
	~prune_printer:prune_printer
	(Limit.make Limit.Nothing sif.halt_on f_ordered
	   (Limit.make_default_logger (fun n -> n.incurred_cost)
	      (wrap sif.get_sol_length)))
	beam_width (make_initial sif.initial)
	(wrap sif.goal_p)
	(wrap_expand sif.domain_expand sif.h no_record)
	f_ordered
	f_ordered
	sif.equals
	sif.hash
	(wrap sif.key)
	(fun a -> "")
    )


let call_f_bf_beam_search_dups_noderec sif args =
  let beam_width = Search_args.get_int
    "Recording_beam.call_f_bf_beam_search_dups_noderec" args 0 in
  let er = Recorders.expansion_recorder
    sif.key_printer
    (fun n -> sif.key n.data)
    (fun n -> n.incurred_cost)
    (fun n -> 0)
    (fun n -> n.cost) in
  let info = Limit.make Limit.Nothing sif.halt_on f_ordered
    (Limit.make_default_logger (fun n -> n.incurred_cost)
       (wrap sif.get_sol_length)) in
    Limit.unwrap_sol6 unwrap_sol_node (
      bf_beam_search_dups
	info
	beam_width (make_initial sif.initial)
	(wrap sif.goal_p)
	(record_wrap_expand sif.domain_expand sif.h (er info))
	f_ordered
	f_ordered
	sif.equals
	sif.hash
	(wrap sif.key)
    )





let call_f_bf_beam_search_dups_prunerec sif args =
  (*
    have to make something that will take none or some and add the somes
    and ignore the nones.

    also have to make something that can take a Limit and provide a list
    of the pruned nodes.
  *)
  let beam_width = Search_args.get_int
    "Recording_beam.call_f_bf_beam_search_dups_prunerec" args 0 in
  let prune_tracker, prune_printer =
    let pruned = ref [] in
    let pp i =
      match !pruned with [] -> ()
	|	_ -> 
(*		  Verb.pe Verb.often "%i" i.Limit.expanded;*)
		  List.iter (fun (a,n) -> Verb.pe 
			       Verb.often "pruned,%f,%f,%s,replaced:,%f,%f,%s\n"
			       n.incurred_cost
			       n.cost
			       (sif.key_printer (sif.key n.data))
			       a.incurred_cost
			       a.cost
			       (sif.key_printer (sif.key a.data))

			    )
		    (!pruned);
(*		  Verb.pe Verb.often "\n";*)
		  pruned := []
    in
    let pt (newnode,pn) = match pn with 
	None -> ()
      | Some a -> 
	  (
	    if(sif.equals (sif.key a.data) (sif.key newnode.data))
	    then ()
	    else 
	    pruned := (newnode,a) :: !pruned
	  )
    in 
      pt, pp in

    Limit.unwrap_sol6 unwrap_sol_node (
      bf_beam_search_dups
	~prune_tracker:prune_tracker
	~prune_printer:prune_printer
	(Limit.make Limit.Nothing sif.halt_on f_ordered
	   (Limit.make_default_logger (fun n -> n.incurred_cost)
	      (wrap sif.get_sol_length)))
	beam_width (make_initial sif.initial)
	(wrap sif.goal_p)
	(wrap_expand sif.domain_expand sif.h no_record)
	f_ordered
	f_ordered
	sif.equals
	sif.hash
	(wrap sif.key)
    )
