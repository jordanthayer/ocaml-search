(* maintaining search stats and parameters

*)


(********** logging *********)


let make_simple_logger node_cost saved_cost ch =
  (** logs every leaf *)
  Verb.pf Verb.always ch "# Nodes\tLeaves\tCurr\tBest\tTime\n" ;
  let start = Sys.time () in
    fun i node ->
      match i#curr_best with
	None -> failwith "logging before setting best?"
      | Some sol ->
	  let s = i#curr_stats in
	    Verb.pf Verb.always ch "%1d\t%1d\t%g\t%g\t%g\n"
	      s.nodes s.leaves (node_cost node) (saved_cost sol)
	      (Sys.time () -. start);
	    flush ch


let make_new_best_logger get_cost strm =
  Verb.pf Verb.always strm "# Nodes\tLeaves\tBest\tTime\n" ; flush strm ;
  let start = Sys.time ()
  and prev = ref None in
    (fun i n ->
       let best = i.best_so_far in
	 if (best != !prev)
	 then (
	   prev := best ;
	   match best with
	     None -> failwith "logging before checking better_than?"
	   | Some sol ->
	       (Verb.pf Verb.always strm "%1d\t%1d\t%e\t%e\n"
		  i.nodes i.leaves (get_cost sol)
		  (Sys.time () -. start)) ;
	       flush strm))

      
(**************************** object based **********************)


(* type 'a log_info =
    < curr_best :  'a option ;
 curr_stats : int * int * int * int >
*)		      

class ['node, 'saved] basic
  (* type parameterized by search node and saved search node types.
     methods aren't actually polymorphic, given these class parameters *)
  (do_get_child : 'node -> int -> 'node option)
  (do_leaf_p : 'node -> bool)
  (do_better_p : 'node -> 'saved -> bool)
  (do_optimal_p : 'saved -> bool)
  (do_copy_state : 'node -> 'saved)
  (do_prune_p : 'saved -> 'node -> bool)
  do_log (* : 'saved log_info -> 'node -> unit) *)
  prev_best
  halt_spec =
object (self)
  (* nodes generated = leaves + branches + prunes *)
  val mutable nodes = 0
  val mutable leaves = 0
  val mutable branches = 0
  val mutable prunes = 0
  val mutable best_so_far = prev_best
			      (* mutable only for initialization *)
  val mutable halt_p = (fun () -> false)

			 
  method optimal_so_far =
    (match best_so_far with
       None -> false
     | Some sol -> do_optimal_p sol)


  method leaf_or_prune_p n =
    (do_leaf_p n) ||
    (match best_so_far with
       None -> false
     | Some sol -> do_prune_p sol n)

    
  method make_halt spec =
    match spec with
      Never -> (fun () -> false)
    | Nodes max -> (fun () -> nodes >= max)
    | Leaves max -> (fun () -> leaves >= max)
    | Branches max -> (fun () -> branches >= max)
    | Time duration ->
	let limit = duration +. Sys.time () in
	  (fun () -> Sys.time () >= limit)
	  
  initializer halt_p <- self#make_halt halt_spec
    
  method check_halt = halt_p ()
			
  method get_halt = halt_p
		      
  method set_halt_or spec f =
    let n = self#make_halt spec in
      halt_p <- (fun () -> (n ()) || (f ()))

	
  method leaf_p n =
    if (do_leaf_p n)
    then (leaves <- leaves + 1;
	  true)
    else false

      
  method prune_p = fun n ->
    if (match best_so_far with
	  None -> false
	| Some s -> do_prune_p s n)
    then (prunes <- prunes + 1;
	  true)
    else false

      
  method check_best_and_optimal n =
    (* also calls logger *)
    if (match best_so_far with
	  None -> true
	| Some sol -> do_better_p n sol)
    then best_so_far <- Some (do_copy_state n);
    do_log self n;
    do_optimal_p (match best_so_far with
		    None -> failwith "impossible"
		  | Some s -> s)

      
  method incr_branches =
    branches <- branches + 1


  method call_child n index =
    let c = do_get_child n index in
      (match c with
	 None -> ()
       | Some _ -> nodes <- nodes + 1);
      c
	

  method curr_stats =
    { nodes = nodes;
      leaves = leaves;
      branches = branches;
      prunes = prunes;
    }


  method curr_best = best_so_far
		       
end

  
(* EOF *)
