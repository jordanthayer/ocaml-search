(* $Id: incr_dfs.ml,v 1.2 2003/09/11 21:45:42 ruml Exp ruml $

   an incremental version of improved limited-discrepancy search
*)


open Incrs


type 'a frame = {
  node : 'a;
  (* which child next? *)
  mutable index : int;
}


let make_frame n =
  { node = n;
    index = 0; }


let init
  (** returns 4 closures *)
  ?(optimal_p = (Wrutils.constantly1 false))
  ?(prune_p = (Wrutils.constantly2 false))
  ?(prev_best = None)
  ?(log = Wrutils.no_op2)
  ?(halt = Info.Never)
  copy_state
  max_depth
  better_p
  leaf_p
  get_child
  initial
  =
  let info = new Info.basic get_child leaf_p better_p optimal_p copy_state
	       prune_p log prev_best halt in
  let stack = ref [(make_frame initial)] in
  let done_p () =
    (** true iff search space exhausted *)
    !stack = []
  and do_step () =
    (** returns ??? *)
    match !stack with
      frame::rest ->
	let node = frame.node in
	  stack := (if (info#leaf_p node)
		    then (if (info#check_best_and_optimal node)
			    (* have optimal solution - curtail search *)
			  then []
			  else rest)
		    else if (info#prune_p node)
		    then rest
		    else (let index = frame.index in
			    if index == 0 then info#incr_branches;
			    match info#call_child node index with
			      None -> rest
			    | Some c ->
				frame.index <- index + 1;
				(make_frame c)::!stack))
     (* we have exhausted the search space *)
    | [] -> ()
  and get_best () =
    (** best solution seen so far as an optional value *)
    info#curr_best
  and stats () =
    (** returns an Info.stats structure *)
    info#curr_stats
  in
    { exhausted_p = done_p;
      do_step = do_step;
      curr_best = get_best;
      curr_stats = stats }


let test () =
  let s = init
	    ~optimal_p:Bounded_depth_test.optimal_p
	    Bounded_depth_test.better_p
	    Bounded_depth_test.leaf_p
	    Bounded_depth_test.get_child
	    (Bounded_depth_test.make_initial 2 3)
  in
    while not (s.exhausted_p ()) do
      s.do_step ()
    done;
    s.curr_stats ()


(* EOF *)
