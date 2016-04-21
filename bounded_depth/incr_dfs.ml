(* $Id: incr_dfs.ml,v 1.2 2003/09/11 21:45:42 ruml Exp ruml $

   an incremental version of depth-first search
*)


open Incrs


type 'a frame = {
  node : 'a;
  (* which child next? *)
  mutable index : int;
  mutable max : int;
}


let make_frame n =
  { node = n;
    index = 0;
    max = -1; }


let init
  (** returns 4 closures *)
  ?(optimal_p = (Fn.constantly1 false))
  ?(copy_state = Fn.identity)
  ?(prune_p = (Fn.constantly2 false))
  ?(prev_best = None)
  ?(log = Fn.no_op2)
  better_p
  leaf_p
  num_children
  get_child
  initial
  =
  let halt = [Info.Never] in
  let info = Info.make num_children get_child leaf_p better_p optimal_p
	       copy_state prune_p log prev_best halt in
  let stack = ref [(make_frame initial)] in
  let done_p () =
    (** true iff search space exhausted *)
    !stack = []
  and do_step () =
    match !stack with
      frame::rest ->
	let node = frame.node in
	  stack := (if (Info.leaf_p info node)
		    then (if (Info.optimal_p info node)
			    (* have optimal solution - curtail search *)
			  then []
			  else rest)
		    else if (Info.prune_p info node)
		    then rest
		    else (let index = frame.index in
			    if index == 0 then
			      (Info.incr_branches info;
			       frame.max <- num_children node);
			    if index < frame.max then
			      let c = Info.get_child info node index in
				frame.index <- index + 1;
				(make_frame c)::!stack
			    else
			      rest))
	    (* we have exhausted the search space *)
    | [] -> ()
  and get_best () =
    (** best solution seen so far as an optional value *)
    Info.curr_best info
  and stats () =
    Info.stats info
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
	    Bounded_depth_test.num_children
	    Bounded_depth_test.get_child
	    (Bounded_depth_test.make_initial 2 3)
  in
    while not (s.exhausted_p ()) do
      s.do_step ()
    done;
    s.curr_stats ()


(* EOF *)
