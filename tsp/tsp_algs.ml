(* $Id: algs.ml,v 1.1 2006/06/25 00:03:07 ruml Exp ruml $

   TSP algorithms based on best-first search
*)


open Tsp_instances
open Tsp


(********** utils **********)





let make_logger () =
  (** prevent anytime A* from writing thousands of lines - only write if
    improved significantly *)
  let start = Sys.time ()
  and threshold = ref infinity in
    (fun _ g i ->
       (** cost, expanded, gen, time *)
       if g <= !threshold then
	 let t = (Sys.time ()) -. start in
	   Wrutils.pr "%f\t%d\t%d\t%f\n%!" g i.Limit.expanded
	     i.Limit.generated t;
	   threshold := g *. 0.999)

let make_logger2 () =
  (** prevent anytime A* from writing thousands of lines - only write if
    improved significantly *)
  let start = Sys.time ()
  and threshold = ref infinity in
    (fun _ g gen ex ->
       (** cost, expanded, gen, time *)
       if g <= !threshold then
	 let t = (Sys.time ()) -. start in
	   Wrutils.pr "%f\t%d\t%d\t%f\n%!" g ex
	     gen t;
	   threshold := g *. 0.999)


let make_ara_logger () =
  (** prevent anytime A* from writing thousands of lines - only write if
    improved significantly *)
  let start = Sys.time ()
  and threshold = ref infinity in
    (fun data g i epsilon ->
       let gen = i.Limit.generated
       and ex  = i.Limit.expanded in
       (** cost, expanded, gen, time *)
       if g <= !threshold then
	 let t = (Sys.time ()) -. start in
	   Wrutils.pr "%f\t%d\t%d\t%f\n%!" g ex
	     gen t;
	   threshold := g *. 0.999)
(********** algorithms ***********)


let enumerate p lim =
  (** tries to be as simple and foolproof as possible *)
  let d = p.dists
  and best = ref []
  and best_cost = ref infinity
  and calls = ref 0 in
    match (Tsp.int_list 0 ((Tsp.prob_size d) - 1)) with
	[] -> ([], 0.), 0, 0, 0, 0
      | first::cities ->
	  let rec next remaining tour cost =
	    incr calls;
	    if remaining = [] then
	      (let cost = cost +. d.(List.hd tour).(first) in
		 if cost < !best_cost then
		   (best := first::tour;
		    best_cost := cost))
	    else
	      List.iter (fun city ->
			   next (Wrlist.remove_firstq city remaining)
			     (city::tour)
			     (cost +. d.(List.hd tour).(city)))
		remaining
	  in
	    next cities [first] 0.;
	    ((List.rev !best), !best_cost), !calls, !calls, 0, 0


let nn_bb p =
  (** branch-and-bound using nearest-neighbor *)
  let d = p.dists
  and best = ref []
  and best_cost = ref infinity
  and calls = ref 0 in
    match (Tsp.int_list 0 ((Tsp.prob_size d) - 1)) with
	[] -> ([], 0.), 0, 0, 0, 0
      | first::cities ->
	  let rec next remaining tour cost =
	    incr calls;
	    let dists = d.(List.hd tour) in
	      if remaining = [] then
		(let cost = cost +. dists.(first) in
		   if cost < !best_cost then
		     (best := first::tour;
		      best_cost := cost))
	      else
		let by_dist a b =
		  if dists.(a) < dists.(b) then -1
		  else if dists.(a) > dists.(b) then 1
		  else 0
		in
		  List.iter (fun city ->
			       let new_cost = cost +. dists.(city) in
				 if new_cost < !best_cost then
				   next (Wrlist.remove_firstq city remaining)
				     (city::tour) new_cost)
		    (List.sort by_dist remaining)
	  in
	    next cities [first] 0.;
	    ((List.rev !best), !best_cost), !calls, !calls, 0, 0

let unwrap_nnbb fivet =
  Limit.unwrap_sol5 (fun (t,len) ->
		       Some (List.rev t,len))
    fivet

let branchbound p lim =
  unwrap_nnbb (nn_bb p)


let unwrap_results five_tuple =
  Limit.unwrap_sol5 (fun s ->
		       match s with
			   None -> None
			 | Some (n, f) ->
			     Some ((List.rev n.tour), n.cost_so_far))
    five_tuple

let unwrap_results2 six_tuple =
  let s,x2,x3,x4,x5,_ =
    Limit.unwrap_sol6 (fun s ->
			 match s with
			     None -> None
			   | Some (n, f) ->
			       Some ((List.rev n.tour), n.cost_so_far))
      six_tuple in
    s,x2,x3,x4,x5


let wrap_key k =
  (fun n -> float_of_int (k n))


let a_star_pk p lim =
  (** Uses Pearl and Kim heuristic *)
  unwrap_results (Astar.a_star_search
		    ~limit:lim (Tsp.make_initial p.dists p.symmetric)
		    Tsp.goal_p (Tsp.make_expand p.dists p.symmetric)
		    (Tsp.make_h_pk p.dists p.symmetric))


let a_star_mst p lim =
  (** Uses MST heuristic *)
  unwrap_results (Astar.a_star_search ~limit:lim
		    (Tsp.make_initial p.dists p.symmetric)
		    Tsp.goal_p (Tsp.make_expand p.dists p.symmetric)
		    (Tsp.make_h_mst p.dists p.symmetric))


let ida_star p lim =
  unwrap_results2
    (Dfs.ida_star ~lim:lim
       (Tsp.make_h_mst p.dists p.symmetric)
       (Tsp.make_initial p.dists p.symmetric)
       (Tsp.make_expand p.dists p.symmetric)
       Tsp.goal_p key)

let greedy p lim =
  unwrap_results (Greedy.greedy ~limit:lim
		    (Tsp.make_initial p.dists p.symmetric)
		    Tsp.goal_p (Tsp.make_expand p.dists p.symmetric)
		    (Tsp.make_h_mst p.dists p.symmetric))


let speedy p lim =
  let hd = (Tsp.make_hd p.dists p.symmetric) in
  unwrap_results (Greedy.speedy ~limit:lim
		    (Tsp.make_initial p.dists p.symmetric)
		    Tsp.goal_p
		    (Tsp.make_expand p.dists p.symmetric)
		    hd)


let wted_a_star wt p lim =
  unwrap_results (Wted_astar.wted_a_star ~limit:lim
		    (Tsp.make_initial p.dists p.symmetric)
		    Tsp.goal_p (Tsp.make_expand p.dists p.symmetric)
		    (Tsp.make_h_mst p.dists p.symmetric) wt)

let dyn_wted_a_star wt p lim =
  unwrap_results (Dwastar.dyn_wted_a_star ~limit:lim
		  (Tsp.make_initial p.dists p.symmetric) Tsp.goal_p
		  (Tsp.make_expand p.dists p.symmetric)
		  (Tsp.make_h_mst p.dists p.symmetric) wt
		  (Tsp.prob_size p.dists))


let dwa_redux wt p lim =
  unwrap_results (Dwa_redux.no_dups ~limit:lim
		    (Tsp.make_initial p.dists p.symmetric)
		    Tsp.goal_p (Tsp.make_expand p.dists p.symmetric)
		    (Tsp.make_hd p.dists p.symmetric) wt)



let dwa wt p lim =
  unwrap_results (Dwa_redux.wa_no_dups ~limit:lim
		    (Tsp.make_initial p.dists p.symmetric)
		    Tsp.goal_p (Tsp.make_expand p.dists p.symmetric)
		    (Tsp.make_hd p.dists p.symmetric) wt)


let anyt_dyn_astar wt p lim =
  unwrap_results (Anytime_dyn_astar.anytime_dyn_astar ~limit:lim
		    (Tsp.make_initial p.dists p.symmetric)
		    Tsp.goal_p (Tsp.make_expand p.dists p.symmetric)
		    (Tsp.make_h_mst p.dists p.symmetric) wt
		    (Tsp.prob_size p.dists) (make_logger ()) )


let anytime_dwaredux wt p lim =
  unwrap_results
    (Anytime_dwastar.search
       ~limit:lim
       ~weight:wt
       (Tsp.make_initial p.dists p.symmetric)
       Tsp.goal_p
       (Tsp.make_expand p.dists p.symmetric)
       (Tsp.make_hd p.dists p.symmetric)
       (make_logger ()))


(* Needs a key function written for the domain *)

let richter_l1 p lim =
   unwrap_results (Richter.richter
		     ~limit:lim
		     (Tsp.make_initial p.dists p.symmetric)
		     Tsp.goal_p
		     (Tsp.make_expand p.dists p.symmetric)
		     key
		     (Tsp.make_h_mst p.dists p.symmetric)
		     Richter.richterl1
		     (make_logger2()))

let richter_l2 p lim =
   unwrap_results (Richter.richter
		     ~limit:lim
		     (Tsp.make_initial p.dists p.symmetric)
		     Tsp.goal_p
		     (Tsp.make_expand p.dists p.symmetric)
		     key
		     (Tsp.make_h_mst p.dists p.symmetric)
		     Richter.richterl2
		     (make_logger2()))

(********** Compare **********)

(*let compare_algs fn =
  let in_chnl = open_in fn in
  let prob = read_tsplib in_chnl in
    (*test3 a_star prob;*)
    (*test3 a_star_mst prob;*)
    test5 (anytime_aseps 3.) prob*)

(* EOF *)
