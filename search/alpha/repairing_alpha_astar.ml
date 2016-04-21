(** Restarting Alpha A*  Jordan - Feb 2010 *)

type 'a node = {
  data : 'a;
  g : float;
  f : float;
  in_p : bool;
  mutable cost : float;
  mutable pos : int;
  parent : 'a node}

let wrap f =
  (** takes a function to be applied to the data payload
      such as the goal-test or the domain heuristic and
      wraps it so that it can be applied to the entire
      node *)
  (fun n -> f n.data)


let unwrap_sol s =
  (** Unwraps a solution which is in the form of a search node and presents
      it in the format the domain expects it, which is domain data followed
      by cost *)
  match s with
      Limit.Nothing -> None
    | Limit.Incumbent (q,n) -> Some (n.data, n.g)


let cost_then_g a b =
  (** expansion ordering predicate, and also works for ordering duplicates
      assuming that h is the same for both
      (hence f will be lower when g is lower). *)
  ((a.cost : float) < b.cost) ||
  ((a.cost = b.cost) && (a.g >= b.g))


let just_f a b =
  (** Sorts nodes solely on total cost information *)
  (a.f : float) <= b.f


let setpos n i =
  (** Sets the location of a node, used by dpq's *)
  n.pos <- i


let getpos n =
  (** Returns the position of the node in its dpq.
      Useful for swapping nodes around on the open list *)
  n.pos


let make_alpha_g () =
  let last = ref neg_infinity in
    (fun n -> n.parent.g >= !last),
  (fun last_expand _ -> last := last_expand.g)


let make_alpha_h () =
  let last = ref infinity in
    (fun n -> (n.f -. n.g) <= !last),
  (fun last_expand _  ->
     last := (last_expand.f -. last_expand.g))


let make_alpha_prime_g () =
  let furthest = ref neg_infinity in
    (fun n -> n.g >= !furthest),
  (fun _ chlidren ->
     List.iter (fun c -> furthest := Math.fmax !furthest c.g))


let make_alpha_prime_h () =
  let closest = ref infinity in
    (fun n -> (n.f -. n.g) <= !closest),
  (fun _ chlidren ->
     List.iter (fun c -> closest := Math.fmin !closest (c.f -. c.g)))


let search new_wt fixcost alpha update expand key goal initial i hash equals =
  let openlist = Dpq.create cost_then_g setpos 100 (fst initial)
  and closed = Htable.create hash equals 100
  and incos = Htable.create hash equals 100
  and previous_itteration = -17 in

  let insert (n1,n2) =
    Limit.incr_gen i;
    if not (Limit.promising_p i n1)
    then Limit.incr_prune i
    else (let state = key n1 in
	    try
	      (let p1,p2 = Htable.find closed state in
		 if not (just_f p1 n1)
		 then (Htable.replace closed state (n1,n2);
		       (if getpos p1 >= 0
			then (Dpq.swap openlist (getpos p1) n1;
			      Dpq.swap openlist (getpos p2) n2)
			else (if getpos p1 = previous_itteration
			      then (Dpq.insert openlist n1;
				    Dpq.insert openlist n2)
			      else (try let (p1,p2) = Htable.find incos state in
				      if not (just_f p1 n1)
				      then Htable.replace incos state (n1,n2)
				    with Not_found ->
				      Htable.add incos state (n1,n2))))))
	    with Not_found ->
	      Htable.add closed state (n1,n2);
	      Dpq.insert openlist n1;
	      Dpq.insert openlist n2) in

  let extract_best () =
    let iterate, _ = Dpq.make_iterator_unsafe openlist in
    let rec go n0 =
      match iterate() with
	| None -> failwith "Reached end of queue?"
	| Some n -> (let in_p = alpha n in
		       if in_p = n.in_p
		       then n
		       else (if not n.in_p
			     then (match n0 with
				       None -> go (Some n)
				     | Some m -> (if n.cost < m.cost
						  then go (Some n)
						  else go n0))
			     else  (match n0 with
					None -> go n0
				      | Some m -> (if n.cost > m.cost
						   then m
						   else go n0)))) in
    let next = go None in
    let (in_p,nin_p) = Htable.find closed (key next) in
      Dpq.remove openlist (getpos in_p);
      Dpq.remove openlist (getpos nin_p);
      next in

  let rec do_loop () =
    if not (Limit.halt_p i)
    then
      (if (Dpq.empty_p openlist)
       then (if new_wt ()
	     then
	       (Htable.iter (fun key ele ->
			       if (getpos (fst ele)) = Dpq.no_position
			       then (setpos (fst ele) previous_itteration;
				     setpos (snd ele) previous_itteration))
		  closed;
		Htable.iter
		  (fun key ele ->
		     Htable.replace closed key ele;
		     fixcost (snd ele);
		     Dpq.insert openlist (fst ele);
		     Dpq.insert openlist (snd ele)) incos;
		Htable.clear incos;
		do_loop ()))
       else (let next = extract_best () in
	       if goal next
	       then (Limit.new_incumbent i (Limit.Incumbent (0.,next));
		     if new_wt ()
		     then
		       (Dpq.iter (fun e -> fixcost e;
				    Dpq.see_update openlist (getpos e)) openlist;
			Htable.iter
			  (fun key ele ->
			     Htable.replace closed key ele;
			     fixcost (snd ele);
			     Dpq.insert openlist (fst ele);
			     Dpq.insert openlist (snd ele)) incos;
			Htable.clear incos;
			do_loop ()))
	       else (let children = expand next in
		       update next (List.map fst children);
		       Limit.incr_exp i;
		       List.iter insert (expand next);
		       do_loop ())))
  in
    insert initial;
    do_loop();
    i


let make_expand w1 wtlist expand h =
  (** Takes the domain expand function and a heuristic calculator
      and creates an expand function which returns search nodes. *)
  assert (0. < w1);
  let wts = ref wtlist in
    (fun n ->
       let w2 = List.hd !wts in
	 List.map
	   (fun (d,g) ->
	      let f = g +. (h d) in
		{data = d; f = f; g = g; in_p = true;
		 cost =  w1 *. f; pos = Dpq.no_position; parent = n;},
	      {data = d; f = f; g = g; in_p = false;
	       cost =  w2 *. f; pos = Dpq.no_position; parent = n;})
	   (expand n.data n.g)),
  (fun () -> wts := List.tl !wts; List.length !wts > 0),
  (fun n -> if n.cost > n.f then n.cost <- n.f *. (List.hd !wts))

let make_ipair init =
  let rec n =
    {data = init;
     g = 0.;
     f = neg_infinity;
     in_p = true;
     cost = neg_infinity;
     pos = Dpq.no_position;
     parent = n} in
    n, {n with in_p = false}


(* make the assumption that w2 = bound and w1 = 1 *)
let no_dups (alpha, update) (expand,next,fixcost) sface =
  Limit.unwrap_sol5 unwrap_sol
    (Limit.results5
       (search
	  next
	  fixcost
	  alpha
	  update
	  expand
	  (wrap sface.Search_interface.key)
	  (wrap sface.Search_interface.goal_p)
	  (make_ipair sface.Search_interface.initial)
	  (Limit.make Limit.Nothing sface.Search_interface.halt_on
	     just_f (Limit.make_default_logger (fun n -> n.f)
		    (wrap sface.Search_interface.get_sol_length)))
	  sface.Search_interface.hash
	  sface.Search_interface.equals))


let dups (alpha, update) (expand,next,fixcost) sface =
  Limit.unwrap_sol6 unwrap_sol
    (Limit.results6
       (search
	  next
	  fixcost
	  alpha
	  update
	  expand
	  (wrap sface.Search_interface.key)
	  (wrap sface.Search_interface.goal_p)
	  (make_ipair sface.Search_interface.initial)
	  (Limit.make Limit.Nothing sface.Search_interface.halt_on
	     just_f (Limit.make_default_logger (fun n -> n.f)
		    (wrap sface.Search_interface.get_sol_length)))
	  sface.Search_interface.hash
	  sface.Search_interface.equals))

(* Search Calls *)

let alpha_g_nodups sface args =
  let bound = Search_args.get_float "Alpha_astar.alpha_g_nodups" args 0 in
  no_dups (make_alpha_g ())
    (make_expand 1. (Arastar.mk_wtlist bound 0.2)
       sface.Search_interface.domain_expand
       sface.Search_interface.h) sface

and alpha_g_dups sface args =
  let bound = Search_args.get_float "Alpha_astar.alpha_g_dups" args 0 in
  dups (make_alpha_g ())
    (make_expand 1. (Arastar.mk_wtlist bound 0.2)
       sface.Search_interface.domain_expand
       sface.Search_interface.h) sface

let alpha_h_nodups sface args =
  let bound = Search_args.get_float "Alpha_astar.alpha_h_nodups" args 0 in
  no_dups (make_alpha_h ())
    (make_expand 1. (Arastar.mk_wtlist bound 0.2)
       sface.Search_interface.domain_expand
       sface.Search_interface.h) sface

and alpha_h_dups sface args =
  let bound = Search_args.get_float "Alpha_astar.alpha_h_dups" args 0 in
  dups (make_alpha_h ())
    (make_expand 1. (Arastar.mk_wtlist bound 0.2)
       sface.Search_interface.domain_expand
       sface.Search_interface.h) sface

let alpha_primeg_nodups sface args =
  let bound = Search_args.get_float "Alpha_astar.alpha_primeg_nodups" args 0 in
  no_dups (make_alpha_prime_g ())
    (make_expand 1. (Arastar.mk_wtlist bound 0.2)
       sface.Search_interface.domain_expand
       sface.Search_interface.h) sface

and alpha_primeg_dups sface args =
  let bound = Search_args.get_float "Alpha_astar.alpha_primeg_dups" args 0 in
  dups (make_alpha_prime_g ())
    (make_expand 1. (Arastar.mk_wtlist bound 0.2)
       sface.Search_interface.domain_expand
       sface.Search_interface.h) sface

let alpha_primeh_nodups sface args =
  let bound = Search_args.get_float "Alpha_astar.alpha_primeh_nodups" args 0 in
  no_dups (make_alpha_prime_h ())
    (make_expand 1. (Arastar.mk_wtlist bound 0.2)
       sface.Search_interface.domain_expand
       sface.Search_interface.h) sface

and alpha_primeh_dups sface args =
  let bound = Search_args.get_float "Alpha_astar.alpha_primeh_dups" args 0 in
  dups (make_alpha_prime_h ())
    (make_expand 1. (Arastar.mk_wtlist bound 0.2)
       sface.Search_interface.domain_expand
       sface.Search_interface.h) sface

(* EOF *)
