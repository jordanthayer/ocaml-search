(** Best first utility guided search, YES as seen through the search
    interface *)

type 'a node = {
  (* there are several reason a field may need to be recomputed:
     1) secs_per_expand has changed
     2) correction factors (see utility_func) have changed
     3) better route to this node has been found
     each field is marked with the applicable reasons *)
  (* estimated highest utility of any goal below this node, assuming we
     head directly for it.  1,2,3 *)
  mutable est_u : float;
  (* components of est_u (corrected values), for recomputation or
     breaking ties.  1,2,3 *)
  mutable expands_to_go : float;
  (* 1,2,3 *)
  mutable est_cost : float;
  (* for expand.  note that cost_to_go = est_cost - cost_so_far. 3 *)
  mutable cost_so_far : float;
  (* the uncorrected values *)
  cheap : float * float;
  close : float * float;
  (* modified when expanded, needed for g update propagation *)
  mutable children : ('a node * float) list;
  (* for Dpq *)
  mutable q_pos : int;
  data : 'a;
}


let init_pos = -99
and closed_pos = -1

let setpos n i =
  n.q_pos <- i


let wrap f =
  (fun n -> f n.data)


type 'a hd = {
  (* estimates cost (h) and distance (d) to some solution *)
  func : 'a -> (float * float);
  (* average amount these are underestimated per step *)
  mutable h_under : float;
  mutable d_under : float;
  mutable h_accum : float;
  mutable d_accum : float;
}


let init_hd f =
  { func = f;
    h_under = 0.;
    d_under = 0.;
    h_accum = 0.;
    d_accum = 0.;
  }


type utility_func = {
  (* want separate cost and time coeffs so it's easy to specify that only
     cost or only time matters *)
  cost_coeff : float;
  time_coeff : float;
  (* utility of default (empty) solution.  stop searching if best util is
     smaller than this.  neg_infinity ensures search until solution is found
     or deadline arrives. *)
  default_u : float;
  (* non-linearity: time at which [time_coeff] goes to infinity *)
  deadline : float;
}


let init_uf cost_coeff time_coeff default_u deadline =
  { cost_coeff = cost_coeff;
    time_coeff = time_coeff;
    default_u = default_u;
    deadline = deadline; }


(******************** expansion ********************)


let corrected_u u secs_per hd (h, dist) g =
  let dist = dist *. (1. +. hd.d_under) in
  let time = dist *. secs_per
  and h = h +. (dist *. hd.h_under) in
  let cost = g +. h in
    (** - ((c1 * cost) + (c2 * time)) *)
  let u = -. ((cost *. u.cost_coeff) +. (time *. u.time_coeff)) in
    u, dist, cost


let make_utility hd_a hd_b u =
  (fun a_vals b_vals secs_per g ->
     (** returns utility and the d and f values used to compute it*)
     let (a_u, _, _) as a_stuff = corrected_u u secs_per hd_a a_vals g
     and (b_u, _, _) as b_stuff = corrected_u u secs_per hd_b b_vals g in
       if a_u >= b_u then
	 a_stuff
       else
	 b_stuff)


let vals_promising_p g (cheap_h, _) i =
  match i.Limit.incumbent with
      Limit.Incumbent (q,n) -> (g +. cheap_h) < n.cost_so_far
    | _ -> true


let make_evaluate cheap close accum uf i =
  let utility = make_utility cheap close uf in
  (fun openlist secs_per curr_time data g hook ->
    let cheap_vals = cheap.func data
    and close_vals = close.func data in
    (* see values even if will get pruned *)
    Wrutils.push (cheap_vals, close_vals, g) accum;
    if not (vals_promising_p g cheap_vals i) then
      Limit.incr_prune i
    else
      let u,d,f = utility cheap_vals close_vals secs_per g in
      let at_sol = (d *. secs_per) +. curr_time in
      if d = 0. || (at_sol <= uf.deadline && u > uf.default_u) then
	let c = { est_u = u;
		  expands_to_go = d;
		  est_cost = f;
		  cost_so_far = g;
		  cheap = cheap_vals;
		  close = close_vals;
		  children = [];
		  q_pos = init_pos;
		  data = data; } in
	Dpq.insert openlist c;
	hook c), utility


(*** redirection ***)

let child_updates = ref 0

let update_descendants openlist utility secs_per n g =
  (** update [n], which is on closed, and its descendants, using the new
      [g] and [parent] values *)
  let rec update_child g n =
    (** update [n] (possibly on open) with new [g] *)
    incr child_updates;
    let u,d,f = utility n.cheap n.close secs_per g in
      n.est_u <- u;
      n.expands_to_go <- d;
      n.est_cost <- f;
      n.cost_so_far <- g;
      if n.q_pos != closed_pos then
	(* update before others are also inconsistent *)
	Dpq.see_update openlist n.q_pos
      else
	(* already expanded *)
	List.iter (fun (child, op) ->
		     let g = g +. op in
		       (* are we child's best parent? *)
		       if g < child.cost_so_far then
			 update_child g child)
	  n.children
  in
    update_child g n


let update_open openlist secs_per utility n g =
  (** update [n], which is on open, with new [g] *)
  let u,d,f = utility n.cheap n.close secs_per g in
    n.est_u <- u;
    n.expands_to_go <- d;
    n.est_cost <- f;
    n.cost_so_far <- g;
    Dpq.see_update openlist n.q_pos


(*** main expansion functions ***)


let make_expand_basic expand cheap close see uf _ i =
  let accum = ref [] in
  let evaluate, _ = make_evaluate cheap close accum uf i in
    (fun openlist _ generated n curr_time secs_per ->
       List.iter (fun (data, g) ->
		    Limit.incr_gen i;
		    (* assume new state *)
		    evaluate openlist secs_per curr_time data g Fn.no_op1)
	 (expand n.data n.cost_so_far);
       see n !accum;
       accum := [];
       [])


let make_expand_redirect expand cheap close see uf key i =
  let accum = ref [] in
  let evaluate, utility = make_evaluate cheap close accum uf i in
    (fun openlist nodes generated n curr_time secs_per ->
       (** returns the children of [n] *)
       let children = ref [] in
	 List.iter (fun (data, g) ->
		      Limit.incr_gen i;
			let key = key data in
			  try
			    let old = Htable.find nodes key in
			      Limit.incr_dups i;
			      (* won't call evaluate - push ourselves *)
			      Wrutils.push (old.cheap, old.close, g) accum;
			      if g <  old.cost_so_far then
				(* better path to previous state *)
				if old.q_pos == closed_pos then
				  update_descendants openlist utility
				    secs_per old g
				else
				  update_open openlist secs_per utility old g
			  with Not_found ->
			    (* new state *)
			    evaluate openlist secs_per curr_time data g
			      (fun c ->
				 Htable.add nodes key c;
				 let op = c.cost_so_far -. n.cost_so_far in
				   Wrutils.push (c, op) children))
	   (expand n.data n.cost_so_far);
	 see n !accum;
	 accum := [];
	 !children)


(******************* the search loop ****************)


let util_then_time_then_cost a b =
  (** true if ordered or equal *)
  (a.est_u > b.est_u) ||
  ((a.est_u = b.est_u) &&
   ((a.expands_to_go < b.expands_to_go) ||
    ((a.expands_to_go = b.expands_to_go) &&
     (* est_cost not necessarily equal if cost_coeff = 0. *)
     ((a.est_cost < b.est_cost) ||
      ((a.est_cost = b.est_cost) &&
       (* like A*, break ties on f using high g (only applicable if f <> g) *)
       (a.cost_so_far >= b.cost_so_far))))))

let init_lists pred initial key hash equals setpos =
  let openlist = Dpq.create pred setpos 100 initial
  and nodes = Htable.create hash equals 100 in
  Dpq.insert openlist initial;
  Htable.add nodes (key initial.data) initial;
  openlist, nodes


let do_search u get_secs_per initial is_goal expand key hash equals i =
  (** differences from best_first_dups: 1) expands/sec estimation, 2) prune
      using deadline *)
  let openlist, nodes = (init_lists util_then_time_then_cost initial
			   key hash equals setpos)
  and secs_per = ref neg_infinity in
  let rec expand_best () =
    Limit.curr_q i (Dpq.count openlist);
    if not (Dpq.empty_p openlist) then
      let n = Dpq.extract_first openlist in
      if not (Limit.promising_p i n) then
	(Limit.incr_prune i;
	 (* any future duplicate will be pruned by bound *)
	 Htable.remove nodes (key n.data);
	 expand_best ())
      else if is_goal n then
	Limit.new_incumbent i (Limit.Incumbent (0., n))
      else if not (Limit.halt_p i) then
	(* Check deadline because much may have happened since the
	   pruning at generation time.  Don't call get_secs_per_expand
	   unless we expand! *)
	let curr_time = Sys.time () in
	let time_at_sol = curr_time +. (n.expands_to_go *. !secs_per) in
	if time_at_sol > u.deadline then
	  (* no need to add to closed - dups will have same
	     time_at_sol and will thus be pruned at generation *)
	  expand_best ()
	else
	  (setpos n closed_pos;
	   secs_per := get_secs_per openlist;
	   n.children <- expand openlist nodes i n curr_time !secs_per;
	   Limit.incr_exp i;
	   expand_best ())
  in
  expand_best ();
  Datafile.write_pairs stdout ["sec per expansion", string_of_float !secs_per;
			       "child updates", string_of_int !child_updates;]


(***** estimating h and d errors *****)

(* choices:
   1. learn one-step errors
   2. compute errors from trace-back of top nodes on queue
      every once in a while
*)


let best_child_vals vals =
  match vals with
      [] -> failwith "no hd data in accum?"
    | ((cheap_h, cheap_d), (close_h, close_d), g)::rest ->
	let min_f = ref (g +. cheap_h)
	and min_f_d = ref cheap_d
	and min_d = ref close_d
	and min_d_f = ref (g +. close_h) in
	  List.iter (fun ((cheap_h, cheap_d), (close_h, close_d), g) ->
		       (let cheap_f = g +. cheap_h in
			  if cheap_f < !min_f then
			    (min_f := cheap_f;
			     min_f_d := cheap_d)
			  else if ((cheap_f = !min_f) &&
				     (cheap_d < !min_f_d)) then
			    min_f_d := cheap_d);
		       let close_f = g +. close_h in
			 if close_d < !min_d then
			   (min_d := close_d;
			    min_d_f := close_f)
			 else if ((close_d = !min_d) &&
				    (close_f < !min_d_f)) then
			   min_d_f := close_f)
	    rest;
	  !min_f, !min_f_d, !min_d, !min_d_f


let init_running_average cheap close =
  let n = ref 0 in
  let see_expansion parent child_vals =
    (* if no children, error is infinite - ignore! *)
    if child_vals != [] then
      (incr n;
       let child_f, child_f_d, child_d, child_d_f =
	 best_child_vals child_vals in
	 (* cheap_hd *)
	 (let parent_h, parent_d = parent.cheap in
	    (let h_increase = child_f -. (parent.cost_so_far +. parent_h) in
	       cheap.h_accum <- cheap.h_accum +. h_increase);
	    let d_increase = child_f_d -. (parent_d -. 1.) in
	      cheap.d_accum <- cheap.d_accum +. d_increase);
	 (* close_hd *)
	 (let parent_h, parent_d = parent.close in
	    (let h_increase = child_d_f -. (parent.cost_so_far +. parent_h) in
	       close.h_accum <- close.h_accum +. h_increase);
	    let d_increase = child_d -. (parent_d -. 1.) in
	      close.d_accum <- close.d_accum +. d_increase))
  and set_corrections () =
    let n = float !n in
      cheap.h_under <- cheap.h_accum /. n;
      cheap.d_under <- cheap.d_accum /. n;
      close.h_under <- close.h_accum /. n;
      close.d_under <- close.d_accum /. n
  in
    see_expansion, set_corrections


(***************** time estimation *****************)


let reorder_q cheap close secs_per u q =
  (** recomputes utility given new secs_per and corrections *)
  let utility = make_utility cheap close u in
    Dpq.iter (fun n ->
		(let u,d,f = utility n.cheap n.close secs_per n.cost_so_far in
		   n.est_u <- u;
		   n.expands_to_go <- d;
		   n.est_cost <- f);
		(* q_pos will reflect true location of n amidst updates *)
		Dpq.see_update q n.q_pos)
      q


let make_est_secs_per cheap close est_expands_per_sec
    notify set_corrections u =
  (** returns function (to be called before every expansion) taking queue
    to secs_per.  The queue might be modified.  Continuously re-estimates. *)
  (* ALSO TRACK TOTAL AVERAGE??  Try this and see how unstable it is... *)
  let secs_per = ref (1. /. (float est_expands_per_sec))
  and current_stage = ref Fn.no_op1 in
  let do_current_stage q =
    !current_stage q;
    !secs_per
  and expands = ref 0
  and start_time = ref neg_infinity
  and check_after_expands = ref 20
  and check_after_time = ref neg_infinity in
  let rec count_until_tick q =
    (** stage 3: estimate at next tick, then reset *)
    incr expands;
    let now = Sys.time () in
      if now > !check_after_time then
	(* clock just ticked - reorder the queue! *)
	(set_corrections ();
	 secs_per := (now -. !start_time) /. (float !expands);
	 reorder_q cheap close !secs_per u q;
	 notify !secs_per cheap.h_under cheap.d_under
	   close.h_under close.d_under;
	 (* start checking clock after 1.8 * expands *)
	 check_after_expands := (!expands * 9) / 5;
	 expands := 0;
	 start_time := now;
	 (* assuming many expands per tick, otherwise would wait_for_tick *)
	 current_stage := count_until_expands)
  and count_until_expands _ =
    (** stage 2: low overhead until sufficient expands *)
    incr expands;
    if !expands > !check_after_expands then
      (check_after_time := Sys.time ();
       current_stage := count_until_tick) in
  let wait_for_tick _ =
    (** stage 1: until we hear a tick *)
    let now = Sys.time () in
      if now > !check_after_time then
	(* clock has ticked! *)
	(start_time := now;
	 current_stage := count_until_expands) in
  let init _ =
    (** stage 0: very first call *)
    check_after_time := Sys.time ();
    current_stage := wait_for_tick
  in
    current_stage := init;
    do_current_stage


(***** peripheral search utilities *******)


let wrap_incumbent incumbent u expands_per =
  match incumbent with
      None -> Limit.Nothing
    | Some (data, g) ->
	Limit.Incumbent (0., { est_u = -. (g *. u.cost_coeff);
			       expands_to_go = 0.;
			       est_cost = g;
			       cost_so_far = g;
			       cheap = 0., 0.;
			       close = 0., 0.;
			       children = [];
			       q_pos = init_pos;
			       data = data; })


let g_better_p a b =
  a.cost_so_far <= b.cost_so_far


let make_root cheap close expands_per uf initial =
  let cheap_vals = cheap.func initial
  and close_vals = close.func initial in
  let u,d,f = ((make_utility cheap close uf) cheap_vals close_vals
		 (1. /. (float expands_per)) 0.) in
    { est_u = u;
      expands_to_go = d;
      est_cost = f;
      cost_so_far = 0.;
      cheap = cheap_vals;
      close = close_vals;
      children = [];
      q_pos = init_pos;
      data = initial; }


let unwrap_sol s =
  match s with
  | Limit.Incumbent (q,n) -> Some (n.data, n.cost_so_far)
  | _ -> None


(******************** top-level interface ***************)


let search ?(dups = true) ?(incumbent = None) ?(limit = [Limit.Never])
    initial is_goal expand key hash equals get_depth cheap_hd
    close_hd cost_coeff time_coeff
    ?(default_u = neg_infinity) ?(deadline = infinity)
    ?(notify_estimates = Fn.no_op5)
    est_expands_per_sec =
  (** [expand] takes a node and a g and returns (node,new g) pairs.
      each of the [_hd]_functions returns a (cost,dist) pair.
      [default_u] is utility of empty solution. [notify_estimates] is
      called when expands per sec has been estimated.  Returns
      optional (goal_node, g cost) pair, nodes expanded and generated.
      If [dups] is true, then domain must have path-independent arc
      costs (due to propagation of new g values)! *)
  let uf = init_uf cost_coeff time_coeff default_u deadline
  and cheap_hd = init_hd cheap_hd
  and close_hd = init_hd close_hd in
  let i = Limit.make Limit.Nothing
    limit g_better_p (Limit.make_default_logger
			(fun n -> n.cost_so_far)
			(fun n -> get_depth n.data))
  and see, set = init_running_average cheap_hd close_hd in
  let get_secs_per = (make_est_secs_per cheap_hd close_hd
			est_expands_per_sec notify_estimates set uf)
  and root = make_root cheap_hd close_hd est_expands_per_sec uf initial
  and is_goal = wrap is_goal
  and expand = ((if dups then make_expand_redirect else make_expand_basic)
		  expand cheap_hd close_hd see uf key i)
  in
    do_search uf get_secs_per root is_goal expand key hash equals i;
    Limit.results6 i


let no_dups sface args =
  let cost_coefficient = Search_args.get_float "Bugsy.no_dups" args 0
  and time_coefficient = Search_args.get_float "Bugsy.no_dups" args 1
  and exp_per_sec = Search_args.get_int "Bugsy.no_dups" args 2 in
  let initial = sface.Search_interface.initial
  and is_goal = sface.Search_interface.goal_p
  and expand = sface.Search_interface.domain_expand
  and key = sface.Search_interface.key
  and hash = sface.Search_interface.hash
  and equals = sface.Search_interface.equals
  and get_depth = sface.Search_interface.get_sol_length
  and cheap_hd = sface.Search_interface.hd
  and close_hd = sface.Search_interface.hd in
    Limit.unwrap_sol6 unwrap_sol (search initial
				    is_goal expand key hash equals
				    get_depth cheap_hd
				    close_hd cost_coefficient
				    time_coefficient ~dups:false exp_per_sec)


let dups sface args =
  let cost_coefficient = Search_args.get_float "Bugsy.dups" args 0
  and time_coefficient = Search_args.get_float "Bugsy.dups" args 1
  and exp_per_sec = Search_args.get_int "Bugsy.dups" args 2 in
  let initial = sface.Search_interface.initial
  and is_goal = sface.Search_interface.goal_p
  and expand = sface.Search_interface.domain_expand
  and key = sface.Search_interface.key
  and hash = sface.Search_interface.hash
  and equals = sface.Search_interface.equals
  and get_depth = sface.Search_interface.get_sol_length
  and cheap_hd = sface.Search_interface.hd
  and close_hd = sface.Search_interface.hd in
    Limit.unwrap_sol6 unwrap_sol (search initial
				    is_goal expand key hash equals
				    get_depth cheap_hd
				    close_hd cost_coefficient
				    time_coefficient ~dups:true  exp_per_sec)

(* EOF *)
