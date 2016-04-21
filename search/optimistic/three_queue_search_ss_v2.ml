(** Three queue search really needs to correct d as well as h.
    This implementation of the three queue search module seeks to correct
    this oversight - Jordan December 2009 *)

(* as geq resort is currently written (and as a result as three_queue_fast)
   we can handle updates of d much the same way in which we can handle
   updates of estimated f *)

type 'a node = {
  mutable est_f : float;
  mutable est_d : float;
  h : float;
  d : float;
  h_err : float;
  d_err : float;
  g : float;
  f : float;
  depth : int;
  mutable q_pos : int;
  mutable fh_pos : int;
  mutable geqe : 'a node Geq.entry;
  mutable d_pos : int;
  data : 'a;
}


(**** Utility functions ****)

let set_qpos n i =
  n.q_pos <- i

let get_qpos n =
  n.q_pos

let set_fhpos n i =
  n.fh_pos <- i

let get_fhpos n =
  n.fh_pos

let set_geqe n e =
  n.geqe <- e

let get_geqe n =
  n.geqe

let set_dpos n i =
  n.d_pos <- i

let get_dpos n =
  n.d_pos

let wrap f =
  (fun n -> f n.data)

let get_f n =
  n.f

let get_estf n =
  n.est_f

let get_estd n =
  n.est_d

let make_child s g dep h d est_f est_d geqe h_err d_err f =
  { est_f = est_f;
    est_d = est_d;
    h = h;
    d = d;
    g = g;
    f = f;
    h_err = h_err;
    d_err = d_err;
    depth = dep;
    q_pos = Dpq.no_position;
    fh_pos = Dpq.no_position;
    d_pos = Dpq.no_position;
    geqe = geqe;
    data = s; }

let wrap_incumbent i =
  match i with
      None -> Limit.Nothing
    | Some (n) -> Limit.Incumbent (0., n)

let make_initial initial hd =
  let h, d = neg_infinity, neg_infinity (*infinity, infinity*) (*hd initial*) in
(*  assert (Math.finite_p h);
    assert (Math.finite_p d);
    assert (h > 0.);
    assert (d > 0.);*)
  let rec n =
    { est_f = h;
      est_d = d;
      h_err = 0.;
      d_err = 0.;
      h = h;
      d = d;
      g = 0.;
      f = h;
      depth = 0;
      q_pos = Dpq.no_position;
      fh_pos = Dpq.no_position;
      d_pos = Dpq.no_position;
      geqe = Geq.make_dummy_entry();
      data = initial; } in n


let est_f_then_d_then_g a b =
  let aef = a.est_f
  and bef = b.est_f
  and ag = a.g
  and bg = b.g in
  (aef < bef)  ||  (* sort by fhat *)
    ((aef = bef) &&
       ((ag >= bg)
	||       (* break ties on low d *)
	  ((ag = bg) &&
	     (a.d < b.d)))) (* break ties on high g *)


let d_then_f_then_g a b =
  (** convenience ordering predicate *)
  let af = a.est_f
  and bf = b.est_f in
  (a.est_d < b.est_d) ||
    ((a.est_d = b.est_d) && ((af < bf) ||
			       ((af = bf) && (a.g >= b.g))))

let better_p a b =
  (a.f) <= (b.f)


let f_order a b =
  let (af:float) = a.f
  and (bf:float) = b.f in
    af < bf ||
      (af = bf &&
	  a.g >= b.g)


let make_close_enough bound =
  (fun a b ->
(*     assert (Math.finite_p a.est_f);
     assert (Math.finite_p b.est_f);
     assert (b.est_f >= 0.);
     assert (a.est_f >= 0.);*)
     (b.est_f <= (a.est_f *. bound)))


let unwrap_sol s =
  match s with
      Limit.Incumbent (q, n) -> Some (n.data, n.g)
    | _ -> None


let on_fhat = ref 0
and on_dhat = ref 0
and on_f = ref 0
and delayed = ref 0


let reset () =
  on_fhat := 0;
  on_dhat := 0;
  on_f := 0;
  delayed := 0

let incr r =
  r := !r + 1

let alt_col_name = "served"

let output_col_hd () =
  Datafile.write_alt_colnames stdout alt_col_name
    ["on_fhat"; "on_dhat"; "on_f"; "delayed";]


let output_row () =
  Datafile.write_alt_row_prefix stdout alt_col_name;
  Verb.pr Verb.always "%i\t%i\t%i\t%i\n" !on_fhat !on_dhat !on_f !delayed

let output_geometric_sched ?(duration = 2) output =
  output_col_hd ();
  let i = ref 0
  and next = ref duration in
    (fun force ->
       if !i >= !next || force
       then (i := !i + 1;
	     next := (!next * 15) / 10;
	     output ())
       else i := !i + 1)

let get_node bound f_q geq i =
  (*let sfq = Dpq.count f_q
  and sgeq = Geq.count geq in
    assert (sfq = sgeq);*)
  let best_f =     try Dpq.peek_first f_q with _ -> failwith "bestf"
  and best_fh = Geq.peek_doset geq
  and best_d = try Geq.peek_best geq with _ -> failwith "bestd" in
  let wf = (best_f.f) *. bound in
    if best_d.est_f  <= wf
    then ((*Verb.pe Verb.always "Got Best d\n";*)
	  incr on_dhat;
	  best_d)
    else (if best_fh.est_f <= wf
	  then ((*Verb.pe Verb.always "Got Best fh\n";*)
		incr on_fhat;
		best_fh)
	  else ((*Verb.pe Verb.always "Got Best f\n";*)
		incr on_f;
		best_f))


let no_d_getnode bound fq geq i =
  let best_f = Dpq.peek_first fq
  and best_fh = Geq.peek_doset geq in
  let wf = (best_f.f) *. bound in
    if best_fh.est_f <= wf
    then ((*Verb.pe Verb.debug "Got Best fh\n";*)
	  incr on_fhat;
	  best_fh)
    else ((*Verb.pe Verb.debug "Got Best f\n";*)
	  incr on_f;
	  best_f)


let no_fh_getnode bound fq geq i =
  let best_f = Dpq.peek_first fq
  and best_d = Geq.peek_best geq in
  let wf = (best_f.f) *. bound in
    if best_d.est_f  <= wf
    then (incr on_dhat; best_d)
    else (incr on_f; best_f)


let get_min_f fq dq =
  if Dpq.empty_p dq
  then (Dpq.peek_first fq)
  else (let best_fq = Dpq.peek_first fq
	and best_dq = Dpq.peek_first dq in
	  if (better_p best_fq best_dq) then best_fq else best_dq)


let get_node_dd bound =
  (fun fq dq geq i goal ->
     let best_fh = Geq.peek_doset geq
     and best_d = Geq.peek_best geq
     and best_f = get_min_f fq dq in
     let wf = (best_f.f) *. bound in
       (match i.Limit.incumbent with
	    Limit.Nothing -> ()
	  | Limit.Incumbent (q,node) ->
	      if wf >= node.g
	      then (goal := true;
		    Limit.new_incumbent i (Limit.Incumbent (bound,node))));
       if best_d.est_f <= wf
       then ((*Verb.pe Verb.debug "Got Best d\n";*)
	     incr on_dhat;
	     best_d)
       else (if best_fh.est_f <= wf
	     then ((*Verb.pe Verb.debug "Got Best fh\n";*)
		   incr on_fhat;
		   best_fh)
	     else ((*Verb.pe Verb.debug "Got Best f\n";*)
		   incr on_f;
		   best_f)))


let get_node_hard_delay bound =
  let found_sol = ref false in
  (fun fq dq geq i goal ->
     let best_fh = Geq.peek_doset geq
     and best_d = Geq.peek_best geq
     and best_f = get_min_f fq dq  in
     let wf = (best_f.f) *. bound in
       (match i.Limit.incumbent with
	    Limit.Nothing -> ()
	  | Limit.Incumbent (q,node) ->
	      found_sol := true;
	      if wf >= node.g
	      then (goal := true;
		    Limit.new_incumbent i (Limit.Incumbent (bound,node))));
       if best_d.est_f  <= wf
       then ((*Verb.pe Verb.debug "Got Best d\n";*)
	     incr on_dhat;
	     best_d)
       else (if best_fh.est_f <= wf
	     then ((*Verb.pe Verb.debug "Got Best fh\n";*)
		   incr on_fhat;
		   best_fh)
	     else ((*Verb.pe Verb.debug "Got Best f\n";*)
		   incr on_f;
		   if not !found_sol
		   then Dpq.peek_first fq
		   else best_f )))


let no_d_getnode_dd bound =
  (fun fq dq geq i goal ->
     let best_fh = Geq.peek_doset geq
     and best_f = get_min_f fq dq in
     let wf = (best_f.f) *. bound in
       (match i.Limit.incumbent with
	    Limit.Nothing -> ()
	  | Limit.Incumbent (q,node) ->
	      if wf >= node.g
	      then (goal := true;
		    Limit.new_incumbent i (Limit.Incumbent (bound,node))));
       if best_fh.est_f <= wf
       then ((*Verb.pe Verb.debug "Got Best fh\n";*)
	     incr on_fhat;
	     best_fh)
       else ((*Verb.pe Verb.debug "Got Best f\n";*)
	     incr on_f;
	     best_f))


let no_fh_getnode_dd bound =
  (fun fq dq geq i goal ->
     let best_d = Geq.peek_best geq
     and best_f = get_min_f fq dq in
     let wf = (best_f.f) *. bound in
       (match i.Limit.incumbent with
	    Limit.Nothing -> ()
	  | Limit.Incumbent (q,node) ->
	      if wf >= node.g
	      then (goal := true;
		    Limit.new_incumbent i (Limit.Incumbent (bound,node))));
       if best_d.est_f  <= wf
       then ((*Verb.pe Verb.debug "Got Best d\n";*) best_d)
       else ((*Verb.pe Verb.debug "Got Best f\n";*) best_f))


let make_update fd_calc =
  (fun c -> let est_f,est_d = fd_calc c in
     c.est_f <- est_f;
     c.est_d <- est_d)

let make_expand expand hd timer calc_hd_data fd_calc =
  let init = Geq.make_dummy_entry()
  and update = make_update fd_calc in
    (fun n ->
       let best_f = ref infinity
       and best_child = ref n
       and reorder = timer()
       and nd = n.depth + 1 in
       let children = (List.map (fun (s, g) ->
				   let h, d = hd s in
				   let f = g +. h in
				   let h_err = f -. n.f +. n.h_err
				   and d_err = d -. n.d +. 1. +. n.d_err in
				   let h_err = (if Math.finite_p h_err
						then h_err else n.h_err)
				   and d_err = (if Math.finite_p d_err
						then d_err else n.d_err) in
				   let c = (make_child s g nd h d 0. 0.
					      init h_err d_err f) in
				     if  f < !best_f then
				       (best_child := c;
					best_f := f)
				     else if f = !best_f then
				       (if d < !best_child.d then
					  (best_child := c;
					   best_f := f));
				     c)
			 (expand n.data n.g))
       in
	 if not ((List.length children) = 0)
	 then
	   (calc_hd_data n !best_child children;
	    List.iter update children;
(*	    List.iter (fun c ->
			 Verb.pe Verb.always "%f %f %f %f %f %f %f\n%!"
			   c.g c.h c.d c.est_f c.est_d c.h_err c.d_err)
	      children;*));
	 reorder,children)


let make_expand_pathmax expand hd timer calc_hd_data fd_calc =
  let init = Geq.make_dummy_entry()
  and update = make_update fd_calc in
    (fun n ->
       let best_f = ref infinity
       and best_child = ref n
       and reorder = timer()
       and nd = n.depth + 1 in
       let children = (List.map (fun (s, g) ->
				   let hval, d = hd s
				   and t_cost = g -. n.g in
				   let h = Math.fmax hval (n.h -. t_cost) in
				   let f = g +. h in
				   let h_err = f -. n.f +. n.h_err
				   and d_err = d -. n.d +. 1. +. n.d_err in
				   let h_err = (if Math.finite_p h_err
						then h_err
						else n.h_err)
				   and d_err = (if Math.finite_p d_err
						then d_err
						else n.d_err) in
				   let c = (make_child s g nd h d h_err d_err
					      init h_err d_err f) in
				     if  f < !best_f then
				       (best_child := c;
					best_f := f)
				     else if f = !best_f then
				       (if d < !best_child.d then
					  (best_child := c;
					   best_f := f));
				     c)
			 (expand n.data n.g))
       in
	 if not ((List.length children) = 0)
	 then
	   (calc_hd_data n !best_child children;
	    List.iter update children);
	 reorder,children)





let make_simple_expand expand hd timer =
  let init = Geq.make_dummy_entry() in
    (fun n ->
       let nd = n.depth + 1 in
	 timer(),
       (List.map (fun (s, g) ->
		    let h, d = hd s in
		      (make_child s g nd h d 0. 0. init 0. 0. (g +. h)))
	  (expand n.data n.g)))


(****************************** Search *************************************)
let make_interface sface timer calc_hd_data fd_calc =
  let i_node = make_initial sface.Search_interface.initial
    sface.Search_interface.hd in
  let record_served = output_geometric_sched output_row in
    Search_interface.make
      ~resort_expand:(make_expand sface.Search_interface.domain_expand
			sface.Search_interface.hd timer calc_hd_data fd_calc)
      (*~goal_p:(wrap sface.Search_interface.goal_p)*)
      ~goal_p:
      (fun n -> let b = sface.Search_interface.goal_p n.data in
	 record_served b; b)
      ~halt_on:sface.Search_interface.halt_on
      ~hash:sface.Search_interface.hash
      ~key:(wrap sface.Search_interface.key)
      ~equals:sface.Search_interface.equals
      sface.Search_interface.domain
      i_node
      better_p
      (Limit.make_default_logger (fun n -> n.f)
	 (wrap sface.Search_interface.get_sol_length))


let make_anytime_interface sface timer calc_hd_data fd_calc =
  let i_node = make_initial sface.Search_interface.initial
    sface.Search_interface.hd
  and record_served = output_geometric_sched output_row in
    Search_interface.make
      ~resort_expand:(make_expand sface.Search_interface.domain_expand
			sface.Search_interface.hd timer calc_hd_data fd_calc)
      ~goal_p:
      (fun n -> let b = sface.Search_interface.goal_p n.data in
	 record_served false;
(*	 if b then Verb.pe Verb.always "goal f %f\n" n.g;*) b)
      ~halt_on:sface.Search_interface.halt_on
      ~hash:sface.Search_interface.hash
      ~key:(wrap sface.Search_interface.key)
      ~equals:sface.Search_interface.equals
      sface.Search_interface.domain
      i_node
      better_p
      (Limit.make_default_logger (fun n -> n.f)
	 (wrap sface.Search_interface.get_sol_length))


let make_interface_pmax sface timer calc_hd_data fd_calc =
  let i_node = make_initial sface.Search_interface.initial
    sface.Search_interface.hd in
  let record_served = output_geometric_sched output_row in
    Search_interface.make
      ~resort_expand:(make_expand_pathmax sface.Search_interface.domain_expand
			sface.Search_interface.hd timer calc_hd_data fd_calc)
      ~goal_p:
      (fun n -> let b = sface.Search_interface.goal_p n.data in
	 record_served b; b)
      ~halt_on:sface.Search_interface.halt_on
      ~hash:sface.Search_interface.hash
      ~key:(wrap sface.Search_interface.key)
      ~equals:sface.Search_interface.equals
      sface.Search_interface.domain
      i_node
      f_order
      (Limit.make_default_logger (fun n -> n.f)
	 (wrap sface.Search_interface.get_sol_length))



let make_interface_simple sface timer =
  let i_node = make_initial sface.Search_interface.initial
    sface.Search_interface.hd in
  let record_served = output_geometric_sched output_row in
    Search_interface.make
      ~resort_expand:(make_simple_expand sface.Search_interface.domain_expand
			sface.Search_interface.hd timer)
      (*~goal_p:(wrap sface.Search_interface.goal_p)*)
      ~goal_p:
      (fun n -> let b = sface.Search_interface.goal_p n.data in
	 record_served b; b)
      ~halt_on:sface.Search_interface.halt_on
      ~hash:sface.Search_interface.hash
      ~key:(wrap sface.Search_interface.key)
      ~equals:sface.Search_interface.equals
      sface.Search_interface.domain
      i_node
      f_order
      (Limit.make_default_logger (fun n -> n.f)
	 (wrap sface.Search_interface.get_sol_length))


let no_dups ?(node_get = get_node) sface bound timer calc_hd_data fd_calc =
  reset();
  Limit.unwrap_sol5 unwrap_sol
    (Three_queue_fast.no_dups
       (make_interface sface timer calc_hd_data fd_calc)
       f_order
       d_then_f_then_g
       est_f_then_d_then_g
       (make_close_enough bound)
       better_p
       set_qpos
       get_qpos
       set_fhpos
       get_fhpos
       set_geqe
       get_geqe
       (node_get bound)
       (make_update fd_calc))


let dups ?(node_get = get_node) ?(kp = (fun _ -> ())) sface bound timer
    calc_hd_data fd_calc =
  reset();
  Limit.unwrap_sol6 unwrap_sol
    (Three_queue_fast.dups
       ~kp:(fun ch node -> (kp (sface.Search_interface.key node.data)))
       (make_interface sface timer calc_hd_data fd_calc)
       f_order
       d_then_f_then_g
       est_f_then_d_then_g
       (make_close_enough bound)
       better_p
       set_qpos
       get_qpos
       set_fhpos
       get_fhpos
       set_geqe
       get_geqe
       (node_get bound)
       (make_update fd_calc))


let delay ?(node_get = get_node_dd) sface bound timer calc_hd_data fd_calc =
  reset();
  Limit.unwrap_sol6 unwrap_sol
    (Three_queue_fast.delay
       (make_interface sface timer calc_hd_data fd_calc)
       f_order
       d_then_f_then_g
       est_f_then_d_then_g
       (make_close_enough bound)
       better_p
       set_qpos
       get_qpos
       set_fhpos
       get_fhpos
       set_geqe
       get_geqe
       set_dpos
       get_dpos
       (node_get bound)
       (make_update fd_calc))

let s_no_dups ?(node_get = get_node) sface bound timer calc_hd_data fd_calc =
  reset();
  Limit.unwrap_sol5 unwrap_sol
    (Three_queue_fast.sno_dups
       (make_interface_simple sface timer)
       f_order
       d_then_f_then_g
       est_f_then_d_then_g
       (make_close_enough bound)
       better_p
       set_qpos
       get_qpos
       set_fhpos
       get_fhpos
       set_geqe
       get_geqe
       (node_get bound)
       (make_update fd_calc)
       calc_hd_data)


let s_dups ?(node_get = get_node) ?(kp = (fun _ -> ())) sface bound timer
    calc_hd_data fd_calc =
  reset();
  Limit.unwrap_sol6 unwrap_sol
    (Three_queue_fast.sdups
       ~kp:(fun ch node -> (kp (sface.Search_interface.key node.data)))
       (make_interface_simple sface timer)
       f_order
       d_then_f_then_g
       est_f_then_d_then_g
       (make_close_enough bound)
       better_p
       set_qpos
       get_qpos
       set_fhpos
       get_fhpos
       set_geqe
       get_geqe
       (node_get bound)
       (make_update fd_calc)
       calc_hd_data)




let no_dups_pm ?(node_get = get_node) sface bound timer calc_hd_data fd_calc =
  reset();
  Limit.unwrap_sol5 unwrap_sol
    (Three_queue_fast.no_dups
       (make_interface_pmax sface timer calc_hd_data fd_calc)
       f_order
       d_then_f_then_g
       est_f_then_d_then_g
       (make_close_enough bound)
       better_p
       set_qpos
       get_qpos
       set_fhpos
       get_fhpos
       set_geqe
       get_geqe
       (node_get bound)
       (make_update fd_calc))


let dups_pm ?(node_get = get_node) ?(kp = (fun _ -> ())) sface bound timer
    calc_hd_data fd_calc =
  reset();
  Limit.unwrap_sol6 unwrap_sol
    (Three_queue_fast.dups
       ~kp:(fun ch node -> (kp (sface.Search_interface.key node.data)))
       (make_interface_pmax sface timer calc_hd_data fd_calc)
       f_order
       d_then_f_then_g
       est_f_then_d_then_g
       (make_close_enough bound)
       better_p
       set_qpos
       get_qpos
       set_fhpos
       get_fhpos
       set_geqe
       get_geqe
       (node_get bound)
       (make_update fd_calc))


let anytime_no_dups ?(node_get = get_node) sface bound timer calc_hd_data
    fd_calc =
  reset();
  Limit.unwrap_sol5 unwrap_sol
    (Continued_three_queue.no_dups
       (make_anytime_interface sface timer calc_hd_data fd_calc)
       f_order
       d_then_f_then_g
       est_f_then_d_then_g
       (make_close_enough bound)
       better_p
       set_qpos
       get_qpos
       set_fhpos
       get_fhpos
       set_geqe
       get_geqe
       (node_get bound)
       (make_update fd_calc))


let anytime_dups ?(node_get = get_node) ?(kp = (fun _ -> ())) sface bound timer
    calc_hd_data fd_calc =
  reset();
  Limit.unwrap_sol6 unwrap_sol
    (Continued_three_queue.dups
       ~kp:(fun ch node -> (kp (sface.Search_interface.key node.data)))
       (make_anytime_interface sface timer calc_hd_data fd_calc)
       f_order
       d_then_f_then_g
       est_f_then_d_then_g
       (make_close_enough bound)
       better_p
       set_qpos
       get_qpos
       set_fhpos
       get_fhpos
       set_geqe
       get_geqe
       (node_get bound)
       (make_update fd_calc))

(* EOF *)
