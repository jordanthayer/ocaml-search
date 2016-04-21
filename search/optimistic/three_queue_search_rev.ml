(** Three queud search with reverse heuristics
    Jordan - August 2009 *)


type 'a node = {
  mutable est_f : float;
  h : float;
  d : float;
  g : float;
  depth : int;
  rev_h : float;
  rev_d : float;
  mutable q_pos : int;
  mutable fh_pos : int;
  mutable geqe : 'a node;
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

let wrap f =
  (fun n -> f n.data)

let get_f n =
  n.g +. n.h

let get_estf n =
  n.est_f

let make_child s g h d rh rd est_f geqe depth =
  { est_f = est_f;
    h = h;
    d = d;
    g = g;
    depth = depth;
    rev_h = rh;
    rev_d = rd;
    q_pos = Dpq.no_position;
    fh_pos = Dpq.no_position;
    geqe = geqe;
    data = s; }

let wrap_incumbent i =
  match i with
      None -> Limit.Nothing
    | Some (n) -> Limit.Incumbent (0., n)

let make_initial initial hd =
  let h, d = hd initial in
  let rec n =
    { est_f = h;
      h = h;
      d = d;
      g = 0.;
      depth = 0;
      rev_h = 0.;
      rev_d = 0.;
      q_pos = Dpq.no_position;
      fh_pos = Dpq.no_position;
      geqe = n;
      data = initial; } in n


let est_f_then_d_then_g a b =
  (a.est_f < b.est_f) ||  (* sort by fhat *)
    ((a.est_f = b.est_f) &&
       ((a.g >= b.g)
	||       (* break ties on low d *)
	  ((a.g == b.g) &&
	     (a.d <= b.d)))) (* break ties on high g *)


let d_then_f_then_g a b =
  (** convenience ordering predicate *)
  let af = a.g +. a.h
  and bf = b.g +. b.h in
  (a.d < b.d) ||
  ((a.d = b.d) && ((af < bf) ||
		   ((af = bf) && (a.g >= b.g))))

let better_p a b =
  (a.g +. a.h) <= (b.g +. b.h)

let f_order a b =
  let af = a.g +. a.h
  and bf = b.g +. b.h in
    af < bf ||
      (af = bf &&
	  a.g > b.g) ||
      (af = bf && a.g = b.g && a.d < b.d)


let make_close_enough bound =
  (fun a b ->
     (b.est_f <= (a.est_f *. bound)))


let unwrap_sol s =
  match s with
      Limit.Incumbent (q, n) -> Some (n.data, n.g)
    | _ -> None


let get_node bound f_q geq i =
  let best_f = Dpq.peek_first f_q
  and best_fh = Safe_geq.peek_doset geq
  and best_d = Safe_geq.peek_best geq in
  let wf = (best_f.g +. best_f.h) *. bound in
    if best_d.est_f  <= wf
    then best_d
    else (if best_fh.est_f <= wf
	  then best_fh
	  else best_f)


let no_d_getnode bound fq geq i =
  let best_f = Dpq.peek_first fq
  and best_fh = Safe_geq.peek_doset geq in
  let wf = (best_f.g +. best_f.h) *. bound in
    if best_fh.est_f <= wf
    then ((*Verb.pe Verb.debug "Got Best fh\n";*) best_fh)
    else ((*Verb.pe Verb.debug "Got Best f\n";*) best_f)


let no_fh_getnode bound fq geq i =
  let best_f = Dpq.peek_first fq
  and best_d = Safe_geq.peek_best geq in
  let wf = (best_f.g +. best_f.h) *. bound in
    if best_d.est_f  <= wf
    then ((*Verb.pe Verb.debug "Got Best d\n";*) best_d)
    else ((*Verb.pe Verb.debug "Got Best f\n";*) best_f)


let make_expand init expand hd rev_hd timer calc_h_data f_calc =
  (fun n ->
     let best_f = ref infinity
     and best_child = ref n
     and reorder = timer() in
     let children = (List.map (fun (s, g) ->
				 let h, d = hd s
				 and rh, rd = rev_hd s in
				 let f = g +. h in
				 let c = (make_child s g h d rh rd 0.
					    init (n.depth + 1)) in
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
	 (calc_h_data n !best_child children;
	  List.iter (fun c -> c.est_f <- f_calc c) children);
       reorder,children)


let make_update fcalc =
  (fun n -> n.est_f <- fcalc n)

(****************************** Search *************************************)
let no_dups ?(node_get = get_node) sface bound timer calc_h_data f_calc =
  let i_node =
    make_initial sface.Search_interface.initial sface.Search_interface.hd in
  let search_interface = Search_interface.make
    ~resort_expand:(make_expand i_node sface.Search_interface.domain_expand
		    sface.Search_interface.hd sface.Search_interface.rev_hd
		    timer calc_h_data f_calc)
    ~goal_p:(wrap sface.Search_interface.goal_p)
    ~halt_on:sface.Search_interface.halt_on
    ~hash:sface.Search_interface.hash
    ~equals:sface.Search_interface.equals
    sface.Search_interface.domain
    i_node
    f_order
    (Limit.make_default_logger (fun n -> n.g +. n.h)
       (wrap sface.Search_interface.get_sol_length)) in
    Limit.unwrap_sol5 unwrap_sol
      (Three_queue.no_dups
	 search_interface
	 f_order
	 d_then_f_then_g
	 est_f_then_d_then_g
	 (make_close_enough bound)
	 f_order
	 set_qpos
	 get_qpos
	 set_fhpos
	 get_fhpos
	 set_geqe
	 get_geqe
	 (node_get bound)
	 (make_update f_calc))


let dups ?(node_get = get_node) sface bound timer calc_h_data f_calc =
  let i_node =
    make_initial sface.Search_interface.initial sface.Search_interface.hd in
  let search_interface = Search_interface.make
    ~resort_expand:(make_expand i_node sface.Search_interface.domain_expand
		    sface.Search_interface.hd sface.Search_interface.rev_hd
		    timer calc_h_data f_calc)
    ~goal_p:(wrap sface.Search_interface.goal_p)
    ~halt_on:sface.Search_interface.halt_on
    ~hash:sface.Search_interface.hash
    ~equals:sface.Search_interface.equals
    ~key:(wrap sface.Search_interface.key)
    sface.Search_interface.domain
    i_node
    f_order
    (Limit.make_default_logger (fun n -> n.g +. n.h)
       (wrap sface.Search_interface.get_sol_length)) in
    Limit.unwrap_sol6 unwrap_sol
      (Three_queue.dups
	 search_interface
	 f_order
	 d_then_f_then_g
	 est_f_then_d_then_g
	 (make_close_enough bound)
	 f_order
	 set_qpos
	 get_qpos
	 set_fhpos
	 get_fhpos
	 set_geqe
	 get_geqe
	 (node_get bound)
	 (make_update f_calc))


(* EOF *)
