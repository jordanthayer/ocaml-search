(** Three queud search
    Jordan - July 2009 *)


type 'a node = {
  mutable est_f : float;
  h : float;
  d : float;
  mutable g : float;
  mutable q_pos : int;
  mutable fh_pos : int;
  mutable geqe : 'a node Geq.entry;
  mutable d_pos : int;
  mutable children : 'a node list;
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
  n.g +. n.h

let get_estf n =
  n.est_f

let make_child s g h d est_f geqe =
  { est_f = est_f;
    h = h;
    d = d;
    g = g;
    q_pos = Dpq.no_position;
    fh_pos = Dpq.no_position;
    d_pos = Dpq.no_position;
    geqe = geqe;
    data = s;
    children = [];}

let wrap_incumbent i =
  match i with
      None -> Limit.Nothing
    | Some (n) -> Limit.Incumbent (0., n)

let make_initial initial hd =
  let h, d = neg_infinity, neg_infinity in
  let rec n =
    { est_f = h;
    h = h;
    d = d;
    g = 0.;
    q_pos = Dpq.no_position;
    fh_pos = Dpq.no_position;
    d_pos = Dpq.no_position;
    geqe = Geq.make_dummy_entry();
    data = initial;
    children = []} in n


let est_f_then_d_then_g a b =
  (a.est_f < b.est_f)  ||  (* sort by fhat *)
    ((a.est_f = b.est_f) &&
       ((a.g >= b.g)
	||       (* break ties on low d *)
	  ((a.g == b.g) &&
	     (a.d < b.d)))) (* break ties on high g *)


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
	  a.g >= b.g)


let f_order_dep a b =
  let af = a.g +. a.h
  and bf = b.g +. b.h in
    af < bf ||
      (af = bf &&
	  a.g > b.g) ||
      (af = bf && a.g = b.g && a.d <= b.d)



let make_close_enough bound =
  (fun a b ->
     (b.est_f <= (a.est_f *. bound)))


let unwrap_sol s =
  match s with
      Limit.Incumbent (q, n) -> Some (n.data, n.g)
    | _ -> None


let get_node bound f_q geq i =
  let best_f = Dpq.peek_first f_q
  and best_fh = Geq.peek_doset geq
  and best_d = Geq.peek_best geq in
  let wf = (best_f.g +. best_f.h) *. bound in
    if best_d.est_f  < wf
    then ((*Verb.pe Verb.debug "Got Best d\n";*) best_d)
    else (if best_fh.est_f < wf
	  then ((*Verb.pe Verb.debug "Got Best fh\n";*) best_fh)
	  else ((*Verb.pe Verb.debug "Got Best f\n";*) best_f))


let no_d_getnode bound fq geq i =
  let best_f = Dpq.peek_first fq
  and best_fh = Geq.peek_doset geq in
  let wf = (best_f.g +. best_f.h) *. bound in
    if best_fh.est_f <= wf
    then ((*Verb.pe Verb.debug "Got Best fh\n";*) best_fh)
    else ((*Verb.pe Verb.debug "Got Best f\n";*) best_f)


let no_fh_getnode bound fq geq i =
  let best_f = Dpq.peek_first fq
  and best_d = Geq.peek_best geq in
  let wf = (best_f.g +. best_f.h) *. bound in
    if best_d.est_f  <= wf
    then ((*Verb.pe Verb.debug "Got Best d\n";*) best_d)
    else ((*Verb.pe Verb.debug "Got Best f\n";*) best_f)


let get_min_f fq dq =
  if Dpq.empty_p dq
  then (Dpq.peek_first fq)
  else (let best_fq = Dpq.peek_first fq
	and best_dq = Dpq.peek_first dq in
	  if (better_p best_fq best_dq) then best_fq else best_dq)


let make_expand expand hd timer calc_h_data f_calc =
  let init = Geq.make_dummy_entry() in
    (fun n ->
       let best_f = ref infinity
       and best_child = ref n
       and reorder = timer() in
       let children = (List.map (fun (s, g) ->
				   let h, d = hd s in
				   let f = g +. h in
				   let c = (make_child s g h d 0.
					      init) in
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
	    List.iter (fun c -> c.est_f <- f_calc c) children;
	    n.children <- children);

	 reorder,children)


let alt_col_name = "g_update"

let output_col_hdr () =
  Datafile.write_alt_colnames stdout alt_col_name ["update";]

let update_gs fix_data old_node new_node =
  (** fix_data needs to take the data and correct the parent pointers *)
  assert (old_node.g > new_node.g);
  let g_reduction = old_node.g -. new_node.g in
(*    Datafile.write_alt_row_prefix stdout alt_col_name;
      Verb.pr Verb.always "%f\n" g_reduction;*)
  let rec cascade parent child =
    child.g <- child.g -. g_reduction;
    fix_data child.data parent.data;
    List.iter (cascade child) child.children in
    new_node.children <- old_node.children;
    List.iter (cascade new_node) new_node.children


let update_gs_2 fix_data old_node new_node =
  (** fix_data needs to take the data and correct the parent pointers *)
  assert (old_node.g > new_node.g);
  let g_reduction = old_node.g -. new_node.g in
(*    Datafile.write_alt_row_prefix stdout alt_col_name;
      Verb.pr Verb.always "%f\n" g_reduction;*)
  let rec cascade parent child =
    child.g <- child.g -. g_reduction;
    fix_data child.data parent.data;
    List.iter (cascade child) child.children in
    new_node.children <- old_node.children;
    List.iter (cascade new_node) new_node.children


let update_estf fcalc n =
  n.est_f <- fcalc n


(****************************** Search *************************************)
let setup sface bound timer calc_h_data f_calc =
  output_col_hdr();
  let i_node = make_initial sface.Search_interface.initial
    sface.Search_interface.hd in
  let search_interface = Search_interface.make
    ~resort_expand:(make_expand sface.Search_interface.domain_expand
		    sface.Search_interface.hd timer calc_h_data f_calc)
    ~goal_p:(wrap sface.Search_interface.goal_p)
    ~halt_on:sface.Search_interface.halt_on
    ~hash:sface.Search_interface.hash
    ~key:(wrap sface.Search_interface.key)
    ~equals:sface.Search_interface.equals
    sface.Search_interface.domain
    i_node
    f_order
    (Limit.make_default_logger (fun n -> n.g +. n.h)
       (wrap sface.Search_interface.get_sol_length)) in
  let est_f_updater = update_estf f_calc
  and g_updater = update_gs sface.Search_interface.parent_update in
    i_node, search_interface, est_f_updater, g_updater


let no_dups ?(node_get = get_node) sface bound timer calc_h_data f_calc =
  let i_node, search_interface, est_f_updater, g_updater =
    setup sface bound timer calc_h_data f_calc in
    Limit.unwrap_sol5 unwrap_sol
      (Three_queue_gupdate.no_dups
	 search_interface
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
	 est_f_updater
	 g_updater)


let dups ?(node_get = get_node) ?(kp = (fun _ -> ())) sface bound timer
    calc_h_data f_calc =
  let i_node, search_interface, est_f_updater, g_updater =
    setup sface bound timer calc_h_data f_calc in
    Limit.unwrap_sol6 unwrap_sol
      (Three_queue_gupdate.dups
	 ~kp:(fun ch node -> (kp (sface.Search_interface.key node.data)))
	 search_interface
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
	 est_f_updater
	 g_updater)

(* EOF *)

