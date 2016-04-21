(** Lesion 3 of EES - Jordan Feb 2010
    Going to be based around the continued focal searches *)

type 'a node = {
  mutable est_f : float;
  mutable est_d : float;
  h : float;
  d : float;
  g : float;
  depth : int;
  mutable d_pos : int;
  mutable f_pos : int;
  data : 'a;
}

let set_fpos n i =
  n.f_pos <- i

let get_fpos n =
  n.f_pos

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

let get_estd n =
  n.est_d


let make_child s g dep h d est_f est_d geqe =
  { est_f = est_f;
    est_d = est_d;
    h = h;
    d = d;
    g = g;
    depth = dep;
    d_pos = Dpq.no_position;
    f_pos = Dpq.no_position;
    data = s; }


let make_initial initial =
  make_child initial 0. 0 neg_infinity neg_infinity neg_infinity neg_infinity
    (Geq.make_dummy_entry())

let wrap_incumbent i =
  match i with
      None -> Limit.Nothing
    | Some (n) -> Limit.Incumbent (0., n)


let est_f_then_d_then_g a b =
  (a.est_f < b.est_f)  ||  (* sort by fhat *)
    ((a.est_f = b.est_f) &&
       ((a.g >= b.g)
	||       (* break ties on low d *)
	  ((a.g = b.g) &&
	     (a.d < b.d)))) (* break ties on high g *)


let d_then_f_then_g a b =
  (** convenience ordering predicate *)
  let af = a.g +. a.h
  and bf = b.g +. b.h in
  (a.est_d < b.est_d) ||
    ((a.est_d = b.est_d) && ((af < bf) ||
			       ((af = bf) && (a.g >= b.g))))

let better_p a b =
  (a.g +. a.h) <= (b.g +. b.h)


let f_order a b =
  let af = a.g +. a.h
  and bf = b.g +. b.h in
    af < bf ||
      (af = bf &&
	  a.g >= b.g)


let make_close_enough bound =
  (fun a b ->
     (b.est_f <= (a.est_f *. bound)))


let unwrap_sol s =
  match s with
      Limit.Incumbent (q, n) -> Some (n.data, n.g)
    | _ -> None


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
       (*and reorder = timer()*)
       and nd = n.depth + 1 in
       let children = (List.map (fun (s, g) ->
				   let h, d = hd s in
				   let f = g +. h in
				   let c = (make_child s g nd h d 0. 0.
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
	   (calc_hd_data n !best_child children;
	    List.iter update children);
	 children)


(****************************** Search *************************************)
let make_interface sface timer calc_hd_data fd_calc =
  let i_node = make_initial sface.Search_interface.initial in
    Search_interface.make
      ~node_expand:(make_expand sface.Search_interface.domain_expand
			sface.Search_interface.hd timer calc_hd_data fd_calc)
      ~goal_p:(wrap sface.Search_interface.goal_p)
      ~halt_on:sface.Search_interface.halt_on
      ~hash:sface.Search_interface.hash
      ~key:(wrap sface.Search_interface.key)
      ~equals:sface.Search_interface.equals
      sface.Search_interface.domain
      i_node
      better_p
      (Limit.make_default_logger (fun n -> n.g +. n.h)
	 (wrap sface.Search_interface.get_sol_length))


let continued_no_dups sface bound timer calc_hd_data fd_calc =
  Limit.unwrap_sol5 unwrap_sol
    (Continued_focal_search.search
       (make_interface sface timer calc_hd_data fd_calc)
       d_then_f_then_g
       (make_close_enough bound)
       est_f_then_d_then_g
       f_order
       set_dpos
       get_dpos)

let continued_dups sface bound timer calc_hd_data fd_calc =
  Limit.unwrap_sol6 unwrap_sol
    (Continued_focal_search.search_dups
       (make_interface sface timer calc_hd_data fd_calc)
       d_then_f_then_g
       (make_close_enough bound)
       est_f_then_d_then_g
       f_order
       set_dpos
       get_dpos)


let restart_no_dups sface bound timer calc_hd_data fd_calc =
  let wtlist = Arastar.mk_wtlist bound 0.2 in
    Limit.unwrap_sol5 unwrap_sol
      (Restarting_focal_search.search
	 wtlist
	 (make_interface sface timer calc_hd_data fd_calc)
	 d_then_f_then_g
	 make_close_enough
	 est_f_then_d_then_g
	 f_order
	 set_dpos
	 get_dpos)

let restart_dups sface bound timer calc_hd_data fd_calc =
  let wtlist = Arastar.mk_wtlist bound 0.2 in
    Limit.unwrap_sol6 unwrap_sol
      (Restarting_focal_search.search_dups
	 wtlist
	 (make_interface sface timer calc_hd_data fd_calc)
	 d_then_f_then_g
	 make_close_enough
	 est_f_then_d_then_g
	 f_order
	 set_dpos
	 get_dpos)


let repairing_no_dups sface bound timer calc_hd_data fd_calc =
  let wtlist = Arastar.mk_wtlist bound 0.2 in
    Limit.unwrap_sol5 unwrap_sol
      (Repairing_focal_search.no_dups
	 ~continue:true
	 (make_interface sface timer calc_hd_data fd_calc)
	 wtlist
	 d_then_f_then_g
	 est_f_then_d_then_g
	 make_close_enough
	 f_order
	 f_order
	 (set_dpos,get_dpos)
	 (set_fpos,get_fpos)
	 get_f)

let repairing_dups sface bound timer calc_hd_data fd_calc =
  let wtlist = Arastar.mk_wtlist bound 0.2 in
    Limit.unwrap_sol6 unwrap_sol
      (Repairing_focal_search.dups
	 ~continue:true
	 (make_interface sface timer calc_hd_data fd_calc)
	 wtlist
	 d_then_f_then_g
	 est_f_then_d_then_g
	 make_close_enough
	 f_order
	 f_order
	 (set_dpos,get_dpos)
	 (set_fpos,get_fpos)
	 get_f)

(* EOF *)
