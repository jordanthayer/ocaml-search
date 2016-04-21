(** Anytime Variant of A* epsilon *)

type 'a node = {
  f : float;
  g : float;
  d : float;
  (* position in q if applicable, or presence on closed *)
  mutable q_pos : int; (* Q position of geq *)
  mutable i_pos : int; (* Q position of inconsistent *)
  data : 'a;
}

(* Sorting Predicates *)

let just_f a b =
  (** quality ordering predicate.  distinctions between nodes with the same
    f are made by the convenience ordering predicate. *)
  a.f <= b.f

let d_then_f_then_g a b =
  (** convenience ordering predicate *)
  (a.d < b.d) ||
  ((a.d = b.d) && ((a.f < b.f) ||
		   ((a.f = b.f) && (a.g >= b.g))))


let get_q_pos n =
  (** returns the position of node [n] in the dpq maintained by the geq*)
  n.q_pos

let set_q_pos n pos =
  (** sets the position of node [n] to [pos] *)
  n.q_pos <- pos

let get_i_pos n =
  n.i_pos

let set_i_pos n i =
  n.i_pos <- i


let make_close_enough_p weight =
  (** builds the close enough predicate required by the geq *)
  assert (weight >= 1.);
  (fun a b ->
     (** is b good enough compared to the benchmark a? *)
     b.f <= (a.f *. weight))


let make_root initial =
  (** builds the root of the search tree *)
  { f = 0.;
    g = 0.;
    d = 0.;
    q_pos = Dpq.no_position;
    i_pos = Dpq.no_position;
    data = initial; }


let unwrap n =
  (** takes the solution found by the search and returns it in a format that
      the domain expects *)
  match n with
      Limit.Incumbent (q,n) -> Some (n.data, n.g)
    | _ -> None


let wrap_expand expand hd =
  (** takes a domain [expand] function and a cost and distance estimator [hd]
      and returns a node expand function *)
  (fun n ->
     List.map (fun (n, g) ->
		 let h, d = hd n in
		   { f = g +. h;
		     g = g;
		     d = d;
		     q_pos = Dpq.no_position;
		     i_pos = Dpq.no_position;
		     data = n; })
     (expand n.data n.g))


let wrap fn =
  (** wraps the function [fn] which is made to operate on domain objects so
      that it can be applied to search objects *)
  (fun n -> fn n.data)


(****************** Searches *******************)
let make_iface sface =
Search_interface.make
    ~node_expand:(wrap_expand sface.Search_interface.domain_expand
		    sface.Search_interface.hd)
    ~key:(wrap sface.Search_interface.key)
    ~goal_p:(wrap sface.Search_interface.goal_p)
    ~halt_on:sface.Search_interface.halt_on
    ~hash:sface.Search_interface.hash
    ~equals:sface.Search_interface.equals
    sface.Search_interface.domain
    (make_root sface.Search_interface.initial)
    just_f
    (Limit.make_default_logger (fun n -> n.f)
       (wrap sface.Search_interface.get_sol_length))


let no_dups sface args =
  (** A* epsilon search on domains without many duplicates.
      [sface] the domain interface
      [wt] the desired suboptimality bound *)
  let wt = Search_args.get_float "Aseps.no_dups" args 0 in
  Limit.unwrap_sol5 unwrap
    (Continued_focal_search.search
       (make_iface sface)           (*sface*)
       just_f                       (*ordered1*)
       (make_close_enough_p wt)     (*close enough *)
       d_then_f_then_g              (*ordered 2 *)
       just_f                       (* better_p*)
       set_q_pos                    (* set_q_pos*)
       get_q_pos)                   (* get_q_pos*)


let dups sface args =
  (** A* epsilon search on domains many duplicates.
      [sface] the domain interface
      [wt] the desired suboptimality bound *)
  let wt = Search_args.get_float "Aseps.dups" args 0 in
  Limit.unwrap_sol6 unwrap
    (Continued_focal_search.search_dups
       (make_iface sface)
       just_f
       (make_close_enough_p wt)
       d_then_f_then_g
       just_f
       set_q_pos
       get_q_pos)


let repairing_nodups sface args =
  let wt = Search_args.get_float "Aseps.dups" args 0 in
    Limit.unwrap_sol5 unwrap
      (Repairing_focal_search.no_dups
	 (make_iface sface)
	 (Arastar.mk_wtlist wt 0.2)
	 d_then_f_then_g
	 just_f
	 make_close_enough_p
	 just_f
	 just_f
	 (set_q_pos, get_q_pos)
	 (set_i_pos, get_i_pos)
	 (fun n -> n.f))


let repairing_dups sface args =
  let wt = Search_args.get_float "Aseps.dups" args 0 in
    Limit.unwrap_sol6 unwrap
      (Repairing_focal_search.dups
	 (make_iface sface)
	 (Arastar.mk_wtlist wt 0.2)
	 d_then_f_then_g
	 just_f
	 make_close_enough_p
	 just_f
	 just_f
	 (set_q_pos, get_q_pos)
	 (set_i_pos, get_i_pos)
	 (fun n -> n.f))


let restarting_nodups sface args =
  let wt = Search_args.get_float "Aseps.dups" args 0 in
    Limit.unwrap_sol5 unwrap
      (Restarting_focal_search.search
	 (Arastar.mk_wtlist wt 0.2)
	 (make_iface sface)
	 just_f
	 make_close_enough_p
	 d_then_f_then_g
	 just_f
	 set_q_pos
	 get_q_pos)


let restarting_dups sface args =
  let wt = Search_args.get_float "Aseps.dups" args 0 in
    Limit.unwrap_sol6 unwrap
      (Restarting_focal_search.search_dups
	 (Arastar.mk_wtlist wt 0.2)
	 (make_iface sface)
	 just_f
	 make_close_enough_p
	 d_then_f_then_g
	 just_f
	 set_q_pos
	 get_q_pos)


(* EOF *)
