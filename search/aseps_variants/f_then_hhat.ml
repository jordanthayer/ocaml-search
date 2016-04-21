(** A* epsilon running on a supplied h^ *)

type 'a node = {
  f : float;
  g : float;
  d : float;
  mutable hhat : float;
  (* position in q if applicable, or presence on closed *)
  mutable q_pos : int;
  data : 'a;
}

(* Sorting Predicates *)

let just_f a b =
  (** quality ordering predicate.  distinctions between nodes with the same
    f are made by the convenience ordering predicate. *)
  (*a.f <= b.f*)
  let dif = b.f -. a.f in
    Math.is_zero dif || Math.is_positive dif

let hhat_then_f_then_g a b =
  (** convenience ordering predicate *)
  (a.hhat < b.hhat) ||
  ((a.hhat = b.hhat) && ((a.f < b.f) ||
		   ((a.f = b.f) && (a.g >= b.g))))


let get_q_pos n =
  (** returns the position of node [n] in the dpq maintained by the geq*)
  n.q_pos

let set_q_pos n pos =
  (** sets the position of node [n] to [pos] *)
  n.q_pos <- pos


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
    hhat = 0.;
    d = 0.;
    q_pos = Dpq.no_position;
    data = initial; }


let unwrap n =
  (** takes the solution found by the search and returns it in a format that
      the domain expects *)
  match n with
      Limit.Incumbent (q,n) -> Some (n.data, n.g)
    | _ -> None


let wrap_expand expand hd hhat =
  (** takes a domain [expand] function and a cost and distance estimator [hd]
      and returns a node expand function *)
  (fun n ->
     List.map (fun (n, g) ->
		 let h, d = hd n in
		   { f = g +. h;
		     d = d;
		     g = g;
		     hhat = hhat n;
		     q_pos = Dpq.no_position;
		     data = n; })
     (expand n.data n.g))


let wrap_ss_expand expand hd calc_h_data f_calc =
  (** takes a domain [expand] function and a cost and distance estimator [hd]
      and returns a node expand function *)
  (fun n ->
     (** [expand] is the domain expand [hd] is a cost and distance
	 estimator [timer] returns true every so often, to tell the
	 openlist to resort [calc_h_data] takes the parent, the best
	 child, and all children in order to make a better h estimator
	 [f_calc] uses the estimated h values to calculate the bounded
	 f estimates *)
     let best_f = ref infinity
     and best_child = ref n in
     let children = (List.map (fun (s, g) ->
				 let h, d = hd s in
				 let f = g +. h in
				 let c =
				   { f = f;
				     g = g;
				     d = d;
				     hhat = f;
				     q_pos = Dpq.no_position;
				     data = s;} in
				   if  f < !best_f then
				     (best_child := c;
				      best_f := f)
				   else if f = !best_f then
				     (if g < !best_child.g then
					(best_child := c;
					 best_f := f));
				   c)
		       (expand n.data n.g))
     in
       if not ((List.length children) = 0)
       then
	 (calc_h_data n !best_child children;
	  List.iter (fun c -> c.hhat <- f_calc c) children);
       children)


let wrap fn =
  (** wraps the function [fn] which is made to operate on domain objects so
      that it can be applied to search objects *)
  (fun n -> fn n.data)


(****************** Searches *******************)
let make_iface sface =
Search_interface.make
    ~node_expand:(wrap_expand sface.Search_interface.domain_expand
		    sface.Search_interface.hd sface.Search_interface.h)
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



let make_online_iface sface calc_hdata f_calc =
Search_interface.make
    ~node_expand:(wrap_ss_expand sface.Search_interface.domain_expand
		    sface.Search_interface.hd calc_hdata f_calc)
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



let no_dups_supplied sface args =
  (** A* epsilon search on domains without many duplicates.
      [sface] the domain interface
      [wt] the desired suboptimality bound *)
  let wt = Search_args.get_float "F_then_hhat.no_dups_supplied" args 0 in
  Limit.unwrap_sol5 unwrap
    (Focal_search.search
       (make_iface sface)           (*sface*)
       just_f                       (*ordered1*)
       (make_close_enough_p wt)     (*close enough *)
       hhat_then_f_then_g              (*ordered 2 *)
       just_f                       (* better_p*)
       set_q_pos                    (* set_q_pos*)
       get_q_pos)                   (* get_q_pos*)


let dups_supplied sface args =
  (** A* epsilon search on domains many duplicates.
      [sface] the domain interface
      [wt] the desired suboptimality bound *)
  let wt = Search_args.get_float "F_then_hhat.dups_supplied" args 0 in
  Limit.unwrap_sol6 unwrap
    (Focal_search.search_dups
       (make_iface sface)
       just_f
       (make_close_enough_p wt)
       hhat_then_f_then_g
       just_f
       set_q_pos
       get_q_pos)


(* Drop dups would not be admissible and so is not included *)

let no_dups sface args =
  (** A* epsilon search on domains without many duplicates.
      [sface] the domain interface
      [wt] the desired suboptimality bound *)
  let hcalc, fcalc =
    (Global_h_ss.make_h_only_correction
       (fun n -> n.g)
       (fun n -> n.f -. n.g)
       (fun n -> n.d)) in
  let wt = Search_args.get_float "F_then_hhat.no_dups" args 0 in
  Limit.unwrap_sol5 unwrap
    (Focal_search.search
       (make_online_iface sface hcalc fcalc)    (*sface*)
       just_f                       (*ordered1*)
       (make_close_enough_p wt)     (*close enough *)
       hhat_then_f_then_g              (*ordered 2 *)
       just_f                       (* better_p*)
       set_q_pos                    (* set_q_pos*)
       get_q_pos)                   (* get_q_pos*)


let dups sface args =
  (** A* epsilon search on domains many duplicates.
      [sface] the domain interface
      [wt] the desired suboptimality bound *)
  let hcalc, fcalc =
    (Global_h_ss.make_h_only_correction
       (fun n -> n.g)
       (fun n -> n.f -. n.g)
       (fun n -> n.d)) in
  let wt = Search_args.get_float "F_then_hhat.dups" args 0 in
  Limit.unwrap_sol6 unwrap
    (Focal_search.search_dups
       (make_online_iface sface hcalc fcalc)
       just_f
       (make_close_enough_p wt)
       hhat_then_f_then_g
       just_f
       set_q_pos
       get_q_pos)

(* EOF *)
