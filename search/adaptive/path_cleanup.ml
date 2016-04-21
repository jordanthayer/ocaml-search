(**

    @author jtd7
    @since 2010-10-16

   Similar to the previously existing generic cleanup code, but this time we're
   using path based error correction
*)

type 'a node = {
  est_f : float;
  f : float;
  g : float;
  d : float;
  depth : int;
  h_err : float;
  d_err : float;
  mutable ppos : int;
  mutable fpos : int;
  mutable dpos : int;
  data : 'a;
}



let get_f_pos a =
  (** returns the f position of node a *)
  a.fpos

let get_d_pos a =
  (** returns the delayed position of node [a] *)
  a.dpos

let get_pq_pos a =
  (** returns the open position of node [a] *)
  a.ppos


let set_f_pos a i =
  (** sets the cleanup position of node [a] to [i] *)
  a.fpos <- i


let set_pq_pos a i =
  (** sets the open position of node [a] to [i] *)
  a.ppos <- i

let set_d_pos a i =
  (** sets the delay position of node [a] to [i] *)
  a.dpos <- i


let wrap f =
  (** Wraps a function [f] which works on domain data and makes it so
      that it can be applied to nodes *)
  (fun n -> f n.data)


let est_f_then_d_then_g a b =
  (** sorts nodes in order of estimated f, breaking ties in favor of
      low d values, and then in favor of high g *)
  (a.est_f < b.est_f) ||  (* sort by fhat *)
    ((a.est_f = b.est_f) &&
       a.d < b.d) ||
    (a.est_f = b.est_f && a.d = b.d &&
	((a.g >= b.g)))

let better_p a b =
  (** Determines which of the nodes represents a better solution *)
  (a.g) <= (b.g)

let ordered_f a b =
  (** Determines which of the nodes represents a better solution *)
  (a.f) < (b.f) ||
      (a.f = b.f && a.g >= b.g)


let unwrap_sol s =
  (** Decomposes the solution [s] into a form which the domains are expecting
      when doing validation *)
  match s with
      Limit.Incumbent (q, n) -> if q = 0. then None else Some (n.data, n.g)
    | _ -> None



let make_expand expand hd f_calc bound =
  (** [expand] is the domain expand
      [hd] is a cost and distance estimator
      [f_calc] g h, d, depth h_err, d_err to calculate f^ estimates *)
  (fun n ->
     let depth' = n.depth + 1 in
     List.map (fun (s,g) ->
		 let h,d = hd s in
		 let f = g +. h in
		 let h_err = f -. (n.f) +. n.h_err
		 and d_err = (d +. 1.) -. n.d +. n.d_err in
		   { est_f = f_calc ~g ~h ~d ~depth:depth' ~h_err ~d_err ~bound;
		     h_err = h_err;
		     d_err = d_err;
		     f = f;
		     g = g;
		     d = d;
		     depth = depth';
		     fpos = Dpq.no_position;
		     dpos = Dpq.no_position;
		     ppos = Dpq.no_position;
		     data = s;}) (expand n.data n.g))

let make_expand_pm expand hd f_calc bound =
  (** [expand] is the domain expand
      [hd] is a cost and distance estimator
      [f_calc] g h, d, depth h_err, d_err to calculate f^ estimates *)
  (fun n ->
     let depth' = n.depth + 1 in
     List.map (fun (s,g) ->
		 let h,d = hd s in
		 let h = Math.fmax h (n.f -. g) in
		 let f = g +. h in
		 let h_err = f -. (n.f) +. n.h_err
		 and d_err = (d +. 1.) -. n.d +. n.d_err in
		   { est_f = f_calc ~g ~h ~d ~depth:depth' ~h_err ~d_err ~bound;
		     h_err = h_err;
		     d_err = d_err;
		     f = f;
		     g = g;
		     d = d;
		     depth = depth';
		     fpos = Dpq.no_position;
		     dpos = Dpq.no_position;
		     ppos = Dpq.no_position;
		     data = s;}) (expand n.data n.g))



let get_node fq pq i bound =
  (** Returns the next node to be expanded *)
  let fn = Dpq.peek_first fq
  and incumbent = i.Limit.incumbent in
    match incumbent with
      | Limit.Nothing -> raise Optimistic_framework.NoIncumbent
      | Limit.Incumbent(qual,inc) ->
	  if ((fn.f) *. bound) >= (inc.g)
	  then raise Optimistic_framework.Done;
	  (* let fpn = Dpq.peek_first pq in
	     if fpn.est_f < (inc.g) then
	     (Dpq.remove pq fpn.ppos;
	     Dpq.remove fq fpn.fpos;
	       fpn)
	     else*)
	      (let trf = fn.fpos
	       and trp = fn.ppos in
		 Dpq.remove fq trf;
		 Dpq.remove pq trp;
		 fn)


let make_sface bound f_calc sface =
  let h,d = sface.Search_interface.hd sface.Search_interface.initial in
  Search_interface.make
    ~node_expand:(make_expand sface.Search_interface.domain_expand
		    sface.Search_interface.hd f_calc bound)
    ~goal_p:(wrap sface.Search_interface.goal_p)
    ~halt_on:(sface.Search_interface.halt_on)
    ~hash:sface.Search_interface.hash
    ~key:(wrap sface.Search_interface.key)
    ~equals:sface.Search_interface.equals
    sface.Search_interface.domain
    { est_f = h;
      f = h;
      d = d;
      g = 0.;
      depth = 0;
      h_err = 0.;
      d_err = 0.;
      ppos = Dpq.no_position;
      dpos = Dpq.no_position;
      fpos = Dpq.no_position;
      data = sface.Search_interface.initial;}
    better_p
    (Limit.make_default_logger (fun n -> n.f)
       (wrap sface.Search_interface.get_sol_length))


let make_pm_sface bound f_calc sface =
  let h,d = sface.Search_interface.hd sface.Search_interface.initial in
  Search_interface.make
    ~node_expand:(make_expand_pm sface.Search_interface.domain_expand
		    sface.Search_interface.hd f_calc bound)
    ~goal_p:(wrap sface.Search_interface.goal_p)
    ~halt_on:(sface.Search_interface.halt_on)
    ~hash:sface.Search_interface.hash
    ~key:(wrap sface.Search_interface.key)
    ~equals:sface.Search_interface.equals
    sface.Search_interface.domain
    { est_f = h;
      f = h;
      d = d;
      g = 0.;
      depth = 0;
      h_err = 0.;
      d_err = 0.;
      ppos = Dpq.no_position;
      dpos = Dpq.no_position;
      fpos = Dpq.no_position;
      data = sface.Search_interface.initial;}
    better_p
    (Limit.make_default_logger (fun n -> n.f)
       (wrap sface.Search_interface.get_sol_length))



(******* F calcs *****)

let unclamped_austin ~g ~h ~d ~depth ~h_err ~d_err ~bound =
  (** A correction of the single step error correction wheeler and I
      did, corrected to a geometric series by austin *)
  let depth = float depth in
  let d_step = Math.fmin (d_err /. depth) 1.
  and h_step = h_err /. depth in
  let nd = d /. (1. -. d_step) in
    if Math.finite_p nd
    then g +. (h +. nd *. h_step) *. bound
    else infinity


let test_skept ~g ~h ~d ~depth ~h_err ~d_err ~bound =
  (** A correction of the single step error correction wheeler and I
      did, corrected to a geometric series by austin *)
  let depth = float depth in
  let d_step = Math.fmin (d_err /. depth) 1.
  and h_step = h_err /. depth in
  let nd =
    (let dhat = d /. (1. -. d_step) in
       if Math.finite_p dhat then dhat
       else depth +. d) in
  let h_hat = Math.fmin (bound *. h) (h +. nd *. h_step) in
    g +. h_hat *. bound

(*************************** Searches ***************************)



let no_dups f_calc sface args =
  let bound = Search_args.get_float "Wted_astar.dups_on_gen" args 0 in
    Limit.unwrap_sol5 unwrap_sol
      (Optimistic_framework.no_dups
	 (make_sface bound f_calc sface)
	 get_node
	 est_f_then_d_then_g
	 bound
	 better_p
	 ordered_f
	 set_pq_pos
	 set_f_pos)


let dups f_calc sface args =
  let bound = Search_args.get_float "Wted_astar.dups_on_gen" args 0 in
    Limit.unwrap_sol6 unwrap_sol
      (Optimistic_framework.dups
	 (make_sface bound f_calc sface)
	 get_node
	 est_f_then_d_then_g
	 bound
	 better_p
	 ordered_f
	 set_pq_pos
	 set_f_pos
	 get_pq_pos
	 get_f_pos)


let delay_dups f_calc sface args =
  let bound = Search_args.get_float "Wted_astar.dups_on_gen" args 0 in
    Limit.unwrap_sol6 unwrap_sol
      (Optimistic_framework.delay
	 (make_sface bound f_calc sface)
	 get_node
	 est_f_then_d_then_g
	 bound
	 better_p
	 ordered_f
	 set_pq_pos
	 set_d_pos
	 set_f_pos
	 get_pq_pos
	 get_d_pos
	 get_f_pos)




let no_dups_pm f_calc sface args =
  let bound = Search_args.get_float "Wted_astar.dups_on_gen" args 0 in
    Limit.unwrap_sol5 unwrap_sol
      (Optimistic_framework.no_dups
	 (make_pm_sface bound f_calc sface)
	 get_node
	 est_f_then_d_then_g
	 bound
	 better_p
	 ordered_f
	 set_pq_pos
	 set_f_pos)


let dups_pm f_calc sface args =
  let bound = Search_args.get_float "Wted_astar.dups_on_gen" args 0 in
    Limit.unwrap_sol6 unwrap_sol
      (Optimistic_framework.dups
	 (make_pm_sface bound f_calc sface)
	 get_node
	 est_f_then_d_then_g
	 bound
	 better_p
	 ordered_f
	 set_pq_pos
	 set_f_pos
	 get_pq_pos
	 get_f_pos)


let delay_dups_pm f_calc sface args =
  let bound = Search_args.get_float "Wted_astar.dups_on_gen" args 0 in
    Limit.unwrap_sol6 unwrap_sol
      (Optimistic_framework.delay
	 (make_pm_sface bound f_calc sface)
	 get_node
	 est_f_then_d_then_g
	 bound
	 better_p
	 ordered_f
	 set_pq_pos
	 set_d_pos
	 set_f_pos
	 get_pq_pos
	 get_d_pos
	 get_f_pos)

(*** Call search section ***)

let austin_uc sface args = no_dups unclamped_austin sface args
and austin_uc_dups sface args = dups unclamped_austin sface args
and austin_uc_dd sface args = delay_dups unclamped_austin sface args


let skept_uc sface args = no_dups test_skept sface args
and skept_uc_dups sface args = dups test_skept sface args
and skept_uc_dd sface args = delay_dups test_skept sface args


let austin_uc_pm sface args = no_dups unclamped_austin sface args
and austin_uc_dups_pm sface args = dups unclamped_austin sface args
and austin_uc_dd_pm sface args = delay_dups unclamped_austin sface args


let skept_uc_pm sface args = no_dups_pm test_skept sface args
and skept_uc_dups_pm sface args = dups_pm test_skept sface args
and skept_uc_dd_pm sface args = delay_dups_pm test_skept sface args


(* EOF *)
