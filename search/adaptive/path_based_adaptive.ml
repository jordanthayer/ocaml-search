(**

    @author jtd7
    @since 2010-10-11
*)

type 'a node = {
  est_f : float;
  h_err : float;
  d_err : float;
  d : float;
  f : float;
  g : float;
  depth : int;
  mutable q_pos : int;
  data : 'a;
}

let setpos n i =
  (** Updates the [q_pos] of node [n], setting it to [i] *)
  n.q_pos <- i


let getpos n =
  (** returns the current [q_pos] of node [n] *)
  n.q_pos


let wrap f =
  (** Wraps a function [f] which works on domain data and makes it so
      that it can be applied to nodes *)
  (fun n -> f n.data)


let ordered_p a b =
  (** sorts nodes in order of estimated f, breaking ties in favor of
      low d values, and then in favor of high g *)
  (a.est_f < b.est_f) ||  (* sort by fhat *)
    (a.est_f = b.est_f &&
	((a.f < b.f))) ||
    (a.est_f = b.est_f) && (a.f = b.f) && a.g >= b.g


let better_p a b =
  (** Determines which of the nodes represents a better solution *)
  a.f <= b.f


let unwrap_sol s =
  (** Decomposes the solution [s] into a form which the domains are expecting
      when doing validation *)
  match s with
      Limit.Incumbent (q, n) -> Some (n.data, n.f)
    | _ -> None

(* f_calc should probably take revh, revd as well *)

let make_expand expand hd f_calc =
  (** [expand] is the domain expand
      [hd] is a cost and distance estimator
      [f_calc] g h, d, depth h_err, d_err to calculate f^ estimates *)
  (fun n ->
     let depth' = n.depth + 1 in
     List.map (fun (s,g) ->
		 let h,d = hd s in
		 let f = g +. h in
		 let h_err = (f -. (n.f)) +. n.h_err
		 and d_err = d -. n.d +. 1. +. n.d_err in
		   { est_f = f_calc  ~g ~h ~d ~depth:depth' ~h_err ~d_err;
		     h_err = if Math.finite_p h_err then h_err else n.h_err;
		     d_err = if Math.finite_p h_err then d_err else n.d_err;
		     d = d;
		     f = f;
		     g = g;
		     depth = depth';
		     q_pos = Dpq.no_position;
		     data = s;}) (expand n.data n.g))


let recurh_expand expand hd f_calc =
  (** [expand] is the domain expand
      [hd] is a cost and distance estimator
      [f_calc] g h, d, depth h_err, d_err to calculate f^ estimates *)
  (fun n ->
     let depth' = n.depth + 1 in
     List.map (fun (s,g) ->
		 let h,d = hd s in
		 let f = g +. h
		 and c = g -. n.g  in
		   (*
		     f - n.f = g + h - (n.g +. n.h)
		             = g - n.g + h - n.h
		             = c(n,p) + h - n.h
		             = e_h
		   *)
		 let h_err = (if c >= 0. then ((f -. n.f) /. c)  +. n.h_err
			      else n.h_err)
		 and d_err = d -. n.d +. 1. +. n.d_err in
		   { est_f = f_calc  ~g ~h ~d ~depth:depth' ~h_err ~d_err;
		     h_err = if Math.finite_p h_err then h_err else n.h_err;
		     d_err = if Math.finite_p h_err then d_err else n.d_err;
		     d = d;
		     f = f;
		     g = g;
		     depth = depth';
		     q_pos = Dpq.no_position;
		     data = s;}) (expand n.data n.g))


let just_h_expand expand hd f_calc =
  (** [expand] is the domain expand
      [hd] is a cost and distance estimator
      [f_calc] g h, d, depth h_err, d_err to calculate f^ estimates *)
  (fun n ->
     let depth' = n.depth + 1 in
     List.map (fun (s,g) ->
		 let h, _ = hd s in
		 let f = g +. h in
		 let c = g -. n.g
		 and ph = n.f -. n.g in
		 let h_err = n.h_err +. ((h +. c) /. ph) in
		   { est_f = f_calc  ~g ~h ~d:nan
		       ~depth:depth' ~h_err ~d_err:nan;
		     h_err = if Math.finite_p h_err then h_err else 1.;
		     d_err = nan;
		     d = nan;
		     f = f;
		     g = g;
		     depth = depth';
		     q_pos = Dpq.no_position;
		     data = s;}) (expand n.data n.g))


let make_root h d data =
  (** Constructs a root node *)
  { est_f = h;
    h_err = 0.;
    d_err = 0.;
    d = d;
    g = 0.;
    f = h;
    depth = 0;
    q_pos = Dpq.no_position;
    data = data; }

(**************************************************************************)

let make_interface f_calc sface =
  let hd = sface.Search_interface.hd in
  let h,d = hd sface.Search_interface.initial in
    assert(Math.finite_p d);
    assert(Math.finite_p h);
  let sface = Search_interface.make
    ~node_expand:(make_expand sface.Search_interface.domain_expand
	       hd f_calc)
    ~goal_p:(wrap sface.Search_interface.goal_p)
    ~halt_on:(sface.Search_interface.halt_on)
    ~hash:sface.Search_interface.hash
    ~equals:sface.Search_interface.equals
    ~key:(wrap sface.Search_interface.key)
    sface.Search_interface.domain
    (make_root h d sface.Search_interface.initial)
    better_p
    (Limit.make_default_logger (fun n -> n.f)
       (wrap sface.Search_interface.get_sol_length)) in
    sface


let make_justh_interface f_calc sface =
  let h,d = sface.Search_interface.hd sface.Search_interface.initial in
    assert(Math.finite_p d);
    assert(Math.finite_p h);
  let sface = Search_interface.make
    ~node_expand:(just_h_expand sface.Search_interface.domain_expand
	       sface.Search_interface.hd f_calc)
    ~goal_p:(wrap sface.Search_interface.goal_p)
    ~halt_on:(sface.Search_interface.halt_on)
    ~hash:sface.Search_interface.hash
    ~equals:sface.Search_interface.equals
    ~key:(wrap sface.Search_interface.key)
    sface.Search_interface.domain
    (make_root h d sface.Search_interface.initial)
    better_p
    (Limit.make_default_logger (fun n -> n.f)
       (wrap sface.Search_interface.get_sol_length)) in
    sface

let make_recur_interface f_calc sface =
  let h,d = sface.Search_interface.hd sface.Search_interface.initial in
    assert(Math.finite_p d);
    assert(Math.finite_p h);
  let sface = Search_interface.make
    ~node_expand:(recurh_expand sface.Search_interface.domain_expand
	       sface.Search_interface.hd f_calc)
    ~goal_p:(wrap sface.Search_interface.goal_p)
    ~halt_on:(sface.Search_interface.halt_on)
    ~hash:sface.Search_interface.hash
    ~equals:sface.Search_interface.equals
    ~key:(wrap sface.Search_interface.key)
    sface.Search_interface.domain
    (make_root h d sface.Search_interface.initial)
    better_p
    (Limit.make_default_logger (fun n -> n.f)
       (wrap sface.Search_interface.get_sol_length)) in
    sface


  (* curry over f_calc to get the search call *)

let do_no_dups f_calc sface args =
  let iface = make_interface f_calc sface in
    Limit.unwrap_sol5 unwrap_sol
      (Best_first.search iface ordered_p better_p)


let do_dups f_calc sface args =
  let iface = make_interface f_calc sface in
    Limit.unwrap_sol6 unwrap_sol
      (Best_first.search_dups iface ordered_p better_p setpos getpos)


let do_drop f_calc sface args =
  let iface = make_interface f_calc sface in
    Limit.unwrap_sol6 unwrap_sol
      (Best_first.search_drop_dups iface ordered_p better_p setpos getpos)


let justh_no_dups f_calc sface args =
  let iface = make_justh_interface f_calc sface in
    Limit.unwrap_sol5 unwrap_sol
      (Best_first.search iface ordered_p better_p)


let justh_dups f_calc sface args =
  let iface = make_justh_interface f_calc sface in
    Limit.unwrap_sol6 unwrap_sol
      (Best_first.search_dups iface ordered_p better_p setpos getpos)


let justh_drop f_calc sface args =
  let iface = make_justh_interface f_calc sface in
    Limit.unwrap_sol6 unwrap_sol
      (Best_first.search_drop_dups iface ordered_p better_p setpos getpos)


let recur_no_dups f_calc sface args =
  let iface = make_recur_interface f_calc sface in
    Limit.unwrap_sol5 unwrap_sol
      (Best_first.search iface ordered_p better_p)


let recur_dups f_calc sface args =
  let iface = make_recur_interface f_calc sface in
    Limit.unwrap_sol6 unwrap_sol
      (Best_first.search_dups iface ordered_p better_p setpos getpos)


let recur_drop f_calc sface args =
  let iface = make_recur_interface f_calc sface in
    Limit.unwrap_sol6 unwrap_sol
      (Best_first.search_drop_dups iface ordered_p better_p setpos getpos)


(*** F calc section ***)

let test_f_calc  ~g ~h ~d ~depth ~h_err ~d_err =
  (* an f_calc just for testing purposes, returns f(n) *)
  g +. h


let unclamped_austin  ~g ~h ~d ~depth ~h_err ~d_err =
    (** A correction of the single step error correction wheeler and I
	did, corrected to a geometric series by austin *)
  let depth = float depth in
  let d_step = Math.fmin (d_err /. depth) 1.
  and h_step = h_err /. depth in
  let nd = d /. (1. -. d_step) in
    if Math.finite_p nd then g +. h +. nd *. h_step else infinity


let clamped_austin ~bound  ~g ~h ~d ~depth ~h_err ~d_err =
  (** A correction of the single step error correction wheeler and I
      did, corrected to a geometric series by Austin, this one is
      bounded *)
  let depth = float depth in
  let d_step = Math.fmin (d_err /. depth) 1.
  and h_step = h_err /. depth in
  let nd = d /. (1. -. d_step) in
  let max = bound *. (g +. h) in
(*    Verb.pe Verb.always "%f %f %f %f %f\n"
      depth d_step h_step nd max;*)
    if Math.finite_p nd
    then Math.fmin (g +. h +. nd *. h_step) max
    else max

let clamped_austin_prime ~bound  ~g ~h ~d ~depth ~h_err ~d_err =
  (** A correction of the single step error correction wheeler and I
      did, corrected to a geometric series by Austin, this one is
      bounded *)
  let depth = float depth in
  let d_step = Math.fmin (d_err /. depth) 1.
  and h_step = h_err /. depth in
  let nd = d /. (1. -. d_step) in
  let max = bound *. (g +. h) in
(*    Verb.pe Verb.always "%f %f %f %f %f\n"
      depth d_step h_step nd max;*)
    if Math.finite_p nd
    then Math.fmin (g +. (h +. nd *. h_step) *. bound) max
    else max


let greedy ~g ~h ~d ~depth ~h_err ~d_err =
  let depth = float depth in
  let d_step = Math.fmin (d_err /. depth) 1.
  and h_step = h_err /. depth in
  let nd = d /. (1. -. d_step) in
    (*Verb.pe Verb.always "%f %f %f %f\n" depth d_err d_step nd ;*)
    if Math.finite_p nd then h +. (nd *. h_step) else infinity

let speedy ~g ~h ~d ~depth ~h_err ~d_err =
  let depth = float depth in
  let d_step = Math.fmin (d_err /. depth) 1. in
  let nd = d /. (1. -. d_step) in
    (*Verb.pe Verb.always "%f %f %f %f\n" depth d_err d_step nd ;*)
  if Math.finite_p nd then nd else infinity


let speediest ~g ~h ~d ~depth ~h_err ~d_err =
  let depth = float depth in
  let d_step = Math.fmin (d_err /. depth) 1. in
  let nd = d /. (1. -. d_step) in
  let nd = nd /. (depth +. nd) in
    (*Verb.pe Verb.always "%f %f %f %f\n" depth d_err d_step nd ;*)
  if Math.finite_p nd then nd else infinity


let seeded_greedy ~herr_seed ~derr_seed ~seed_ct =
  assert (derr_seed <= 1.);
  let dmass = seed_ct *. derr_seed
  and hmass = seed_ct *. herr_seed in
    (fun ~g ~h ~d ~depth ~h_err ~d_err ->
       let depth = float depth in
       let d_step = Math.fmin ((d_err +. dmass) /. (depth +. seed_ct)) 1.
       and h_step = (h_err +. hmass) /. (depth +. seed_ct) in
       let nd = d /. (1. -. d_step) in
	 (*Verb.pe Verb.always "%f %f %f %f\n" depth d_err d_step nd ;*)
	 if Math.finite_p nd then h +. (nd *. h_step) else infinity)


let seeded_austin ~herr_seed ~derr_seed ~seed_ct =
  (** A correction of the single step error correction wheeler and I
      did, corrected to a geometric series by Austin, this one is
      bounded *)
  let dmass = seed_ct *. derr_seed
  and hmass = seed_ct *. herr_seed in
    (fun ~g ~h ~d ~depth ~h_err ~d_err ->
       let depth = float depth in
       let d_step = Math.fmin ((d_err +. dmass) /. (depth +. seed_ct)) 1.
       and h_step = (h_err +. hmass) /. (depth +. seed_ct) in
       let nd = d /. (1. -. d_step) in
	 (*    Verb.pe Verb.always "%f %f %f %f %f\n"
	       depth d_step h_step nd max;*)
	 if Math.finite_p nd
	 then (g +. h +. nd *. h_step)
	 else infinity)




let recurh ~g ~h ~d ~depth ~h_err ~d_err =
  let depth = float depth in
  let h_step = Math.fmin (h_err /. depth) 1. in
    (*Verb.pe Verb.always "%f %f %f\n%!" depth h_err h_step;*)
    h /. (1. -. h_step)

let just_h  ~g ~h ~d ~depth ~h_err ~d_err =
  let depth = float depth in
  let h_step = (h_err /. depth) in
    (*Verb.pe Verb.always "fact : %f\n" (h_step);*)
    h *. (h_step)


(*** Call search section ***)

(* these run A* using the above code *)
let test sface args = do_no_dups test_f_calc sface args
and test_dups sface args = do_dups test_f_calc sface args
and test_dd sface args = do_drop test_f_calc sface args

let austin_uc sface args = do_no_dups unclamped_austin sface args
and austin_uc_dups sface args = do_dups unclamped_austin sface args
and austin_uc_dd sface args = do_drop unclamped_austin sface args

let austin_clamped sface args =
  let bound = Search_args.get_float "Path_based_adaptive" args 0 in
    do_no_dups (clamped_austin ~bound) sface args

and austin_clamped_dups sface args =
  let bound = Search_args.get_float "Path_based_adaptive" args 0 in
    do_dups (clamped_austin ~bound) sface args

and austin_clamped_dd sface args =
  let bound = Search_args.get_float "Path_based_adaptive" args 0 in
  do_drop (clamped_austin ~bound) sface args


let austin_clamped_prime sface args =
  let bound = Search_args.get_float "Path_based_adaptive" args 0 in
    do_no_dups (clamped_austin_prime ~bound) sface args

and austin_clamped_dups_prime sface args =
  let bound = Search_args.get_float "Path_based_adaptive" args 0 in
    do_dups (clamped_austin_prime ~bound) sface args

and austin_clamped_dd_prime sface args =
  let bound = Search_args.get_float "Path_based_adaptive" args 0 in
  do_drop (clamped_austin_prime ~bound) sface args


let greedy_path sface args = do_no_dups greedy sface args
and greedy_path_dups sface args = do_dups greedy sface args
and greedy_path_dd sface args = do_drop greedy sface args

let speedy_path sface args = do_no_dups speedy sface args
and speedy_path_dups sface args = do_dups speedy sface args
and speedy_path_dd sface args = do_drop speedy sface args

let speediest_path sface args = do_no_dups speediest sface args
and speediest_path_dups sface args = do_dups speediest sface args
and speediest_path_dd sface args = do_drop speediest sface args


let seeded_greedy_path sface args =
  let herr_seed = Search_args.get_float "Seeded_greedy" args 0
  and derr_seed = Search_args.get_float "Seeded_greedy" args 1
  and seed_ct = Search_args.get_float "Seeded_greedy" args 2 in
    do_no_dups (seeded_greedy ~herr_seed ~derr_seed ~seed_ct) sface args

and seeded_greedy_path_dups sface args =
  let herr_seed = Search_args.get_float "Seeded_greedy" args 0
  and derr_seed = Search_args.get_float "Seeded_greedy" args 1
  and seed_ct = Search_args.get_float "Seeded_greedy" args 2 in
    do_dups (seeded_greedy ~herr_seed ~derr_seed ~seed_ct) sface args

and seeded_greedy_path_dd sface args =
let herr_seed = Search_args.get_float "Seeded_greedy" args 0
  and derr_seed = Search_args.get_float "Seeded_greedy" args 1
  and seed_ct = Search_args.get_float "Seeded_greedy" args 2 in
  do_drop (seeded_greedy ~herr_seed ~derr_seed ~seed_ct) sface args



let recur_path sface args = recur_no_dups recurh sface args
and recur_path_dups sface args = recur_dups recurh sface args
and recur_path_dd sface args = recur_drop recurh sface args


let justh_path sface args = justh_no_dups just_h sface args
and justh_path_dups sface args = justh_dups just_h sface args
and justh_path_dd sface args = justh_drop just_h sface args

(* EOF *)
