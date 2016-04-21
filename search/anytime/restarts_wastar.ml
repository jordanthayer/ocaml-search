(** Restatrs weighted A* implemented in the current search framework
    Jordan Thayer - July 2009 *)

let wlist1 = [10.; 5.; 3.; 2.; 1.]
and richterl1 = [5.; 3.; 2.; 1.5; 1.;]
and richterl2 = [3.; 2.; 1.5; 1.25; 1.;]
(*and synth_list = [25.; 20.; 15.; 10.; 5.; 3.; 2.; 1.]*)
and synth_list = [25.; 20.; 15.; 10.; 7.; 5.; 3.; 2.5; 2.; 1.75; 1.5; 1.]

and icaps_09_l1 = [2.; 1.8; 1.6; 1.4; 1.2; 1.;]
and icaps_09_l2 = [2.; 1.5; 1.25; 1.125; 1.]
and icaps_review_list1 = [3.; 1.]
and icaps_review_list2 = [5.; 1.]
(*
let richterl1 = icaps_09_l1
and richterl2 = icaps_09_l2
*)
type 'a node = {
  mutable wf : float;
  f : float;
  g : float;
  h : float;
  data : 'a;
  mutable pos : int;
}


let update n wt =
  (** Updates the wf of a node [n] based on the new weight [wt] *)
    n.wf <- n.g +. n.h *. wt


let make_expand expand h =
  (** takes the domain [expand] function and a cost estimator [h] and
      returns a node expand function *)
  (fun parent ->
     List.map (fun (n, g) ->
		 let h = h n in
		   { wf = neg_infinity;
		     f = g +. h;
		     g = g;
		     h = h;
		     data = n;
		     pos = Dpq.no_position; })
     (expand parent.data parent.g))


let better a b =
  (** Determines if [a] represents a better solution than [b] *)
  a.f <= b.f

let ordered a b =
  (** are [a] and [b] in order of increase wf? *)
  (a.wf < b.wf) ||
  ((a.wf = b.wf) &&
   ((a.f < b.f) ||
    ((a.f = b.f) &&
     (a.g >= b.g))))

let f_then_g a b =
  (** are [a] and [b] in f order? *)
  a.f <= b.f ||
    a.f = b.f &&
      a.g >= b.g

let setpos n i =
  (** sets the position of node [n] to [i] *)
  n.pos <- i

let getpos n =
  (** returns the position of node [n]*)
  n.pos

let wrap f =
  (** wraps the function [f] which was meant to operate on nodes in the
      domain space so that it can operate on search nodes *)
  (fun n -> f n.data)


let make_root initial =
  (** takes a root in the domain and returns the root of the search space *)
  { wf = neg_infinity;
    f = neg_infinity;
    g = 0.;
    h = 0.;
    data = initial;
    pos = Dpq.no_position; }


let wrap_incumbent i =
  (** takes an incumbent solution and returns a limit.info *)
  match i with
    None -> Limit.Nothing
  | Some (n, g) -> Limit.Incumbent (0.,
				    { wf = g;
				      f = g;
				      g = g;
				      h = 0.;
				      data = n;
				      pos = Dpq.no_position; })


let unwrap_sol s =
  (** takes the solution found by the search and returns something that the
      domains can easily operate on *)
  match s with
    Limit.Incumbent (q,n) -> Some (n.data, n.g)
    | _ -> None


(***************************************************************************)

let make_iface sface =
  let def_log = Limit.make_default_logger (fun n -> n.f)
    (wrap sface.Search_interface.get_sol_length) in
    Search_interface.make
      ~node_expand:(make_expand sface.Search_interface.domain_expand
		      sface.Search_interface.h)
      ~goal_p:(wrap sface.Search_interface.goal_p)
      ~halt_on:sface.Search_interface.halt_on
      ~key:(wrap sface.Search_interface.key)
      ~hash:sface.Search_interface.hash
      ~equals:sface.Search_interface.equals
      sface.Search_interface.domain
      { data = sface.Search_interface.initial;
	wf = neg_infinity;
	f = neg_infinity;
	g = 0.;
	h = neg_infinity;
	pos = Dpq.no_position;}
      better
      (fun i ->
	 sface.Search_interface.info.Limit.log
	   (Limit.unwrap_info (fun n -> n.data) i);
	 def_log i)



let no_dups sface args =
  (** Performs an A* search from the initial state to a goal,
      for domains with no duplicates. *)
  let wts = Array.to_list
    (Search_args.get_float_array "Restarts_wastar.no_dups" args) in
  Limit.unwrap_sol5 unwrap_sol
    (Restarting_search.no_dups
       (make_iface sface)
       wts
       ordered
       f_then_g
       better
       setpos
       getpos
       (List.length wts)
       update)


let dups sface args =
  (** Performs an A* search from the initial state to a goal,
      for domains with no duplicates. *)
  let wts = Array.to_list
    (Search_args.get_float_array "Restarts_wastar.dups" args) in
    Limit.unwrap_sol6 unwrap_sol
      (Restarting_search.dups
	 (make_iface sface)
	 wts
	 ordered
	 f_then_g
	 better
	 setpos
	 getpos
	 (List.length wts)
	 update)


let delay_dups sface args =
  (** Performs an A* search from the initial state to a goal,
      for domains with no duplicates. *)
  let wts = Array.to_list
    (Search_args.get_float_array "Restarts_wastar.delay_dups" args) in
    Limit.unwrap_sol6 unwrap_sol
      (Restarting_search.delay_dups
	 (make_iface sface)
	 wts
	 ordered
	 f_then_g
	 better
	 setpos
	 getpos
	 (List.length wts)
	 update)

(* EOF *)
