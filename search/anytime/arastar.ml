(** Anytime Repairing A*, using the repairing search framework.

    Jordan Thayer - July 2009 *)


type 'a node = {
  mutable fp : float; (* value of node, used in priority queue *)
  f : float;
  g : float;
  h : float;
  data : 'a;
  mutable fp_ind: int;
  mutable f_ind : int;
}

let make_initial dat h wt =
  let hofdat = h dat in
  { fp = hofdat *. wt;
    f = hofdat;
    g = 0.;
    h = hofdat;
    data = dat;
    fp_ind = Dpq.no_position;
    f_ind = Dpq.no_position;}


let update n wt =
  (** Updates the f prime value of a node based on the new weight *)
  n.fp <- n.g +. wt *. n.h


let better_p a b =
  (** Determins if [a] is a better incumbent solution than b. *)
  a.g < b.g


let f_order a b =
  (** are [a] and [b] in order of increasing f? *)
  (a.f < b.f) ||
    ((a.f = b.f) &&
       a.g > b.g)


let fp_order a b =
  (** are [a] and [b] in increasing order of f primes?*)
  (a.fp < b.fp) ||
    ((a.fp = b.fp) && (f_order a b))


let set_fpi a i =
  (** sets the [fp_ind] of node [a] to [i] *)
  a.fp_ind <- i


let set_fi a i =
  (** sets the [f_ind] of node [a] to [i] *)
  a.f_ind <- i


let get_fpi a =
  (** returns the [fp_ind] of node [a] *)
  a.fp_ind


let get_fi a =
  (** returns the [f_ind] of node [a] *)
  a.f_ind


let wrap_expand expand h =
  (** Wraps the domain [expand] function, returning an expand function
      suitable for nodes.  Additionally requires a heuristic estimator
      of cost to go [h] *)
  (fun n ->
     let children = expand n.data n.g in
       List.map (fun (d,g) ->
		   let hofc = h d in
		     {fp = neg_infinity;
		      f = g +. hofc;
		      h = hofc;
		      g = g;
		      data = d;
		      fp_ind = Dpq.no_position;
		      f_ind = Dpq.no_position}) children)


let unwrap s =
  (** Takes the solution found, [s], and converts it into something that the
      domain expects for validation purposes *)
  match s with
      Limit.Nothing -> None
    | Limit.Incumbent (q, n) -> Some (n.data, n.g)


let wrap fn =
  (** takes a function [fn] meant to be used on domain values and makes it
      apply to nodes *)
  (fun n -> fn n.data)


let mk_wtlist start decr =
  (** returns a weight list to be used by ARA*, starting at [start] and
      decrementing by [decr].  It is assumed that there is some value x
      such that start - x * decr = 1 *)
  List.rev
    (Array.to_list
       (Array.init ((int_of_float ((start -. 1.) /. decr)) + 1)
	  (fun i -> 1. +. (float_of_int i) *. decr)))

(*****************************************************************************)
let make_iface sface wts =
  let def_log = Limit.make_default_logger (fun n -> n.f)
    (wrap sface.Search_interface.get_sol_length) in
    Search_interface.make
      ~node_expand:(wrap_expand sface.Search_interface.domain_expand
		      sface.Search_interface.h)
      ~key:(wrap sface.Search_interface.key)
      ~goal_p:(wrap sface.Search_interface.goal_p)
      ~halt_on:sface.Search_interface.halt_on
      ~hash:sface.Search_interface.hash
      ~equals:sface.Search_interface.equals
      sface.Search_interface.domain
      (make_initial sface.Search_interface.initial sface.Search_interface.h
	 (List.hd wts))
      better_p
      (fun i ->
	 sface.Search_interface.info.Limit.log
	   (Limit.unwrap_info (fun n -> n.data) i);
	 def_log i)


let no_dups sface args =
  (** Performs ARA* on domains with few duplicate states. *)
  let wts = Array.to_list
    (Search_args.get_float_array "Arastar.no_dups" args) in
  let search_interface = make_iface sface wts in
    assert (wts <> []);
    Limit.unwrap_sol5 unwrap
      (Repairing_search.no_dups
	 search_interface
	 wts
	 fp_order
	 f_order
	 f_order
	 better_p
	 update
	 set_fpi
	 set_fi
	 get_fpi
	 get_fi
	 (fun n -> n.f))


let dups sface args =
  (** Performs ARA* on domains with duplicate states. *)
  let wts = Array.to_list (Search_args.get_float_array "Arastar.dups" args) in
  let search_interface = make_iface sface wts in
    assert (wts <> []);
    Limit.unwrap_sol6 unwrap
      (Repairing_search.dups
	 search_interface
	 wts
	 fp_order
	 f_order
	 f_order
	 better_p
	 update
	 set_fpi
	 set_fi
	 get_fpi
	 get_fi
	 (fun n -> n.f))


(* EOF *)
