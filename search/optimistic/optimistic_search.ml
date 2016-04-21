(** Optimistic Search *)


type 'a node = {
  mutable fp : float;
  h : float;
  mutable g : float;
  mutable ppos : int;
  mutable fpos: int;
  mutable dpos: int;
  data : 'a;
}



let ordered_f a b =
  (** are nodes [a] and [b] in order of increasing f? *)
  let af = a.g +. a.h
  and bf = b.g +. b.h in
    af < bf ||
      (af = bf &&
	  a.g >= b.g)


let ordered_p a b =
  (** are [a] and [b] in best first order *)
  let af = a.g +. a.h
  and bf = b.g +. b.h in
  (a.fp < b.fp) ||
  ((a.fp = b.fp) &&
     af < bf) ||
    ((a.fp = b.fp && af = bf && a.g >= b.g))


let better_p a b =
  (** is [a] a better solution than [b] *)
  (a.g +. a.h) <= (b.g +. b.h)


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
  (** takes a function [f] meant for domain nodes and makes it so that it can
      be applied to search nodes *)
  (fun n -> f n.data)


let unwrap_sol s =
  (** Takes a search space solution and converts it into a domain space
      solution *)
  match s with
    Limit.Incumbent (q,n) -> if q = 0. then None else Some (n.data, n.g)
    | _ -> None


let make_expand expand h weight =
  (** takes the [expand] function from the domain and returns a search space
      expand function *)
  let make_child =
    (fun (n, g) ->
       let h = h n in
	 { fp = g +. (weight *. h);
	   h = h;
	   g = g;
	   fpos = Dpq.no_position;
	   ppos = Dpq.no_position;
	   dpos = Dpq.no_position;
	   data = n;})  in
    (fun parent ->
       List.map make_child (expand parent.data parent.g))


let get_node fq pq i bound =
  (** Returns the next node to be expanded *)
  let fn = Dpq.peek_first fq
  and incumbent = i.Limit.incumbent in
    match incumbent with
	Limit.Nothing ->
	  raise Optimistic_framework.NoIncumbent
      | Limit.Incumbent(qual,inc) ->
	  if ((fn.g +. fn.h) *. bound) >= (inc.g +. inc.h)
	  then raise Optimistic_framework.Done;
	  let fpn = Dpq.peek_first pq in
	    if fpn.fp < (inc.g +. inc.h) then
	      (Dpq.remove pq fpn.ppos;
	       Dpq.remove fq fpn.fpos;
	       fpn)
	    else
	      (let trf = fn.fpos
	       and trp = fn.ppos in
		 Dpq.remove fq trf;
		 Dpq.remove pq trp;
		 fn)

(***************************************************************************)
let make_interface sface wt =
  Search_interface.make
    ~node_expand:(make_expand sface.Search_interface.domain_expand
		    sface.Search_interface.h wt)
    ~key:(wrap sface.Search_interface.key)
    ~goal_p:(wrap sface.Search_interface.goal_p)
    ~halt_on:sface.Search_interface.halt_on
    ~hash:sface.Search_interface.hash
    ~equals:sface.Search_interface.equals
    sface.Search_interface.domain
    { fp = neg_infinity;
      h = neg_infinity;
      g = 0.;
      ppos = Dpq.no_position;
      fpos = Dpq.no_position;
      dpos = Dpq.no_position;
      data = sface.Search_interface.initial}
    better_p
    (Limit.make_default_logger (fun n -> n.h +. n.g)
       (wrap sface.Search_interface.get_sol_length))


let no_dups sface args =
  (** Performs optimistic search in domains with few or no duplicates *)
  let bound = Search_args.get_float "Optimistic_search.no_dups" args 0
  and wt = Search_args.get_float "Optimistic_search.no_dups" args 1 in
  let wt = (bound -. 1.) *. wt +. 1. in
  let search_interface =  make_interface sface wt  in
    Limit.unwrap_sol5 unwrap_sol
      (Optimistic_framework.no_dups
	 search_interface
	 get_node
	 ordered_p
	 bound
	 better_p
	 ordered_f
	 set_pq_pos
	 set_f_pos)


let dups sface args =
  (** Performs optimistic search in domains with duplicates *)
  let bound = Search_args.get_float "Optimistic_search.dups" args 0
  and wt = Search_args.get_float "Optimistic_search.dups" args 1 in
  let wt = (bound -. 1.) *. wt +. 1. in
  let search_interface =  make_interface sface wt in
    Limit.unwrap_sol6 unwrap_sol
      (Optimistic_framework.dups
	 search_interface
	 get_node
	 ordered_p
	 bound
	 better_p
	 ordered_f
	 set_pq_pos
	 set_f_pos
	 get_pq_pos
	 get_f_pos)


let delay_dups sface args =
  (** Performs optimistic search in domains with duplicates.  Duplicate states
      are only considered for expansion during the cleanup phase*)
  let bound = Search_args.get_float "Optimistic_search.delay_dups" args 0
  and wt = Search_args.get_float "Optimistic_search.delay_dups" args 1 in
  let wt = (bound -. 1.) *. wt +. 1. in
  let search_interface =  make_interface sface wt in
    Limit.unwrap_sol6 unwrap_sol
      (Optimistic_framework.delay
	 search_interface
	 get_node
	 ordered_p
	 bound
	 better_p
	 ordered_f
	 set_pq_pos
	 set_d_pos
	 set_f_pos
	 get_pq_pos
	 get_d_pos
	 get_f_pos)


(* EOF *)
