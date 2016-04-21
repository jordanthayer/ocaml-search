(** A functional simple temporal network.

    This is an implementation of the algorithms described in: Cesta
    and Oddi, TIME-96, "Gaining Efficiency and Flexibility in the
    Simple Temporal Problem."

    TODO:

    * Make constraint removal faster.

    * "Garbage time point collection."

    * Store UBP,LBP,UB,LB, and the queue used for propagation in the
    structure to prevent re-allocating each time propagation occurs.

    @author eaburns
    @since 2010-02-15
*)

module Node = struct
  (* A node in the network. *)
  type t = int

  let compare a b = if (a:int) < b then ~-1 else if b < a then 1 else 0

  let equal a b = (a:int) = b

  let hash a = a

end

type node = Node.t

module NodeMap = Map.Make(Node)

module NodeSet = Set.Make(Node)

module NodeHash = Hashtbl.Make(Node)

module Arcs = struct
  (* A mapping of weighted, directed arcs in the graph. *)

  type t = {
    in_arcs : (int NodeMap.t) NodeMap.t;
    out_arcs : (int NodeMap.t) NodeMap.t;
  }

  let do_iter u f map =
    (** [iter u f map] iterates [f] over all (u, v) tuples. *)
    try NodeMap.iter f (NodeMap.find u map) with Not_found -> ()

  let iter_in u f t = do_iter u f t.in_arcs
    (** [iter_in u t f] iterates over all arcs going in from [u]. *)

  let iter_out u f t = do_iter u f t.out_arcs
    (** [iter_in u t f] iterates over all arcs going out from [u]. *)


  let do_fold u f map init =
    (** [do_fold u f map init] folds [f] over the given map of
	maps. *)
    try NodeMap.fold f (NodeMap.find u map) init
    with Not_found -> init


  let fold_in u f t init = do_fold u f t.in_arcs init
    (** [fold_in u f t init] folds [f] over the incoming arcs for [u]
	in [t]. *)


  let map_find map n = try NodeMap.find n map with Not_found -> NodeMap.empty
    (** [map_find map n] finds the value for [n] in [map].  If there
	is no value bound yet, then just result in the empty map. *)


  let add t i j wt =
    (** [add t i j wt] adds an arc between [i] and [j] of weight
	[wt]. *)
    {
      in_arcs =
	NodeMap.add j (NodeMap.add i wt (map_find t.in_arcs j)) t.in_arcs;
      out_arcs =
	NodeMap.add i (NodeMap.add j wt (map_find t.out_arcs i)) t.out_arcs;
    }


  let remove t i j =
    (** [remove t i j] remove the arc between [i] -> [j]. *)
    {
      in_arcs =
	NodeMap.add j (NodeMap.remove i (map_find t.in_arcs j)) t.in_arcs;
      out_arcs =
	NodeMap.add i (NodeMap.remove j (map_find t.out_arcs i)) t.out_arcs;
    }

    let empty = { in_arcs = NodeMap.empty; out_arcs = NodeMap.empty }
      (** [empty] is an empty arc map. *)

end

module Bounds = struct
  (* Sets used for tracking the upper and lower bounds on the times of
     an event. *)

  let infinity = max_int

  let neg_infinity = min_int

  type t = {
    from_zero : int array;
    to_zero : int array;
  }


  let copy t =
    (** [copy t] copies the bounds. *)
    {
      to_zero = Array.copy t.to_zero;
      from_zero = Array.copy t.from_zero;
    }


  let iter f t =
    (** [iter f t] iterates [f] over the bounds for all nodes. *)
    for i = 0 to Array.length t.from_zero - 1 do
      f i t.to_zero.(i) t.from_zero.(i)
    done


  let initial nnodes =
    (** [initial nnodes] the initial bound setting. *)
    let t = {
      to_zero = Array.create (nnodes + 1) neg_infinity;
      from_zero = Array.create (nnodes + 1) infinity;
    } in
      t.to_zero.(0) <- 0;
      t.from_zero.(0) <- 0;
      t


  let add_nodes t num =
    (** [add_node t num] adds a fresh set of bounds for however many
	nodes. *)
    let n = Array.length t.to_zero in
    let t' = {
      to_zero = Array.create (n + num) neg_infinity;
      from_zero = Array.create (n + num) infinity;
    } in
      Array.blit t.to_zero 0 t'.to_zero 0 n;
      Array.blit t.from_zero 0 t'.from_zero 0 n;
      t'


  let find t i =
    (** [find t i] finds the bounds for [i]. *)
      t.to_zero.(i), t.from_zero.(i)


  let all t =
    (** [all t] gets a list of (id, lower, upper) bound tuples. *)
    let n = Array.length t.to_zero in
    let a = Array.create n (0, 0, 0) in
      for i = 0 to n - 1 do
	a.(i) <- (i, t.to_zero.(i), t.from_zero.(i));
      done;
      a
end

type t = {
  (* The actual STN directed graph. *)

  arcs : Arcs.t;
  (* The set of arcs. *)

  bounds : Bounds.t;
  (* The mapping between nodes and their bounds. *)

  pu : node NodeMap.t;
  (* The predecessor in the upper-bound tree. *)

  pl : node NodeMap.t;
  (* The predecessor in the lower-bound tree. *)

  nnodes : int;
  (* The number of nodes currently in the network. *)
}


type constrnt = node * node * int * int


let pair_id nnodes a b = a * nnodes + b
  (** [pair_id nnodes a b] get a unique integer ID from the given pair
      of nodes.  This is used to index arrays based on an edge. *)

(** {6 Creating} ****************************************)

let empty =
  {
    arcs = Arcs.empty;
    bounds = Bounds.initial 0;
    pu = NodeMap.empty;
    pl = NodeMap.empty;
    nnodes = 1;
  }


let add_nodes t num =
  let nnodes' = t.nnodes + num in
    t.nnodes, nnodes' - 1, { t with
			       bounds = Bounds.add_nodes t.bounds num;
			       nnodes = nnodes';
			   }


let create num =
  let _, _, t = add_nodes empty num in
    t


let add_node t =
  let n, _, t' = add_nodes t 1 in
    n, t'


let output outch t =
  Bounds.iter
    (fun u to_zero from_zero ->
       Printf.fprintf outch "%d (%d (%d), %d (%d)):\n"
	 u ~-to_zero
	 (try NodeMap.find u t.pl with Not_found -> ~-1)
	 from_zero
	 (try NodeMap.find u t.pu with Not_found -> ~-1);
       Arcs.iter_out u (fun v wt ->
			  Printf.fprintf outch "\t(%d, %d) = %d\n" u v wt)
	 t.arcs;)
    t.bounds


(** {6 Constraint Propogation} ****************************************)

exception Inconsistent

let rec propagate_lower seen arcs bounds pl u =
  let u_dist = bounds.Bounds.to_zero.(u) in
    Arcs.iter_in u (fun v wt ->
		      let dist = Math.subtract_ints u_dist wt
		      and v_dist = bounds.Bounds.to_zero.(v) in
			if v_dist < dist then begin
			  if bounds.Bounds.from_zero.(v) < dist
			  then raise Inconsistent;
			  bounds.Bounds.to_zero.(v) <- dist;
			  if Bitset.mem seen v then raise Inconsistent;
			  NodeHash.replace pl v u;
			  Bitset.insert seen v;
			  propagate_lower seen arcs bounds pl v;
			  Bitset.remove seen v;
			end) arcs


let rec propagate_upper seen arcs bounds pu u =
  let u_dist = bounds.Bounds.from_zero.(u) in
    Arcs.iter_out u (fun v wt ->
		       let dist = Math.add_ints u_dist wt
		       and v_dist = bounds.Bounds.from_zero.(v) in
			 if dist < v_dist then begin
			   if bounds.Bounds.to_zero.(v) > dist
			   then raise Inconsistent;
			   bounds.Bounds.from_zero.(v) <- dist;
			   if Bitset.mem seen v then raise Inconsistent;
			   NodeHash.replace pu v u;
			   Bitset.insert seen v;
			   propagate_upper seen arcs bounds pu v;
			   Bitset.remove seen v;
			 end) arcs


let propagate
    nnodes ?(seen=Bitset.create nnodes) arcs bounds pu_new pl_new u =
  (** [propagate nnodes ?seen arcs bounds pu_new pl_new u]
      performs propagation after an outgoing edge is added from [u]. *)
  propagate_upper seen arcs bounds pu_new u;
  propagate_lower seen arcs bounds pl_new u


let propagate_constraint nnodes arcs bounds pu pl i j =
  let pu_new = NodeHash.create 100
  and pl_new = NodeHash.create 100 in
  let seen = Bitset.create nnodes in
    propagate nnodes ~seen arcs bounds pu_new pl_new i;
    propagate nnodes ~seen arcs bounds pu_new pl_new j;
    (NodeHash.fold (fun src dst map -> NodeMap.add src dst map) pu_new pu,
     NodeHash.fold (fun src dst map -> NodeMap.add src dst map) pl_new pl)


let repropagate nnodes arcs bounds pu pl set =
  (** [repropagate nnodes arcs bounds pu pl set] performs
      re-propagation after a constraint has been deleted. *)
  let pu_new = NodeHash.create 100
  and pl_new = NodeHash.create 100 in
  let seen = Bitset.create nnodes in
    NodeSet.iter (propagate nnodes ~seen arcs bounds pu_new pl_new)set;
    (NodeHash.fold (fun src dst map -> NodeMap.add src dst map) pu_new pu,
     NodeHash.fold (fun src dst map -> NodeMap.add src dst map) pl_new pl)



(** {6 Dependency Tree} ****************************************)


let depend_preds succs arcs i =
  (** [depend_preds succs arcs i] gets the set of predecessors of [i]
      in the dependency tree defined by the [succs] map. *)
  Arcs.fold_in i (fun j _ s ->
		    try
		      if (NodeMap.find j succs) = i
		      then NodeSet.add j s
		      else s
		    with Not_found -> s)
    arcs NodeSet.empty


let rec depend_subtree succs arcs i =
  (** [depend_subtree succs arcs i] gets the dependency subtree rooted
      at [i]. *)
  let set = depend_preds succs arcs i in
    NodeSet.fold (fun j accum ->
		    NodeSet.union
		      (depend_subtree succs arcs j)
		      accum)
      set set


let in_depend_tree succs i j =
  (** [in_depend_tree succs i j] tests if the edge [i] -> [j]
      is in the dependency tree. *)
  try (NodeMap.find j succs) = i with Not_found -> false


(** {6 Testing} ****************************************)


let all_pairs_shortest_paths t =
  let cost = Array.make_matrix t.nnodes t.nnodes Bounds.infinity in
    for i = 0 to t.nnodes - 1 do
      Arcs.iter_out i (fun j vl -> cost.(i).(j) <- vl) t.arcs;
    done;
    for i = 0 to t.nnodes - 1 do
      cost.(i).(i) <- 0
    done;
    for k = 0 to t.nnodes - 1 do
      for i = 0 to t.nnodes - 1 do
	for j = 0 to t.nnodes - 1 do
	  cost.(i).(j) <-
	    Math.imin cost.(i).(j) (Math.add_ints cost.(i).(k) cost.(k).(j))
	done
      done
    done;
    for i = 0 to t.nnodes - 1 do
      for j = 0 to t.nnodes - 1 do
	cost.(i).(j) <- abs cost.(i).(j);
      done;
    done;
    cost


let check_bounds t =
  let costs = all_pairs_shortest_paths t in
  let bounds = Bounds.all t.bounds in
    Array.iter (fun (i, to_zero, from_zero) ->
		  if costs.(i).(0) > min_int then begin
		    (try assert (costs.(i).(0) = to_zero )
		     with _ ->
		       Verb.pr Verb.always "%d -> 0 = %d, expected %d\n%!"
			 i to_zero costs.(i).(0);
		       output stdout t;
		       assert false);
		  end;
		  (try assert (costs.(0).(i) = from_zero)
		   with _ ->
		     Verb.pr Verb.always "%d -> 0 = %d, expected %d\n%!"
		       i from_zero costs.(0).(i);
		     output stdout t;
		     assert false);)
      bounds

(** {6 Modifying} ****************************************)


let add_arcs arcs i j a b =
  (** [add_arcs t i j a b] adds the arcs for a single constraint. *)
    let arcs' =
      (* j - i <= b *)
      if b < Bounds.infinity
      then Arcs.add arcs i j b
      else arcs in
    let arcs'' =
      (* i - j <= -a *)
      if a > Bounds.neg_infinity
      then Arcs.add arcs' j i ~-a
      else arcs'
    in arcs''


let add_constraint t (i, j, a, b) =
  let arcs = add_arcs t.arcs i j a b in
  let bounds = Bounds.copy t.bounds in
  let pu, pl = propagate_constraint t.nnodes arcs bounds t.pu t.pl i j in
  let t' = { t with
	       arcs = arcs;
	       bounds = bounds;
	       pu = pu;
	       pl = pl;
	   }
  in
    if Verb.level Verb.never then check_bounds t';
    t'


let add_constraints t cs =
  let nnodes = t.nnodes in
  let bounds = Bounds.copy t.bounds in
  let arcs, pu, pl =
    List.fold_left (fun (arcs, pu, pl) (i, j, a, b) ->
		      let arcs' = add_arcs arcs i j a b in
		      let pu', pl' =
			propagate_constraint nnodes arcs' bounds pu pl i j
		      in arcs', pu', pl')
      (t.arcs, t.pu, t.pl)
      cs
  in
  let t' = { t with
	       arcs = arcs;
	       bounds = bounds;
	       pu = pu;
	       pl = pl;
	   }
  in
    if Verb.level Verb.never then check_bounds t';
    t'


let remove_constraint t i j =
  (** [remove_constraint t i j] removes a constraint between [i] and
      [j] in [t]. *)
  let pu = t.pu and pl = t.pl and arcs = t.arcs in
  let upper_subtree_i = depend_subtree pu arcs i
  and lower_subtree_i = depend_subtree pl arcs i
  and upper_subtree_j = depend_subtree pu arcs j
  and lower_subtree_j = depend_subtree pl arcs j in
  let v_m0 = (if in_depend_tree pu i j
	      then upper_subtree_i
	      else (if in_depend_tree pu j i
		    then upper_subtree_j
		    else NodeSet.empty))
  and v_m1 = (if in_depend_tree pl i j
	      then lower_subtree_i
	      else (if in_depend_tree pl j i
		    then lower_subtree_j
		    else NodeSet.empty)) in
  let v_m = NodeSet.union v_m0 v_m1 in
  let q =
    NodeSet.fold
      (fun i accum ->
	 Arcs.fold_in i (fun j _ s -> NodeSet.add j s) t.arcs accum)
      (NodeSet.add j (NodeSet.singleton i)) v_m
  in
  let arcs = Arcs.remove (Arcs.remove t.arcs i j) j i in
  let bounds = Bounds.copy t.bounds in
  let to_zero = bounds.Bounds.to_zero
  and from_zero = bounds.Bounds.from_zero in
    NodeSet.iter (fun i -> from_zero.(i) <- Bounds.infinity) upper_subtree_i;
    NodeSet.iter (fun i -> to_zero.(i) <- Bounds.infinity) lower_subtree_i;
    NodeSet.iter (fun i -> from_zero.(i) <- Bounds.infinity) upper_subtree_j;
    NodeSet.iter (fun i -> to_zero.(i) <- Bounds.infinity) lower_subtree_j;
    let pu, pl = repropagate t.nnodes arcs bounds t.pu t.pl q in
      { t with
	  arcs = arcs;
	  bounds = bounds;
	  pu = pu;
	  pl = pl;
      }


let create_with num cs =
  (** [create_with num cs] makes a new STN with [num] initial nodes
      and the constraints [cs]. *)
  let t = create num in
    add_constraints t cs


let all_bounds t = Bounds.all t.bounds


let bounds t n = Bounds.find t.bounds n

(** {6 Constraint Building} ****************************************)

let before i j a = (i, j, a, Bounds.infinity)


let after i j a = before j i a


let no_later_than i d = (0, i, Bounds.neg_infinity, d)


let not_earlier_than i s = (0, i, s, Bounds.infinity)


let in_window i s d = (0, i, s, d)


(* Scratch

   #use "use.ml";;
   let t = Simple_temp_net.create_with 4
   [ (0, 1, 10, 20);
   (1, 2, 60, max_int);
   (3, 4, 20, 30);
   (3, 2, 10, 20);
   (0, 4, 60, 70) ];;
   Simple_temp_net.output stdout t;;
*)
