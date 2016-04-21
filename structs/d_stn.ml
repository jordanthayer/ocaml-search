(** A destructive STN which can be used with backtracking searches.

    TODO:

    Constraint removal.

    @author eaburns
    @since 2010-03-06
*)

type node = int

type change = {
  arcs : node list;
  (* List of nodes which had arcs added to them.  For each node in
     this list, and undo operation should remove an element from the
     in_arcs and out_arcs list for the given node. *)

  prev_to_zero : (node * int) list;
  (* List of the previous distances to zero. *)

  prev_from_zero : (node * int) list;
  (* List of the previous distances from zero. *)
}

type t = {
  mutable in_arcs : (node * int) list array;
  mutable out_arcs : (node * int) list array;
  mutable from_zero : int array;
  mutable to_zero : int array;

  undo : change Stack.t;
}

type constrnt = node * node * int * int

let infinity = max_int

let neg_infinity = min_int

module NodeHash = Hashtbl.Make(struct
				 type t = node
				 let equal a b = (a:node) = b
				 let hash = Hashtbl.hash
			       end)

let create num =
  let n = num + 1 in
  {
    in_arcs = Array.create n [];
    out_arcs = Array.create n [];
    from_zero = Array.init n (fun i -> if i = 0 then 0 else infinity);
    to_zero = Array.init n (fun i -> if i = 0 then 0 else neg_infinity);
    undo = Stack.create ();
  }


let copy ?(copy_undo=false) t =
  {
    in_arcs = Array.copy t.in_arcs;
    out_arcs = Array.copy t.out_arcs;
    from_zero = Array.copy t.from_zero;
    to_zero = Array.copy t.to_zero;
    undo = if copy_undo then Stack.copy t.undo else Stack.create ();
  }


(** Gets the number of nodes in [t]. *)
let nnodes t = Array.length t.in_arcs


let add_nodes t num =
  let first = nnodes t in
  let last = first + num in
  let t' = { t with
	       in_arcs = Wrarray.extend t.in_arcs num [];
	       out_arcs = Wrarray.extend t.out_arcs num [];
	       from_zero = Wrarray.extend t.from_zero num infinity;
	       to_zero = Wrarray.extend t.to_zero num neg_infinity;
	   }
  in first, last, t'


let add_node t =
  let first, _, t' = add_nodes t 1 in
    first, t'


(** Updates distance to zero.  If it has not changed during this
    update then record the previous value in the given table. *)
let update_to_zero t prev_dists u b =
  if not (NodeHash.mem prev_dists u) then
    NodeHash.add prev_dists u t.to_zero.(u);
  t.to_zero.(u) <- b


(** Updates distance from zero.  If it has not changed during this
    update then record the previous value in the given table. *)
let update_from_zero t prev_dists u b =
  if not (NodeHash.mem prev_dists u) then
    NodeHash.add prev_dists u t.from_zero.(u);
  t.from_zero.(u) <- b


(** Adds arcs between [i] and [j].  Returns a list of nodes which
    actually had out arcs added. *)
let add_arcs t i j a b =
  let ij = (if b < infinity then begin
	      t.out_arcs.(i) <- (j, b) :: t.out_arcs.(i);
	      t.in_arcs.(j) <- (i, b) :: t.in_arcs.(j);
	      [i]
	    end else
	      []) in
  let ji = (if a > neg_infinity then begin
	      t.out_arcs.(j) <- (i, ~-a) :: t.out_arcs.(j);
	      t.in_arcs.(i) <- (j, ~-a) :: t.in_arcs.(i);
	      [j]
	    end else
	      [])
  in ij @ ji


(** Propagates the lower bounds. *)
let rec propagate_lower t seen update_dist u =
  let to_zero = t.to_zero and from_zero = t.from_zero in
  let u_dist = to_zero.(u) in
    List.iter (fun (v, wt) ->
		 let dist = Math.subtract_ints u_dist wt in
		 let v_dist = to_zero.(v) in
		   if dist > v_dist then begin
		     if from_zero.(v) < dist || Bitset.mem seen v then
		       raise Simple_temp_net.Inconsistent;
		     update_dist v dist;
		     Bitset.insert seen v;
		     propagate_lower t seen update_dist v;
		     Bitset.remove seen v;
		   end)
      t.in_arcs.(u)


(** Propagates the upper bounds. *)
let rec propagate_upper t seen update_dist u =
  let to_zero = t.to_zero and from_zero = t.from_zero in
  let u_dist = from_zero.(u) in
    List.iter (fun (v, wt) ->
		 let dist = Math.add_ints u_dist wt in
		 let v_dist = from_zero.(v) in
		   if dist < v_dist then begin
		     if to_zero.(v) > dist || Bitset.mem seen v then
		       raise Simple_temp_net.Inconsistent;
		     update_dist v dist;
		     Bitset.insert seen v;
		     propagate_upper t seen update_dist v;
		     Bitset.remove seen v;
		   end)
      t.out_arcs.(u)


(** Propagates the constraint added between [i] and [j].  The result
    is a tuple of lists of previous distance values (prev_to_zero *
    prev_from_zero). *)
let propagate_constraint t ~update_to ~update_from i j =
  let seen = Bitset.create (nnodes t) in
    propagate_lower t seen update_to i;
    propagate_upper t seen update_from i;
    propagate_lower t seen update_to j;
    propagate_upper t seen update_from j


(** Add the change to the stack and return the STN. *)
let change t ~prev_to_zero ~prev_from_zero out_arcs_added =
  let prev_to_zero =
    NodeHash.fold (fun n d l -> (n, d) :: l) prev_to_zero [] in
  let prev_from_zero =
    NodeHash.fold (fun n d l -> (n, d) :: l) prev_from_zero [] in
  let change = { arcs = out_arcs_added;
		 prev_to_zero = prev_to_zero;
		 prev_from_zero = prev_from_zero; }
  in
    Stack.push change t.undo;
    t

(** Reverts the changed arcs. *)
let undo_arcs t arcs =
  List.iter (fun i ->
	       match t.out_arcs.(i) with
		 | [] -> failwith "undo: No out arc to undo"
		 | (j,_) :: i_tl ->
		     begin match t.in_arcs.(j) with
		       | [] -> failwith "undo: No in arc to undo"
		       | (k,_) :: j_tl when k <> i ->
			   failwith "undo: in arc doesn't match out arc"
		       | (k,_) :: j_tl ->
			   t.out_arcs.(i) <- i_tl;
			   t.in_arcs.(j) <- j_tl;
		     end)
    arcs


(** Resets the bounds given a pair of hash tables with the previous
    bound settings.  This is used if a propagation shows an
    inconsistency to revert the changes. *)
let reset_bounds t ~prev_to ~prev_from =
  NodeHash.iter (fun i b -> t.to_zero.(i) <- b) prev_to;
  NodeHash.iter (fun i b -> t.from_zero.(i) <- b) prev_from


let add_constraint t (i, j, a, b) =
  let out_arcs_added = add_arcs t i j a b in
  let prev_to_zero = NodeHash.create 149
  and prev_from_zero = NodeHash.create 149 in
  let update_to = update_to_zero t prev_to_zero
  and update_from = update_from_zero t prev_from_zero in
    begin try
      propagate_constraint t ~update_to ~update_from i j;
    with Simple_temp_net.Inconsistent ->
      undo_arcs t out_arcs_added;
      reset_bounds t ~prev_to:prev_to_zero ~prev_from:prev_from_zero;
      raise Simple_temp_net.Inconsistent
    end;
    change t ~prev_to_zero ~prev_from_zero out_arcs_added


let add_constraints t cs =
  let prev_to_zero = NodeHash.create 149 in
  let prev_from_zero = NodeHash.create 149 in
  let update_to = update_to_zero t prev_to_zero in
  let update_from = update_from_zero t prev_from_zero in
  let out_arcs_added =
    List.fold_left (fun out_arcs (i, j, a, b) ->
		      let arcs = add_arcs t i j a b in
			begin try
			  propagate_constraint t ~update_to ~update_from i j
			with Simple_temp_net.Inconsistent ->
			  undo_arcs t arcs;
			  reset_bounds t
			    ~prev_to:prev_to_zero ~prev_from:prev_from_zero;
			  raise Simple_temp_net.Inconsistent
			end;
			arcs @ out_arcs)
      [] cs
  in change t ~prev_to_zero ~prev_from_zero out_arcs_added


let create_with num cs =
  let t = create num in
    add_constraints t cs


let all_bounds t =
  Array.mapi (fun i _ -> i, t.to_zero.(i), t.from_zero.(i)) t.to_zero


let bounds t i = t.to_zero.(i), t.from_zero.(i)


let output outch t =
  Array.iter
    (fun (u, to_zero, from_zero) ->
       Printf.fprintf outch "%d (%d (-1), %d (-1)):\n" u to_zero from_zero;
       List.iter (fun (v, wt) ->
		    Printf.fprintf outch "\t(%d, %d) = %d\n" u v wt)
	 t.out_arcs.(u))
    (all_bounds t)


let undo t =
  let change = Stack.pop t.undo in
    undo_arcs t change.arcs;
    List.iter (fun (i, d) -> t.to_zero.(i) <- d) change.prev_to_zero;
    List.iter (fun (i, d) -> t.from_zero.(i) <- d) change.prev_from_zero;
    t

(** {6 Constraint Building} ****************************************)

let before i j a = (i, j, a, infinity)


let after i j a = before j i a


let no_later_than i d = (0, i, neg_infinity, d)


let not_earlier_than i s = (0, i, s, infinity)


let in_window i s d = (0, i, s, d)

(** Macros for testing legality of stn **)

let legal_bounds (lower, upper) = lower < upper


let legal_task t id =
  legal_bounds (bounds t id)


let legal t =
  let return = ref true
  and i = ref 1
  and upper = ((Array.length t.to_zero) - 1)
  and check = legal_task t in
  while !return && !i < upper
  do
    (return := check !i;
     i := !i + 1)
  done;
    !return

