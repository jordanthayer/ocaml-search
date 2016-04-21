(** A persistent simple temporal network.

    Don't actually use this: It is slower to use this than to just
    make copies of the D_stn.


    @author eaburns
    @since 2011-01-23
*)

open Lazy

module M = Map.Make(struct
		      type t = int
		      let compare (a:int) b =
			if a = b then 0 else if a < b then ~-1 else 1
		    end)

type 'a int_map = 'a M.t

type time = int

type node_id = int

type node = {
  out_arcs : (node_id * time) list;
  in_arcs : (node_id * time) list;

  (* Only mutated during propagation. *)
  mutable from0 : time;
  mutable to0 : time;
}

type t = {
  nodes : node int_map;			(* map from node IDs to nodes. *)
  nnodes : int;
}

type constrnt = node_id * node_id * time * time

let infinity = max_int

let neg_infinity = min_int


(** Create an empty node. *)
let empty_node () =
  {
    out_arcs = [];
    in_arcs = [];
    from0 = infinity;
    to0 = neg_infinity;
  }


(** Makes a new STN. *)
let create nnodes =
  let zero = { out_arcs = []; in_arcs = []; from0 = 0; to0 = 0; } in
    {
      nodes = M.add 0 zero M.empty;
      nnodes = nnodes + 1;
    }


(** Add new nodes to the STN. *)
let add_nodes net nnew =
  { net with nnodes = net.nnodes + nnew; }


(** Adds arcs to the STN.  The result is a new nodes map.*)
let add_arcs net i j a b =
  if b < infinity || a > neg_infinity then
    let n_i = try M.find i net.nodes with Not_found -> empty_node () in
    let n_j = try M.find j net.nodes with Not_found -> empty_node () in
    let i_out = n_i.out_arcs and i_in = n_i.in_arcs in
    let n_i' =
      { n_i with
	  out_arcs = if b < infinity then (j, b) :: i_out else i_out;
	  in_arcs = if a > neg_infinity then (j, ~-a) :: i_in else i_in }
    in
    let j_out = n_j.out_arcs and j_in = n_j.in_arcs in
    let n_j' =
      { n_j with
	  out_arcs = if a > neg_infinity then (i, ~-a) :: j_out else j_out;
	  in_arcs = if b < infinity then (i, b) :: j_in else j_in }
    in
      M.add i n_i' (M.add j n_j' net.nodes)
  else
    net.nodes


let rec propagate_lower nodes seen update_to ns u =
  let u = force ns.(u) in
  let u_dist = u.to0 in
    List.iter (fun (v_id, wt) ->
		 let v = force ns.(v_id) in
		 let dist = Math.subtract_ints u_dist wt in
		 let v_dist = v.to0 in
		   if dist > v_dist then begin
		     if v.from0 < dist || Bitset.mem seen v_id then
		       raise Simple_temp_net.Inconsistent;
		     update_to v_id dist;
		     Bitset.insert seen v_id;
		     propagate_lower nodes seen update_to ns v_id;
		     Bitset.remove seen v_id;
		   end)
      u.in_arcs



let rec propagate_upper nodes seen update_from ns u =
  let u = force ns.(u) in
  let u_dist = u.from0 in
    List.iter (fun (v_id, wt) ->
		 let v = force ns.(v_id) in
		 let dist = Math.add_ints u_dist wt in
		 let v_dist = v.from0 in
		   if dist < v_dist then begin
		     if v.to0 > dist || Bitset.mem seen v_id then
		       raise Simple_temp_net.Inconsistent;
		     update_from v_id dist;
		     Bitset.insert seen v_id;
		     propagate_upper nodes seen update_from ns v_id;
		     Bitset.remove seen v_id;
		   end)
      u.out_arcs


(** Propagates the constraints. *)
let propagate nodes seen ~update_to ~update_from ns i j =
  propagate_lower nodes seen update_to ns i;
  propagate_upper nodes seen update_from ns i;
  propagate_lower nodes seen update_to ns j;
  propagate_upper nodes seen update_from ns j


(** Makes a copy of a node. *)
let copy_node n =
  { n with in_arcs = n.in_arcs }


(** Get functions for modifying node distances to/from 0. *)
let modify_funs nodes nnodes =
  let ns =
    Array.init nnodes
      (fun i -> lazy (try M.find i nodes with Not_found -> empty_node ())) in
  let updated = Bitset.create nnodes in
  let updates = ref [] in
  let node i =
    if Bitset.mem updated i then
      force ns.(i)
    else begin
      let n = copy_node (force ns.(i)) in
	Bitset.insert updated i;
	updates := i :: !updates;
	ns.(i) <- lazy_from_val n;
	n
    end
  in
  let update_to i v = (node i).to0 <- v in
  let update_from i v = (node i).from0 <- v in
  let get_updates () = !updates in
    update_to, update_from, get_updates, ns


(** Adds a constraint to the network. *)
let add_constraint net (i, j, a, b) =
  let nnodes = net.nnodes and nodes' = add_arcs net i j a b in
  let update_to, update_from, get_updates, ns = modify_funs nodes' nnodes in
  let seen = Bitset.create net.nnodes in
    propagate nodes' seen ~update_to ~update_from ns i j;
    let nodes'' =
      List.fold_left (fun m i -> M.add i (force ns.(i)) m)
	nodes' (get_updates ())
    in
      { net with nodes = nodes'' }


(** Get the bounds for the node. *)
let bounds net i =
  try
    let n = M.find i net.nodes in
      n.to0, n.from0
  with Not_found ->
    neg_infinity, infinity
