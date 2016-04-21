(** A fixed-depth tree with uniform branching but random heuristic
    scores associated with nodes and random leaf-costs.

    In the 'perfect view' of the tree, leaves are sorted by their
    costs from best (left) to worst (right).  The cost of a leaf is
    some function of cost of the previous leaf in the ranking.  The
    perfect cost value associated with each edge is the cost of the
    best leaf beneath the given edge.

    For the 'imperfect view', the cost of each edge is some function
    of the true cost.

    @author eaburns
    @since 2010-12-13
*)

open Format


type t =
  | L of float
  | N of (t * float) array
      (* (kids, kid costs) *)


let normal_tree ~branching ~depth =
  let next = ref 0 in
  let rec tree d =
    if d = depth then
      let rank = float !next in
	incr next;
	L rank, rank
    else
      let kids = Array.init branching (fun _ -> tree (d + 1)) in
      let best = snd (kids.(0)) in
	N kids, best
  in
  let root, _ = tree 0 in root


let make leaf_cost edge_cost ~branching ~depth =
  (** [make leaf_cost edge_cost ~branching ~depth] makes a random tree
      with uniform branching of [branching] and depth [depth].

      [leaf_cost] is a function that is given the rank and the
      previously ranked leaf cost and returns the cost of the next
      leaf.

      [edge_cost] is given the cost of the best leaf beneath the
      child, the best-leaf costs of all of its siblings and returns
      the cost of the given edge. *)
  let prev = ref nan in
  let rec add_error = function
    | L rank ->
	let cost = leaf_cost rank !prev in
	  prev := cost;
	  L cost
    | N kids ->
	let sibs = Array.map snd kids in
	let kids' =
	  Array.map (fun (k, c) ->
		       let cost = edge_cost c sibs in
			 add_error k, cost)
	    kids
	in N kids'
  in
    add_error (normal_tree ~branching ~depth)


let make_simple ~branching ~depth =
  (** [make_simple ~branching ~depth] makes a simple tree where the
      leaf costs are equal to their rank and the edge cost is the same
      as the best ranked leaf below the child node. *)
  let leaf_cost r _ = r in
  let edge_cost c _ = c in
    make leaf_cost edge_cost ~branching ~depth


let make_random sigma ~branching ~depth =
  (** [make_random sigma ~branching ~depth] makes a simple tree with
      noise added to the edge costs.  Leaf costs are normalized by
      dividing by the best sibling cost and then gaussian noise is
      added.

      [sigma] is the standard devian of the gaussian noise. *)
  let noise x = Wrrandom.log_normal 1. sigma in
  let leaf_cost r _ = r in
  let edge_cost c siblings =
    let best = siblings.(0) +. 1. in
    let norm = c /. best in
      norm +. noise norm
  in
    make leaf_cost edge_cost ~branching ~depth


let fprint f root =
  (** [fprint f root] prints the tree in a "human-readable" format. *)
  let rec pr_tree f = function
    | N kids ->
	fprintf f "N(@[";
	pr_kids kids;
	fprintf f "@])";
    | L cost ->
	fprintf f "L(%g)" cost
  and pr_kids kids =
    Array.iteri (fun i (k, c) ->
		   fprintf f "%g, %a" c pr_tree k;
		   if i < (Array.length kids) - 1 then fprintf f "; @\n";)
      kids;
  in fprintf f "%a@?\n" pr_tree root
