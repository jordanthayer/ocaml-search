(** Bounded depth search in a TSP.  This is the (worse) nearest
    neighbor representation.

    @author eaburns
    @since 2010-01-05
*)

open Printf
open Tsp
open Tsp_instances
open Fn


type bounded_depth_node = {
  loc : int;
  (** The current city.  This city will be marked as visited in the
      children. *)

  part_tour : int list;
  (** The visit order of the current partial tour *)

  rem_edges : edge list;
  (** The remaining edges. *)

  rem_cities : (int * float) array;
  (** The remaining cities to visit (in sorted order by their f
      value) *)

  cost : float;
  (** The cost of this partial tour. *)

  f : float;
  (** The f value of a node is its cost and the cost on the MST of the
      remaining cities. *)

  depth : int;
  (** Depth of this node *)
}

(************************************************************)
(** {6 Minimal spanning tree cost} *)

let mst_cost ncities edges =
  let rec get_mst_cost ?(cost=0.) sets = function
    | [] -> cost
    | e :: es ->
	if Dufi.same_p sets e.u e.v then
	  get_mst_cost ~cost sets es
	else begin (* e is a member of the spanning tree *)
	  Dufi.join sets e.u e.v;
	  get_mst_cost ~cost:(cost +. e.weight) sets es
	end
  in
  let sets = Dufi.make ncities in
    for i = 0 to ncities - 1 do Dufi.init sets i done;
    get_mst_cost sets edges


(** Filters out all edges to or from city [c]. *)
let filter_edges edges c =
  List.filter (fun e -> e.u <> c && e.v <> c) edges


(************************************************************)
(** {6 Search node} *)


(** Gets the initial cities for the given problem along with their
    distances from the start city. *)
let init_cities prob h =
  let dists = prob.dists in
  let ncities = Array.length dists in
  let cities = ref [] in
    for i = 0 to ncities - 1 do
      if i <> start_city && i <> delayed_city then
	let f = dists.(start_city).(i) +. h in
	  cities := (i, f) :: !cities
    done;
    let cmp (_, a) (_, b) = Math.fcompare a b in
      Array.of_list (List.sort cmp !cities)


(** Gets the initial node. *)
let make_initial prob =
  let ncities = Array.length prob.dists in
  let edges = make_edges prob.dists prob.symmetric in
  let rem_edges = filter_edges edges start_city in
  let kidh = mst_cost ncities rem_edges in
  let remaining = init_cities prob kidh in
    {
      loc = start_city;
      part_tour = [ start_city ];
      rem_edges = rem_edges;
      rem_cities = remaining;
      cost = 0.;
      f = mst_cost ncities edges;
      depth = 0;
    }


(** Gets the remaining cities after moving to [loc]. *)
let rem_cities dist h loc cities =
  let cities' =
    Array.fold_left (fun cs (c, _) ->
		       if c <> loc then (c, dist.(c) +. h) :: cs else cs)
      [] cities
  in
  let cmp (_, a) (_, b) = Math.fcompare a b in
    Array.of_list (List.sort cmp cities')


(** Makes a function to gets the [nth] child of a given node. *)
let make_nth_child prob =
  let dists = prob.dists in
  let ncities = Array.length dists in
  let delay_depth = delay_layer prob.dists prob.symmetric in
    (fun node n ->
       let loc = node.loc in
       let loc', f = node.rem_cities.(n) in
       let depth' = node.depth + 1 in
       let rem_edges = filter_edges node.rem_edges loc' in
       let rem_mst_cost = mst_cost ncities rem_edges in
       let cities =
	 if depth' = delay_depth then
	   Wrarray.extend node.rem_cities 1 (delayed_city, nan)
	 else
	   node.rem_cities in
       let rem_cities = rem_cities dists.(loc') rem_mst_cost loc' cities in
       let move_cost = dists.(loc).(loc') in
	 if Array.length rem_cities = 0 then
	   let cost = node.cost +. move_cost +. dists.(loc').(start_city) in
	     {
	       loc = start_city;
	       part_tour = start_city :: loc' :: node.part_tour;
	       rem_edges = [];
	       rem_cities = [||];
	       cost = cost;
	       f = cost;
	       depth = depth';
	     }
	 else
	   {
	     loc = loc';
	     part_tour = loc' :: node.part_tour;
	     rem_edges = rem_edges;
	     rem_cities = rem_cities;
	     cost = node.cost +. move_cost;
	     f = f;
	     depth = depth';
	   })


(** Gets the number of children for a given node. *)
let num_children node =
  Array.length node.rem_cities


(** Tests if the node is a leaf. *)
let is_leaf node =
  let rem = Array.length node.rem_cities in
    assert (rem > 0 || node.loc = start_city);
    rem = 0


(** Tests if the [node] is better than the incumbent. *)
let is_better node incumbent =
  assert (is_leaf node);
  node.cost < incumbent.cost


(** Copies a state into an incumbent. *)
let copy_state x = x


(** Gets the maximum depth given the problem. *)
let max_depth prob =
  (* -1 because we start with a city already defined... the
     start_city. *)
  (Array.length prob.dists) - 1


(** Tests if the given node should be pruned. *)
let should_prune incumbent node =
  node.f >= incumbent.cost


(** Tests if the given incumbent is optimal. *)
let is_optimal _ = false


(** Gets the cost of a leaf.  *)
let leaf_cost node = node.cost


(** Gets the cost of each child. *)
let child_costs node =
  Array.to_list (Array.map snd node.rem_cities)


let make_interface p =
  (** [make_interface p] makes a bounded depth interface for [p]. *)
  let module I = Bounded_depth_interface in
    (make_initial p,
     { (I.default
	  num_children (make_nth_child p)
	  is_leaf is_better Fn.identity)
       with
	 I.max_depth = max_depth p;
	 I.should_prune = should_prune;
	 I.leaf_cost = leaf_cost;
	 I.child_costs = child_costs;
     })
