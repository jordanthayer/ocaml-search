(* search spaces for testing

*)


(************* simple regular fixed-depth tree with one goal ***********)


type node = {
  depth : int;
  nodes_to_left : int;
  id : int;
  max_depth : int;
  branching : int;
}


let sum_exp base start finish =
  let sum = ref 0 in
    for i = start to finish do
      sum := !sum + (Math.int_exp base i)
    done;
    !sum


let goal_id n =
  (** goal is 2nd leaf in the rightmost subtree. *)
  (* num_nodes in other subtrees of root *)
  ((sum_exp n.branching 0 (n.max_depth - 1)) * (n.branching - 1)) +
  (* nodes on leftmost branch of this subtree, including root *)
  n.max_depth + 1


(******** useful stuff *******)


let make_initial depth branching =
  assert (depth >= 0);
  assert (branching >= 1);
  Verb.pr Verb.debug "Making root of test tree of depth %d and branching factor %d.\n" depth branching;
  { depth = 0;
    nodes_to_left = 0;
    id = 0;
    max_depth = depth;
    branching = branching }


let leaf_p n =
  Verb.pr Verb.debug "Checking if %d is a leaf.\n" n.id;
  n.depth = n.max_depth


let num_children n =
  n.branching


let get_child n i =
  Verb.pr Verb.debug "Checking for child %d of %d.\n" i n.id;
  assert (n.depth < n.max_depth);
  assert (i >= 0);
  if i < n.branching then
    let to_left = (n.nodes_to_left * n.branching) + i in
    let id = (sum_exp n.branching 0 n.depth) + to_left in
      Wrio.pause ();
      Verb.pr Verb.debug "Expanding %d to get %d.\n" n.id id;
      { n with
	  depth = n.depth + 1;
	  nodes_to_left = to_left;
	  id = id; }
  else
    failwith (Wrutils.str "%d has no child %d.\n" n.id i)


let optimal_p n =
  Verb.pr Verb.debug "Checking if %d is optimal.\n" n.id;
  n.id = (goal_id n)


let better_p a b =
  Verb.pr Verb.debug "Checking if %d is better than %d.\n" a.id b.id;
  let g = goal_id a in
   (abs (a.id - g)) < (abs (b.id - g))


(* EOF *)
