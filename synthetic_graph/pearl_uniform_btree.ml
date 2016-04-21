(** The uniform binary tree describe on page 150 of Heuristics by Pearl *)

(* The Pproblem: Find a cheapest path leading from the root of a tree to any of
   its leaves.  The tree is a uniform binary tree of height N.  Each branch
   independantly may have a cost of 1 or 0 with probabilities p or 1 - p
   respectively. *)

type data = {
  depth : int;
  cost : int;
}


type pearl_tree = {
  p : float;
  n : int;
  seed : int;
  t : data Random_tree.tree;
}


(****************************************************************************)

let make_expand p n =
  (** generates an expand function for a pearl tree, given the appropriate
      p and n.  We don't feed in the pearl tree because we need the
      underlying random tree in order to build the pearl tree *)
  (fun node -> (* Random_tree.wrap_expand does the seed init for us! *)
     let data = node.Random_tree.data in
       if n > data.depth
       then (let nd = data.depth + 1 in
	     let next () = (if Math.true_with_prob p
			    then {depth = nd; cost = data.cost + 1}
			    else {depth = nd; cost = data.cost}) in
	       [next();next()])
       else [])


let make_goal t =
  (** generates a goal predicate for the given pearl tree *)
  (fun node -> node.Random_tree.data.depth == t.n)


let make_tree p n seed =
  (** generates a new pearl tree from a probability, maximum depth, and a seed.
      seed comes last so that we can generate many similar trees from a
      list or an array of seeds *)
  {p = p;
   n = n;
   seed = seed;
   t =  Random_tree.make_tree seed {depth = 0; cost = 0} (make_expand p n)}


let admiss_heuristic node = 0.
  (** since edges can have no cost, this is the only admissible heuristic
      in this domain *)


let make_reasonable_heuristic t =
  (** heuristic value is the expected cost from here to a leaf. *)
  (fun n ->
     t.p *. (float_of_int (t.n - n.depth)))


(************************* Printing functions ********************************)

let data_to_string d =
  Wrutils.str "depth: %i\t cost: %i\n" d.depth d.cost

let print_tree t = Random_tree.print_tree data_to_string t.t

(******************************* Default Interfaces **************************)

(* EOF *)
