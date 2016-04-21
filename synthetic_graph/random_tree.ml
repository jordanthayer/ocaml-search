(** Tree generated from random seeds *)

type 'a node = {
  seed : int;             (* may not be unique *)
  data : 'a;              (* Data payload *)
}


type 'a tree = {
  root   : 'a node;
  expand : 'a node -> 'a node list;
}

let my_max = (2.**30.) -. 1.

(***************************************************************************)

let new_node bir data seed =
  (** takes a [data], a random [seed], and generates a 'a node *)
  { seed = seed;
    data = data;}


let wrap_expand bir exp_fn =
  (** Wraps the expand functios passed in to the tree so that it operates on
      nodes instead of on data 'a s. *)
  (fun node ->
     List.map
       (fun (data,seed,id) -> new_node bir data seed)
       (Park_miller.init node.seed;
	List.map (fun child_data -> child_data,
		    (truncate ((Park_miller.next()) *. my_max)),
		    (truncate ((Park_miller.next()) *. my_max)))
	  (exp_fn node)))


let make_tree iseed data expand =
  (** Generates a new 'a tree from
      [data], the 'a of root
      [expand], the expansion function for 'as
      [goal] a goal predicate on 'a nodes
      [seeder], which turns 'as into a seed for a rng *)
  let bir = ref Big_int.zero_big_int in
    {root = new_node bir data iseed;
     expand = (wrap_expand bir expand);}


(*********************** Various traversal functions ************************)

let postorder_traversal fn tree =
  let rec visit_subtree root =
    List.iter visit_subtree (tree.expand root);
    fn root in
    visit_subtree tree.root


let levelorder_traversal fn tree =
  (** Aka breadth first traversal *)
  let q = Queue.create () in
    Queue.push tree.root q;
    while not (Queue.is_empty q)
    do
      (let next = Queue.pop q in
	 fn next;
	 List.iter (fun n -> Queue.push n q) (tree.expand next))
    done


let inorder_traversal fn tree =
  let rec visit_subtree root =
    match (tree.expand root) with
	[] -> fn root
      | hd::tl -> (visit_subtree hd;
		   fn root;
		   List.iter visit_subtree tl) in
    visit_subtree tree.root


let preorder_traversal fn tree =
  let rec visit_subtree root =
    fn root;
    List.iter visit_subtree (tree.expand root) in
    visit_subtree tree.root

(************************ Printing *****************************************)

let base_node_to_string node =
  (* needs to be fixed in order to properly print key *)
  Wrutils.str "Seed: %i" node.seed

let node_to_string fn node =
  Wrutils.str "%s\n%s" (base_node_to_string node) (fn node.data)

let base_print_tree tree =
  levelorder_traversal (fun n -> Verb.pr Verb.always
			  "%s\n" (base_node_to_string n)) tree

let print_tree fn tree =
  levelorder_traversal (fun n -> Verb.pr Verb.always
			  "%s\n" (node_to_string fn n)) tree

(****** Testing trees ******)
let fixed_depth_b_tree iseed max_depth =
  make_tree
    iseed
    0
    (fun node ->
       if (node.data < max_depth)
       then let nd = node.data + 1 in
	 [nd;nd;]
       else [])

(* EOF *)
