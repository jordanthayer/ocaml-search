(* $Id: sep_test.ml,v 1.2 2004/01/11 01:24:42 ruml Exp ruml $

   tests for the separate action cost model
*)


open Separate


(***** a tree and a separate model made from the same known parameters ******)


let make_model_with params depth branching max_bins =
  (** returns a separate model that uses the given [params] *)
  let term = Array.make (depth + 1) 0
  and non_term = Array.make (depth + 1) 1 in
    term.(depth) <- 1;
    non_term.(depth) <- 0;
    { non_term = non_term;
      term = term;
      nchildren_counts = Array.make_matrix (depth + 1) (branching + 1) 0;
      total_counts = Array.make (depth + 1) 0;
      max_children = branching;
      pattern = Array.make (depth * branching) 0.;
      saved = Array.copy params;
      show_ex = ignore;
      get_params = (fun () -> params);
      max_bins = max_bins;
      best_completions = Array.make (depth + 1) infinity;
    }


let make_blfs_interface_using model =
  (** returns a blfs_interface based on the given separate model.  If
    the search tree itself is generated from this model, then the search
    should work perfectly.  *)
  let null_update _ _ = () in
    Blfs.make_interface
      (initial_data model)
      null_update
      null_update
      (map_children (fun _ -> 0) model)
      (fun _ -> failwith "called next_bound!")


let tree_from_model m =
  (** given a separate model, returns functions for exploring a search
    tree with costs based on the model *)
  (* state is cost so far, depth *)
  let initial = 0., 0
  and num_children n = m.max_children
  and leaf_p (c,d) =
    if d = (max_depth m)
    then (Verb.pr 5 "Got leaf w/ cost %f.\n" c; true)
    else false
  and better_p (a,_) (b,_) =
    a < b
  and get_child (so_far,d) i =
    Verb.pr 5 "Getting child %d of node at depth %d (cost so far %f).\n%!"
      i d so_far;
    assert (d < (max_depth m));
    if i >= m.max_children
      then failwith "error: no child!"
    else
      let params = m.get_params ()
      and base = d * m.max_children in
	(so_far +. params.(base + i), d + 1)
(*
	  (* currently unused. *)
  and get_leaf_cost (c,_) =
    c
*)
  in
    initial, num_children, leaf_p, better_p, get_child


let get_leaf_cost (c,_) = c


(******** testing with complete uniform tree exactly matching model ********)


let make_random_model depth branching max_bins =
  (** A random separate model.  Costs increase with child rank, but no
    systematic differences across depth. *)
  let params = Array.make (depth * branching) 0. in
    for d = 0 to depth - 1 do
      let base = d * branching in
      let prev = ref 0. in
	for i = base to base + branching - 1 do
	  let incr = 0.5 +. (Random.float 1.0) in
	  let value = !prev +. incr in
	    params.(i) <- value;
	    prev := value
	done
    done;
    make_model_with params depth branching max_bins


let info_from_model model =
  let r, n, l, b, g = tree_from_model model
  and i = make_blfs_interface_using model in

  let info = Info.make n g l b
	       (Fn.constantly1 false) Fn.identity
	       (Fn.constantly2 false) Fn.no_op2 None [Info.Never]
  in
    info, r, i


(******* is bound_for_nodes correct? ************)


let empirical_bound model desired_nodes =
  let f bound =
    let info, root, interface = info_from_model model in
    let _ = Blfs.bounded_dfs interface info bound root in
    let n, _, _, _ = (Info.stats info) in
      (* Wrutils.pr "%f gave %d.\n" bound n; *)
      float n
  in
  let bound, _ = bound_for_nodes model desired_nodes in
  let b,_ = Num_opt.invert f (float desired_nodes) bound (bound *. 1.1)
	      (bound *. 0.001) 100 in
    Wrutils.pr "Empirical bound of %f gives %f nodes.\n" b (f b)


let do_test_bound model desired_nodes verb =
  (** given a tree and model that match exactly, does cost bound give
    the desiref number of nodes? *)
  let bound, _ = Verb.with_level verb
		   (fun () ->
		      bound_for_nodes model desired_nodes)
  and info, root, interface = info_from_model model in
    Wrutils.pr "Got bound of %f for %d nodes.\n" bound desired_nodes;
    flush stdout;
    let complete = Blfs.bounded_dfs interface info bound root in
    let s = Info.stats info in
      Wrutils.pr "Results of using that bound:\n";
      Info.print_results stdout s false complete;
      let n, _, _, _ = s in
	Wrutils.pr "Error = %d (got %.2f of desired).\n"
	  (n - desired_nodes) (Math.div n desired_nodes)


let test_bound ?(max_bins = 100) ?(verb = 4) depth branching desired_nodes =
  Verb.with_level 1
    (fun () ->
       (* Random.self_init (); *)
       let model = make_random_model depth branching max_bins in
	 do_test_bound model desired_nodes verb
	   (*flush stdout;
	     empirical_bound model desired_nodes*)
    )


(**** is learning correct ******)

let test_learning model =
  (** given search tree derived from a separate model, can blfs learn
    it? *)
  false


(****** is entire algorithm reasonable? ******)


let test_blfs depth branching nodes verb =
  (** "Sep_test.test_blfs 100 2 1000000 3" can show if #nodes visited
    grows as desired *)
  Verb.with_level verb
    (fun () ->
       let max_bins = 100 in
       let model = make_random_model depth branching max_bins in
	 (* Wrutils.pr "True model is: ";
	    Vector.write_line stdout (model.get_params ()); *)
	 let root, num_children, leafp, betterp, get_child = tree_from_model model in
	   Separate.blfs_separate depth branching get_leaf_cost
	     ~max_bins:max_bins
	     ~num_probes:20
	     ~halt:([Info.Expanded nodes])
	     Fn.identity
	     betterp leafp num_children get_child root)


(************* leaves numbered sequentially *************)


(* a tree with unique costs at each leaf = numbering from left to right
*)


let make_unique_leaf_model depth branching max_bins =
  let params = Array.make (depth * branching) 0. in
    for d = 0 to (depth - 1) do
      let start = d * branching
      and exp = (depth - 1) - d in
      let factor = (float_of_int branching) ** (float_of_int exp) in
	for c = 0 to (branching - 1) do
	  params.(start + c) <- (float_of_int c) *. factor
	done
    done;
    make_model_with params depth branching max_bins


let test_threshold_unique_leaf_tree depth branching bins desired =
  let m = make_unique_leaf_model depth branching bins in
    Verb.pr Verb.debug "Model: ";
    Vector.write_line stdout (params m);
    let c, _ = bound_for_nodes m desired in
      Verb.pr Verb.debug "For %d nodes got bound of %f.\n" desired c


let do_test () =
  assert ((Array.length Sys.argv) = 5);
  let d = int_of_string Sys.argv.(1)
  and b = int_of_string Sys.argv.(2)
  and bins = int_of_string Sys.argv.(3)
  and des = int_of_string Sys.argv.(4) in
    test_threshold_unique_leaf_tree d b bins des


(***************** leaf cost = number of nodes to reach *********)


(* cost of arc = (number of nodes in all sibling subtrees to my left)
   + 1 for the node we're going to.  leftmost arc is always 1.  cost at
   leaf is the number of nodes explored to reach that leaf (assuming
   dfs-like ordering)
*)


let nodes_in_tree b d =
  (** number of nodes in a complete regular [b]-ary tree of [d] levels
    (including the root) *)
  (* 1 + b + b^2 + ... + b^k = (b^(k+1) - 1)/ (b-1) *)
  let b = float_of_int b
  and d = float_of_int (d - 1) in
    ((b ** (d +. 1.)) -. 1.) /. (b -. 1.)


let make_num_nodes_model depth branching max_bins =
  let params = Array.make (depth * branching) 0. in
    for d = 0 to (depth - 1) do
      let offset = d * branching
      and nodes_per_subtree = nodes_in_tree branching (depth - d) in
	for c = 0 to (branching - 1) do
	  (* one subtree for every sibling of the node we're going to,
	     plus one for that node *)
	  params.(offset + c) <- ((float_of_int c) *. nodes_per_subtree) +. 1.
	done
    done;
    make_model_with params depth branching max_bins


let test_threshold d b nodes_desired =
  let depth = d
  and branching = b
  and max_bins = 100 in
  let m = make_num_nodes_model depth branching max_bins in
    bound_for_nodes m nodes_desired


let test_num_node_behavior d b n =
  (** test that search behaves as expected in an actual search tree
    based on this model *)
  let m = make_num_nodes_model d b 100 in
    do_test_bound m n


(* EOF *)
