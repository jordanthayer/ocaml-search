(** A simple binary tree domain for debugging.

    @author eaburns
    @since 2010-01-11
*)

open Printf

type t = Nil | Node of t * float * t


let exp_cost max_depth =
  (fun depth -> function
     | 0 -> 0.
     | 1 -> 2. ** (float (max_depth - depth))
     | _ -> invalid_arg "exp_cost: bad child rank")


let exp_cost_plus max_depth =
  (fun depth -> function
     | 0 -> 1.
     | 1 -> (2. ** (float (max_depth - depth))) +. 1.
     | _ -> invalid_arg "exp_cost: bad child rank")


let make cost_fun max_depth =
  let rec build_tree cost depth =
    if depth > max_depth
    then Nil
    else Node (build_tree (cost +. (cost_fun depth 0)) (depth + 1),
	       cost,
	       build_tree (cost +. (cost_fun depth 1)) (depth + 1))
  in build_tree 0. 0


let rec print ch ?(indent="") = function
  | Nil -> fprintf ch "%s(Nil)\n" indent
  | Node (left, cost, right) ->
      fprintf ch  "%s(%f\n" indent cost;
      print ch ~indent:(indent ^ "  ") left;
      print ch ~indent:(indent ^ "  ") right;
      fprintf ch "%s)\n" indent


let node_cost = function
  | Nil -> invalid_arg "node_cost: Given a Nil node"
  | Node (_, c, _) -> c


let max_children = 2


let copy_state t = t


let is_better a b = (node_cost a) < (node_cost b)


let is_leaf = function
  | Nil -> invalid_arg "is_leaf: Given a Nil node"
  | Node (Nil, c, Nil) ->
      Verb.pr Verb.optional "Leaf=%f\n" c;
      true
  | _ -> false


let num_children t = if is_leaf t then 0 else 2


let nth_child t n =
  match t with
    | Nil -> invalid_arg "nth_child: Given a Nil node"
    | Node (l, _, r) -> if n = 0 then l else r


(*
module Tree = Blfs_fake_binary_tree;;
let t = Tree.make (Tree.exp_cost 4) 4;;
Tree.print stdout t;;
*)

let blfs_search max_depth =
  let cfun = exp_cost_plus max_depth in
  Blfs_search_fake_tree.search
    node_cost
    max_depth
    2
    cfun
    copy_state
    is_better
    is_leaf
    num_children
    nth_child
    (make cfun max_depth)


let blfs_lds_search max_depth =
  let cfun = exp_cost_plus max_depth in
    Blfs_lds_model.search
      node_cost
      max_depth
      2					(* max_children *)
      copy_state
      is_better
      is_leaf
      num_children
      nth_child
      (make cfun max_depth)
