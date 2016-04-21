(** A set of functions and other information used by bounded depth
    searches.

    @author eaburns
    @since 2010-01-04
*)

type ('node, 'saved) t = {
  (* required *)
  num_children : 'node -> int;
  nth_child : 'node -> int -> 'node;
  is_leaf : 'node -> bool;
  is_better : 'node -> 'saved -> bool;
  copy_state : 'node -> 'saved;		(* required because of typing. *)

  (* optional with default *)
  max_depth : int;
  should_prune : 'saved -> 'node -> bool;
  is_optimal : 'saved -> bool;

  (* optional with fail *)
  leaf_cost : 'node -> float;
  child_costs : 'node -> float list;
}


let default num_children nth_child is_leaf is_better copy_state =
  (** [default num_children nth_child is_leaf is_better copy_state] makes an
      interface given the required functions.

      [max_depth] defaults to ~-1.
      [should_prune] defaults to the constantly false function.
      [is_optimal] defaults to the constantly false function.

      The remaining optional functions are set to return failure.

      NOTE: The [leaf_cost] function should almost definitely be
      implemented so that the "final sol cost" key can be computed.
  *)
  {
    (* required *)
    num_children = num_children;
    nth_child = nth_child;
    is_leaf = is_leaf;
    is_better = is_better;
    copy_state = copy_state;

    (* optional with a default value *)
    max_depth = ~-1;
    should_prune = Fn.constantly2 false;
    is_optimal = Fn.constantly1 false;

    (* optional with failure *)
    leaf_cost = (fun _ -> failwith "leaf_cost is not provided");
    child_costs = (fun _ -> failwith "child_costs is not provided");
  }
