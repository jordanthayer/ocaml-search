(** Some useful functions for remembering statistics about a tree for
    BLFS search.

    @author eaburns
    @since 2010-01-11
*)

type t = {
  terms : int array;
  (* Number of terminals seen at each depth. *)

  non_terms : int array;
  (* Number of non-terminals seen at each depth. *)

  nchildren_counts : int array array;
  (* Number of children seen for each rank at each depth. *)

  max_children : int;
  (* The maximum number of children an node will ever have. *)

  max_depth : int;
  (* The maximum depth in the search tree.  All nodes at [max_depth]
     will be leaf nodes. *)

}


let make ~max_children ~max_depth =
  (** [make max_children max_depth] makes a new structure for remembering
      staticsics about nodes in a tree. *)
  {
      terms = Array.make (max_depth + 1) 0;
      non_terms = Array.make (max_depth + 1) 0;
      nchildren_counts =
	Array.make_matrix (max_depth + 1) max_children 0;
      max_children = max_children;
      max_depth = max_depth;
  }


let clear t =
  (** [clear t] reinitializes the structure. *)
  Wrarray.fill_all t.terms 0;
  Wrarray.fill_all t.non_terms 0;
  Array.iter (fun ary -> Wrarray.fill_all ary 0) t.nchildren_counts


let branch_prob t depth  =
  (** [branch_prob t depth] gets the probability that child of node at
      level d is NOT a leaf *)
  let term = t.terms.(depth)
  and non_term = t.non_terms.(depth) in
  let total = non_term + term in
    if total = 0 then
      if depth = t.max_depth
      then 0.
      else 1. (* avg TODO - should make this an average *)
    else (float non_term) /. (float total)


let see_branch t ~depth ~nchildren =
  (** [see_branch t depth nchildren] make note of a branch that has
      been encountered. *)
  let nchildren_counts = t.nchildren_counts.(depth) in
    t.non_terms.(depth) <- t.non_terms.(depth) + 1;
    for c = 0 to nchildren - 1 do
      nchildren_counts.(c) <- nchildren_counts.(c) + 1;
    done


let see_terminal t depth =
  (** [see_terminal t depth] makes note of a terminal seen at a given
      depth. *)
  t.terms.(depth) <- t.terms.(depth) + 1


(** [p_with_child_rank m depth rank] gets the proportion of parents
    at depth [depth] with a child of rank [rank]. *)
let p_with_child_rank t ~depth ~rank =
  let total = t.non_terms.(depth) in
    if total = 0
    then 1.0				(* anything is possible. *)
    else (float t.nchildren_counts.(depth).(rank)) /. (float total)
