(** 2-way number partitioning of arbitrary percision integers.

    An implementation of CKK 2-way number partitioning using a skew
    heap instead of an ordered list... it seems to be a bit slower
    than the list implementation.

    @author eaburns
    @since 2009-12-28
*)

open Printf
open Big_int

module Num = struct

  type t =
    | Leaf of big_int
    | Sum of t * big_int * t
	(* left, value, right *)
    | Diff of t * big_int * t
	(* left, value, right *)

  let value = function
      (** [value n] gets the value of a number. *)
    | Leaf x -> x
    | Sum (_, x, _) -> x
    | Diff (_, x, _) -> x


  let rec to_string = function
    | Leaf x -> sprintf "(Leaf %s)" (string_of_big_int x)
    | Sum (l, x, r) ->
	sprintf "(Sum %s, %s, %s)"
	  (to_string l) (string_of_big_int x) (to_string r)
    | Diff (l, x, r) -> sprintf "(Diff %s, %s, %s)"
	(to_string l) (string_of_big_int x) (to_string r)


  let print ch n =
    fprintf ch "%s\n" (to_string n)


  let rec compare a b = compare_big_int (value a) (value b)
    (** [compare a b] compares two numbers. *)

  let before a b = ge_big_int (value a) (value b)
    (** [before a b] tests if [a] comes before [b] in the ordering. *)

end

module NumHeap = Skew_heap.Make(Num)

type t = {
  nums : NumHeap.t;
  (* A sorted list of the numbers or differences left. *)

  sum : big_int;
  (* The sum of the numbers. *)
}


let make_initial inst =
  (** [make_initial inst] makes an initial state given an instance. *)
  { nums =
      List.fold_left
	NumHeap.insert
	NumHeap.empty
	(List.map (fun i -> Num.Leaf i) (Array.to_list inst));
    sum = Array.fold_left add_big_int zero_big_int inst; }


let max_depth inst = Array.length inst
  (** [max_depth inst] is the maximum depth that can be used to solve
      the given instance. *)


let is_leaf n = NumHeap.is_singleton n.nums
  (** [is_leaf n] tests if the given node is a leaf. *)


let partitions n =
  (** [partitions n] gets the partition given a leaf. *)
(*
  if Verb.level Verb.debug
  then begin
    Verb.pr Verb.always "\n\nsolution:\n";
    Num.print stdout (NumHeap.peek_min n.nums);
  end;
*)
  let rec do_partitions this that = function
    | Num.Leaf v -> v :: this, that
    | Num.Sum (l, _, r) ->
	let this, that = do_partitions this that l in
	  do_partitions this that r
    | Num.Diff (l, _, r) ->
	let this, that = do_partitions this that l in
	let that, this = do_partitions that this r in
	  this, that
  in do_partitions [] [] (NumHeap.peek_min n.nums)


let leaf_value n =
  (** [leaf_value n] get the value of a leaf node. *)
  assert (is_leaf n);
  Num.value (NumHeap.peek_min n.nums)


let leaf_cost n =
  (** [leaf_cost n] gets the cost of a lead.  This is the log10
      (difference + 1).  (We add one so that we never log10 a 0). *)
  log10 ((float_of_big_int (leaf_value n)) +. 1.)


let print_partitions = Two_partition.make_print_partitions partitions


let make_is_optimal = Two_partition.make_is_optimal leaf_value


let is_better = Two_partition.make_is_better leaf_value


let num_children n = if is_leaf n then 0 else 2
  (** [num_children n] gets the number of children of the node. *)


let two_largest n =
  (** [two_largest n] gets the two largest ints from [n]. *)
  let h', one  = NumHeap.extract_min n.nums in
  let h'', two = NumHeap.extract_min h' in
    one, two, h''


let diff_two n =
  (** [diff_two n] removes the two largest elements from the set and
      adds their difference to it.  The result is the new state. *)
  let left, right, rest = two_largest n in
  let right_vl = Num.value right in
  let diff = sub_big_int (Num.value left) right_vl in
  let n' = { nums = NumHeap.insert rest (Num.Diff (left, diff, right));
	     sum = sub_big_int n.sum (add_big_int right_vl right_vl); }
  in
(*
    if Verb.level Verb.debug
    then begin
      Verb.pr Verb.always "parent\n";
      NumHeap.iter (Num.print stdout) n.nums;
      Verb.pr Verb.always "child (diff)\n";
      NumHeap.iter (Num.print stdout) n'.nums;
      Verb.pr Verb.always "\n\n\n";
    end;
*)
    n'


let sum_two n =
  (** [sum_two n] removes the two largest elements from the set and
      adds their sum to it.  The result is the new state. *)
  let left, right, rest = two_largest n in
  let sum = add_big_int (Num.value left) (Num.value right) in
  let n' = { n with
	       nums = NumHeap.push_min rest (Num.Sum (left, sum, right)) }
  in
(*
    if Verb.level Verb.debug
    then begin
      Verb.pr Verb.always "parent\n";
      NumHeap.iter (Num.print stdout) n.nums;
      Verb.pr Verb.always "child (sum %s + %s)\n"
	(string_of_big_int (Num.value left))
	(string_of_big_int (Num.value right));
      NumHeap.iter (Num.print stdout) n'.nums;
      Verb.pr Verb.always "\n\n\n\n";
    end;
*)
    n'


let propogate_diffs n =
  (** [propogate_diffs n] if the largest number in the set is larger
      than the sum of the remaining then the rest of the state is
      differenced. *)
  let rec do_propogate n =
    assert (not (NumHeap.is_empty n.nums));
    if NumHeap.is_singleton n.nums
    then n
    else do_propogate (diff_two n)
  in
  let max_vl = Num.value (NumHeap.peek_min n.nums) in
    if ge_big_int max_vl (sub_big_int n.sum max_vl)
    then do_propogate n
    else n


let nth_child n i =
  (** [nth_child n i] gets the [i]th child of node [n]. *)
  assert (not (is_leaf n));
  if i = 0
  then propogate_diffs (diff_two n)
  else propogate_diffs (sum_two n)


let make_interface inst =
  (** [make_interface inst] makes the interface to the domain for the
      various search algorithms. *)
  let module I = Bounded_depth_interface in
    (make_initial inst,
     { (I.default num_children nth_child is_leaf is_better Fn.identity)
       with
	 I.max_depth = max_depth inst;
	 I.is_optimal = make_is_optimal inst;
	 I.leaf_cost = leaf_cost;
     },
     partitions)


let dump_leaves inst outch =
  (** [dump_leaves inst outch] dumps the instance to the given output
      file.  This does a complete traversal of the tree outputting
      info on each leaf seen. *)
  let max_depth = max_depth inst in
  let leaf_no = ref 0 in
  let rec visit trajectory node =
    if is_leaf node
    then begin
      let path = Array.make max_depth ~-1 in
      let tlen = List.length trajectory in
	Array.blit (Array.of_list (List.rev trajectory)) 0 path 0 tlen;
	fprintf outch "%d,\t%f,\t" !leaf_no (leaf_cost node);
	for d = 0 to max_depth - 1 do
	  fprintf outch "%d" path.(d);
	  if d = max_depth - 1
	  then fprintf outch "\n"
	  else fprintf outch ",\t"
	done;
	incr leaf_no;
	Verb.pr Verb.often "%d leaves seen\n" !leaf_no;
    end else begin
      let nchildren = num_children node in
	for n = 0 to nchildren - 1 do
	  visit (n :: trajectory) (nth_child node n)
	done
    end
  in
    fprintf outch "\"leaf number\",\t\"log10 cost\",\t";
    for d = 0 to max_depth - 1 do
      fprintf outch "\"branch %d\"" d;
      if d = max_depth - 1
      then fprintf outch "\n"
      else fprintf outch ",\t"
    done;
    visit [] (make_initial inst)
