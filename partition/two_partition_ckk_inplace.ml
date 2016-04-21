(** A CKK implementation of 2-way number partitioning using inplace
    modification.

    @author eaburns
    @since 2009-12-29
*)

open Big_int
open Printf

type t = {
  (* The single state that is modified by the search.  The search
     actually gets simple integers as its states, these represent the
     depths in the undo stack. *)

  undo : num Stack.t;
  (* The undo stack of numbers. *)

  mutable depth : int;
  (* The depth of the current stack. *)

  nums : num Dpq.t;
  (* The queue of numbers. *)

  mutable sum : big_int;
  (* The sum of the numbers in the queue. *)
}

and num = {
  value : big_int;
  (* The value of this entry. *)

  why : operation;
  (* How did this number come to be. *)

  left : num;
  (* If prominence <> Leaf this is the first number of the add or
     subtract.*)

  right : num;
  (* If prominence <> Leaf this is the second number of the add or
     subtract.*)

  mutable index : int;
  (* Index into the PQ *)
}

and operation = Invalid | Leaf | Sum | Diff


let string_of_why = function
  | Invalid -> "Invalid"
  | Leaf -> "Leaf"
  | Sum -> "Sum"
  | Diff -> "Diff"


let rec no_number =
  (** [no_number] an empty number that can be used to initialize
      various structures. *)
  {
    value = zero_big_int;
    why = Invalid;
    left = no_number;
    right = no_number;
    index = Dpq.no_position;
  }


let num_print ch n =
  (** [num_print ch n] prints a num to the given channel. *)
  fprintf ch "value: %s\n" (string_of_big_int n.value);
  fprintf ch "why:   %s\n" (string_of_why n.why);
  if n.left.why <> Invalid
  then fprintf ch "left:  %s\n" (string_of_big_int n.left.value);
  if n.right.why <> Invalid
  then fprintf ch "right: %s\n" (string_of_big_int n.right.value);
  fprintf ch "index: %d\n" n.index


let num_pred a b = gt_big_int a.value b.value
  (** [num_pred a b] tests if [a] is a predecessor of [b] in the
      heap. *)


let num_set_index n i =
  (** [num_set_index n i] sets the index field of [n] to [i]. *)
  if Verb.level Verb.debug
  then begin
    num_print stdout n;
    fprintf stdout "index to: %d\n" i
  end;
  n.index <- i


let make inst =
  (** [make inst] makes a structure for CKK number partitioning from
      an instance. *)
  let t = {
    undo = Stack.create ();
    depth = 0;
    nums = Dpq.create num_pred num_set_index 100 no_number;
    sum = Array.fold_left add_big_int zero_big_int inst;
  } in
    Array.iter (fun i ->
		  let num = { value = i;
			      why = Leaf;
			      left = no_number;
			      right = no_number;
			      index = Dpq.no_position; }
		  in Dpq.insert t.nums num)
      inst;
    t


let max_depth inst = Array.length inst
  (** [max_depth inst] is the maximum depth that can be used to solve
      the given instance. *)


let diff_two t =
  (** [diff_two t] differences the two largest numbers.  The result is
      an integer that represents the new depth in the undo stack. *)
  let left = Dpq.extract_first t.nums
  and right = Dpq.extract_first t.nums in
  let diff = sub_big_int left.value right.value in
  let num = {
    value = diff;
    why = Diff;
    left = left;
    right = right;
    index = Dpq.no_position;
  } in
    Verb.pr Verb.debug "(%02d) Diffing %s and %s = %s\n"
      (t.depth + 1)
      (string_of_big_int left.value)
      (string_of_big_int right.value)
      (string_of_big_int diff);
    Stack.push num t.undo;
    Dpq.insert t.nums num;
    t.depth <- t.depth + 1;
    t.sum <- sub_big_int t.sum (add_big_int right.value right.value);
    t.depth


let sum_two t =
  (** [sum_two t] sum the two largest numbers.
      The result is an integer that represents the new depth in the
      undo stack. *)
  let left = Dpq.extract_first t.nums
  and right = Dpq.extract_first t.nums in
  let sum = add_big_int left.value right.value in
  let num = {
    value = sum;
    why = Sum;
    left = left;
    right = right;
    index = Dpq.no_position;
  } in
    Verb.pr Verb.debug "(%02d) Summing %s and %s = %s\n"
      (t.depth + 1)
      (string_of_big_int left.value)
      (string_of_big_int right.value)
      (string_of_big_int sum);
    Stack.push num t.undo;
    Dpq.insert t.nums num;
    t.depth <- t.depth + 1;
    t.depth


let undo t =
  (** [undo t] performs a single undo in [t]. *)
  Verb.pr Verb.debug "Undoing %d\n" t.depth;
  let top = Stack.pop t.undo in
    assert (t.depth >= 0);
    (try
      assert (top.index <> Dpq.no_position);
    with _ ->
      num_print stdout top;
      assert false;
    );
    assert (top.why <> Leaf);
    assert (top.why <> Invalid);
    assert (top.left.index = Dpq.no_position);
    assert (top.right.index = Dpq.no_position);
    Verb.pr Verb.debug "(%02d) Undoing: %s of %s and %s\n"
      (t.depth - 1)
      (string_of_why top.why)
      (string_of_big_int top.left.value)
      (string_of_big_int top.right.value);
    Dpq.remove t.nums top.index;
    Dpq.insert t.nums top.left;
    Dpq.insert t.nums top.right;
    t.sum <- (add_big_int
		(add_big_int top.left.value top.right.value)
		(sub_big_int t.sum top.value));
    t.depth <- t.depth - 1


let undo_to t depth =
  (** [undo_to t depth] performs undo operations in [t] until it is at
      depth [depth].*)
  Verb.pr Verb.debug "Undoing to %d\n" depth;
  assert (depth >= 0);
  assert (depth <= t.depth);
  while t.depth > depth do undo t done


let make_copy_state t =
  (** [make_copy_state t] makes a copy of the current configuration
      and adds it to the copy array.  The result is the negation of
      the index of the copy. *)
  (fun n ->
     undo_to t n;
     Dpq.peek_first t.nums)


let rec do_partition ?(depth = 0) partitions ind num =
  (** [do_partition partitions ind num] partition into two sets given
      [partitions], the array of sets (0 and 1) and the index
      [ind]. *)
  let other c = ((c + 1) mod 2) in
    match num.why with
      | Sum ->
	  Verb.pr Verb.debug "%02d Sum: vl=%s, ind=%d\n"
	    depth (string_of_big_int num.value) ind;
	  do_partition ~depth:(depth + 1) partitions ind num.left;
	  do_partition ~depth:(depth + 1) partitions ind num.right;
      | Diff ->
	  Verb.pr Verb.debug "%02d Diff: vl=%s, ind=%d\n"
	    depth (string_of_big_int num.value) ind;
	  do_partition ~depth:(depth + 1) partitions ind num.left;
	  do_partition ~depth:(depth + 1) partitions (other ind) num.right;
      | Leaf ->
	  Verb.pr Verb.debug "%02d Leaf: vl=%s, ind=%d\n"
	    depth (string_of_big_int num.value) ind;
	  partitions.(ind) <- num.value :: partitions.(ind);
      | Invalid -> invalid_arg "Trying to partition an invalid number"


let partitions root =
  (** [partitions t] make a function that gets the
      partitions. *)
  let partitions = [| []; [] |] in
    do_partition partitions 0 root;
    Verb.pr Verb.debug "Got partitions\n%!";
    partitions.(0), partitions.(1)


let leaf_value n = n.value
  (** [leaf_value n] gets the difference between the two sets. *)


let make_is_better t =
  (** [make_is_better t] makes a better test. *)
  (fun a b ->
     undo_to t a;
     let a = Dpq.peek_first t.nums in
       lt_big_int (leaf_value a) (leaf_value b))


let make_is_leaf t =
  (** [make_is_leaf t] makes a function that tests if a node is a
      leaf. *)
  (fun n ->
     undo_to t n;
     (Dpq.count t.nums) = 1)


let make_num_children t =
  (** [make_num_children t] makes a function that gets the number of
      children for a node. *)
  (fun n ->
     undo_to t n;
     if (Dpq.count t.nums) = 1
     then 0
     else 2)


let make_nth_child t =
  (** [make_nth_child t] makes a function that gets the ith child. *)
  (fun n i ->
     Verb.pr Verb.debug "Getting child %d of %d\n" i n;
     undo_to t n;
     (try
       assert ((Dpq.count t.nums) >= 2);
     with _ ->
       Verb.pr Verb.always "Getting children for #num=%d\n" (Dpq.count t.nums);
       assert false;
     );
     if i <> 0 && i <> 1
     then invalid_arg (Printf.sprintf "child: There cannot be child %d" i);
     let n' = if i = 0 then diff_two t else sum_two t in
     let biggest = Dpq.peek_first t.nums in
       if ge_big_int biggest.value (sub_big_int t.sum biggest.value)
       then begin (* diff the remaining. *)
	 Verb.pr Verb.debug "Diff the remaining\n%!";
	 while (Dpq.count t.nums) > 1 do ignore (diff_two t) done;
	 Verb.pr Verb.debug "depth=%d\n" t.depth;
	 t.depth
       end else n')


let make_interface inst =
  (** [make_interface inst] makes an interface to the search given the
      instance. *)
  let module I = Bounded_depth_interface in
  let t = make inst in
    (t.depth,
     { (I.default (make_num_children t) (make_nth_child t)
	  (make_is_leaf t) (make_is_better t) (make_copy_state t))
       with
	 I.max_depth = max_depth inst;
	 I.is_optimal = Two_partition.make_is_optimal leaf_value inst;
     },
     partitions)
