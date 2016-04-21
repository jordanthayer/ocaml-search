(** 2-way number partitioning of arbitrary percision integers using
    the greedy approach.

    @author eaburns
    @since 2009-12-28
*)

open Big_int

type t = {
  lista : big_int list;
  listb : big_int list;
  index : int;
  (* The index of the next number to partition. *)
  diff : big_int;
  (* The current difference between the unassigned values. *)
}

let unassigned = ~-1
  (** [unassigned] labels an unassigned partition. *)


let make_initial inst =
  (** [make_initial inst] makes an initial state given an instance. *)
  {
    lista = [inst.(0)];
    listb = [];
    index = 1;
    diff = inst.(0);
  }


let max_depth inst = Array.length inst
  (** [max_depth inst] is the maximum depth that can be used to solve
      the given instance. *)


let make_is_leaf inst =
  (** [make_is_leaf inst] makes a leaf test function. *)
  let size = Array.length inst in
    (fun n -> n.index = size)


let make_partitions inst = (fun n -> n.lista, n.listb)
  (** [make_partitions inst] makes a function to get a solution
      from a leaf. *)


let leaf_value n = abs_big_int n.diff
  (** [leaf_value n] gets the value of the leaf (the differences
      between the two partitions). *)


let leaf_cost n =
  (** [leaf_cost n] gets the cost of a lead.  This is the log10
      (difference + 1).  (We add one so that we never log10 a 0). *)
  log10 ((float_of_big_int (leaf_value n)) +. 1.)


let make_print_partitions inst =
  Two_partition.make_print_partitions (make_partitions inst)


let make_is_optimal inst = Two_partition.make_is_optimal leaf_value inst


let make_is_better inst = Two_partition.make_is_better leaf_value


let make_num_children inst =
  (** [num_children n] gets the number of children of the node. *)
  let is_leaf = make_is_leaf inst in
    (fun n -> if is_leaf n then 0 else 2)


let add_to_smaller inst n =
  (** [add_to_smaller inst n] adds the next number to the smaller
      set. *)
  if le_big_int n.diff zero_big_int
  then
    (* set b is larger. *)
    { n with
	lista = inst.(n.index) :: n.lista;
	index = n.index + 1;
	diff = add_big_int n.diff inst.(n.index);
    }
  else
    (* set a is larger *)
    { n with
	listb = inst.(n.index) :: n.listb;
	index = n.index + 1;
	diff = sub_big_int n.diff inst.(n.index);
    }


let add_to_larger inst n =
  (** [add_to_larger inst n] adds the next number to the larger
      set. *)
  if le_big_int n.diff zero_big_int
  then
    (* set b is larger. *)
    { n with
	listb = inst.(n.index) :: n.listb;
	index = n.index + 1;
	diff = sub_big_int n.diff inst.(n.index);
    }
  else
    (* set a is larger *)
    { n with
	lista = inst.(n.index) :: n.lista;
	index = n.index + 1;
	diff = add_big_int n.diff inst.(n.index);
    }

let make_nth_child inst =
  (** [make_nth_child inst] gets the [i]th child of node [n]. *)
  let is_leaf = make_is_leaf inst in
    (fun n i ->
       assert (not (is_leaf n));
       if i <> 0 && i <> 1
       then
	 invalid_arg (Printf.sprintf "child: There cannot be child %d" i)
       else
	 if i = 0 then add_to_smaller inst n else add_to_larger inst n)


let make_child_costs inst =
  (** [make_child_costs inst] makes a function that gets the costs of
      the two children. *)
  (fun n ->
     let biggest = inst.(n.index) in
     let diff0, diff1 =
       if le_big_int n.diff zero_big_int
       then				(* b is bigger *)
	 add_big_int n.diff biggest, sub_big_int n.diff biggest
       else				(* a is bigger. *)
	 sub_big_int n.diff biggest, add_big_int n.diff biggest
     in [log10 ((float_of_big_int (abs_big_int diff0)) +. 1.);
	 log10 ((float_of_big_int (abs_big_int diff1)) +. 1.)])


let make_interface inst =
  (** [make_interface inst] makes the interface to the domain for the
      various search algorithms. *)
  let module I = Bounded_depth_interface in
    (make_initial inst,
     { (I.default (make_num_children inst) (make_nth_child inst)
	  (make_is_leaf inst) (make_is_better inst) Fn.identity)
       with
	 I.max_depth = max_depth inst;
	 I.is_optimal = make_is_optimal inst;
	 I.child_costs = make_child_costs inst;
	 I.leaf_cost = leaf_cost;
     },
     make_partitions inst)
