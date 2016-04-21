(** 2-way number partitioning of arbitrary percision integers using
    the greedy approach with in place modification.

    @author eaburns
    @since 2009-12-28
*)

open Big_int

type t = {
  inst : Two_partition_instance.t;
  (* The instance. *)
  assigns : int array;
  (* The partitions for the numbers: ~-1 is unassigned. *)
  diffs : big_int array;
  (* The partitions for the numbers: ~-1 is unassigned. *)
  mutable index : int;
  (* The index of the next number to partition. *)
}

let make inst =
  (** [make inst] a structure for in place modification. *)
  {
    inst = inst;
    assigns =
      Array.init (Array.length inst) (function 0 -> 0 | _  -> ~-1);
    diffs = Array.init (Array.length inst) (function
					      | 0 -> inst.(0)
					      | _ -> zero_big_int);
    index = 1;
  }

let diff t = t.diffs.(t.index - 1)
  (** [diff t] gets the current set difference. *)


let max_depth inst = Array.length inst
  (** [max_depth inst] is the maximum depth that can be used to solve
      the given instance. *)


let undo_to t n = t.index <- n
  (** [undo_to t n] undo in the [t] structure to depth [n]. *)


let make_copy_state t =
  (** [make_copy_state t] gets a copy of state [n].  Since the
      states are immutable there is no real need to do any actual
      copying here. *)
  (fun n ->
     undo_to t n;
     { t with
	 assigns = Array.copy t.assigns;
	 diffs = Array.copy t.diffs; })


let make_is_leaf t =
  (** [make_is_leaf t] makes a leaf test function. *)
  let size = Array.length t.inst in
    (fun n ->
       undo_to t n;
       t.index = size)


let partitions t =
  (** [partitions t] makes a function to get a solution from a
      leaf. *)
  let a = ref [] and b = ref [] in
    for i = 0 to (Array.length t.assigns) - 1 do
      if t.assigns.(i) = 0 then a := t.inst.(i) :: !a
      else if t.assigns.(i) = 1 then b := t.inst.(i) :: !b
      else assert false
    done;
    !a, !b


let leaf_value t = abs_big_int (diff t)
  (** [leaf_value t] gets the value of the leaf (the differences
      between the two partitions). *)


let make_print_partitions t =
  Two_partition.make_print_partitions partitions


let make_is_optimal t = Two_partition.make_is_optimal leaf_value t.inst


let make_is_better t =
  let is_better = Two_partition.make_is_better leaf_value in
    (fun num saved ->
       undo_to t num;
       is_better t saved)


let make_num_children t =
  (** [make_num_children t] gets the number of children of the node. *)
  let is_leaf = make_is_leaf t in
    (fun n -> if is_leaf n then 0 else 2)


let add_to_smaller t =
  (** [add_to_smaller t] adds the next number to the smaller
      set. *)
  if le_big_int (diff t) zero_big_int
  then begin
    (* set 1 is larger. *)
    t.assigns.(t.index) <- 0;
    t.diffs.(t.index) <- add_big_int (diff t) t.inst.(t.index);
  end else begin
    (* set 0 is larger *)
    t.assigns.(t.index) <- 1;
    t.diffs.(t.index) <- sub_big_int (diff t) t.inst.(t.index);
  end;
  t.index <- t.index + 1


let add_to_larger t =
  (** [add_to_larger t] adds the next number to the larger
      set. *)
  if le_big_int (diff t) zero_big_int
  then begin
    (* set 1 is larger. *)
    t.assigns.(t.index) <- 1;
    t.diffs.(t.index) <- sub_big_int (diff t) t.inst.(t.index);
  end else begin
    (* set 0 is larger *)
    t.assigns.(t.index) <- 0;
    t.diffs.(t.index) <- add_big_int (diff t) t.inst.(t.index);
  end;
  t.index <- t.index + 1


let make_nth_child t =
  (** [make_nth_child t] gets the [i]th child of node [n]. *)
  (fun n i ->
     undo_to t n;
     if i <> 0 && i <> 1
     then
       invalid_arg (Printf.sprintf "child: There cannot be child %d" i)
     else begin
       if i = 0 then add_to_smaller t else add_to_larger t;
       t.index
     end)


let make_interface inst =
  (** [make_interface inst] makes the interface to the domain for the
      various search algorithms. *)
  let module I = Bounded_depth_interface in
  let t = make inst in
    (1,
     { (I.default
	  (make_num_children t)
	  (make_nth_child t)
	  (make_is_leaf t)
	  (make_is_better t)
	  (make_copy_state t))
       with
	 I.max_depth = max_depth inst;
	 I.is_optimal = make_is_optimal t;
     },
     partitions)
