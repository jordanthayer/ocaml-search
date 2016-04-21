(** Shared functions for two-way number partitioning.

    @author eaburns
    @since 2009-12-29
*)

open Big_int
open Printf

let make_print_partitions partitions =
  (** [make_print_partitions partitions] prints the
      partitions to the given output channel. *)
  (fun ch n ->
     let a, b = partitions n in
     let a = List.sort compare_big_int a in
     let b = List.sort compare_big_int b in
     let asum = List.fold_left add_big_int zero_big_int a
     and bsum = List.fold_left add_big_int zero_big_int b in
     let diff = abs_big_int (sub_big_int asum bsum) in
       fprintf ch "(%s) [" (string_of_big_int asum) ;
       List.iter (fun i -> fprintf ch "%s; " (string_of_big_int i)) a;
       fprintf ch "]\n(%s) [" (string_of_big_int bsum);
       List.iter (fun i -> fprintf ch "%s; " (string_of_big_int i)) b;
       fprintf ch "]\n";
       fprintf ch "difference: %s\n" (string_of_big_int diff))


let make_is_optimal leaf_value inst =
  (** [make_is_optimal leaf_value ints] makes a function to
      test if the leaf node [n] is an optimal solution. *)
  let sum = Array.fold_left add_big_int zero_big_int inst in
  let two = add_big_int unit_big_int unit_big_int in
  let even = eq_big_int (mod_big_int sum two) zero_big_int in
    if even
    then (fun n -> eq_big_int (leaf_value n) zero_big_int)
    else (fun n -> eq_big_int (leaf_value n) unit_big_int)


let make_is_better leaf_value =
  (** [is_better leaf_value] make a function to test if one leaf is
      better than another. *)
  (fun a b ->
     let val_a = leaf_value a
     and val_b = leaf_value b
     in lt_big_int val_a val_b)
