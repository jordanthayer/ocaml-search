(*

  bitpacked array.  This provides a specialized array that contains
  integers, but the integers are packed such that as many of them as
  possible are stuffed into an ocaml integer.  

*)

type bpa = 
{
  total_items: int;
  bits_per_item: int; (*bits per int*)
  base: int array; (*actually contains the integers*)
}


(*finds the left most bit in the argument that has been set.*)
let rec find_leftmost_bit i =
  if i != 0 
  then (find_leftmost_bit (i lsr 1)) + 1
  else 0


(*
  makes a bunch of ones.  For example, 
  make_ones 3 = 000000000000000000000000111
  make_ones 4 = 000000000000000000000001111

  the number of prepended zeroes will depend on the machine, and is
  probably not the same as whatever was above.
*)
let make_ones num = 
  lnot ((lnot 0) lsl num)
(*
  let rec make_ones_helper i n = 
    if i = 1 then n
    else make_ones_helper (i - 1) ((n lsl 1) + 1) in
    make_ones_helper num 1
*)


let get_max_num bpa = 
  Math.int_exp 2 bpa.bits_per_item


(*gets the length of the array backing the bit packed array.*)
let get_length bpa = 
  Array.length bpa.base


let get_items_per_int bpa = 
  (Sys.word_size - 1) / bpa.bits_per_item


(*gets the length of the bpa, the number of items that may be put
  into it.*)
let length arr = 
  arr.total_items



(*
  makes a bit packed array.
*)
let make_bpa_zero length max_value = 
  assert (max_value > 0);
  assert (length > 0);
  let bits_per_item = find_leftmost_bit max_value in
  let items_per_int = (Sys.word_size - 1) / bits_per_item in
  let total_ints = length / items_per_int + 1 in
  {
    total_items = length;
    bits_per_item = bits_per_item;
    base = Array.make total_ints 0;
  }


(** finds the item at index of arr.*)
let get arr index = 
  if index >= arr.total_items then
    (
      let fs = Printf.sprintf 
	"(get) array index %d out of bounds in bit packed array!"
	index in
	failwith fs;
    );
  let ipi = get_items_per_int arr in
  let major_index = index / ipi in
  let minor_index = index mod ipi in

  let mask = (make_ones arr.bits_per_item) lsl 
    (minor_index * arr.bits_per_item) in
  let unshifted_item = arr.base.(major_index) land mask in
    unshifted_item lsr (minor_index * arr.bits_per_item)


(**sets the index of arr to value*)
let set arr index value = 
  if index >= arr.total_items then
    failwith "(set) array index out of bounds in bit packed array!";

  let ipi = get_items_per_int arr in
  let major_index = index / ipi in
  let minor_index = index mod ipi in

  let mask = (make_ones arr.bits_per_item) lsl 
    (minor_index * arr.bits_per_item) in

  let rev_mask = lnot mask in
  let original_number = arr.base.(major_index) in
  let land_new = value lsl (minor_index * arr.bits_per_item) in
    assert (value <= (get_max_num arr));
    assert (index < (length arr));
    Array.set arr.base major_index 
      ((original_number land rev_mask) lor land_new)




(*makes a new bit packed array of size length with the specified max
  value and populates it with the specified initial value.*)
let make length max_value initial = 
  assert (initial <= max_value);
  let new_array = make_bpa_zero length max_value in
    for i = 0 to (length -1)
    do 
      set new_array i initial;
    done; 
    new_array

(*prints the array space delimited out on the specified out channel.*)
let print_all outch arr = 
  let len = get_length arr in
    for i = 0 to len - 1
    do
      Printf.fprintf outch "%d " (get arr i)
    done;
    Printf.fprintf outch "\n";
    flush outch
  


(*takes the array and turns it into a bit packed array.*)
let make_from_array target = 
  let biggest = (fst (Wrarray.max_by (fun a -> a) target)) in
  let new_array = make (Array.length target) biggest 0 in
    for i = 0 to (Array.length target) - 1
    do
      (*negative values can't go in these bit packed arrays.*)
      assert (target.(i) >= 0);
      set new_array i target.(i);
    done;
    new_array


(*takes the list and turns it into a bit packed array.*)
let make_from_list target = 
  failwith "Bp_array.make_from_list not implemented"


let fprint_array outch interposed ar =
  (** prints {ar] on [outch], printing [interposed] between the
    elements.  [key] returns the string to print for each element *)
  for i = 0 to ((ar.total_items) - 1)
  do
    Printf.fprintf outch "%d%s" (get ar i) interposed;
  done;
  flush outch

let write_ints_nl ch ar = 
  fprint_array ch "\n" ar


let to_array arr = 
  Array.init (arr.total_items)
    (fun i -> get arr i)


let copy arr = 
  {
    total_items = arr.total_items;
    bits_per_item = arr.bits_per_item;
    base = Array.copy arr.base;
  }


let fold_lefti f start ar = 
  let res = ref start in
    for i = 0 to (ar.total_items) - 1
    do
      res := f !res i (get ar i)
    done;
    !res


let iteri f ar = 
  for i = 0 to (ar.total_items)- 1
  do
    f i (get ar i)
  done


let fold_left f initial ar = 
  let res = ref initial in
    for i = 0 to (ar.total_items) - 1
    do
      res := f !res (get ar i)
    done;
    !res


