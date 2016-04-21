(** A fixed-size, destructive bitset.

    @author eaburns
    @since 2009-09-24
*)

open Printf

type t = {
  mutable bits : int array;
}


let elm_size = Sys.word_size - 1
  (** [elm_size] is the number of bits in an integer. *)


let get_index i = i / elm_size
  (** [get_index i] gets the array index for bit [i].

      Ocaml will perform a divide-by-zero test here each time this is
      called even thought elm_size is always non-zero... this should be
      fixed some how. *)


let get_mask i = 1 lsl (i mod elm_size)
  (** [get_mask i] gets the mask for selecting bit [i] from its array
      element. *)


let create num = { bits = Array.make ((get_index num) + 1) 0; }
  (** [create atleast] creates a new bitset that can hold at least
      [num] elements. *)


let create_rev num = { bits = Array.make ((get_index num) + 1) (lnot 0); }
  (** [create atleast] creates a new bitset that can hold at least
      [num] elements where the elements are initialized to be all
      bits set. *)



let resize t num =
  (** [resize t num] resize [t] to hold at least [num] elements.  This
      can both grow and shrink the size. *)
  let tbits = t.bits in
  let n = Array.length tbits in
  let bits' = Array.init ((get_index num) + 1) (fun i ->
						  if i <= n
						  then tbits.(i)
						  else 0)
  in t.bits <- bits'


let insert t i =
  (** [insert t i] sets bit [i]. *)
  let bits = t.bits in
  let ind = get_index i and mask = get_mask i
  in bits.(ind) <- bits.(ind) lor mask


let remove t i =
  (** [remove t i] clears bit [i]. *)
  let bits = t.bits in
  let ind = get_index i and mask = lnot (get_mask i)
  in bits.(ind) <- bits.(ind) land mask


let toggle t i =
  (** [toggle t i] toggles the bit [i]. *)
  let bits = t.bits in
  let ind = get_index i and mask = get_mask i
  in bits.(ind) <- bits.(ind) lxor mask


let mem t i = (t.bits.(get_index i) land (get_mask i)) <> 0
  (** [mem t i] returns the value of the [i]th bit as a boolean. *)


let clear t =
  (** [clear t] zeroes all bits in the set. *)
  let bits = t.bits in
  let n = Array.length bits in
    for i = 0 to n - 1 do bits.(i) <- 0 done


let max t = ((Array.length t.bits) * elm_size) - 1
  (** [max t] gets the maximum allowable element number of the set. *)


let union a b =
  (** [union a b] gets the union of set [a] and set [b]. *)
  let ma = max a and mb = max b in
  let larger = if ma > mb then a else b in
  let smaller = if ma > mb then b else a in
  let s = { bits = Array.copy larger.bits; } in
  let n = get_index (max smaller) in
  let sbits = s.bits and abits = a.bits and bbits = b.bits in
    for i = 0 to n do
      sbits.(i) <- abits.(i) lor bbits.(i);
    done;
    s


let subtract a b =
  (** [subtract a b] a new set that is the subtraction of [b] from
      [a]. *)
  let smaller = if max a < max b then a else b in
  let s = { bits = Array.copy a.bits; } in
  let n = get_index (max smaller) in
  let sbits = s.bits and abits = a.bits and bbits = b.bits in
    for i = 0 to n do
      sbits.(i) <- abits.(i) land (lnot bbits.(i));
    done;
    s

let complement t =
  (** [complement t] get the complement of the set (the set of
      elements that are not members.*)
  let s = { bits = Array.copy t.bits; } in
  let n = get_index (max t) in
  let sbits = s.bits and tbits = t.bits in
    for i = 0 to n do
      sbits.(i) <- lnot tbits.(i);
    done;
    s


let is_empty t =
  (** [is_empty t] tests if the set is empty. *)
  let rec do_is_empty tbits n i =
    if i < n
    then tbits.(i) = 0 && do_is_empty tbits n (i + 1)
    else true
  in
  let tbits = t.bits in
  let n = Array.length tbits in
    do_is_empty tbits n 0


let copy t = { bits = Array.copy t.bits; }
  (** [copy t] creates a new copy of [set]. *)


let random_selected t = 
  (**
     takes a bit set and returns the index of an unspecified nonzero
     bit.  If there are no nonzero bits, it returns -1.
  *)
  try 
    let ix = Wrarray.find (fun a -> a != 0) t.bits in
      if ix = -1 then ix
      else 
	(
	  let number = ref t.bits.(ix) in
	  let bit_to_return = ref 0 in
	    while (!number != 0) do
	      if((!number land 1) = 1)
	      then (number := 0)
	      else 
		(
		  number := !number lsr 1;
		  bit_to_return := !bit_to_return + 1;
		)
	    done;
	    !bit_to_return + (ix * (elm_size))
	)
  with Not_found -> -1
(*

#use "use.ml";;
let bs = Bitset.create 10;;
Bitset.insert bs 6;;
Bitset.insert bs 2;;
Bitset.random_selected bs;;

*)      


let to_string t =
  (** [to_string t] gets the string representation of the set. *)
  let n = max t in
  let buf = Buffer.create n
  and zeroes = Buffer.create n in
    for i = 0 to n - 1 do
      match mem t i with
	| true ->
	    Buffer.add_buffer buf zeroes;
	    Buffer.clear zeroes;
	    Buffer.add_char buf '1'
	| false -> Buffer.add_char zeroes '0'
    done;
    Buffer.contents buf


let of_string str =
  (** [of_string str] gets a bitset from a string. *)
  let n = String.length str in
  let t = create n in
    for i = 0 to n - 1 do
      match str.[i] with
	| '1' -> insert t i
	| '0' -> ()
	| c -> invalid_arg (Wrutils.str "%c invalid bit" c)
    done;
    t
