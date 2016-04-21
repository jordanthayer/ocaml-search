(** A lazy vector is like an array that is not of a fixed size.
    Additionally, the elements are initialized lazily only when they
    are accessed.

    @author eaburns
    @since 2010-10-12
*)

open Lazy

type 'a inner_array = 'a lazy_t array

type 'a outter_array = 'a inner_array lazy_t array

type 'a t = {
  outter : 'a outter_array;

  s1_exp : int;
  (* The exponent of the first array that is not collapsed into index
     0. *)

  mutable max : int;
  (* The max of the vector... the maximum element ever accessed. *)
}


let pow2 x =
  (** [pow2 x] gets 2^[x]. *)
  1 lsl x


let floor_log2 x =
  (** [floor_log2 x] gets the truncated log base 2 of [x]. *)
  (* This can be achieved by bit-operations using a binary search on
     the set bits.  See
     /usr/src/linux/include/asm-generic/bitops/fls.h. *)
  let log_of_2 = log 2. in
    truncate (log (float x) /. log_of_2)


let outter_size s1_exp =
  (** [outter_size s1_exp] gets the size of the outter array so that
      the final element of the outter array should be for an inner array
      of the closest possible to the max array size. *)
  (floor_log2 Sys.max_array_length) - s1_exp


let inner_size s1_exp i =
  (** [inner_size s1_exp i] gets the size of the inner array at outter
      index [i]. *)
  if i = 0 then (pow2 s1_exp) - 1 else pow2 (s1_exp + (i - 1))


let min_index s1_exp i =
  (** [min_index s1_exp i] gets the minimum index of the inner array
      at outter index [i]. *)
  if i = 0 then 0 else (pow2 (s1_exp + (i - 1))) - 1


let outter_index s1_exp i =
  (** [outter_index s1_exp i] gets the outter array index given the
      logical index [i]. *)
  let l = floor_log2 (i + 1) in
  let oi = (l - s1_exp) + 1 in
    if oi < 0 then 0 else oi


let inner_index s1_exp oi i =
  (** [inner_index s1_exp oi i] gets the inner array index given the
      logical index[i] and the outter array index [oi]. *)
  let mi = min_index s1_exp oi in
    i - mi


let default_init_size = 128


let init ?(init_size=default_init_size) f =
  (** [init ?init_size f] creates a lazy vector where
      elements are lazily initialized. *)
  let l = floor_log2 init_size in
  let s1_exp = if (pow2 l) = init_size then l else l + 1 in
  let outter_size = outter_size s1_exp in
  let init_inner oi =
    let size = inner_size s1_exp oi in
    let min_index = min_index s1_exp oi in
    let init_elm j =
      let ind = min_index + j in lazy (f ind)
    in
      Array.init size init_elm
  in
    {
      outter = Array.init outter_size (fun i -> lazy (init_inner i));
      s1_exp = s1_exp;
      max = 0;
    }


let make ?init_size v =
  (** [make ?init_size v] creates a lazy vector filled with [v]. *)
  init ?init_size (fun _ -> v)


let get v i =
  (** [get v i] gets the element at index [i]. *)
  let s1_exp = v.s1_exp in
  let oi = outter_index s1_exp i in
  let ii = inner_index s1_exp oi i in
  let inner = force v.outter.(oi) in
    if i >= v.max then v.max <- i + 1;
    force inner.(ii)


let set v i e =
  (** [set v i e] sets the value at index [i] to be [e]. *)
  let s1_exp = v.s1_exp in
  let oi = outter_index s1_exp i in
  let ii = inner_index s1_exp oi i in
  let inner = force v.outter.(oi) in
    if i >= v.max then v.max <- i + 1;
    inner.(ii) <- lazy_from_val e


let iter_range ?(force_inner=false) v start finish f =
  (** [iter_range ?force_inner v start finish f] iterates [f] over the
      given range.  If [force_inner] is true then non-forced inner
      arrays are forced, otherwise they are left. *)
  let s1_exp = v.s1_exp in
  let outter = v.outter in
  let start = max start 0 in
  let do_inner lower upper inner =
    for j = max lower start to min finish upper do
      let v = inner.(j - lower) in
	f j v
    done;
  in
  let rec iter_inner oi lower =
    let size = inner_size s1_exp oi in
    let upper = lower + (size - 1) in
      if upper >= start && lower <= finish then begin
	let inner = outter.(oi) in
	  if force_inner || lazy_is_val inner then
	    do_inner lower upper (force inner);
      end;
      if lower <= finish then iter_inner (oi + 1) (upper + 1)
  in
    if finish >= v.max then v.max <- finish + 1;
    iter_inner 0 0


let iter_range_rev ?(force_inner=false) v start finish f =
  (** [iter_range_rev ?force_inner v start finish f] iterates [f] over
      the given range.  If [force_inner] is true then non-forced inner
      arrays are forced, otherwise they are left. *)
  let s1_exp = v.s1_exp in
  let outter = v.outter in
  let start = max start 0 in
  let do_inner lower upper inner =
    for j = min finish upper downto max lower start do
      let v = inner.(j - lower) in
	f j v
    done;
  in
  let rec iter_inner oi upper =
    let size = inner_size s1_exp oi in
    let lower = upper - (size - 1) in
      if upper >= start && lower <= finish then begin
	let inner = outter.(oi) in
	  if force_inner || lazy_is_val inner then
	    do_inner lower upper (force inner);
      end;
      if upper >= start && oi > 0 then iter_inner (oi - 1) (lower - 1)
  in
    if finish >= v.max then v.max <- finish + 1;
    let oi = (Array.length v.outter) - 1 in
      iter_inner oi ((min_index s1_exp oi) + (inner_size s1_exp oi) - 1)


let force_range v s f =
  (** [force_range v s f] forces the values between [s] and [f]. *)
  (* This could be implemented more efficiently. *)
  iter_range ~force_inner:true v s f (fun _ v -> force v)


let iteri f v =
  (** [iteri f v] iterates over the vector by calling [f] on each
      index and element.  If 'later' elements of the vector change
      during iteration then these elements will be iterated over with
      their modified values. *)
  iter_range v 0 (v.max - 1) (fun i v -> if lazy_is_val v then f i (force v))


let iteri_rev f v =
  (** [iteri_rev f v] iterates over the vector by calling [f] on each
      index and element.  If 'later' elements of the vector change
      during iteration then these elements will be iterated over with
      their modified values. *)
  iter_range_rev v 0 (v.max - 1)
    (fun i v -> if lazy_is_val v then f i (force v))


module Double_ended = struct
  type 'a vec = 'a t

  type 'a t = {
    negative : 'a vec;
    non_neg : 'a vec;
  }

  let init ?(init_size=128) f =
    let s = init_size / 2 in
    { negative = init ~init_size:s f;
      non_neg = init ~init_size:s f;
    }


  let make ?init_size init_elm =
    init ?init_size (fun _ -> init_elm)


  let get v i =
    if i < 0 then get v.negative ((abs i) - 1) else get v.non_neg i


  let set v i e =
    if i < 0 then set v.negative ((abs i) - 1) e else set v.non_neg i e


  let iteri f v =
    iteri_rev (fun i v -> f ~-(i + 1) v) v.negative;
    iteri f v.non_neg

end

(*
  module Dv = Double_ended;;
  let v = Dv.init (fun _ -> 8);;
  Dv.get v ~-1;;
  Dv.get v ~-5;;
  Dv.get v 0;;
  Dv.get v 10;;
  Dv.iteri (fun i e -> Printf.printf "%d: %d\n" i e) v;;
*)

