(* $Id: wrrandom.ml,v 1.1 2003/09/13 21:28:50 ruml Exp ruml $

   random number generators

*)

(*generates a random integer between a and b*)
exception Illegal_range
let randbetween a b = 
  (**)
  if a > b then raise Illegal_range;
  a + (Random.int (b-a+1));;
let randbetween_f a b = a +. (Random.float (b -. a))


(********** with a specified lower bound ************)

let float_in_range low high = (Random.float (high -. low)) +. low

let int_in_range low high = (Random.int (high - low)) + low

(********** using different seeds with built-in ************)


let with_seed f arg seed =
  let old = Random.get_state () in
    Wrutils.unwind_protect
      (fun () ->
	 Random.init seed;
	 f arg) ()
      Random.set_state old


(********** fast generation of samples **********)


type table = float array


let table_size = 8192
		   (* 2^13 *)


let init_table dist_func =
  let step = 1. /. (float_of_int table_size) in
  let shift = step /. 2. in
    Array.init table_size
      (fun i ->
	 let quantile = ((float_of_int i) *. step) +. shift in
	   dist_func quantile)


let from_table table =
  table.(Random.int table_size)


let print_table t =
  (** for debugging *)
  for i = 0 to (min 20 (table_size - 1)) do
    Wrutils.pr "%4d: %f\n" i t.(i)
  done


let normal_table mean stddev =
  init_table (fun z ->
		((Probability.normal_quantile z) *. stddev) +. mean)


(********** normal distribution  **********)



(***  A revised version of Kinderman-Monahan by Joseph Leva
  (see ACM Trans Math Soft vol 18, no 4, Dec 1992, pp 449-455,
  algorithm 712)

  Should only do log 1.2% of the time.
***)

let sqrt_8_over_e = sqrt (8. /. (exp 1.))
and leva_s = 0.449871
and leva_t = -0.386595
and leva_a = 0.19600
and leva_b = 0.25472
and leva_inner = 0.27597
and leva_outer = 0.27846


let non_zero_float () =
  let u = ref (Random.float 1.) in
    while !u = 0. do
      u := Random.float 1.
    done;
    !u


let rec std_normal_leva () =
  let u = non_zero_float ()
  and v = ((non_zero_float ()) -. 0.5) *. sqrt_8_over_e in
  let x = u -. leva_s
  and y = (abs_float v) -. leva_t in
  let q = (x *. x) +. (y *. ((leva_a *. y) -.
			     (leva_b *. x))) in
    if q < leva_inner then
      (* q inside inner acceptance region *)
      v /. u
    else if q > leva_outer then
      (* q outside outer acceptance region *)
      std_normal_leva ()
    else if (v *. v) <= (-4. *. u *. u *. (log u)) then
      (* real acceptance test passed *)
      v /. u
    else
      std_normal_leva ()


let normal mean sd =
  ((std_normal_leva ()) *. sd) +. mean


let log_normal mean st =
  let n = normal 0. 1. in
    exp (mean +. st *. n)


let normal_from_table =
  let t = normal_table 0. 1. in
    (fun mean sd ->
       ((from_table t) *. sd) +. mean)


(* EOF *)
