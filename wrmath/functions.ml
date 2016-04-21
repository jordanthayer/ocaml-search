(* $Id: functions.ml,v 1.1 2003/09/27 01:13:33 ruml Exp ruml $

   special functions

*)


(*************** versions of gamma *****************)


let rec gamma_ln =
  let coeffs = [|76.18009172947146;
		 -86.50532032941677;
		 24.01409824083091;
		 -1.231739572450155;
		 0.1208650973866179e-2;
		 -0.5395239384953e-5|] in
    fun x ->
      (** ported from CLASP.

	Returns the natural logarithm of the Gamma function evaluated at
	[x].  Mathematically, the Gamma function is defined to be the
	integral from 0 to Infinity of t^x exp\(-t\) dt.  The
	implementation is copied, with extensions for the reflection
	formula, from Numerical Recipes in C, section 6.1.  The argument
	[x] must be positive.  Full accuracy is obtained for x>1.  For
	x<1, the reflection formula is used. *)
      (* For this implementation, we reverse the NRinC notation:  x <-> xx *)
      if (x <= 0.)
      then invalid_arg "arg to gamma_ln must be positive"
      else if (x > 1.0e302)
      then invalid_arg "arg to gamma_ln too large"
      else if (x = 0.5)
	(* special case used by the error-function *)
      then log (sqrt Math.pi)
      else if (x < 1.)
      then
	(* Use reflection formula:  Gamma(1-z) = z*pi/(Gamma(1+z)sin(pi*z)) *)
	let z = 1. -. x in
	  (* faster to do the * and / than log? *)
	  ((log z) +. (log Math.pi)) -.
	  ((gamma_ln (1. +. z)) +.
	     (log (sin (Math.pi *. z))))
      else
	let xx = ref (x -. 1.)
	and ser = ref 1.000000000190015 in
	let tmp = !xx +. 5.5 in
	let tmp = ((!xx +. 0.5) *. (log tmp)) -. tmp in
	  Array.iter (fun coef ->
			xx := !xx +. 1.;
			ser := !ser +. (coef /. !xx))
	    coeffs;
	  tmp +. (log (!ser *. 2.5066282746310005))


exception Converged

let incomplete_gamma_via_series a x =
  let max_iters = 100
  and eps = 3e-7
  and ap = ref a
  and del = ref (1. /. a) in
  let sum = ref !del in
    try
      for i = 0 to max_iters do
	Math.incf ap 1.;
	del := !del *. (x /. !ap);
	sum := !sum +. !del;
	if (abs_float !del) < ((abs_float !sum) *. eps) then
	  raise Converged
      done;
      failwith "Math.incomplete_gamma_via_series didn't converge!"
    with Converged ->
      !sum *. (exp (((a *. (log x)) -. x) -. (gamma_ln a)))


let incomplete_gamma_via_fractions a x =
  let max_iters = 100
  and eps = 3e-7
  and a0 = ref 1.
  and a1 = ref x
  and b0 = ref 0.
  and b1 = ref 1.
  and fac = ref 1.
  and gold = ref 0. in
    try
      for i = 0 to max_iters do
	(let an = float (i + 1) in
	   (let ana = an -. a in
	      a0 := !fac *. (!a1 +. (!a0 *. ana));
	      b0 := !fac *. (!b1 +. (!b0 *. ana)));
	   (let anf = !fac *. an in
	      a1 := (x *. !a0) +. (anf *. !a1);
	      b1 := (x *. !b0) +. (anf *. !b1)));
	if !a1 <> 0. then
	  (fac := 1. /. !a1;
	   let g = !b1 *. !fac in
	   let acc = abs_float ((g -. !gold) /. g) in
	     gold := g;
	     if acc < eps then raise Converged)
      done;
      failwith "Math.incomplete_gamma_via_fractions didn't converge!"
    with Converged ->
      1. -. ((exp (((a *. (log x)) -. x) -. (gamma_ln a))) *.
	       !gold)


let incomplete_gamma a x =
  if x = 0. then
    0.
  else
    if x < (a +. 1.) then
      incomplete_gamma_via_series a x
    else
      incomplete_gamma_via_fractions a x


(********** beta *****)


let beta z w =
  (** Returns the value of the Beta function, defined in terms of the
    complete gamma function, G, as: G(z)G(w)/G(z+w).  The implementation
    follows Numerical Recipes in C, section 6.1 *)
  exp (((gamma_ln z) +. (gamma_ln w)) -.
	 (gamma_ln (z +. w)))


let clamp_small x =
  if (abs_float !x) < 1e-30 then x := 1e-30


let betacf a b x =
  (** evaluates continued fraction for incomplete beta function (Numerical
    Recipes in C, section 6.4) *)
  (* NOT VERY WELL TESTED *)
  let max_iters = 100.
  and eps = 3e-7
  and qab = a +. b
  and qap = a +. 1.
  and qam = a -. 1. in
  let c = ref 1.
  and d = ref (1. -. ((qab *. x) /. qap)) in
    clamp_small d;
    d := 1. /. !d;
    let h = ref !d in
    let rec iter m =
      let m2 = 2. *. m in
      let aa = (m *. (b -. m) *. x) /.
		 ((qam +. m2) *. (a +. m2)) in
	d := 1. +. (aa *. !d);
	clamp_small d;
	c := 1. +. (aa /. !c);
	clamp_small c;
	d := 1. /. !d;
	h := !h *. !d *. !c;
	let aa = ((a +. m) *. (qab +. m) *. (-. x)) /.
		   ((a +. m2) *. (qap +. m2)) in
	  d := 1. +. (aa *. !d);
	  clamp_small d;
	  c := 1. +. (aa /. !c);
	  clamp_small c;
	  d := 1. /. !d;
	  let del = !d *. !c in
	    h := !h *. del;
	    if (abs_float (del -. 1.)) < eps then
	      !h
	    else if m > max_iters then
	      failwith "a or b too big in betacf"
	    else
	      iter (m +. 1.)
    in
      iter 1.


let beta_incomplete a b x =
  (** useful in defining the cumulative distributions for Student's t and
    the F distribution *)
  (* NOT VERY WELL TESTED *)
  assert (a >= 0.);
  assert (b >= 0.);
  assert ((x >= 0.) && (x <= 1.));
  let bt = (if (x = 0.) || (x = 1.) then 0. else
	      exp ((((gamma_ln (a +. b)) -. (gamma_ln a)) -. (gamma_ln b)) +.
		     (a *. (log x)) +. (b *. (log (1. -. x))))) in
    if x < ((a +. 1.) /. (a +. b +. 2.)) then
      (bt *. (betacf a b x)) /. a
    else
      1. -. ((bt *. (betacf b a (1. -. x))) /. b)


(******************** some other functions ******************)

let trunc_to f_val n_decimals =
  (** Truncates [f_val] at [n_decimal] decimal points. Right now
      length is hard-coded to zero. Need to work on this. *)
  (*let f_acc = 1.0 /. (10.0**(float n_decimals)) in
    f_acc *. (float (truncate (f_val /. f_acc)))*)
  let s = Printf.sprintf "%0.*f" n_decimals f_val in
    float_of_string s

let rec gcd x y =
  (** for integers.  Euclid's algorithm *)
  if y = 0
  then
    abs x
  else
    let z = x mod y in
      gcd y z


let factorial n =
  (** returns a big_int *)
  if n < 0
  then invalid_arg "factorial of < 0"
  else
    let res = ref Big_int.unit_big_int in
      for i = 2 to n do
	res := Big_int.mult_big_int !res (Big_int.big_int_of_int i)
      done;
      !res


let factorial_float n =
  (** returns a float *)
  Big_int.float_of_big_int (factorial n)


let factorial_approx n =
  (** returns a float.  should be faster than factorial_float *)
  exp (gamma_ln ((float_of_int n) +. 1.))


(** Stirling's approximation is n! \approx e^{(n (ln n)) - n} *)


let choose n k =
  (** ways of choosing [k] from [n] without regard to order.  returns
    an exact big int. *)
  Big_int.div_big_int
    (factorial n)
    (Big_int.mult_big_int
       (factorial (n - k))
       (factorial k))


let choose_float n k =
  Big_int.float_of_big_int (choose n k)


let choose_approx n k =
  (* CALL GAMMA_LN DIRECTLY, avoid exp until last minute *)
  Wrutils.write_this ();
  (factorial_approx n) /.
  ((factorial_approx (n - k)) *.
     (factorial_approx k))


(*********** monotonic regression *************)


let prefix_sums a n =
  (** returns an array one longer than the given one in which elt i is
    the sum through elt i-1 in the original *)
  let sum = Array.make (n + 1) 0.
  and accum = ref 0. in
    for i = 0 to (n - 1) do
      accum := !accum +. a.(i);
      sum.(i + 1) <- !accum
    done;
    sum


let monotonic_regression a offset len =
  (** destructive.  simple O(len^2) algorithm via isoMDS.c by Brian
    D. Ripley *)
  let n = offset + len in
    assert ((offset >= 0) &&
	    (n <= Array.length a));
    let sums = prefix_sums a n
    and left = ref offset in
      (* start at left *)
      while (!left <> n) do
	(* find right end of interval with lowest average *)
	let left_sum = sums.(!left)
	and min = ref a.(!left)
	and min_right = ref (!left + 1) in
	  for right = !left + 2 to n do
	    let avg = (sums.(right) -. left_sum) /.
			(float_of_int (right - !left)) in
	      if avg < !min
	      then (min := avg;
		    min_right := right)
	  done;
	  (* fill interval with average *)
	  for i = !left to !min_right - 1 do
	    a.(i) <- !min
	  done;
	  left := !min_right
      done

(** {6 The Error Function} ****************************************)

type float_ref = { mutable data : float }

let float_ref x = { data = x }

let (<--) x y = x.data <- y

let (!!) x = x.data

let half_sqrt_pi = (sqrt Math.pi) /. 2.

let two_over_root_pi = 2. /. (sqrt Math.pi)

let erf x =
  (** [erf x] computes a taylor approximation of the error function.
      Careful of overflow with values greater than 1 from zero.*)
  let inf = 50 in
  let sum = float_ref 0. in
  let n_fact = float_ref 1. in
    for n = 0 to inf do
      let nf = float n in
      let sign = if (n mod 2) = 0 then 1. else ~-.1. in
      let two_n_plus_1 = 2. *. nf +. 1. in
	sum <-- !!sum +.
	  (sign *. (x ** two_n_plus_1)) /. (!!n_fact *. two_n_plus_1);
	n_fact <-- !!n_fact *. (nf +. 1.);
    done;
    let y = two_over_root_pi *. !!sum in
(*
      (try assert (y >= ~-.1.);
       with _ ->
	 Printf.printf "x=%g, erf(x)=%g, erf(x) must be >= -1\n" x y;
	 assert false);
*)
      if y < ~-.1. then begin
	Verb.pr Verb.always "Clamping erf(%g) to -1\n" x;
	~-.1.
      end else y


let erf_inv x =
  (** [erf_inv x] computes a taylor approximation of the inverse of
      the error function. *)
  let inf = 100 in
  let cache = Cache.create 100 in
  let rec c k = Cache.find_or_compute cache compute_c k
  and compute_c k =
    if k = 0
    then 1.
    else begin
      let sum = float_ref 0. in
	for m = 0 to k - 1 do
	  let mf = float m
	  and cm = c m
	  and c_k_1_m = c (k - 1 - m) in
	  let m2 = 2. *. mf in
	    sum <-- !!sum +. (cm *. c_k_1_m) /. ((mf +. 1.) *. (m2 +. 1.))
	done;
	!!sum
    end
  in
    assert (x < 1.0);
    assert (x > ~-.1.0);
    let sum = float_ref 0. in
    let t = half_sqrt_pi *. x in
      for k = 0 to inf do
	let kf = float k in
	let k2_1 = kf *. 2. +. 1. in
	  sum <-- !!sum +. ((c k) /. k2_1) *. (t ** k2_1)
      done;
      !!sum

(* EOF *)
