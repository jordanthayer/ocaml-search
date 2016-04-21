(* $Id: math.ml,v 1.2 2003/09/26 01:25:33 ruml Exp ruml $

   some basic generally useful mathematical routines
*)


let clamp x min_v max_v =
  min (max x min_v)
    max_v


let between x a b =
  (a <= x) && (x <= b)


(***************** operations on ints **************)


let subtract_ints a b =
  (** a - b, clamping at min_int or max_int *)
  if b >= 0 then
    let threshold = min_int + b in
      if a < threshold then
	min_int
      else
	a - b
  else
    (* trying to make a larger *)
    let threshold = max_int + b in
      if a > threshold then
	max_int
      else
	a - b


let add_ints a b =
  (** a + b, clamping at min_int or max_int *)
  if b >= 0 then
    let threshold = max_int - b in
      if a > threshold then
	max_int
      else
	a + b
  else
    (* making a smaller *)
    let threshold = min_int - b in
      if a < threshold then
	min_int
      else
	a + b

let rec int_factorial i =
  if i = 0 then 1 else i * (int_factorial (i - 1))

let divisible a b =
  (a mod b) = 0


let even_p x =
  divisible x 2

let odd_p x =
  not (even_p x)


let int_exp base exp =
  (** raises the integer [base] to the integer [exp] *)
  assert (exp >= 0);
  match exp with
    0 -> 1
  | 1 -> base
  | _ ->
      let res = ref (base * base)
      and so_far = ref 2 in
      let next = ref (!so_far * 2) in
	while (!next <= exp) do
	  res := !res * !res;
	  so_far := !next;
	  next := !next * 2
	done;
	for i = !so_far to (exp - 1) do
	  res := !res * base;
	done;
	!res


(**** built-in min and max always call compare! *****)


let imin x y =
  if x <= y then (x : int) else y


let imax x y =
  if x >= y then (x : int) else y


let icompare x y =
  if (x < (y : int)) then
    -1
  else if (x > y) then
    1
  else
    0


(******************** operations on floats **********************)


let pi = 3.14159265358979323846
	   (* from http://www.math.com/tables/constants/pi.htm.
	      another approach would be acos(-1.) *)


let e = 2.718281828459045235360
	  (* from http://antwrp.gsfc.nasa.gov/htmltest/gifcity/e.1mil.
	     another approach would be exp 1. *)


let is_positive (x:float) = x > epsilon_float


let div0 x y =
  (** takes 0. /. 0. to 0. instead of nan *)
  if (y = 0.) && (x = 0.) then
    0.
  else
    (* will give infinity or neg_infinity if y = 0. *)
    x /. y


let square x =
  x *. x


let nth_root n x = x ** (1. /. n)


let fmid x y =
  (x +. y) /. 2.


let smallest_multiple_larger a b =
  (** smallest multiple of [a] that is greater than [b].  all floats. *)
  a *. (ceil (b /. a))


let logarithm x b =
  (** logarithm of [x], base [b] *)
  (log x) /. (log b)

let log2 x =
  logarithm x 2.


let safe_log10 ?(bound = -9.) x =
  (** clamps zero to [bound] *)
  if x > 0. then
    max bound (log10 x)
  else if x = 0. then
    bound
  else
    invalid_arg (Wrutils.str "negative number %f at safe_log10" x)


let within a b err =
  (abs_float (a -. b)) <= err

let within_rel a b percent_err =
  (abs_float ((a -. b) /. a)) <= percent_err


let nan_p x =
  match classify_float x with
    | FP_nan -> true
    | _ -> false

let finite_p x =
  match classify_float x with
    |  FP_infinite | FP_nan -> false
    | _ -> true

let is_zero x =
  match classify_float x with
    | FP_zero -> true
    | _ -> false


let incf f_ref v =
  f_ref := !f_ref +. v

let decf f_ref v =
  f_ref := !f_ref -. v

let fmod a b =
  (* a mod b *)
  a -. (float_of_int (truncate (a /. b))) *. b


(**** built-in min and max always call compare! *****)


let fmin x y =
  if x <= y then (x : float) else y


let fmax x y =
  if x >= y then (x : float) else y


let fcompare x y =
  if (x < (y : float)) then
    -1
  else if (x > y) then
    1
  else
    0


(******************** floats and integers **********************)


let max_int_float = float max_int
let min_int_float = float min_int


let round a =
  (** towards nearest integer. *)
  let a = (if a >= 0. then
	     a +. 0.5
	   else
	     a -. 0.5) in
    if a > max_int_float then
      max_int
    else if a < min_int_float then
      min_int
    else
      truncate a


let integral_p f =
  (** [f] is a floating point representation of an integral number *)
  (mod_float f 1.) = 0.


let mul x y =
  (** multiple an int by a float and return an int *)
  truncate ((float x) *. y)


let div x y =
  (** integer division that returns a float *)
  (float x) /. (float y)


let is_power_of x b =
  integral_p (logarithm x b)


let ceil_int x =
  (** returns an int where Pervasives.ceil returns a float *)
  int_of_float (ceil x)

let floor_int x =
  (** returns an int where Pervasives.floor returns a float *)
  int_of_float (floor x)


let to_radians degrees =
  (** converts degrees (as an int) to radians (as a float) *)
  (float degrees) *. ((2. *. pi) /. 360.)

let to_degrees radians =
  (** returns an int *)
  round (radians *. (360. /. (2. *. pi)))


(****** randomness *******)


let true_with_prob p =
  (Random.float 1.) < p


let random_int () =
  (** returns an random integer between 0 and (2^30)-1 inclusive *)
  Random.bits ()


let random_state_from seed =
  (** actually, returns a ref *)
  let prev = Random.get_state () in
    Random.init seed;
    let s = ref (Random.get_state ()) in
      Random.set_state prev;
      s


let with_random_state f s =
  (** actually, takes a ref *)
  let prev = Random.get_state () in
    Random.set_state !s;
    let res = f () in
      s := Random.get_state ();
      Random.set_state prev;
      res


let with_random_seed f seed =
  let prev = Random.get_state () in
    Random.init seed;
    let r = f () in
      Random.set_state prev;
      r


let rec n_choose_k_helper (n,k) =
  match (n,k) with
      (_,0) -> 1
    | (0,_) -> 0
    | (a,b) -> ((n_choose_k_helper ((n-1),(k-1))) + 
		  (n_choose_k_helper ((n-1),(k))))


let n_choose_k n k = 
  if(n < k) then 1
  else 
    n_choose_k_helper (n,k)


(* EOF *)
