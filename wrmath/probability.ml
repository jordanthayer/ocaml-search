(* $Id: probability.ml,v 1.1 2003/09/13 21:28:41 ruml Exp ruml $
   
   basic probability
*)


(******************* normal distribution *******************)


let normal_quantile area =
  (** given a probability mass, returns the value for which that much
    mass is to the left in the standard normal distribution.  This is
    the inverse of the cumulative distribution function.

    # Probability.normal_quantile 0.975;;
    - : float = 1.95996398229
    # Probability.normal_quantile 0.5;;
    - : float = 0.
    # Probability.normal_quantile 0.025;;
    - : float = -1.95996398229
    # Probability.normal_quantile 0.25;;
    - : float = -0.674489749691
    # Probability.normal_quantile 0.75;;
    - : float = 0.674489749691
  *)
  (* ported from Common Lisp, probably the CLASP package.  adapted from
     AS 111 --- FORTRAN routine ppnd see Beasley and Springer, Applied
     Statistics, 1977, vol. 26, p.118 *)
  assert ((area > 0.) && (area < 1.));
  let a0 = 2.50662823884
  and a1 = -18.61500062529
  and a2 = 41.39119773534
  and a3 = -25.44106049637
  and b1 = -8.4735109309
  and b2 = 23.08336743743
  and b3 = -21.06224101826
  and b4 = 3.13082909833
  and c0 = -2.78718931138
  and c1 = -2.29796479134
  and c2 = 4.85014127135
  and c3 = 2.32121276858
  and d1 = 3.54388924762
  and d2 = 1.63706781897 in
  let p = area in
  let q = p -. 0.5 in
    if ((abs_float q) <= 0.42) then
      let r = q *. q in
	q *. (((((((a3 *. r) +.
		     a2) *. r) +.
		   a1) *. r) +.
		 a0) /.
		((((((((b4 *. r) +.
			 b3) *. r) +.
		       b2) *. r) +.
		     b1) *. r) +.
		   1.))
    else
      let r = sqrt (-. (log (if (q > 0.) then 1. -. p else p))) in
      let result = ((((((c3 *. r) +.
			  c2) *. r) +.
			c1) *. r) +.
		      c0) /.
		     ((((d2 *. r) +.
			  d1) *. r) +.
			1.) in
	if (q < 0.) then
	  -. result
	else
	  result


let z_critical_point percent_between_both_tails =
  (* the value from the normal distribution such that [percent] of the
     area is between + and - the value *)
  normal_quantile (1. -. ((1. -. percent_between_both_tails) /. 2.))


(******************** student's t distribution *********************)

(*
let students_t_significance t dof way =
  (** This function computes the significance of `t-statistic.'  Values
    range from 0.0 to 1.0: small values suggest that the null
    hypothesis---that `t-statistic' is drawn from a t distribution---should
    be rejected.  The `t-statistic' parameter should be a float, while `dof'
    should be an integer.

    The null hypothesis is roughly that `t-statistic' is zero; you must
    specify your alternative hypothesis (H1) via the `tails' parameter,
    which must be :both, :positive or :negative.  The first corresponds to
    a two-tailed test: H1 is that `t-statistic' is not zero, but you are
    not specifying a direction.  If the parameter is :positive, H1 is that
    `t-statistic' is positive, and similarly for :negative.

    This implementation follows Numerical Recipes in C, section 6.3. *)
  if dof > 10000 then
    gaussian_significance t way
  else
    let dof = float dof in
    let a = beta_incomplete (dof *. 0.5) 0.5 (dof /. (dof +. (t * t))) in
      match way with
	Both -> a
      | Positive ->
	  let halfa = a *. 0.5 in
	    if t >= 0. then halfa else (1. -. halfa)
      | Negative ->
	  let halfa = a *. 0.5 in
	    if t >= 0. then (1. -. halfa) else halfa
*)	      
	      
let t025 =
  let table = [|
    nan;
    12.7;
    4.3;
    3.18;
    2.78;
    (* 5 *)
    2.57;
    2.45;
    2.36;
    2.31;
    2.26;
    (* 10 *)
    2.23;
    2.2;
    2.18;
    2.16;
    2.14;
    (* 15 *)
    2.13;
    2.12;
    2.11;
    2.10;
    2.09;
    (* 20 *)
    2.09;
    2.08;
    2.07;
    2.07;
    2.06;
    (* 25 *)
    2.06;
    2.06;
    2.05;
    2.05;
    2.04;
    (* 30 *)
  |] in
    (fun df ->
       if df < 30 then
	 table.(df)
       else if df < 40 then
	 2.03
       else if df < 60 then
	 2.01
       else if df < 120 then
	 1.99
       else
	 1.96)  


(***************** chi_squared *******************)


let chi_squared_significance x dof =
  (** probability of getting a value [x] or lower in a chi-squared
    distribution with [dof] *)
  1. -. (Functions.incomplete_gamma ((float dof) /. 2.) (x /. 2.))
    

(* EOF *)
