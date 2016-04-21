(* $Id: stats.ml,v 1.1 2003/11/07 20:56:44 ruml Exp ruml $

   basic stats
*)


let mean a =
  (Vector.sum a) /. (float (Array.length a))


let sse a =
  let mean = mean a
  and sum = ref 0. in
    Array.iter (fun x -> Math.incf sum (Math.square (x -. mean))) a;
    !sum


let variance a =
  let l = Array.length a in
    if l < 2 then
      invalid_arg "Stats.variance got array with fewer than 2 elements"
    else
      (sse a) /. (float (l - 1))


let vaf pairs =
  (** (model, truth) array *)
  let truth = Array.map snd pairs in
  let sse = sse truth
  and err = ref 0. in
    Array.iter (fun (m,t) ->
		  Math.incf err (Math.square (m -. t)))
      pairs;
    1. -. (Math.div0 !err sse)


let compare_floats_ascending x (y : float) =
  if x < y then -1 else if x > y then 1 else 0


let tails a conf =
  (** entries in [a] such that [conf] of the values lie between them.
    destructively sorts [a] *)
  Array.sort compare_floats_ascending a;
  let n = Array.length a in
  let nf = float n
  and p = (1. -. conf) /. 2. in
  let i = max 0 (int_of_float (floor (nf *. p)))
  and j = min (n-1) (int_of_float (floor (nf *. (1. -. p)))) in
    a.(i), a.(j)


let median a =
  (** destructively sorts [a] *)
  Array.sort compare_floats_ascending a;
  let len = Array.length a in
  let l2 = len / 2 in
    if Math.even_p len then
      (a.(l2 - 1) +. a.(l2)) /. 2.
    else
      a.(l2)



(***************** confidence intervals ****************)


let binomial_confidence_interval sampled_p num_samples confidence =
  (* otherwise should perhaps use Student's t distribution *)
  assert (num_samples > 99);
  let k = Probability.z_critical_point confidence
  and n = float num_samples in
  let k2 = k *. k
  and n2 = n *. n in
  let scaling = 1. /. (1. +. (k2 /. n))
  and center = sampled_p +. (k2 /. (2. *. n))
  and width = k *. sqrt (((sampled_p *. (1. -. sampled_p)) /. n) +.
			   (k2 /. (4. *. n2))) in
  let low = max (scaling *. (center -. width)) 0.
  and high = min (scaling *. (center +. width)) 1. in
    low, high

(*
let mean_confidence_radius mean stddev num_samples confidence =
  (** confidence intervals on the mean of some data *)
  let dof = num_samples - 1 in
    if dof = 0 then
      0.
    else
      let x = t_critical_point ((1. -. confidence) /. 2) dof in
	(x *. stddev) /. (sqrt (float num_samples))


let mean_confidence_interval mean stddev num_samples confidence =
  let r = mean_confidence_radius mean stddev num_samples confidence in
    mean -. r, mean +. r
*)

(***************** contingency tables ****************)


let x_square mat =
  (** Returns an x-sqaured variable for the hypothesis that the cells
    in the table are the product of the marginal probabilities.  It is
    asympototically distributed as a chi-square variable with (cols - 1)
    * (rows - 1) dof.  Note that the more data one has, the easier it is
    to disprove the hypothesis, so this test is NOT suitable as a
    measure of association.  It is also badly behaved when cell
    frequencies are or approach 0 *)
  let row_total = Matrix.row_sums mat
  and col_total = Matrix.col_sums mat
  and sum = ref 0. in
  let total = Vector.sum row_total in
    Matrix.iterij (fun i j x ->
		     let expected = row_total.(i) *. (col_total.(j) /.
							total) in
		     let diff = Math.square (x -. expected) in
		       if expected = 0. then
			 (if diff <> 0. then
			    (* infinity for x/0, ignore 0/0 *)
			    sum := infinity)
		       else
			 Math.incf sum (diff /. expected))
      mat;
    !sum



(* This code has to get moved, and very soon *)
type tails =
  | Both
  | Positive
  | Negative


let students_t_significance ?(tails = Both) t_stat dfreedom =
(*
(defun students-t-significance (t-statistic dof
                               &optional (tails :both))
 "Student's distribution is much like the Gaussian distribution except with
heavier tails, depending on the number of degrees of freedom, `dof.'  As
`dof' goes to infinity, Student's distribution approaches the Gaussian.
This function computes the significance of `t-statistic.'  Values range
from 0.0 to 1.0: small values suggest that the null hypothesis---that
`t-statistic' is drawn from a t distribution---should be rejected.  The
`t-statistic' parameter should be a float, while `dof' should be an integer.

The null hypothesis is roughly that `t-statistic' is zero; you must
specify your alternative hypothesis (H1) via the `tails' parameter,
which must be :both, :positive or :negative.  The first corresponds to
a two-tailed test: H1 is that `t-statistic' is not zero, but you are
not specifying a direction.  If the parameter is :positive, H1 is that
`t-statistic' is positive, and similarly for :negative.

This implementation follows Numerical Recipes in C, section 6.3."
 ;;(check-type t-statistic float)
 ;;(check-type dof integer)
 (check-type tails (member :both :positive :negative))
 ;;(setf dof (float dof t-statistic))
 (if (> dof 10000)
     ;; avoid errors from beta-incomplete
     (gaussian-significance t-statistic tails)
   (let ((a (beta-incomplete ( * 0.5 dof)
                             0.5 (/ dof (+ dof (square t-statistic))))))
     ;; A is 2*Integral from (abs t-statistic) to Infinity of t-distribution
     (ecase tails
       (:both a)
       ;; The TI compiler actually does the right thing with the following code:
       ;; the repeated expressions only appear once in the object code!
       (:positive (if (plusp t-statistic)
                      ( * .5 a)
                    (- 1.0 ( * .5 a))))
       (:negative (if (plusp t-statistic)
                      (- 1.0 ( * .5 a))
                    ( * .5 a)))))))
*)
  if dfreedom > 10000
  then failwith "Gaussian-significante not implemented!"
  else
    (let dof = float_of_int dfreedom in
     let a = Functions.beta_incomplete (dof *. 0.5) 0.5
       (dof /. (dof +. (t_stat ** 2.)))
     in
       match tails with
	 | Both -> a
	 | Positive -> if t_stat > 0. then a *. 0.5 else (1. -. (a *. 0.5))
	 | Negative -> if t_stat > 0. then (1. -. (a *. 0.5)) else (0.5 *. a))


let t_critical_point ?(accuracy = 0.0001) percent_in_right_tail dfreedom =
(*(defun t-critical-point (percent-in-right-tail dof
  &optional (accuracy 1.0d-9))
  (compute-t-critical-point percent-in-right-tail dof accuracy))*)
(*(defun-memo compute-t-critical-point (percent-in-right-tail dof accuracy)
 (values (nearby-root #'(lambda (x)
                          (- (students-t-significance x dof :positive)
                             percent-in-right-tail))
                      :guess 1.95d0    ; gaussian, dof = inf
                      ;; should be small to ensure big value to detect diff
                      :another-guess 2.0d0
                      ;; 638.0d0                       ; %=.0005, dof=1
                      :accuracy accuracy)))*)
  Num_opt.invert
    (fun x -> students_t_significance ~tails:Positive x dfreedom)
    percent_in_right_tail
    1.95 2. accuracy 314159



let mean_confidence_interval ?(conf = 0.95) mean std_deviation sample_count =
(*
  defun mean-confidence-interval (sample-mean standard-deviation num-samples
                                &optional (confidence 0.95))
 "Returns low, high, range."
 (let ((dof (1- num-samples))
        (radius (if (= dof 0)
                    0
                  (x (t-critical-point (/ (- 1 confidence)
                                          2)
                                       dof)
                     (/ standard-deviation
                        (sqrt num-samples))))))
   (values (- sample-mean radius)
           (+ sample-mean radius)
           (x 2 radius))))
*)
  let degrees_freedom = sample_count - 1 in
  let radius = (if degrees_freedom = 0 then 0.
		else (fst
			(t_critical_point
			 ((1. -. conf) /. 2.) degrees_freedom) *.
			(std_deviation /. (sqrt (float_of_int sample_count)))))
  in
    (mean,
     mean -. radius,
     mean +. radius,
     radius *. 2.)



let t_test_two_sample ?(tails = Both) ?(h0_mean = 0.) data_1 data_2 =
(*
(defun t-test-two-sample (seq-1 seq-2 &key (tails :both)
                                          (h0-mean 0))
 "Returns the t-statistic for the difference in the means of two samples,
which should each be a sequence of numbers.  Let D=mean1-mean2.  The null
hypothesis is that D=0.  The alternative hypothesis is specified by `tails':
`:both' means D/=0, `:positive' means D>0, and `:negative' means D<0.  Unless
you're using :both tails, be careful what order the two samples are in: it
matters!

The function also returns the significance, the standard error, and the degrees
of freedom."
  (multiple-value-bind
      (squares1 mean1 len1) (sample-sum-of-squared-error seq-1)
  (multiple-value-bind
      (squares2 mean2 len2) (sample-sum-of-squared-error seq-2)
    (let* ((dof (+ len1 len2 -2))
           (pooled-variance (/ (+ squares1 squares2)
                               dof))
           (std-error (sqrt ( * pooled-variance (+ (/ len1) (/ len2)))))
           (t-statistic (/ (- (- mean1 mean2) h0-mean)
                           std-error))
           (significance (students-t-significance t-statistic dof tails)))
      (values t-statistic significance std-error dof)))))
*)
  let mean_1 = mean data_1
  and mean_2 = mean data_2
  and var_1 = variance data_1
  and var_2 = variance data_2
  and samples_1 = Array.length data_1
  and samples_2 = Array.length data_2 in
  let degrees_freedom = samples_1 + samples_2 - 2 in
(*  let pooled_variance =  (var_1 +. var_2) /. (float_of_int degrees_freedom) in*)
  let std_error = sqrt ((var_1 /. (float_of_int samples_1)) +. (var_2 /. (float_of_int samples_2))) in
(*  let std_error = sqrt (pooled_variance *.
			  ((1. /. (float_of_int samples_1)) +.
			     (1. /. (float_of_int samples_2)))) in*)
  let t_stat = ((mean_1 -. mean_2) -. h0_mean) /.  std_error in
  let significance = students_t_significance ~tails:tails
    t_stat degrees_freedom in
    significance

let sign_test a b =
  (** [sign_test a b] performs a sign test on paired data [a] and [b].
      [a] and [b] are float arrays where the data is aligned so that
      [a.(i)] is paired with [b.(i)]. *)
  let plus = ref 0 and n = ref 0 in
    assert ((Array.length a) = (Array.length b));
    for i = 0 to (Array.length a) - 1 do
      let diff = a.(i) -. b.(i) in
	if not (Math.is_zero diff)
	then begin
	  incr n;
	  if diff > 0. then incr plus
	end
    done;
    let n = float !n and plus = float !plus in
    let mu = n /. 2. and sigma = sqrt (n /. 4.) in
    let z = (plus -. mu) /. sigma in
      2. *. (Normal.cdf z)


let wilcoxon_signed_rank_test a b =
  (** [wilcoxon_signed_rank_test a b] performs a Wilcoxon signed-rank
      test on paired data [a] and [b].  [a] and [b] are float arrays
      where the data is aligned so that [a.(i)] is paired with
      [b.(i)]. *)
  let diffs = Wrarray.fold_left2 (fun acc a b ->
				    let diff = a -. b in
				      if not (Math.is_zero diff)
				      then diff :: acc
				      else acc) [] a b
  in
  let rec sums ?(plus_sum=0.) ?(minus_sum=0.) ?(same=[]) next_rank = function
    | [] -> plus_sum, minus_sum
    | x :: y :: tl when (abs_float x) = (abs_float y) ->
	sums ~plus_sum ~minus_sum ~same:(x :: same) next_rank (y :: tl)
    | x :: tl ->
	let num_beforef = float (List.length same)
	and next_rankf = float next_rank in
	let mean = (2. *. next_rankf -. num_beforef) /. 2. in
	  (* mean rank incase there are more than 1. *)
	let plus_sum, minus_sum =
	  List.fold_left (fun (ps, ms) v ->
			    if v < 0.
			    then ps, ms +. mean
			    else ps +. mean, ms)
	    (plus_sum, minus_sum) (x ::same)
	in sums ~plus_sum ~minus_sum (next_rank + 1) tl
  in
  let sorted =
    List.sort (fun a b -> compare (abs_float a) (abs_float b)) diffs
  in
  let plus_sum, minus_sum = sums 1 sorted in
  let t = if plus_sum < minus_sum then plus_sum else minus_sum in
  let n = float (List.length diffs) in
  let n_n_plus_1 = n *. (n +. 1.) in
  let mu = n_n_plus_1 /. 4. in
  let sigma = sqrt ((n_n_plus_1 *. (2. *. n +. 1.)) /. 24.) in
  let z = (t -. mu) /. sigma in
  let p = 2. *. (Normal.cdf z) in
    assert (p >= 0.);
    p


(* EOF *)
