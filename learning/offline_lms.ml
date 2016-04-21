(** An offline least mean square learner using lacaml.

    @author eaburns
    @since 2010-01-13
*)

open Format
open Bigarray
open Lacaml.Impl.D
open Lacaml.Io
open Float_ref

let lms features targets =
  (** [lms features targets] takes an array of feature arrays and a
      corresponding array of target values.  The result is an array of
      coefficients from an offline least mean square regression. *)
  let feature_mat = Mat.of_array features in
  let target_mat = Mat.of_array (Array.map (fun t -> [| t |]) targets) in
    ignore (gelsd ~rcond:1e-4 feature_mat target_mat);
    Array.init (Array.length features.(0)) (fun i -> target_mat.{(i + 1), 1})


let test_batch_regression () =
  let v_ar = Array.init 1000
    (fun i -> [|Random.float 100.; Random.float 10.;|]) in
  let t_ar = Array.init 1000
    (fun i -> v_ar.(i).(0) *. 2. +. v_ar.(i).(1) +. (* noise *)
		  (3. -. Random.float 5.)) in
    (* After regression, should be able to pull coefficients back out
       sol.{1,1} ~~ 2 and sol.{2,1} ~~ 1 *)
  let sol =  lms v_ar t_ar in
    sol;;


(** {1 Robust Regression} ****************************************)
(************************************************************)
(*

  Least median of squares is a regression technique described in
  "Robust Regression and Outlier Detection" by Rousseeuw and Leroy
  (Chapter 5).

*)
(************************************************************)


let nsamples eps p prob =
  (** [nsamples eps p prob] computes the number of samples that we
      need given [eps] (the fraction of bad values in our data), [p]
      (the size of each sample) and [prob] (the probability that we
      get at least one good sample).  (See R&L pg. 198 equation
      1.2) *)
  (* Solve for m in: 1 - (1 - (1 - eps)^p)^m *)
  let q = 1. -. (1. -. eps) ** p in
    truncate (ceil (Math.logarithm (1. -. prob) q))


let median_square_err coeffs features targets =
  (** [median_square_err coeffs features targets] computes the median
      square error of a model. *)
  let compute_sq_error i =
    let y = targets.(i) and xs = features.(i) in
    let sum = float_ref 0. in
      Array.iteri (fun i x -> sum <-- !!sum +. coeffs.(i) *. x) xs;
      (y -. !!sum) ** 2.
  in
  let n = Array.length features in
  let half_n = n / 2 in
  let errs = Array.init n compute_sq_error in
    Array.sort Math.fcompare errs;
    if Math.even_p n
    then (errs.(half_n) +. errs.(half_n + 1)) /. 2.
    else errs.(half_n + 1)


let least_median_of_squares ?(prob=0.95) ?(eps=0.25) p features targets =
  (** [least_median_of_squares ?prob ?eps p features targets] computes
      the least median of squares fit by creating samples of size [p]
      and performing a least squares fit to each. *)
  let m = nsamples eps (float p) prob in
  let pairs = Wrarray.combine features targets in
  let best = ref [| |] in
  let best_med = ref nan in
    Verb.pr Verb.optional "Computing %d samples of size %d\n" m p;
    for i = 1 to m do
      let sample = Wrarray.sample p pairs in
      let fs, ts = Wrarray.split sample in
      let coeffs = lms fs ts in
      let med_sq = median_square_err coeffs features targets in
	if !best = [| |] || med_sq < !best_med
	then begin
	  best := coeffs;
	  best_med := med_sq
	end
    done;
    !best


(** {1 Polynomials} ****************************************)

let make_poly_fit lms degree xs ys =
  (** [make_poly_fit lms degree xs ys] fits a polynomial to the given
      set of points.  The result is a tupel of the coefficients and
      the function to compute the y of x. *)
  let poly_coeffs x = Array.init (degree + 1) (fun i -> x ** (float i)) in
  let features = Array.map poly_coeffs xs in
  let coeffs = lms features ys in
    coeffs, (fun x ->
	       let sum = float_ref coeffs.(0) in
	       let prod = float_ref 1. in
		 for i = 1 to degree do
		   prod <-- !!prod *. x;
		   sum <-- !!sum +. coeffs.(i) *. !!prod
		 done;
		 !!sum)


let lms_poly_fit degree xs ys = make_poly_fit lms degree xs ys


let robust_poly_fit degree xs ?(p=(Array.length xs) / 3) ys =
  let prob = None and eps = None in
    make_poly_fit (least_median_of_squares ?prob ?eps p) degree xs ys
