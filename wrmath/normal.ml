(** Functions on multivariate normal distributions.

    @author eaburns
    @since 2009-08-22
*)

open Matrix2d.Ops

type t = {
  mean : Matrix2d.t;
  (* the mean vector. *)

  cov : Matrix2d.t;
  (* the covariance matrix. *)

  (* The following are not strictly parameters of a gaussian, but they
     are used when computing the PDF and they are not cheap to compute so
     we store them here for efficiency. *)

  cov_inv: Matrix2d.t;
  (* inverse of the cov matrix. *)

  a: float;
  (*  1 / ( ((2 * pi)^(N/2)) * (sqrt |cov|) ) *)
}


let sanity_check t =
  (** [sanity_check t] make sure the normal makes sense. *)
(*
  let valid_float f = match classify_float f with
    | FP_normal | FP_subnormal | FP_zero -> true
    | _ -> false
  in
    assert (Wrarray.for_all (fun f -> valid_float f) t.mean);
    assert (Wrarray.for_all (fun a ->
			       Wrarray.for_all (fun f -> valid_float f) a)
	      t.cov);
    assert ((Matrix.determinant t.cov) > 0.);
    assert (Wrarray.for_all (fun a ->
			       Wrarray.for_all (fun f -> valid_float f) a)
	      t.cov_inv);
    assert (valid_float t.a)
*)
    ()


let make mean cov =
  (** [make mean cov] make a gaussian given the mean vector [mean] and
      the covariance matrix [cov]. *)
  let twopi = 2. *. Math.pi in
  let n = mean.Matrix2d.rows in
    if mean.Matrix2d.cols <> 1
    then invalid_arg "Normal.make: mean should be a Rx1 matrix (a vector).";
    if (cov.Matrix2d.rows) <> n || (cov.Matrix2d.cols) <> n
    then invalid_arg "Normal.make: covarinace matrix has incorrect size."
    else begin
      let t =
	{
	  mean = mean;
	  cov = cov;
	  cov_inv = inv cov;
	  a = 1. /. ( (twopi ** ((float n) /. 2.)) *. (sqrt (det cov)) );
	}
      in
	sanity_check t;
	t
    end

let of_arrays mean cov =
  (** [of_arrays mean cov] makes a new normal distribution given the
      mean and covariance in the two OCaml arrays [mean] and [cov]
      ([cov] is an array of arrays). *)
  let mean = Matrix2d.of_vector mean
  and cov = Matrix2d.of_array2d_copy cov
  in make mean cov


let hallucinate
    ?(mean_min=(~-.100.)) ?(mean_max=100.)
    ?(std_max=100.)
    d =
  (** [hallucinate ?range ?mean_min ?mean_max ?std_max d] creates a
      random gaussian of [d] dimensions. *)
  let rand_float min max = (Random.float (max -. min)) +. min in
  let mean = Matrix2d.init d 1 (fun _ _ -> rand_float mean_min mean_max) in
  let cov = Matrix2d.make d d 0. in
  let rec fill_cov () =
    (* the cov matrix must have a positive determinant. *)
    for i = 0 to d - 1 do
      for j = 0 to d - 1 do
	if j < i
	then Matrix2d.set cov i j (Matrix2d.at cov j i)
	else Matrix2d.set cov i j (rand_float 0. (std_max ** 2.))
      done
    done;
    if (det cov) <= 0. then fill_cov () else ()
  in fill_cov ();
    make mean cov


let pdf t x =
  (** [pdf t x] computes the probability of [x] given the distribution
      [t]. *)
  let x_minus_mu = x -// t.mean in
  let b =
    scaled ((tr (t.cov_inv *// x_minus_mu)) *// x_minus_mu) ~-.0.5
  in
    assert (b.Matrix2d.rows = 1);
    assert (b.Matrix2d.cols = 1);
    t.a *. (exp (Matrix2d.at b 0 0))


let pdf_vector t v =
  (** [pdf_vector t v] computes the probability of [v] given the
      dist. [t] where [v] is an OCaml array of floats. *)
  pdf t (Matrix2d.of_vector v)


(******************** 1-dimensional ********************)

let make_1d mean std =
  (** [make_1d mean std] makes a 1-dimensional gaussian with a mean of
      [mean] and a standard deviation of [std]. *)
  make (Matrix2d.make 1 1 mean) (Matrix2d.make 1 1 (std ** 2.))

let pdf_1d t x = pdf t (Matrix2d.make 1 1 x)
  (** [pdf_1d t x] computes the probability of [x] given the
      1-dimensional gaussian [t].  You probably don't want to use this
      if you are looking for fast 1d normal PDF computations, instead
      use [make_1d_pdf] which doesn't convert 1d data into a
      multi-variate gaussian. *)


let make_1d_pdf mean std =
  (** [make_1d_pdf mean std] make a probability density function for a
      1d gaussian.  This by-passes the multi-variate gaussian type 't'
      defined here and, instead makes a function for doing quick PDF
      computations. *)
  let a = 1.0 /. (std *. (sqrt (Math.pi *. 2.0))) in
  let two_times_cov = 2.0 *. std *. std in
    (fun x ->
       a *. (exp ~-.(((x -. mean) ** 2.0) /. two_times_cov)))


(******************** 2-dimensional ********************)

let make_2d meanx meany stdx stdy =
  (** [make_2d meanx meany stdx stdy] makes a 2-dimensional
      gaussian. *)
  let mean = Matrix2d.make 2 1 meanx in
  let cov = Matrix2d.make 2 2 0. in
    Matrix2d.set mean 1 0 meany;
    Matrix2d.set cov 0 0 (stdx ** 2.);
    Matrix2d.set cov 1 1 (stdy ** 2.);
    make mean cov

let pdf_2d t x y =
  (** [pdf_2d t x y] computes the probability of [x],[y] given the
      2-dimensional gaussian [t]. *)
  let vec = Matrix2d.make 2 1 x in
    Matrix2d.set vec 1 0 y;
    pdf t vec

(** {6 The cumulative density function for a 1-dimensional normal} ******)

let sqrt2 = sqrt 2.

let cdf x =
  (** [cdf x] is the cumulative density under the stardard normal. *)
  let wt = 0.5 *. (1. +. (Functions.erf (x /. sqrt2))) in
    (try assert (wt >= 0.)
     with _ ->
       Printf.printf "x=%g gives wt=%g\n" x wt;
       assert false);
      wt

let cdf_inv x =
  (** [cdf_inv x] the inverse of the normal CDF.  I guess that we also
      have this function in Probability.normal_quantile, but I don't
      know how that works and I alread implemented this -- eab. *)
  (sqrt2 *. (Functions.erf_inv (2. *. x -. 1.)))


