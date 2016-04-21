(* Heavily borrowed from lacaml lin_reg.ml *)

open Format
open Bigarray
open Lacaml.Impl.D
open Lacaml.Io

type dom =
  | Grid
  | Tiles

(* Loads a set of best pairs from a file name, suitable for
   doing the offline regression *)
let conv_pairs (lhs,rhs) run =
  let pairs = Recorded_run.get_best_pairs run in
    (Recorded_run.build_conv_matrix lhs pairs,
     Recorded_run.build_conv_matrix rhs pairs)


let just_pairs run =
  let pairs = Recorded_run.get_best_pairs run in
    pairs


let sol_pairs (lhs,rhs) run =
  let pairs = Recorded_run.get_solution_pairs run in
    (Recorded_run.build_conv_matrix lhs pairs,
     Recorded_run.build_conv_matrix rhs pairs)


(* Converts pairs from a file to matricies suitable for least
   squares calculations.  Dispalys a portion of the contents *)
let pairs_to_mats (val_array,t_array) =
  let a = (Mat.of_array val_array)
  and b = (Mat.of_array t_array) in
  printf
    "\
      @[<2>Predictor variables:\n\
        @\n\
        %a@]\n\
      @\n\
      @[<2>Response variable:\n\
        @\n\
        %a@]@\n\
      @\n"
    pp_fmat a
    pp_rfvec (Mat.col b 1);
    (a,b)


(* The get least squarse call
   v_mat is a
   t_mat is b (of ax = b)
   gelsd:

   clobbers v_mat - No info on what is there at this point.

   clobbers t_mat - Overwritten by the n-by-nrhs solution matrix X
                    If m >= n and rank = n, the residual sum-of-squarse
                    for the solution in the i-th column is given by the sum
                    of squarse of modulus of elements n+1:m in that column

   A is of form m x n where m observations and n features per observation
   B is of form m x nrhs where nrhs will usually be 1 (fit to a single run)

   n is going to be the number of unknows

   b gets over written by X

   X is the n-by-nrhs solution matrix X, so typically n by 1

 *)
let get_least_squares (v_mat, t_mat) =
  let v_copy = v_mat
  and t_copy = t_mat in
    (* the rank of a matrix is the maximal number of linearly independed
       columns in that matrix.  Here, rank is the rank of v_mat
       gelsd is a call in impl2_d
    *)
  let rank = gelsd v_copy t_copy in
  printf
    "\
      @[<2>Regression weights:\n\
        @\n\
        %a@]\n\
      @\n\
      Rank: %d@\n@\n"
    pp_rfvec (Mat.col t_copy 1)
    rank;
    (* the first n elements of t_copy are the weights *)
    t_copy


(* Sanity check call
   v_mat is the initial data vector
   res_mat is the answer returned from the least mean squares call
 *)
let sanity_check (v_mat, res_mat) =
  let res_copy = res_mat in
  let y = gemv v_mat (Mat.col res_mat 1)
  and b = Mat.col res_copy 1 in
  printf
    "\
      @[<2>Check result (must be close to 0):\n\
        @\n\
        %a@]@\n"
    pp_rfvec (Vec.sub y b)


let stream_lms learner (source_arrays, target) =
  let v_length = Array.length source_arrays.(0)
  and samples = Array.length source_arrays in
  let corrections = Array.create samples 0. in
  let show_ex, est, get_wt = learner v_length in
    for i = 0 to (samples - 1)
    do
	let a, _ = (show_ex source_arrays.(i) target.(i).(0)) in
	  corrections.(i) <- a
    done;
    (* corrections at the time, wt list, final estimator *)
    corrections, get_wt(), est


let rand_stream_lms ?(learner = Lms.init_nlms_momentum)
    (source_arrays, target) =
  let v_length = Array.length source_arrays.(0)
  and samples = Array.length source_arrays in
  let sequence = Array.create samples (-1) in
    for i = 0 to (samples - 1)
    do
      while (let ind = Random.int samples in
	       if sequence.(ind) = -1
	       then (sequence.(ind) <- i; false)
	       else true) do () done
    done;
    let corrections = Array.create samples 0. in
    let show_ex, est, get_wt = learner v_length in
      for i = 0 to (samples - 1)
      do
	(let rnd = sequence.(i) in
	 let a, _ = (show_ex source_arrays.(rnd) target.(rnd).(0)) in
	   corrections.(i) <- a)
      done;
      (* corrections at the time, wt list, final estimator *)
      corrections, get_wt(), est


(* Unit testing *)
let test_batch_regression () =
  let v_ar = Array.init 1000
    (fun i -> [|Random.float 100.; Random.float 10.;|]) in
  let t_ar = Array.init 1000
    (fun i -> [|v_ar.(i).(0) *. 2. +. v_ar.(i).(1) +. (* noise *)
		  (3. -. Random.float 5.)|]) in
    (* After regression, should be able to pull coefficients back out
       sol.{1,1} ~~ 2 and sol.{2,1} ~~ 1 *)
  let sol = get_least_squares (pairs_to_mats (v_ar,t_ar)) in
    sol

let test_streamed_regression learner seed =
  Random.init seed;
  (* Configure the data arrays *)
  let ar1 = Array.init 1000
    (fun i -> [|Random.float 100.; Random.float 10.;|]) in
  let ar2 = Array.init 1000
    (fun i -> [|ar1.(i).(0) *. 2. +. ar1.(i).(1)|]) in

(* get the solution, print vals and wts *)
  let _,sol,cf = stream_lms learner (ar1,ar2) in
    for i = 0 to ((Array.length ar1) - 1)
    do
      Verb.pe Verb.often "%f \t %f\n%!" (cf ar1.(i)) ar2.(i).(0)
    done;
    Verb.pe Verb.often "\nwt vector\n";
    for i = 0 to ((Array.length sol) - 1)
    do
      Verb.pe Verb.often "%f\t" sol.(i)
    done;
    Verb.pe Verb.often "\n%!"

(* EOF *)
