(** Nonparametric regression.

    @author eaburns
    @since 2010-07-07
*)

open Float_ref

module Kernel = struct

  let epanechnikov=
    (* According to "All of Statistics" by Larry Wasserman, the
       Epanechnikov kernel can be shown to be optimal at minimizing
       square error, but it doesn't seem to work as well as the Gaussian
       kernel for the functions that I have tried. *)
    let root5 = sqrt 5. in
      (fun x ->
	 if (abs_float x) < root5
	 then 0.75 *. (1. -. (x *. x) /. 5.) /. root5
	 else 0.)


  let gaussian =
    let inv_root2pi = 1. /. (sqrt (2. *. Math.pi)) in
      (fun x -> inv_root2pi *. (exp (~-.(x *. x) /. 2.)))

end


let nadaraya_watson_kernels kernel points h i =
  (** [nadaraya_watson_weight kernel points h i] build a function that
      gets the weight for the [i]th kernel in NW regression. *)
  let x_i, _ = points.(i) in
    (fun x -> (kernel ((x -. x_i) /. h)))


let make_nadaraya_watson_kernels kernel points h =
  (** [make_nadaraya_watson_kernels kernel points h] makes a kernels
      array. *)
  let n = Array.length points in
    Array.init n (nadaraya_watson_kernels kernel points h)


let nadaraya_watson_norm_factor kernels x =
  (** [nadaraya_watson_norm_factor kernels x] *)
  let n = Array.length kernels in
  let sum = float_ref 0. in
    for i = 0 to n - 1 do sum <-- !!sum +. (kernels.(i) x) done;
    !!sum


let make_nadaraya_watson_learner ?kernels kernel points h =
  (** [make_nadaraya_watson_learner ?kernels kernel points h] makes an
      NW regression funciton given the kernel, training points and
      bandwidth ([h]). *)
  let n = Array.length points in
  let k = match kernels with
    | Some w -> w
    | None -> make_nadaraya_watson_kernels kernel points h in
  let sum = float_ref 0. in
    (fun x ->
       let norm = nadaraya_watson_norm_factor k x in
	 sum <-- 0.;
	 for i = 0 to n - 1 do
	   let _, y_i = points.(i) in
	     sum <-- !!sum +. (k.(i) x) *. y_i
	 done;
	 !!sum /. norm)



let find_h xvalidate_size tolerance make_learner points =
  let pts = match xvalidate_size with
    | None -> points
    | Some size when size > (Array.length points) -> points
    | Some size -> Wrarray.sample size points
  in Xvalidate.Leave_one_out.minimize_sse ?tolerance pts make_learner 0. 5.


let nadaraya_watson ?xvalidate_size ?tolerance kernel points =
  (** [nadaraya_watson ?xvalidate_size ?tolerance kernel points] makes a
      Nadaraya-Watson function approximation using leave-one-out cross
      validation to find the bandwidth by minimizing the sum of
      squared error. *)
  let make_learner = make_nadaraya_watson_learner ?kernels:None kernel in
  let h = find_h xvalidate_size tolerance make_learner points in
    match classify_float h with
      | FP_subnormal | FP_normal -> make_learner points  h
      | FP_nan -> failwith "Nan h"
      | FP_zero -> failwith "Zero h"
      | FP_infinite -> failwith "Infinite h"


let nadaraya_watson_confidence kernels points est h width alpha a b =
  (** [nadaraya_watson_confidence kernels points est h width alpha a
      b] gets a confidence interval function assmuing a range of
      [a]-[b] on the data.  This function assumes that [points] is
      sorted on x. *)
  let n = Array.length points in
  let k = kernels in
  let diff_sq_sum = float_ref 0. in
    for i = 0 to n - 2 do
      let _, y = points.(i) in
      let _, y_next = points.(i + 1) in
	diff_sq_sum <-- !!diff_sq_sum +. ((y_next -. y) ** 2.);
    done;
    let var = (1. /. (2. *. (float n -. 1.))) *. !!diff_sq_sum in
    let sigma = sqrt var in
    let std_err x =
      (* standard error at x *)
      let norm = nadaraya_watson_norm_factor kernels x in
      let w2_sum = float_ref 0. in
	for i = 0 to n - 1 do
	  let w_i = (k.(i) x) /. norm in
	    w2_sum <-- !!w2_sum +. w_i *. w_i;
	done;
	sigma *. (sqrt !!w2_sum)
    in
    let m = (b -. a) /. width in
    let q = Normal.cdf_inv ((1. +. ((1. -. alpha) ** (1. /. m))) /. 2.) in
      (fun x -> (std_err x) *. q)


let nadaraya_watson_with_confidence
    ?xvalidate_size ?tolerance ?width kernel points ?(alpha=0.05) a b =
  (** [nadaraya_watson_with_confidence ?xvalidate_size ?tolerance
      ?width kernel points a b] makes a Nadaraya-Watson function
      approximation using leave-one-out cross validation to find the
      bandwidth by minimizing the sum of squared error and a
      confidence band function for computing the upper and lower
      confidence at each point.

      [a] is the minimum x value and [b] is the maximum x value.
  *)
  Array.sort (fun (x0, _) (x1, _) -> compare x0 x1) points;
  let make_learner = make_nadaraya_watson_learner ?kernels:None kernel in
  let h = find_h xvalidate_size tolerance make_learner points in
  let kernels = make_nadaraya_watson_kernels kernel points h in
  let est = make_nadaraya_watson_learner ~kernels kernel points h in
  let width = match width with
    | Some w -> w
    | None -> 3. *. h
	(* 3h is suppose to be a good width for Gaussian kernels. *)
  in
  let conf =
    nadaraya_watson_confidence kernels points est h width alpha a b
  in est, conf


(************************************************************)

let test_nw ochan min max f =
  (* Test the Nadaraya-Watson learner using SPT plots. *)
  let training =
    Array.init 100 (fun _ ->
		      let x = (Random.float (max -. min)) +. min
		      in x, f x)
  and testing =
    Array.init 100 (fun _ ->
		      let x = (Random.float (max -. min)) +. min
		      in x, f x) in
  let est, conf =
    nadaraya_watson_with_confidence Kernel.gaussian training min max
  in
  let actual = Array.append testing training in
    Array.sort (fun (x0, _) (x1, _) -> compare x0 x1) actual;
    Array.sort (fun (x0, _) (x1, _) -> compare x0 x1) testing;
    Printf.fprintf ochan "(let* ((acutal (";
    Array.iter (fun (x, y) -> Printf.fprintf ochan "(%f %f)" x y) actual;
    Printf.fprintf ochan "))\n       (estimate (";
    Array.iter (fun (x, _) ->
		  let y = est x in
		    Printf.fprintf ochan "(%f %f)" x y) testing;

    Printf.fprintf ochan "))\n       (training (";
    Array.iter (fun (x, y) -> Printf.fprintf ochan "(%f %f)" x y) training;

    Printf.fprintf ochan "))\n       (conf-up (";
    Array.iter (fun (x, _) ->
		  let y = est x and u = conf x in
		    Printf.fprintf ochan "(%f %f)" x (y +. u)) testing;
    Printf.fprintf ochan "))\n       (conf-lower (";
    Array.iter (fun (x, _) ->
		  let y = est x and l = conf x in
		    Printf.fprintf ochan "(%f %f)" x (y -. l)) testing;


    Printf.fprintf ochan "))\n";
    Printf.fprintf ochan
      "(ds-acutal (line-dataset :name \"acutal\"\n";
    Printf.fprintf ochan "                                 :points acutal))\n";
    Printf.fprintf ochan
      "(ds-estimate (scatter-dataset :name \"estimate\"\n";
    Printf.fprintf ochan
      "                                   :points estimate))\n";
    Printf.fprintf ochan
      "(ds-training (scatter-dataset :name \"training\"\n";
    Printf.fprintf ochan
      "                                   :glyph \"cross\"\n";
    Printf.fprintf ochan
      "                                   :points training))\n";
    Printf.fprintf ochan
      "(ds-upper (line-dataset :name \"conf-upper\"\n";
    Printf.fprintf ochan
      "                                 :points conf-up))\n";
    Printf.fprintf ochan
      "(ds-lower (line-dataset :name \"conf-lower\"\n";
    Printf.fprintf ochan
      "                                 :points conf-lower))\n";
    Printf.fprintf ochan
      "(plot (num-by-num-plot :title \"Nadaraya-Wattson test\"\n";
    Printf.fprintf ochan "     :legend (legend-location :upper-right)\n";
    Printf.fprintf ochan "                       :dataset ds-acutal\n";
    Printf.fprintf ochan "                       :dataset ds-upper\n";
    Printf.fprintf ochan "                       :dataset ds-lower\n";
    Printf.fprintf ochan "                       :dataset ds-training\n";
    Printf.fprintf ochan "                       :dataset ds-estimate)\n";
    Printf.fprintf ochan ")) (display plot))\n"


(*
Wrio.with_outfile "test.spt"
(fun ochan -> test_nw ochan ~-.10. 10. (fun x -> 2. ** x));;
*)
