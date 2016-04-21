(* $Id: lms.ml,v 1.1 2003/07/18 21:22:40 ruml Exp ruml $

   on-line linear regression

   Lisp code ~ruml/library/code/lisp/learning?
*)

open Float_ref

type learner = int ->
  (float array -> float -> float * float) * (float array -> float) *
  (unit -> float array)


(********* basic LMS/perceptron/widrow-hoff/adaline *********)

let init_lms
    ?(initial_weights = None)
    ?(initial_weight = 0.)
    ?(learning_rate = 0.2)
    ?(bias = Fn.identity)
    n =
  (** rather simple *)
  assert ((0. < learning_rate) && (learning_rate <= 1.));
  assert (match initial_weights with
	    | None -> true
	    | Some ar -> (Array.length ar = n));
  let num = float_ref 0.
  and est = float_ref 0. in
  let weights =
    match initial_weights with
	None -> Array.create n initial_weight
      | Some ar -> ar in

  let show_ex pat target =
    num <-- 0.;
    est <-- 0.;
    for i = 0 to n-1 do
      let this = pat.(i) in
	addf num this;
	addf est (this *. weights.(i))
    done;
    if not (Math.finite_p !!est) then est <-- infinity;
    let under = target -. !!est in
    let under = bias under in
    let update = learning_rate *. (under /. !!num) in
      for i = 0 to n-1 do
	weights.(i) <- weights.(i) +. (pat.(i) *. update)
      done;
      !!est, under
  and est pat =
    Vector.dot weights pat
  and get () =
    weights
  in
    show_ex, est, get

let init_lms2
    ?(initial_weights = None)
    ?(initial_weight = 0.)
    ?(learning_rate = 0.2)
    n =
  (** rather simple *)
  assert ((0. < learning_rate) && (learning_rate <= 1.));
  assert (match initial_weights with
	    | None -> true
	    | Some ar -> (Array.length ar = n));
  let weights =
    match initial_weights with
	None -> Array.create n initial_weight
      | Some ar -> ar in
  let show_ex2 pat target =
    let est = Vector.dot pat weights in
    let under = target -. est in
    let update = learning_rate *. under in
      for i = 0 to n-1 do
	weights.(i) <- weights.(i) +. (pat.(i) *. update)
      done;
      est, under
  and est pat =
    Vector.dot weights pat
  and get () =
    weights
  in
    show_ex2, est, get


(********* normalized LMS with momentum *********)


let init_nlms_momentum
    ?(initial_weights = None)
    ?(initial_weight = 0.)
    ?(learning_rate = 0.8)
    ?(momentum_rate = 0.25)
    ?(exp_sq_noise = 1.0)
    n =
  assert ((0. < learning_rate) && (learning_rate < 2.));
  assert (exp_sq_noise > 0.);
  assert (match initial_weights with
	    | None -> true
	    | Some ar -> (Array.length ar = n));
  let weights =
    (match initial_weights with
	 None -> Array.create n initial_weight
       | Some ar -> ar)
  and momentum = Array.create n 0. in
  let show_example pat target =
    let est = Vector.dot weights pat in
    let under = target -. est in
      (let step = (learning_rate *. under) /.
	 (exp_sq_noise +. (Vector.dot pat pat))
       in
	 for i = 0 to (n-1) do
	   let s = (pat.(i) *. step) +. (momentum.(i) *. momentum_rate)
	   in
	     weights.(i) <- weights.(i) +. s;
	     momentum.(i) <- s
	 done);
      est, under
  and compute_estimate pat =
    Vector.dot weights pat
  and get_weights () =
    weights
  in
    show_example, compute_estimate, get_weights


(********* adaptive learning rate *********)


let fmin (a : float) b =
  if a > b then b else a

let fmax (a : float) b =
  if b > a then b else a


(* NLMS with Murata et al's adaptive learning rate estimation *)
let init_nlms_flow
  ?(initial_weights = None)
  ?(initial_weight = 0.)
  ?(initial_learning_rate = 0.8)
  ?(meta_learning_rate = 0.002)
  ?(exp_sq_noise = 1.0)
  ?(normalization_scale = 20.)
  ?(leakiness = 0.05)
  ?(max_learning_rate = 1.9)
  ?(min_learning_rate = 0.001)
  n =
(*  failwith "flow might be buggy!";*)
  assert (meta_learning_rate > 0.);
  assert (exp_sq_noise > 0.);
  assert (normalization_scale >= 1.);
  assert ((0. < leakiness) && (leakiness < 1.));
  assert (match initial_weights with
	    | None -> true
	    | Some ar -> (Array.length ar = n));
  let weights =
    (match initial_weights with
	 None -> Array.create n initial_weight
       | Some ar -> ar)
  and r = Array.create n 0.
  and learning_rate = float_ref initial_learning_rate
  and max_r_norm = float_ref 0.
  and one_minus_leak = 1. -. leakiness in
  let show_example pat target =
    let learning_coeff = !!learning_rate /.
			   (exp_sq_noise +. (Vector.dot pat pat))
    and est = Vector.dot weights pat in
    let over = est -. target in
      for i = 0 to (n-1) do
	let flow = over *. pat.(i) in
	  weights.(i) <- weights.(i) -. (learning_coeff *. flow);
	  r.(i) <- (one_minus_leak *. r.(i)) +. (leakiness *. flow)
      done;
      (let new_r_norm = Vector.norm r in
	 max_r_norm <-- fmax !!max_r_norm new_r_norm;
	 let beta = normalization_scale /. !!max_r_norm in
	 let desired_rate = !!learning_rate +.
			      (meta_learning_rate *. !!learning_rate *.
				 ((beta *. new_r_norm) -. !!learning_rate)) in
	   learning_rate <-- (fmax min_learning_rate
			       (fmin max_learning_rate desired_rate)));
      est, (-. over)
  and compute_estimate pat =
    Vector.dot weights pat
  and get_weights () =
    weights
  in
    show_example, compute_estimate, get_weights


let init_nlms_flow_default n =
  init_nlms_flow n


(********* stationary test case *********)


let test ?(trials = 100) ?(verb = true) ?(seed = 31415) learner =
  Random.init seed;
  let f x y =
    0.5 +. (0.6 *. x) +. (0.7 *. y)
  in
  let show, est, get = learner 3 in
  let print_wts () =
    print_string "Wts: "; Vector.write_line stdout (get ())
  and make_vec x y =
    Array.of_list [1.; x; y]
  in
    Wrutils.ntimes (fun () ->
		    if verb then print_wts ();
		    let x = Random.float 1. in
		    let y = Random.float 1. in
		    let z = f x y in
		    let est, err = show (make_vec x y) z
		    in
		      Verb.pe Verb.debug
			"For %f and %f (= %f), est %f, err %f.\n"
			x y z est err)
      trials;
    Verb.pe Verb.debug "-- done with training ---\n";
    List.iter (fun (x,y) ->
		 let est = est (make_vec x y)
		 and z = f x y in
		   Verb.pe Verb.debug "For %f and %f got %f instead of %f.\n"
		     x y est z)
      [ 0.5,0.5; 1.,1.; 1.,1.5; 3.2,2.7 ];
    Verb.pe Verb.debug "Final "; print_wts ()


let test2 ?(trials = 100) ?(verb = true) ?(seed = 31415) learner
    value_fun feature_fun =
  Random.init seed;
  let show, est, get = learner (Array.length (feature_fun 0.)) in
  let print_wts () = print_string "Wts: "; Vector.write_line stdout (get ()) in
    Wrutils.ntimes (fun () ->
		    if verb then print_wts ();
		    let x = Random.float 100. in
		    let z = value_fun x in
		    let est, err = show (feature_fun x) z
		    in
		      Verb.pe Verb.debug
			"For %f = %f, est %f, err %f.\n"
			x z est err)
      trials;
    Verb.pe Verb.debug "-- done with training ---\n";
    List.iter (fun (x,_) ->
		 let est = est (feature_fun x)
		 and z = value_fun x in
		   Verb.pe Verb.debug "For %f got %f instead of %f.\n"
		     x est z)
      [ 0.5,0.5; 1.,1.; 1.,1.5; 3.2,2.7 ];
    Verb.pe Verb.debug "Final "; print_wts ()



(*
  100 trials:
  Final Wts: 0.538176496402 0.562749680979 0.666387126734
  Final Wts: 0.516071515361 0.592056440532 0.682740063634

  200 trials:
  Final Wts: 0.502190480849 0.595698897455 0.697090329599
  Final Wts: 0.500250203499 0.599462181687 0.699543493552

*)
let do_test () =
  (* suitable as a main function for testing *)
  assert ((Array.length Sys.argv) = 3);
  let trials = int_of_string Sys.argv.(2)
  in
    match Sys.argv.(1) with
      "mom" ->  test ~trials:trials init_nlms_momentum
    | "flow" -> test ~trials:trials init_nlms_flow
    | x -> invalid_arg x


(********* non-stationary test case *********)

(*
let test_moving
  ?(training = 20000)
  ?(test = 10000)
  ?(print = 100)
  ?(seed = 31415)
  ?(total = 20)
  ?(relevant = 5)
  learner =
  Random.init seed;
  let f x y =
    0.5 +.
*)


(* EOF *)
