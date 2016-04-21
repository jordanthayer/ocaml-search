(* $Id: dist_opt.ml,v 1.1 2003/09/27 01:13:41 ruml Exp ruml $

   optimizing stochastic function to emit a desired distribution
*)


type probe_result = Illegal of float
		    | Counts of int array


(********* comparison function based on dist dissimilarity **********)


let total dist =
  Wrarray.sum dist


let print_vec d =
  Array.iter (fun x -> Wrutils.pr " %.3f" x) d

let print_dist d =
  let t = total d in
    Array.iter (fun x -> Wrutils.pr " %4.3f" (Math.div x t)) d;
    Wrutils.pr " (%5d)" t

let print_status v c l h =
  print_vec v;
  Wrutils.pr ": ";
  print_dist c;
  Wrutils.pr " = %.1f-%.1f\n" l h


let augment a b =
  match b with
    Illegal _ -> failwith "params became illegal?"
  | Counts b -> Wrarray.add a b


let compare_dists dissim tol max gen a c_a b c_b =
  (** returns true iff [a] better than [b] *)
  let rec compare a c_a b c_b =
    let a_m, a_low, a_high = dissim c_a
    and b_m, b_low, b_high = dissim c_b
    and t_a = total c_a
    and t_b = total c_b in
      Wrutils.pr "---------                                                     -------\n";
      print_status a c_a a_low a_high;
      print_status b c_b b_low b_high;
      flush stdout;
      if (a_high < b_low) && (t_a >= t_b) then
	true
      else if (b_high < a_low) && (t_b >= t_a) then
	false
      else if ((Math.within a_high b_low tol) &&
	       (Math.within a_low b_high tol)) then
	(* same *)
	false
      else
	  if ((t_a >= max) && (t_b >= max)) then
	    (* tired *)
	    a_m < b_m
	  else if t_a < t_b then
	    compare a (augment c_a (gen a false)) b c_b
	  else
	    compare a c_a b (augment c_b (gen b false))
  in
    compare a c_a b c_b



let make_dist_compare dissim tol max f =
  (** returns a comparison function that, given two param vectors,
    returns true iff the first is strictly better than the second *)
  (fun a b ->
     let res_a = f a true
     and res_b = f b true in
       match res_a with
	 Illegal i_a -> (match res_b with
			   Illegal i_b -> i_a < i_b
			 | Counts _ -> false)
       | Counts c_a -> (match res_b with
			  Illegal _ -> true
			| Counts c_b ->
			    compare_dists dissim tol max f a c_a b c_b))


(******** the main function *******)


let dist_nearby_min generator
  initial
  stepsizes
  input_tolerances
  output_tolerance
  dissim_and_conf
  verbosity =
  (** [generator] takes param vector and a boolean and returns a
    [probe_result].  Two params are judged same if they are within
    [output_tolerance].  Params adjusted until within [input_tolerance]
    of the local min.  High [output_tolerance] will cause timidity.
    [dissim_and_conf] is assumed to know the dist we are trying to
    achieve.  *)
  let max_trials = 10000 in
  let better_p = make_dist_compare dissim_and_conf output_tolerance
		   max_trials generator in
    Ord_opt.nearby_min_vector better_p initial stepsizes input_tolerances
      50 verbosity



(******** optimizing x-square metric specifically **********)


(**** monte-carlo sampling ****)


let int_cdf a =
  let b = Array.copy a in
    for i = 1 to (Array.length b)-1 do
      b.(i) <- b.(i-1) + b.(i)
    done;
    b


let recreate_counts a cdf total =
  Wrarray.fill_all a 0;
  for sample = 1 to total do
    let s = Random.int total
    and i = ref 0 in
      (* we know last entry is larger than s *)
      while cdf.(!i) < s do
	incr i
      done;
      a.(!i) <- a.(!i) + 1
  done;
  a


let confidence_monte_carlo f obs =
  (** bootstrap confidence intervals of [f] on int dist [obs] *)
  let conf = 0.95
  and num_experiments = 500
  and a = Array.make (Array.length obs) 0
  and cdf = int_cdf obs
  and num_samples = Wrarray.sum obs in
  let experiments = Array.init num_experiments
		      (fun _ ->
			 f (recreate_counts a cdf num_samples))
  in
    Stats.tails experiments conf


(***** reference x-square ******)


let ref_x_square refr obs =
  (** approximates the expected value of x_square we would get if we
    had an infinite number of samples in obs *)
  let total_refr = Wrarray.sum refr
  and total_obs = Wrarray.sum obs in
  let expected = Wrarray.map2 (fun r o ->
				 if ((o = 0) && (r <> 0)) then
				   Math.div r (total_refr + total_obs)
				 else
				   Math.div o total_obs)
		   refr obs in
  let sum = ref 0. in
  let do_row row total =
    Wrarray.iter2 (fun a e ->
		     let expected = total *. e in
		     let diff = Math.square ((float a) -. expected) in
		       if expected = 0. then
			 (if diff <> 0. then
			    sum := infinity)
		       else
			 sum := !sum +. (diff /. expected))
      row expected
  in
    do_row refr (float total_refr);
    do_row obs (float total_obs);
    !sum


let compute_ref_x_square_and_confidence (observed, reference) =
  let low, high = confidence_monte_carlo (ref_x_square reference) observed
  and mid = ref_x_square reference observed in
    mid, low, high


let ref_x_square_and_confidence =
  let cache = Cache.create 6 in
    (fun reference observed ->
       Cache.find_or_compute cache
       compute_ref_x_square_and_confidence (observed, reference))


(******** main function *******)


let x_square_nearby_min f desired_dist initial stepsizes tolerances =
  dist_nearby_min f initial stepsizes tolerances 0.001
    (ref_x_square_and_confidence desired_dist) 5


(* EOF *)
