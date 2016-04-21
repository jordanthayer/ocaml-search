(* $Id: ord_opt.ml,v 1.1 2003/09/27 01:13:46 ruml Exp ruml $

   numerical optimization via a predicate over vectors
*)


(************** on a single real variable *****************)


let bracket better_p a b =
  (* want probe to be same distance from curr as old *)
    let rec try_step old curr step i =
      let probe = curr +. step
      and i = i + 1 in
	if better_p probe curr then
	  try_step curr probe (step *. 1.5) i
	else
	  old, curr, probe, i
    in
      if better_p a b then
	try_step b a (a -. b) 0
      else
	try_step a b (b -. a) 0


let nearby_min better_p a b tol max_evals =
  (** returns point and number of evals *)
  let rec iter left mid right i =
    if (i >= max_evals) || ((abs_float (right -. left)) < tol)
    then mid, i (* We should consider throwing an error here *)
    else
      let left_mid = (left +. mid) /. 2. in
	if better_p left_mid mid then
	  iter left left_mid mid (i+1)
	else
	  let right_mid = (mid +. right) /. 2. in
	    if better_p right_mid mid then
	      iter mid right_mid right (i+2)
	    else
	      iter left_mid mid right_mid (i+2)
  in
    Verb.pr Verb.debug "Bracketing.\n";
    let left, middle, right, evals = bracket better_p a b in
      Verb.pr Verb.debug "Squeezing interval to tolerance %6f.\n" tol;
      iter left middle right evals


(*************** on a vector ******************)


let print_dir dir =
  Array.iter (fun x -> Wrutils.pr " %6f" x) dir;
  Wrutils.newline stdout


let nearby_min_dir p curr dir tols =
  (** minimize in one direction via conversion to line search *)
  let tol = Vector.norm (Wrarray.map2 (fun d t ->
					 if d = 0. then 0. else (t /. d))
			   dir tols) in
  let better a b =
    p (Vector.project curr dir a) (Vector.project curr dir b)
  in
    Verb.force 5 (lazy (Wrutils.pr "Searching in direction:";
			   print_dir dir));
    let scale,_ = nearby_min better 0. 1. tol max_int in
      Vector.project curr dir scale, Vector.multiply_by dir scale


let do_dirs p cur dirs tols =
  (** one pass through all [dirs] *)
  let min_step = 0. in
  let min_norm = sqrt (min_step /. (float (Array.length dirs))) in
  let new_dirs = Array.copy dirs in
  let cur = Wrarray.fold_lefti
	      (fun cur i dir ->
		 if (Vector.norm dir) <> 0. then
		   let cur, new_dir = nearby_min_dir p cur dir tols in
		     if (Vector.norm new_dir) > min_norm then
		       new_dirs.(i) <- new_dir;
		     cur
		 else
		   cur)
	      cur dirs
  in
    cur, new_dirs


let subst_dir dir dirs =
  (** returns a fresh array of dirs *)
  Verb.force 5 (lazy (Wrutils.pr "Adding direction:";
			 print_dir dir));
  let i = Wrarray.max_index_by (Vector.cosine dir) dirs in
  let dirs = Array.copy dirs in
    dirs.(i) <- dir;
    dirs


let find_nearby_min p initial initial_dirs tolerances max_iterations =
  (** does one sequence of moves, stopping when moves stop improving
    or iterations run out. *)
  assert (max_iterations > 0);
  let min_step = 0.
  and num_dirs = Array.length initial_dirs
  in
  let rec one_pass start dirs iters =
    Verb.pr 5 "Starting inner iteration %d.\n" (iters+1);
    let next, dirs = do_dirs p start dirs tolerances
    and iters = iters + 1 in
      if (((Vector.euclidean start next) <= min_step) ||
	  (iters = max_iterations)) then
	next, iters
      else
	let dirs = (if ((iters <> 1) &&
			(Math.divisible (iters-1) num_dirs)) then
		      (Verb.pr 5 "Resetting directions.\n";
		       initial_dirs)
		    else
		      subst_dir (Vector.subtract next start) dirs) in
	  one_pass next dirs iters
  in
    one_pass initial initial_dirs 0


(****** the main function ******)


let make_dirs initial steps =
  let num_dims = Array.length initial in
    Array.mapi (fun i step ->
		  let d = Array.make num_dims 0. in
		    d.(i) <- step;
		    d)
      steps


let nearby_min_vector better_p initial initial_stepsizes tolerances
  max_iterations verbosity =
  (** Params adjusted until within [input_tolerance] of the local min. *)
  let dirs = make_dirs initial initial_stepsizes in
  let rec iteration current i used =
    Verb.pr 5 "Starting outer iteration %d.\n" i;
    let next, u = find_nearby_min better_p current dirs
		    tolerances (max_iterations - used) in
    let used = used + u in
      if (next = current) || (used >= max_iterations) then
	next, used
      else
	iteration next (i+1) used
  in
    Verb.with_level verbosity
      (fun () -> iteration initial 1 0)


(***** testing ****)


let test () =
  let f v =
    Wrutils.pr "evaluating at %6.3f, %6.3f.\n" v.(0) v.(1);
    (Math.square v.(0)) +. (Math.square v.(1))
  in
  let better_p a b =
    f a < f b
  in
  let initial = [|10.; 15.|]
  and steps = [|1.; 1.|]
  and tols = [|0.1; 0.1|] in
    nearby_min_vector better_p initial steps tols 100 5


(* EOF *)
