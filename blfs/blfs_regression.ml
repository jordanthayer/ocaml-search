(** A generic framework for BLFS with a regression model.

    @author eaburns
    @since 2010-01-08
*)

(************************************************************)
(* Regression learners.                                     *)
(************************************************************)

let train_online_lms rate n features targets =
  (** [train_online_lms features targets] gets the coefficients
      trained from an online regression learner.  This is the fallback if
      there are too few examples for the offline regression. *)
  let show, _, get = Lms.init_lms ~learning_rate:rate n in
  let nexamples = Array.length features in
    for i = 0 to nexamples - 1 do
      ignore (show features.(i) targets.(i))
    done;
    get ()


let offline_lms_learner rate n =
  let features = ref [] in
  let targets = ref [] in
  let show_example f t =
    Wrutils.push f features;
    Wrutils.push t targets;
    nan, nan
  and get_coeffs () =
    let fary = Array.of_list !features
    and tary = Array.of_list !targets in
      if fary = [| |] || (Array.length fary) < (Array.length fary.(0))
      then train_online_lms rate n fary tary
      else begin
	Wrutils.pr "Using the real learner\n";
	assert ((Array.length fary) = (Array.length tary));
	let coeffs = Offline_lms.lms fary tary in
(*
	  features := [];
	  targets := [];
*)
	  coeffs
      end
  in
  let estimate pattern =
    let coeffs = get_coeffs () in
      Vector.dot coeffs pattern
  in show_example, estimate, get_coeffs


let lms_learner rate n =
  (** [lms_learner rate n] is a simple least mean square learner with
      normalization. *)
  Lms.init_lms ~learning_rate:rate n


let lms2_learner rate n =
  (** [lms2_learner rate n] is a simple least mean square learner. *)
  Lms.init_lms2 ~learning_rate:rate n


let nlms_momentum_learner rate n =
  (** [nlms_momentum_learner rate n] is a least mean square learner
      using momentum to attempt to alleviate learning rate issues.
      This normalizes the inputs to attempt to make each element have
      a similar range of values. *)
  Lms.init_nlms_momentum ~learning_rate:rate n


let nlms_flow_2_learner rate n =
  (** [nlms_flow_2_learner rate n] is a more advanced least mean
      square learner that attempts to adjust the learning rate.  This
      normalizes the inputs to attempt to make each element have a
      similar range of values. *)
  Lms.init_nlms_flow ~initial_learning_rate:rate n


let learner_by_name name =
  (** [learner_by_name name] gets a regression learner given its
      name. *)
  match name with
    | "offline_lms" -> offline_lms_learner
    | "lms" -> lms_learner
    | "lms2" -> lms2_learner
    | "nlmsm" -> nlms_momentum_learner
    | "nlms_flow_2" -> nlms_flow_2_learner
    | _ ->
	invalid_arg
	  (name ^ ": is not a learner (try: lms, nlmsm or nlms_flow_2)")


(************************************************************)
(* Outputting average deviation.                            *)
(************************************************************)


let make_error_tracker prefix =
  let avg_dev = ref 0.
  and num = ref 0. in
  let col_name =
    if prefix = "" then "learning" else prefix ^ " learning"
  in
  let key_name =
    if prefix = "" then "mean deviation" else prefix ^ " mean deviation"
  in
    Datafile.write_alt_colnames stdout col_name [ key_name; ];
    let accumulate_deviation dev =
      num := !num +. 1.0;
      avg_dev := !avg_dev +. (dev -. !avg_dev) /. !num
    and get_reset_average () =
      let avg = !avg_dev in
	Datafile.write_alt_row_prefix stdout col_name;
	Wrutils.pr "%f\n" avg;
	num := 0.;
	avg_dev := 0.;
	avg
    in
      accumulate_deviation, get_reset_average


(************************************************************)
(* BLFS search interface functions.                         *)
(************************************************************)

let make_iter_children info see_branch edge_cost best_completion
    remember_edge forget_edge =
  (** [make_iter_children info see_node edge_cost remember_edge
      forget_edge] creates a function for visiting child nodes that
      can be used with a BLFS search with a regression model.

      [info] is the info structure for the search.

      [see_branch] called with the depth and node each time a
      new branch is seen (each time the iter function is called).

      [edge_cost] is a function from the depth, child rank and current
      cost that gives the *transition* *cost*.  NOTE: The value
      returned by this is the edge cost not the edge cost plus the
      accumulated cost.  The accumulated cost is only passed so that
      it can be used as a feature to the edge cost computation.

      [best_completion] is a function from depth and rank to
      the best possible completion cost from then on.  This is the
      cost-to-go estimate function.

      [remember_edge] is called with the depth, rank and cost before
      search visits a child.

      [forget_edge] is called with the depth, rank and cost each time
      the search returns from visiting a child. *)
  let rec iter_ranks visit_child notify_skip node cost depth bound
      nchildren rank =
    if rank < nchildren
    then begin
      let h = best_completion (depth + 1) rank
      and g = cost +. (edge_cost depth rank cost) in
      let f = g +. h in
	if f <= bound
	then begin
	  (* Visit the child *)
	  remember_edge depth rank cost;
	  visit_child node rank (g, depth + 1);
	  forget_edge depth rank cost;

	  (* Visit the next child *)
	  iter_ranks visit_child notify_skip node
	    cost depth bound nchildren (rank + 1)
	end else
	  (* Child is out of bounds.  Assume the remaining children
	     are out of bounds too. *)
	  notify_skip ()
    end
  in
    (fun visit_child notify_skip node (cost, depth) bound ->
       see_branch depth node;
       let nchildren = Info.num_children info node in
	 iter_ranks visit_child notify_skip node cost depth bound nchildren 0)


(************************************************************)
(* Normalizing child cost tables.                           *)
(************************************************************)

let smallest_diff a max_d max_b =
  (** returns the smallest non-zero difference between any adjacent
      child costs, or None *)
  let min = ref infinity in
    for d = 0 to max_d - 1 do
      let start = d * max_b in
      let prev = ref a.(start) in
	for b = 1 to max_b - 1 do
	  let this = a.(start + b) in
	    (* assume monotonically nondecreasing, so diff is nonnegative *)
	  let diff = this -. !prev in
	    if (diff > 0.) && (diff < !min)
	    then min := diff;
	    prev := this
	done
    done;
    if !min = infinity
    then None
    else Some !min


let enforce_diff a max_d max_b diff =
  (** destructive on [a] *)
  for d = 0 to max_d - 1 do
    let start = d * max_b in
    let prev = ref a.(start) in
      for i = start + 1 to start + max_b - 1 do
	let min = !prev +. diff
	and this = a.(i) in
	let value = (if this >= min
		     then this
		     else !prev +. (diff *. (0.5 +. (Random.float 1.))))
	in
	  a.(i) <- value;
	  prev := value
      done
  done


let clamp_monotonic a max_d max_b =
  (** force child costs to be monotonically increasing at each depth *)
  for d = 0 to max_d - 1 do
    Functions.monotonic_regression a (d * max_b) max_b
  done;
  let diff = (match smallest_diff a max_d max_b with
		  Some d -> d
		| None -> (a.(0) /. 20.) /. (float_of_int max_d)) in
    enforce_diff a max_d max_b diff


let clamp_non_negative a =
  let n = Array.length a in
    for i = 0 to n - 1 do
      if a.(i) < 0. then a.(i) <- 0.
    done


let normalize_for_zero_completions p max_d max_b =
  (** transform child costs so that after the first arc, the preferred
      child always costs zero.  assumes already monotonic increasing in
      child rank at each depth *)
  let backed_up = ref 0. in
    for d = max_d - 1 downto 0 do
      let base = d * max_b in
      let best = p.(base) in
	p.(base) <- 0.;
	for i = base + 1 to base + max_b - 1 do
	  p.(i) <- p.(i) -. best
	done;
	backed_up := !backed_up +. best
    done;
    !backed_up
(*
    for i = 0 to max_b - 1 do
      p.(i) <- p.(i) +. !backed_up
    done
*)


(************************************************************)
(* Probing in the search space.                             *)
(************************************************************)

exception Halted

let random_child info node =
  (** [random_child info node] gets a random child of the given
      node. *)
  let nchildren = Info.num_children info node in
  let rank = Random.int nchildren in
    Info.get_child info node rank, rank


let nth_child info node rank =
  (** [nth_child info node rank] gets the [rank]th child of a node.
      If [rank] is greater than the number of children then the right
      most child is returned instead. *)
  let nchildren = Info.num_children info node in
  let actual_rank = Math.imin rank (nchildren - 1) in
    Info.get_child info node actual_rank, actual_rank


let random_probe info see_branch see_leaf see_term
    remember_edge forget_edge root =
  (** [random_probe info see_branch see_leaf see_term edge_cost
      remember_edge forget_edge root] makes a random probe into the
      search space down to a leaf.

      [see_branch] called with the depth and node each time a
      new branch is seen.

      [see_leaf] is called with the node and (0., depth)
      each time a leaf is reached.

      [remember_edge] is called with the depth, rank and 0. for the
      cost before search visits a child.

      [forget_edge] is called with the depth, rank and 0. for the cost
      each time the search returns from visiting a child. *)
  let rec visit node depth =
    if Info.halt_p info
    then
      raise Halted
    else begin
      if Info.leaf_p info node
      then begin
	see_leaf node (0., depth);
	ignore (Info.check_best_and_optimal info node)
      end else
	if (Info.num_children info node) = 0
	then see_term node (0., depth)
	else begin
	  Info.incr_branches info;
	  see_branch depth node;
	  let child, rank = random_child info node in
	    remember_edge depth rank 0.;
	    visit child (depth + 1);
	    forget_edge depth rank 0.
	end
    end
  in visit root 0


let always_take_rank info see_branch see_leaf see_term
    remember_edge forget_edge root rank =
  (** [always_take_rank info see_branch see_leaf see_term edge_cost
      remember_edge forget_edge root rank] makes a probe into the
      search space down to a leaf taking the child with [rank] each
      time.

      [see_branch] called with the depth and node each time a
      new branch is seen.

      [see_leaf] is called with the node and (0., depth) each
      time a leaf is reached.

      [remember_edge] is called with the depth, rank and 0. for the
      cost before search visits a child.

      [forget_edge] is called with the depth, rank and 0. for the cost
      each time the search returns from visiting a child. *)
  let rec visit node depth =
    if Info.halt_p info
    then
      raise Halted
    else begin
      if Info.leaf_p info node
      then begin
	see_leaf node (0., depth);
	ignore (Info.check_best_and_optimal info node)
      end else
	if (Info.num_children info node) = 0
	then see_term node (0., depth)
	else begin
	  Info.incr_branches info;
	  see_branch depth node;
	  let child, actual_rank = nth_child info node rank in
	    remember_edge depth actual_rank 0.;
	    visit child (depth + 1);
	    forget_edge depth actual_rank 0.
	end
    end
  in visit root 0


let init_rnd = function
    (** [init_rnd seed] initializes the random number to some seed
	value. *)
  | None -> ()
  | Some s ->
      Verb.pr Verb.optional "Blfs_regression: Using seed = %d\n" s;
      Random.init s


let random_probes ?seed info max_children see_branch see_leaf
    see_term remember_edge forget_edge root num =
  (** [random_probes info max_children see_branch see_leaf
      remember_edge forget_edge root num] performs [num] random probes
      into the search space and also probes the left-most and
      right-most leaves. *)
  init_rnd seed;
  try
    (* left-most leaf. *)
    always_take_rank info see_branch see_leaf see_term
      remember_edge forget_edge root 0;

    (* right-most leaf. *)
    always_take_rank info see_branch see_leaf see_term
      remember_edge forget_edge root (max_children - 1);

    (* [num] random probes *)
    for n = 1 to num do
      random_probe info see_branch see_leaf see_term
	remember_edge forget_edge root
    done;
  with Halted -> ()


let structured_probe info see_branch see_leaf see_term remember_edge
    forget_edge plan root =
  (** [structured_probe info see_branch see_leaf see_term
      remember_edge forget_edge plan root] performs a planned
      trajectory through the tree.  [plan] is a list of (rank,
      target_depth) tuples where the target_depth must be increasing
      in successive elements of the list.  The probe always chooses a
      child of the given rank until the target depth is reached.  When
      the target depth is reached the probe pops this element off of
      the plan and recurs with the next plan element. *)
  let rec visit plan node depth =
    if Info.halt_p info then raise Halted;
    if Info.leaf_p info node
    then begin
      see_leaf node (0., depth);
      ignore (Info.check_best_and_optimal info node)
    end else
      if (Info.num_children info node) = 0
      then see_term node (0., depth)
      else match plan with
	| [] ->
	    invalid_arg "structured_probe: no plan"
	| (rank, target) :: tl when target < depth ->
	    visit tl node depth
	| (rank, _) :: tl ->
	    Info.incr_branches info;
	    see_branch depth node;
	    let child, actual_rank = nth_child info node rank in
	      remember_edge depth actual_rank 0.;
	      visit plan child (depth + 1);
	      forget_edge depth actual_rank 0.
  in visit plan root 0


let structured_probes info max_depth max_children see_branch see_leaf
    see_term remember_edge forget_edge percent root =
  (** [structured_probes info max_depth max_children see_branch
      see_leaf remember_edge forget_edge percent root] does some
      structured probes into the search space taking [percent]
      branches at each rank at the top, bottom and middle of the
      traversal. *)
  try
    let portion = truncate ((float max_depth) *. percent) in
      for r = 0 to max_children - 1 do
	for s = 0 to max_children - 1 do
	  if r <> s
	  then begin
	    let residue = max_depth - portion in
	    let top_portion = residue / 2 in
	    let middle_portion = top_portion + portion in
	      Verb.pr Verb.optional "Probing %d till %d then %d\n" r portion s;
	      structured_probe info see_branch see_leaf see_term remember_edge
		forget_edge [(r, portion); (s, max_depth)] root;

	      Verb.pr Verb.optional "Probing %d till %d then %d\n" s residue r;
	      structured_probe info see_branch see_leaf see_term remember_edge
		forget_edge [(s, residue); (r, max_depth)] root;

	      Verb.pr Verb.optional
		"Probing %d till %d then %d till %d then %d\n"
		s top_portion r middle_portion s;
	      structured_probe info see_branch see_leaf see_term
		remember_edge forget_edge
		[(s, top_portion); (r, middle_portion); (s, max_depth)]
		root;

	      Verb.pr Verb.optional "r=%d, s=%d, max_children - 1=%d\n%!"
		r s (max_children - 1)
	  end
	done
      done
  with Halted -> ()


let initialize_model
    info model max_depth max_children features leaf_cost
    see_terminal see_branch remember_edge forget_edge initial num_probes =
  (** [initialize_model info model max_depth max_children features
      leaf_cost see_terminal see_branch remember_edge forget_edge
      initialize_model num_probes]

      Learn a polynomial on same focused probes into the search space.
      The return is the polynomial which can be used to initialize
      another module. *)
  let init_leaves = ref [] in
  let leaves = ref 0 in
  let feature_size = ref ~-1 in
  let accum_leaves model node (cost, depth) =
    (* Accumulate leaf costs during the initial sampling *)
    see_terminal node (0., depth);
    incr leaves;
    let copy = Array.copy (features model) in
      if !feature_size < 0 then feature_size := Array.length copy;
      Wrutils.push (copy, leaf_cost node) init_leaves
  in
    random_probes info max_children see_branch
      (accum_leaves model) see_terminal remember_edge forget_edge
      initial num_probes;
    while !leaves < !feature_size do
      let p = (Random.float 0.2) +. 0.1 in
	Verb.pr Verb.optional "Structured probe with p=%f\n" p;
	structured_probes info max_depth max_children see_branch
	  (accum_leaves model) see_terminal remember_edge
	  forget_edge p initial;
	Verb.pr Verb.optional "%d leaves seen\n%!" !leaves;
    done;
    let features, targets = List.split !init_leaves in
      Offline_lms.lms (Array.of_list features) (Array.of_list targets)


let make_initializer info degree max_depth max_children leaf_cost
    see_terminal see_branch root =
  (** [make_initializer info degree max_depth max_children leaf_cost
      see_terminal see_branch root] makes a function (in depth and
      rank) that can be used to initialize a model. *)
  let ex_per_feature = 1 in
  let nfeatures = max_children * (degree + 1) + 1 in
  let max_depthf = float max_depth in
  let nexamples = ex_per_feature * nfeatures in

  let leaves = Array.make nexamples ([| |], 0.) in
  let sums = Array.make nfeatures 0. in
  let n = ref 0 in
  let denom = ref 2. in

  let remember_edge depth rank _ =
    let d = (float depth) /. max_depthf in
    let cur_term = ref d in
    let ci = rank * (degree + 1) in
      sums.(ci) <- sums.(ci) +. 1.0;
      for t = 1 to degree do
	let i = ci + t in
	  sums.(i) <- sums.(i) +. !cur_term;
	  cur_term := !cur_term *. d;
      done

  and forget_edge depth rank _ =
    let d = (float depth) /. max_depthf in
    let cur_term = ref d in
    let ci = rank * (degree + 1) in
      sums.(ci) <- sums.(ci) -. 1.0;
      for t = 1 to degree do
	let i = ci + t in
	  sums.(i) <- sums.(i) -. !cur_term;
	  cur_term := !cur_term *. d;
      done

  and see_leaf node (_, depth) =
    let cost = leaf_cost node in
      if !n < nexamples
      then leaves.(!n) <- Array.copy sums, cost;
      incr n;
  in
    while !n < nexamples do
      Wrarray.fill_all sums 0.;
      sums.(nfeatures - 1) <- 1.0;	(* constant feature. *)
(*
      let p = (Random.float 0.2) +. 0.1 in
*)
      let p = 1. /. !denom in
	structured_probes info max_depth max_children see_branch
	  see_leaf see_terminal remember_edge forget_edge p root;
	denom := !denom +. 1.0;
	Verb.pr Verb.optional "n=%d, nexamples=%d\n%!" !n nexamples;
    done;
    let examples, targets = Wrarray.split leaves in
    let coeffs = Offline_lms.lms examples targets in
      (fun depth rank ->
	 let ci = rank * (degree + 1) in
	 let d = (float depth) /. max_depthf in
	 let sum = ref coeffs.(ci) in
	 let cur_term = ref d in
	   for t = 1 to degree do
	     sum := !sum +. (coeffs.(ci + t) *. !cur_term);
	     cur_term := !cur_term *. d;
	   done;
	   !sum), coeffs
