(** Job shop scheduling.

    Implementation of Smith and Cheng, AAAI 93, "Slack-Based Heuristics
    For Constraint Satisfaction Scheduling".

    @author eaburns
    @since 2010-02-17
*)

module Inst = Job_shop_instance

type t = {
  deadline : float;
  instance : Inst.t;
}

type node = {
  (* A node in the search tree. *)

  pairs : (int * int) list;
  (* List of ordering pairs yet to be assigned. *)

  stn : Stn.t;
  (* Some stn if the network is still valid, or None if the network
     was made invalid. *)
}


type saved = {
  (* A saved solution only requires the simple temporal network.  The
     final answer can be gathered by taking the earliest start time for
     each job in a consistent network. *)

  saved_network : Stn.t;
  (* The STN, used to retrieve the actual solution (the earliest start
     times for each operation). *)

  total_time : float;
  (* Stored here for convenient retrieval. *)
}


let prune_me = { pairs = []; stn = Stn.create () }
  (** [prune_me] is a special node that should be pruned. *)


let bounds stn i = float (Stn.earliest stn i), float (Stn.latest stn i)


let get_times instance stn i =
  (** [get_times instance stn i] gets the earliest start time,
      processing time and latest finish time for operation [i]. *)
  let est_i, lst_i = bounds stn i
  and p_i = Inst.duration instance i in
  let lft_i = lst_i +. p_i in
    est_i, p_i, lft_i


let slack t stn p =
  (** [slack t stn p] computes the slack for a pair, [p], in the given
      node. *)
  let i, j = p in
  let est_i, p_i, _ = get_times t.instance stn i in
  let _, p_j, lft_j = get_times t.instance stn j in
    lft_j -. est_i -. (p_i +. p_j)


let bslack n t stn p =
  (** [bslack2 n t stn p] computes the Bslack value of a pair. *)
  let i, j = p in
  let p' = j, i in
  let sl_ij = slack t stn p in
  let sl_ji = slack t stn p' in
  let min_sl, max_sl =
    if sl_ij < sl_ji then sl_ij, sl_ji else sl_ji, sl_ij
  in
  let s = min_sl /. max_sl in
    (sl_ij /. (Math.nth_root n s))


let bslack2 n1 n2 t node p =
  (** [bslack2 n1 n2 t node p] computes the Bslack value of a pair
      given two n values. *)
  let i, j = p in
  let p' = j, i in
  let sl_ij = slack t node p in
  let sl_ji = slack t node p' in
  let min_sl, max_sl =
    if sl_ij < sl_ji then sl_ij, sl_ji else sl_ji, sl_ij
  in
  let s = min_sl /. max_sl in
    (sl_ij /. (Math.nth_root n1 s)) +. (sl_ij /. (Math.nth_root n2 s))


let do_cba t stn pairs =
  (** [do_cba t stn pairs] performs constraint-based
      analysis on the node to prune infeasible orderings. *)
  let rec cba_pairs stn = function
    | [] -> stn, []
    | (i, j) :: tl ->
	let est_i, p_i, lft_i = get_times t.instance stn i in
	let est_j, p_j, lft_j = get_times t.instance stn j in
	let sum = p_i +. p_j
	and i_less_j = lft_i -. est_j
	and j_less_i = lft_j -. est_i
	in
	  if i_less_j < sum && sum <= j_less_i
	  then				(* case 1 *)
	    (* i -> j *)
	    let c = Stn.lower_constraint i j (truncate p_i) in
	      Stn.add_constraint stn c;
	      cba_pairs stn tl
	  else if j_less_i < sum && sum <= i_less_j
	  then				(* case 2 *)
	    (* j -> i *)
	    let c = Stn.lower_constraint j i (truncate p_j) in
	      Stn.add_constraint stn c;
	      cba_pairs stn tl
	  else if sum > j_less_i && sum > i_less_j
	  then 				(* case 3 *)
	    raise Stn.Inconsistent
	  else				(* case 4 *)
	    begin
	      assert (sum <= j_less_i);
	      assert (sum <= i_less_j);
	      (* Can't decide yet so defer the decision to search. *)
	      let stn, pairs = cba_pairs stn tl in
		stn, ((i, j) :: pairs)
	    end
  in cba_pairs stn pairs


let rec min_f_pairs t stn ?(pairs=[]) ?(rest=[]) ?min_f f = function
    (** [min_f_pairs t stn ?pairs ?min_f f pairs] gets the pairs with the
	minimum [f] and the remaining pairs.  The result is a list of
	pairs with the minimum [f] to allow for different tie breaking
	procedures.  Each pair (i, j) is ordered so that the the
	constraint i -> j will create more slack than j -> i. *)
  | [] -> pairs, rest
  | ((i, j) as p) :: tl ->
      let p' = (j, i) in
      let f_val = Math.fmin (f p) (f p')
      and max_sl_pair = Wrutils.arg_max p p' (slack t stn) in
	match min_f with
	  | None ->
	      min_f_pairs t stn
		~pairs:[max_sl_pair] ~rest ~min_f:f_val f tl
	  | Some min_f_vl ->
	      if f_val = min_f_vl
	      then
		min_f_pairs t stn
		  ~pairs:(max_sl_pair::pairs) ~rest ?min_f f tl
	      else if f_val < min_f_vl
	      then
		min_f_pairs t stn
		  ~pairs:[max_sl_pair] ~rest:(pairs@rest)~min_f:f_val f tl
	      else
		min_f_pairs t stn
		  ~pairs ~rest:(max_sl_pair::rest) ?min_f f tl


let min_slack_pairs t stn pairs =
  (** [min_slack_pairs t stn pairs] gets the pairs with the minimum
      slack value.  Each pair (i, j) is ordered so that i -> j has
      more slack than j -> i. *)
  min_f_pairs t stn (slack t stn) pairs


let min_bslack_pairs n t stn pairs =
  (** [min_bslack_pairs n t stn pairs] gets the pairs with the minimum
      Bslack_n value.  Each pair (i, j) is ordered so that i -> j has
      more slack than j -> i. *)
  min_f_pairs t stn (bslack n t stn) pairs


let min_bslack2_pairs n1 n2 t stn pairs =
  (** [min_bslack2_pairs n1 n2 t stn pairs] gets the pairs with the
      minimum Bslack_n value.  Each pair (i, j) is ordered so that i
      -> j has more slack than j -> i. *)
  min_f_pairs t stn (bslack2 n1 n2 t stn) pairs


let rec line_up_job t = function
    (** [line_up_job t ops] gets a list of constraints that will ensure
	that the first operation in the [ops] list comes before all
	following, etc. *)
  | [] -> []
  | i :: [] ->
      let p_i = Inst.duration t.instance i in
	[Stn.upper_constraint 0 i (truncate (t.deadline -. p_i))]
  | i :: ((j :: _) as tl) ->
      let p_i = Inst.duration t.instance i in
	(Stn.lower_constraint 0 i 0)
	:: (Stn.lower_constraint i j (truncate p_i))
	:: (line_up_job t tl)


let make_node var_order t stn pairs =
  (** [make_node var_order t stn pairs] performs constraint-based
      analysys to propogate some constrains (and possibly cause pruning).
      The resulting node has the head pair of its list, (i, j), as the
      pair choosen by variable ordering and the ordering i->j is the value
      choosen by value ordering. *)
  try
    let stn', pairs' = do_cba t stn pairs in
      match var_order t stn' pairs' with
	| [], rest -> { pairs = rest; stn = stn' }
	| p :: ptl, rest -> {  pairs = p :: ptl @ rest; stn = stn'; }
  with Stn.Inconsistent -> prune_me


let initial_node var_order t =
  (** [initial_node var_order t] gets the initial node from the problem. *)
  let nnodes = Inst.number_of_ops t.instance in
  let init_constraints =
    Wrlist.mapcan (line_up_job t) (Inst.job_op_ids t.instance)
  in
  let stn = Stn.create () in
  let pairs = Inst.conflicting_op_ids t.instance in
    for i = 0 to nnodes - 1 do ignore (Stn.add_point stn); done;
    Stn.add_constraints stn init_constraints;
    make_node var_order t stn pairs


let is_leaf n =
  (** [is_leaf n] tests if a node is a leaf. *)
  n.pairs = [] && n != prune_me


let num_children n =
  (** [num_children n] gets the number of children of a node. *)
  if n.pairs = []
  then 0			 (* not an error, may be a deadend. *)
  else 2


let make_nth_child var_order t node n =
  (** [make_nth_child var_order t node n] gets the [n]th child of
      [node]. *)
  assert (not (is_leaf node));
  if n > 1 then invalid_arg "make_nth_child: n > 1";
  let (i, j) = List.hd node.pairs
  and rest = List.tl node.pairs in
    try
      if n = 0
      then begin
	let p_i = Inst.duration t.instance i in
	let constrnt = Stn.lower_constraint i j (truncate p_i) in
	let stn = Stn.copy node.stn in
	  Stn.add_constraint stn constrnt;
	  make_node var_order t stn rest
      end else begin
	let p_j = Inst.duration t.instance j in
	let constrnt = Stn.lower_constraint j i (truncate p_j) in
	let stn = Stn.copy node.stn in
	  Stn.add_constraint node.stn constrnt;
	  make_node var_order t stn rest
      end
    with Stn.Inconsistent -> prune_me


let all_bounds t stn =
  let n = t.instance.Inst.n * t.instance.Inst.m in
  let bounds = Array.create n (0, 0., 0.) in
    for i = 1 to n do
      bounds.(i - 1) <- i, float (Stn.earliest stn i), float (Stn.latest stn i)
    done;
    bounds

let total_time t stn =
  (** [total_time t stn] computes the total time of the given solution
      from the earliest start times in the STN. *)
  let bounds = all_bounds t stn in
    Array.fold_left (fun m (i, est_i, _) ->
		       if i = 0 then m
		       else
			 let p_i = Inst.duration t.instance i in
			 let finish = est_i +. p_i in
			   if finish > m then finish else m)
      neg_infinity
      bounds


let make_leaf_cost t node =
  (** [make_leaf_cost t node] makes a function that gets the cost of a
      leaf node. *)
  assert (is_leaf node);
  total_time t node.stn


let make_is_better t node saved = (total_time t node.stn) < saved.total_time
  (** [make_is_better t node saved] makes a function that checks if
      the given leaf node is better than the saved incumbent. *)


let make_copy_state t node =
  (** [make_copy_state t node] makes a function that copies a leaf
      node into a saved incumbent. *)
  let total_time = total_time t node.stn in
    { saved_network = node.stn; total_time = total_time }


let should_prune _ n =
  (** [should_prune n] test if a node should be pruned. *)
  n == prune_me


let make_start_times t saved =
  (** [make_start_times t saved] gets a list of tuples ((job, op), est),
      which is a solution to the problem. *)
  let bounds = all_bounds t saved.saved_network in
    Array.map (fun (i, est_i, _) -> Inst.op_of_id t.instance i, est_i) bounds


let make_interface var_ordering t =
  (** [make_interface var_ordering t] makes the interface given a
      problem instance. *)
  let module I = Bounded_depth_interface in
  let nth_child = make_nth_child var_ordering t
  and is_better = make_is_better t
  and copy_state = make_copy_state t in
    (initial_node var_ordering t,
     { (I.default num_children nth_child is_leaf is_better copy_state)
       with
	 I.should_prune = should_prune;
	 I.leaf_cost = make_leaf_cost t;
     },
     make_start_times t)

(* Scratch

   #use "use.ml";;

   Verb.set_default Verb.debug;;

   let inst =
   {Job_shop_instance.n = 2; Job_shop_instance.m = 2;
   Job_shop_instance.jobs =
   [|[|{Job_shop_instance.machine = 0; Job_shop_instance.duration = 5.};
   {Job_shop_instance.machine = 1; Job_shop_instance.duration = 6.}|];
   [|{Job_shop_instance.machine = 1; Job_shop_instance.duration = 2.};
   {Job_shop_instance.machine = 0; Job_shop_instance.duration = 10.}|]|]}
   ;;

   let t = { Job_shop.deadline = 18.; Job_shop.instance = inst };;

   let node = Job_shop.initial_node t;;

*)
