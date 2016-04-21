(** Job shop scheduling.

    Implementation of Smith and Cheng, AAAI 93, "Slack-Based Heuristics
    For Constraint Satisfaction Scheduling".


    TODO:

    Use D_stn instead of Simple_temp_net.  The destructive network is
    faster than the other even if copying is required...

    @author eaburns
    @since 2010-02-17
*)

open Printf

module Inst = Job_shop_instance

type t = {
  deadline : int;
  instance : Inst.t;
  mutable best_incumbent : int;
}

type node = {
  (* A node in the search tree. *)

  pairs : (int * int) list;
  (* List of ordering pairs yet to be assigned. *)

  stn : Simple_temp_net.t;
  (* Some stn if the network is still valid, or None if the network
     was made invalid. *)

  slack : float;
  (* The slack for the pair that created this node. *)

  f : float;

  depth : int;

  mutable children : node list;
  (* The children of a node.  This is ordered by the heuristic and it
     is filled in when [num_children] is called. *)

  mutable generated : bool;
  (* Have the children been generated. *)
}


type saved = {
  (* A saved solution only requires the simple temporal network.  The
     final answer can be gathered by taking the earliest start time for
     each job in a consistent network. *)

  stn_copy : Simple_temp_net.t;
  (* The STN, used to retrieve the actual solution (the earliest start
     times for each activity). *)

  make_span : int;
  (* Stored here for convenient retrieval. *)
}


(** [prune_me] is a special node that should be pruned. *)
let prune_me =
  { pairs = [];
    stn = Simple_temp_net.empty;
    slack = nan;
    f = nan;
    depth = ~-1;
    children = [];
    generated = false;}


(** [output_node chan n] outputs a node to [chan]. *)
let output_node chan n =
  Printf.fprintf chan "[ ";
  List.iter (fun (i, j) -> Printf.fprintf chan "(%d, %d); " i j) n.pairs;
  Printf.fprintf chan "]\n";
  Simple_temp_net.output chan n.stn


(** [get_times instance stn i] gets the earliest start time,
    processing time and latest finish time for activity [i]. *)
let get_times instance stn i =
  let est_i, lst_i = Simple_temp_net.bounds stn i
  and p_i = Inst.duration instance i in
  let lft_i = lst_i + p_i in
    est_i, p_i, lft_i


(** [slack t stn p] computes the slack for a pair, [p], in the given
    node. *)
let slack t stn p =
  let i, j = p in
  let est_i, p_i, _ = get_times t.instance stn i in
  let _, p_j, lft_j = get_times t.instance stn j in
    float (lft_j - est_i - (p_i + p_j))


(** [bslack n t stn p] computes the Bslack value of a pair. *)
let bslack n t stn p =
  let i, j = p in
  let p' = j, i in
  let sl_ij = slack t stn p in
  let sl_ji = slack t stn p' in
  let min_sl, max_sl =
    if sl_ij < sl_ji then sl_ij, sl_ji else sl_ji, sl_ij
  in
  let s = min_sl /. max_sl in
    (sl_ij /. (Math.nth_root n s))


(** [bslack2 n1 n2 t node p] computes the Bslack value of a pair
    given two n values. *)
let bslack2 n1 n2 t node p =
  let i, j = p in
  let p' = j, i in
  let sl_ij = slack t node p in
  let sl_ji = slack t node p' in
  let min_sl, max_sl =
    if sl_ij < sl_ji then sl_ij, sl_ji else sl_ji, sl_ij
  in
  let s = min_sl /. max_sl in
    (sl_ij /. (Math.nth_root n1 s)) +. (sl_ij /. (Math.nth_root n2 s))


(** [do_cba t stn pairs] performs constraint-based analysis on the
    node to prune infeasible orderings. *)
let rec do_cba t stn pairs =
  let rec cba_pairs accum stn = function
    | [] -> stn, accum
    | (i, j) :: tl ->
	let est_i, p_i, lft_i = get_times t.instance stn i in
	let est_j, p_j, lft_j = get_times t.instance stn j in
	let sum = p_i + p_j
	and i_less_j = lft_i - est_j
	and j_less_i = lft_j - est_i
	in
	  if i_less_j < sum && sum <= j_less_i
	  then				(* case 1 *)
		(* i -> j *)
		let c = Simple_temp_net.before i j p_i in
		let stn = Simple_temp_net.add_constraint stn c in
		  cba_pairs [] stn (accum @ tl)
	      else if j_less_i < sum && sum <= i_less_j
	      then				(* case 2 *)
		(* j -> i *)
		let c = Simple_temp_net.before j i p_j in
		let stn = Simple_temp_net.add_constraint stn c in
		  cba_pairs [] stn (accum @ tl)
	      else if sum > j_less_i && sum > i_less_j
	      then 				(* case 3 *)
		begin
		  Verb.pr Verb.always "Inconsistent in CBA\n%!";
		  exit 1
		    (*
		      raise Simple_temp_net.Inconsistent
		    *)
		end
	      else				(* case 4 *)
		begin
		  assert (sum <= j_less_i);
		  assert (sum <= i_less_j);
		  (* Can't decide yet so defer the decision to search. *)
		  cba_pairs ((i, j) :: accum) stn tl
		end
      in cba_pairs [] stn pairs


(** [min_f_pairs t stn ?pairs ?min_f f pairs] gets the pairs with
    the minimum [f] and the remaining pairs.  The result is a list
    of pairs with the minimum [f] to allow for different tie
    breaking procedures.

    Each pair (i, j) is ordered so that the the constraint i -> j will
    create more slack than j -> i. *)
(*
let rec min_f_pairs t stn ?(pairs=[]) ?(rest=[]) ?min_f f = function
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
*)
let rec min_f_pairs t stn ?(pairs=[]) ?(rest=[]) ?min_f f = function
  | [] ->
      begin match min_f with
	| None -> pairs, nan, rest
	| Some (minf, _) -> pairs, minf, rest
      end
  | ((i, j) as p) :: tl ->
      let p' = (j, i) in
      let f_val = Math.fmin (f p) (f p') in
      let sl = slack t stn p in
      let max_sl_pair = Wrutils.arg_max p p' (slack t stn) in
	match min_f with
	  | None ->
	      min_f_pairs t stn
		~pairs:[max_sl_pair] ~rest ~min_f:(f_val, sl) f tl
	  | Some (min_f_vl, msl) ->
	      if f_val < min_f_vl || (f_val = min_f_vl && sl > msl) then
		min_f_pairs t stn
		  ~pairs:[max_sl_pair] ~rest:(pairs @ rest)
		  ~min_f:(f_val, sl) f tl
	      else
		min_f_pairs t stn
		  ~pairs ~rest:(max_sl_pair::rest) ?min_f f tl


(** [min_slack_pairs t stn pairs] gets the pairs with the minimum
    slack value.  Each pair (i, j) is ordered so that i -> j has
    more slack than j -> i. *)
let min_slack_pairs t stn pairs =
  min_f_pairs t stn (slack t stn) pairs


(** [min_bslack_pairs n t stn pairs] gets the pairs with the minimum
    Bslack_n value.  Each pair (i, j) is ordered so that i -> j has
    more slack than j -> i. *)
let min_bslack_pairs n t stn pairs =
  min_f_pairs t stn (bslack n t stn) pairs


(** [min_bslack2_pairs n1 n2 t stn pairs] gets the pairs with the
    minimum Bslack_n value.  Each pair (i, j) is ordered so that i
    -> j has more slack than j -> i. *)
let min_bslack2_pairs n1 n2 t stn pairs =
  min_f_pairs t stn (bslack2 n1 n2 t stn) pairs


(** [line_up_job ?first rest_id t ops] gets a list of constraints
    that will ensure that the first activity in the [ops] list
    comes before all following, etc.  [rest_id] is the ID of the
    point in the STN for the 'rest' activity which will be
    constrainted to happen after all activities complete. *)
let rec line_up_job ?(first=true) rest_id t = function
  | [] -> []
  | i :: [] ->
      let p_i = Inst.duration t.instance i in
	[ Simple_temp_net.after rest_id i p_i; ]
  | i :: ((j :: _) as tl) ->
      let p_i = Inst.duration t.instance i in
	(if first then [(Simple_temp_net.not_earlier_than i 0)] else [])
	@ ((Simple_temp_net.before i j p_i)
	   :: (line_up_job ~first:false rest_id t tl))


(** [make_node var_order t stn pairs slack] performs
    constraint-based analysys to propogate some constrains (and
    possibly cause pruning).  The resulting node has the head pair
    of its list, (i, j), as the pair choosen by variable ordering
    and the ordering i->j is the value choosen by value ordering. *)
let make_node var_order t stn pairs slack depth =
  try
    let stn', pairs' = do_cba t stn pairs in
    let best, f, rest = var_order t stn' pairs' in
    let pairs' = best @ rest in
      {  pairs = pairs';
	 stn = stn';
	 depth = depth;
	 slack = slack;
	 f = f;
	 children = [];
	 generated = false; }
	(*
	  match var_order t stn' pairs' with
	  | [], rest -> { pairs = rest; stn = stn' }
	  | p :: ptl, rest -> {  pairs = p :: ptl @ rest; stn = stn'; }
	*)
  with Simple_temp_net.Inconsistent -> prune_me


(** [initial_node var_order t] gets the initial node from the
    problem. *)
let initial_node var_order t =
  let nnodes = (Inst.number_of_ops t.instance) + 1 in
  let init_constraints =
    (Simple_temp_net.no_later_than nnodes t.deadline)
    :: (Wrlist.mapcan (line_up_job nnodes t) (Inst.job_op_ids t.instance))
  in
  let stn = Simple_temp_net.create_with nnodes init_constraints in
  let pairs = Inst.conflicting_op_ids t.instance in
  let n = make_node var_order t stn pairs 0. 0 in
    Verb.pr Verb.debug "deadline = %d\n" t.deadline;
    Verb.pr Verb.debug "%d conflicts\n" (List.length pairs);
    Verb.pr Verb.debug "%d pairs after CBA\n" (List.length n.pairs);
    n


(** [is_leaf n] tests if a node is a leaf. *)
let is_leaf n =
  n.pairs = []
  && n != prune_me
  && n.stn <> Simple_temp_net.empty
  (* The 3rd case should be covered by the 2nd case, but lazy eval
     will prevent it from harming performance and I am afraid. *)


(** [make_nth_child var_order t node n] builds the [n]th child of
    [node]. *)
let make_nth_child var_order t node n =
  assert (not (is_leaf node));
  if n > 1 then invalid_arg "make_nth_child: n > 1";
  let (i, j) as p = List.hd node.pairs
  and rest = List.tl node.pairs in
    try
      if n = 0
      then begin
	let slk = slack t node.stn p in
	let p_i = Inst.duration t.instance i in
	let constrnt = Simple_temp_net.before i j p_i in
	let stn = Simple_temp_net.add_constraint node.stn constrnt in
	  make_node var_order t stn rest slk (node.depth + 1)
      end else begin
	let slack = slack t node.stn (j, i) in
	let p_j = Inst.duration t.instance j in
	let constrnt = Simple_temp_net.after i j p_j in
	let stn = Simple_temp_net.add_constraint node.stn constrnt in
	  make_node var_order t stn rest slack (node.depth + 1)
      end
    with Simple_temp_net.Inconsistent -> prune_me


(** [complete_greedily var_order t stn pairs] completes the greedy
    solution under the given node, if it is feasible (otherwise the
    result is [prune_me]). *)
let complete_greedily var_order t node =
  let rec do_greedy node =
    if node.pairs = [] then
      node
    else
      do_greedy (make_nth_child var_order t node 0)
  in
    try do_greedy node with Simple_temp_net.Inconsistent -> prune_me


(** [compute_make_span t stn] computes the total time of the given
    solution from the earliest start times in the STN.


    You probably want to use [make_span] instead.  This should only
    be used for debugging. *)
let compute_make_span t stn =
  let rest_id = (Inst.number_of_ops t.instance) + 1 in
  let bounds = Simple_temp_net.all_bounds stn in
    Array.fold_left (fun m (i, est_i, _) ->
		       if i = 0 || i = rest_id then m
		       else
			 let p_i = Inst.duration t.instance i in
			 let finish = est_i + p_i in
			   if finish > m then finish else m)
      ~-1
      bounds


(** [make_span t stn] finds the makespan by looking up the earliest
    start time of the 'rest activity' which is constrained to happen
    after all jobs have finished. *)
let make_span t stn =
  let rest_id = (Inst.number_of_ops t.instance) + 1 in
    fst (Simple_temp_net.bounds stn rest_id)


(** [make_nth_child_with_greedy_pruning var_order t node n] makes
    the [n]th child node, but if that node has a greater makespan
    than the best incumbent seen so far, the greedy solution is
    computed under it and that is returned instead. *)
let make_nth_child_with_greedy_pruning var_order t node n =
  let c = make_nth_child var_order t node n in
    if (make_span t node.stn) >= t.best_incumbent
    then complete_greedily var_order t c
    else c


(** [num_children n] the raw number of children function.  Doesn't
    generate children so it can not be used in conjunction with
    [get_nth_child]. *)
let num_children n =
  if n.pairs = [] || is_leaf n then
    0
  else
    2


(** [get_children nth_child n] gets the children of a node.  If
    there is only one child then collapse down until there are two
    children, or the single child is a leaf or there are no
    children. *)
let rec get_children ?(depth=0) nth_child n =
  match nth_child n 0, nth_child n 1 with
    | l, r when l == prune_me && r == prune_me ->
	[]
    | l, r when l == prune_me ->
	if is_leaf r then
	  [ r ]
	else
	  get_children ~depth:(depth + 1) nth_child r
    | l, r when r == prune_me ->
	if is_leaf l then
	  [ l ]
	else
	  get_children ~depth:(depth + 1) nth_child l
    | l, r -> [ l; r; ]


(** [ensure_kids nth_child n] ensures that the children of [n] are
    generated. *)
let ensure_kids nth_child n =
  if n.pairs <> [] && not (is_leaf n) && not n.generated then begin
(*
    n.children <- get_children nth_child n;
*)
    n.children <- [ nth_child n 0; nth_child n 1 ];
    n.generated <- true;
  end


(** [make_num_children nth_child n] gets the number of children of a
    node. *)
let make_num_children nth_child =
  (fun n ->
     if n.pairs = [] || is_leaf n then
       0			 (* not an error, may be a deadend. *)
     else begin
       ensure_kids nth_child n;
       List.length n.children
     end)


(** [get_nth_child node n] gets the [n]th child of [node] *)
let get_nth_child node n =
  assert (n < (List.length node.children));
  List.nth node.children n


(** [make_leaf_cost t node] makes a function that gets the cost of a
    leaf node. *)
let make_leaf_cost t node =
  assert (is_leaf node);
  float (make_span t node.stn)


(** [make_child_costs t nth_child] makes a function that gets the
    cost value for each child that was used to order them.  Costs
    are expected to be increasing from the most preferred child to
    the least. *)
let make_child_costs t nth_child =
  (*
    (fun node ->
    match node.pairs with
    | (i, j) as p :: tl -> [ ~-.(slack t node.stn p);
    ~-.(slack t node.stn (j, i)); ]
    | _ -> invalid_arg "child_costs: node has no pairs.")
  *)
  (fun n ->
     ensure_kids nth_child n;
     List.map (fun c -> ~-.(c.slack)) n.children)


(** [make_log_child_costs t node] makes a function that gets the
    cost value for each child that was used to order them.  Costs
    are expected to be increasing from the most preferred child to
    the least. *)
let make_log_child_costs t node =
  match node.pairs with
    | (i, j) as p :: tl -> [ ~-.(log10 ((slack t node.stn p) +. 1.));
			     ~-.(log10 ((slack t node.stn (j, i)) +. 1.)); ]
    | _ -> invalid_arg "child_costs: node has no pairs."


(** [make_is_better t node saved] makes a function that checks if
    the given leaf node is better than the saved incumbent. *)
let make_is_better t node saved =
  let span = make_span t node.stn in
    span < saved.make_span


(** [make_copy_state t node] makes a function that copies a leaf
    node into a saved incumbent. *)
let make_copy_state t node =
  let span = make_span t node.stn in
    if span < t.best_incumbent
    then t.best_incumbent <- span;
    { stn_copy = node.stn; make_span = span }


(** [make_should_prune t incumbent n] test if a node should be
    pruned. *)
let make_should_prune t incumbent n =
  let l = is_leaf n in
    n == prune_me
    || ((n.pairs = []) && (not l))
    || ((not l) && ((make_span t n.stn) >= incumbent.make_span))


(** Checks that the solution times are actually a valid solution to
    the problem. *)
let check_times inst stn =
  let confs = Inst.conflicting_op_ids inst in
  let not_conflicting (i, j) =
    let t_i, _ = Simple_temp_net.bounds stn i in
    let t_j, _ = Simple_temp_net.bounds stn j in
      if t_i < t_j then
	t_i + (Inst.duration inst i) <= t_j
      else
	t_j + (Inst.duration inst j) <= t_i
  in
  let no_conflicts = List.for_all not_conflicting confs in
  let rec job_inline = function
    | i :: ((j :: _) as rest) ->
	let t_i, _ = Simple_temp_net.bounds stn i in
	let  t_j, _ = Simple_temp_net.bounds stn j in
	let dur = Inst.duration inst i in
	  t_i + dur <= t_j && job_inline rest
    | _ -> true
  in
  let in_line = List.for_all job_inline (Inst.job_op_ids inst) in
    if not no_conflicts then
      failwith "A conflict still exists in the solution";
    if not in_line then
      failwith "Activities of the same job are out of order";
    ()


(** [make_start_times t saved] gets a list of tuples ((job, op),
    est), which is a solution to the problem. *)
let make_start_times t saved =
  let bounds = Simple_temp_net.all_bounds saved.stn_copy in
  let n = Array.length bounds in
  let bounds = Array.sub bounds 1 (n - 2) in
  let times =
    Array.map (fun (i, est_i, _) -> Inst.op_of_id t.instance i, est_i) bounds
  in
    check_times t.instance saved.stn_copy;
    times


(** [make_fast_interface var_ordering log_costs complete_greedy t]
    makes the interface given a problem instance. [log_costs]
    applies log10 to the child costs. *)
let make_fast_interface var_ordering log_costs complete_greedy t =
  let module I = Bounded_depth_interface in
  let nth_child =
    if complete_greedy
    then make_nth_child_with_greedy_pruning var_ordering t
    else make_nth_child var_ordering t
  and is_better = make_is_better t
  and copy_state = make_copy_state t in
  let initial = initial_node var_ordering t in
  let child_costs =
    if log_costs
    then make_log_child_costs t
    else make_child_costs t nth_child
  in
    (initial,
     { (I.default num_children nth_child is_leaf is_better copy_state)
       with
	 I.max_depth = (List.length initial.pairs) + 1;
	 I.should_prune = make_should_prune t;
	 I.leaf_cost = make_leaf_cost t;
	 I.child_costs = child_costs;
     },
     make_start_times t)


(** [make_slow_interface var_ordering log_costs complete_greedy t]
    makes the interface given a problem instance. [log_costs]
    applies log10 to the child costs. *)
let make_slow_interface var_ordering log_costs complete_greedy t =
  let module I = Bounded_depth_interface in
  let nth_child =
(*
    if complete_greedy
    then make_nth_child_with_greedy_pruning var_ordering t
    else make_nth_child var_ordering t
*)
    make_nth_child var_ordering t
  and is_better = make_is_better t
  and copy_state = make_copy_state t in
  let num_children = make_num_children nth_child in
  let initial = initial_node var_ordering t in
  let child_costs =
    if log_costs
    then make_log_child_costs t
    else make_child_costs t nth_child
  in
    (initial,
     { (I.default num_children get_nth_child is_leaf is_better copy_state)
       with
	 I.max_depth = (List.length initial.pairs) + 1;
	 I.should_prune = make_should_prune t;
	 I.leaf_cost = make_leaf_cost t;
	 I.child_costs = child_costs;
     },
     make_start_times t)


let make_interface = make_slow_interface


let default_interface inst deadline =
  let prob =
    { deadline = deadline; instance = inst; best_incumbent = max_int; }
  in
(*
    make_interface (min_bslack2_pairs 3. 4.) false true prob
*)
    make_interface (min_bslack2_pairs 3. 4.) false false prob


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
