(** A job shop scheduling solver using constraint based analysis (CBA,
    "Slack-Based Heuristics for Constraint Satisfaction Scheduling",
    Smith and Cheng AAAI 93).

    This implementation uses in-place modification and therefore it is
    split into two parts.  The first part deals with the current state
    structure and the second part is the search interface which
    represents states as integers (depths).  The search backtracks by
    'poping' the current state to the desired depth.

    @author eaburns
    @since 2010-12-16
*)

open Printf

module Inst = Job_shop_instance
module Stn = D_stn
(*
module Stn = struct
  (* Use the old Simple_temp_net module but mimic the D_stn
     functionality. *)

  type t = Simple_temp_net.t Stack.t

  let add_constraint t c =
    let stn = Stack.top t in
    let stn' = Simple_temp_net.add_constraint stn c in
      Stack.push stn' t


  let create_with nnodes cs =
    let stn = Simple_temp_net.create nnodes in
    let stn' = Simple_temp_net.add_constraints stn cs in
    let stk = Stack.create () in
      printf "Creating an STN\n";
      Stack.push stn' stk;
      stk

  let bounds t =
    let stn = Stack.top t in
      Simple_temp_net.bounds stn

  let all_bounds t =
    let stn = Stack.top t in
      Simple_temp_net.all_bounds stn

  let copy t =
    Stack.copy t

  let undo t =
    Stack.pop t

  let before = Simple_temp_net.before

  let not_earlier_than = Simple_temp_net.not_earlier_than

  let no_later_than = Simple_temp_net.no_later_than

end
*)

type pair = {
  (* An unresolved ordering between jobs [fst] and [snd].
     [fst]->[snd] does not have more slack than [snd]->[fst]. *)
  fst : int;
  snd : int;
  slack_fs : int;
  slack_sf : int;
  f : float;			  (* some function to compare pairs *)
}

type state = {
  (* The current path of the search. *)

  inst : Inst.t;

  rest_node : int;
  (* the STN id of the rest-node (dummy node that occurs after
     everything is finished). *)

  pairs : pair list Stack.t;
  (* unresolved pairs at each depth. *)

  min_pair : pair Stack.t;
  (* the minimum unresolved pair at each depth. *)

  stn_depth : int Stack.t;
  (* The STN may change more than once for a single search depth: this
     tracks the number of STN changes for each search depth. *)

  stn : Stn.t;

  mutable depth : int;
}


let dummy_pair = {
  fst = ~-1;
  snd = ~-1;
  slack_fs = ~-1;
  slack_sf = ~-1;
  f = nan;
}


type solution = {
  stn_copy : Stn.t;
  make_span : int;
}



(** Finds the slack beween i->j. *)
let slack inst stn i j =
  let est_i, _ = Stn.bounds stn i and p_i = Inst.duration inst i in
  let _, lst_j = Stn.bounds stn j and p_j = Inst.duration inst j in
  let lft_j = lst_j + p_j in
    lft_j - est_i - (p_i + p_j)


let bslack n s_ij s_ji =
  let min_s = if s_ij < s_ji then s_ij else s_ji in
  let max_s = if s_ij < s_ji then s_ji else s_ij in
  let s = float min_s /. float max_s in
    float s_ij /. (Math.nth_root n s)


let bslack2 n1 n2 s_ij s_ji =
  let min_s = if s_ij < s_ji then s_ij else s_ji in
  let max_s = if s_ij < s_ji then s_ji else s_ij in
  let s = float min_s /. float max_s in
  let sf = float s_ij in
    sf /. (Math.nth_root n1 s) +. sf /. (Math.nth_root n2 s)


(** Creates an unresolved pair between [i] and [j]. *)
let pair f inst stn i j =
  let s_ij = slack inst stn i j and s_ji = slack inst stn j i in
  let f_ij = f s_ij s_ji and f_ji = f s_ji s_ij in
  let f = if f_ij < f_ji then f_ij else f_ji in
    if s_ij > s_ji then
      { fst = i; snd = j; slack_fs = s_ij ; slack_sf = s_ji; f = f; }
    else
      { fst = j; snd = i; slack_fs = s_ji ; slack_sf = s_ij; f = f; }


(** Updates the pair given the new state. *)
let update_pair f inst stn p =
  pair f inst stn p.fst p.snd


(** Tests if there is still a possible conflict. *)
let still_unresolved inst stn pair =
  let i = pair.fst and j = pair.snd in
  let est_i, lst_i = Stn.bounds stn i and p_i = Inst.duration inst i in
  let est_j, lst_j = Stn.bounds stn j and p_j = Inst.duration inst j in
  let lft_i = lst_i + p_i and lft_j = lst_j + p_j in
  let i_less_j = lft_i - est_j and j_less_i = lft_j - est_i in
  let sum = p_i + p_j in
    if i_less_j < sum && sum <= j_less_i then begin
      ignore (Stn.add_constraint stn (Stn.before i j p_i));
      false
    end else if j_less_i < sum && sum <= i_less_j then begin
      ignore (Stn.add_constraint stn (Stn.before j i p_j));
      false
    end else if sum > j_less_i && sum > i_less_j then begin
      failwith "Inconsistency in CBA";
      (* raise Simple_temp_net.Inconsistent *)
    end else
      true


(** Performs constraint based analysis to resolve some orderings.
    This function updates the STN. *)
let cba f inst stn pairs =
  let rec do_cba ?(ac=[]) ?(ch=0) f inst stn = function
    | [] -> ac, ch
    | p :: ps ->
	if still_unresolved inst stn p then
	  do_cba ~ac:(p :: ac) ~ch f inst stn ps
	else
	  let pairs' =
	    List.fold_left (fun l p -> update_pair f inst stn p :: l)
	      (List.rev_map (update_pair f inst stn) ps) ac
	  in
	    do_cba ~ac:[] ~ch:(ch + 1) f inst stn pairs'
  in
  let pairs', nchanges = do_cba f inst stn pairs in
    if nchanges = 0 then
      List.map (update_pair f inst stn) pairs', nchanges
    else
      pairs', nchanges


(** Gets the pair with the minimum [f] value and the rest.  Ties are
    broken on maximum f->s slack. *)
let get_min_pair pairs =
  let rec get_min ac m = function
    | [] -> m, ac
    | p :: ps ->
	if p.f < m.f || (p.f = m.f && p.slack_fs > m.slack_fs) then
	  get_min (m :: ac) p ps
	else
	  get_min (p :: ac) m ps
  in
    match pairs with
      | [] -> dummy_pair, []
      | p :: ps -> get_min [] p ps



(** Adds to the constraint list [ac] a set of constraints that ensure
    each operation of a job occurs after its predecessors, not before
    time 0 and not after the deadline. *)
let rec line_up_job ?(first=true) inst rest_node ac ops =
  match ops with
    | [] -> ac
    | i :: [] ->
	let p_i = Inst.duration inst i in
	  Stn.before i rest_node p_i :: ac
    | i :: ((j :: _) as tl) ->
	let p_i = Inst.duration inst i in
	let before = if first then [ Stn.not_earlier_than i 0 ] else [] in
	let ac' = before @ (Stn.before i j p_i) :: ac in
	  line_up_job ~first:false inst rest_node ac' tl


(** Make the single state structure that will be used for search. *)
let make_state f inst deadline =
  let nnodes = (Inst.number_of_ops inst) + 1 in
  let rest_node = nnodes in
  let jobs = Inst.job_op_ids inst in
  let cnsts =
    List.fold_left (fun l id -> line_up_job inst rest_node l id)
      [ Stn.no_later_than rest_node deadline ] jobs in
  let stn =  Stn.create_with nnodes cnsts in
  let cnflcts = Inst.conflicting_op_ids inst in
  let pairs = List.map (fun (i, j) -> pair f inst stn i j) cnflcts in
  let pairs', stn_depth = cba f inst stn pairs in
  let min_pair, pairs'' = get_min_pair pairs' in
  let pair_stk = Stack.create () in
  let min_pair_stk = Stack.create () in
  let stn_depth_stk = Stack.create () in
    Verb.pr Verb.debug "%d conflicts\n" (List.length cnflcts);
    Verb.pr Verb.debug "%d pairs after CBA\n" (List.length pairs');
    Stack.push pairs'' pair_stk;
    Stack.push min_pair min_pair_stk;
    Stack.push stn_depth stn_depth_stk;
    {
      inst = inst;
      rest_node = rest_node;
      pairs = pair_stk;
      min_pair = min_pair_stk;
      stn_depth = stn_depth_stk;
      stn = stn;
      depth = 0;
    }


(** Makes a function that tests if the given state is a leaf. *)
let state_is_leaf state =
  let pairs_stk = state.pairs in
  let min_pair_stk = state.min_pair in
    (fun () ->
       (Stack.top pairs_stk) = [] && (Stack.top min_pair_stk) == dummy_pair)


(** Makes a function that gets the number of children of a state. *)
let state_num_children state =
  let is_leaf = state_is_leaf state in
    (fun () -> if is_leaf () then 0 else 2)


(** Makes a function that gets the [rank]'th child of the given
    state. *)
let state_nth_child f state =
  let inst = state.inst in
  let stn = state.stn in
  let pairs_stk = state.pairs in
  let min_pair_stk = state.min_pair in
  let stn_depth_stk = state.stn_depth in
  let nchildren = state_num_children state in
    (fun rank ->
       assert (rank < (nchildren ()));
       let pairs = Stack.top pairs_stk in
       let min_pair = Stack.top min_pair_stk in
       let stn_depth = Stack.top stn_depth_stk in
       let fst = if rank = 0 then min_pair.fst else min_pair.snd in
       let snd = if rank = 0 then min_pair.snd else min_pair.fst in
       let dur = Inst.duration state.inst fst in
	 ignore (Stn.add_constraint state.stn (Stn.before fst snd dur));
	 let pairs' = List.map (update_pair f inst stn) pairs in
	 let pairs'', stn_changes = cba f inst stn pairs' in
	 let min_pair', remaining = get_min_pair pairs'' in
	 let stn_depth' = stn_changes + 1 + stn_depth in
	   (* [stn_changes] from [cba] propogation, +1 for the
	      ordering asserted by the search and [stn_depth] previous
	      depth. *)
	   Stack.push remaining pairs_stk;
	   Stack.push min_pair' min_pair_stk;
	   Stack.push stn_depth' stn_depth_stk;
	   state.depth <- state.depth + 1)


(** Gets a function that gives the costs of the children at the
    current state. *)
let state_child_costs state =
  let min_pair_stk = state.min_pair in
    (fun () ->
       let p = Stack.top min_pair_stk in
	 [ float ~-(p.slack_fs); float ~-(p.slack_sf); ])


(** Makes a function that gets the make span of the current state. *)
let state_make_span state =
  let stn = state.stn and rest_node = state.rest_node in
    (fun () -> let est, _ = Stn.bounds stn rest_node in est)



(** Makes a function that copies the current state into a solution. *)
let state_copy state =
  let stn = state.stn in
  let make_span = state_make_span state in
    (fun () ->
       { stn_copy = Stn.copy stn; make_span = make_span () })


(** Makes a function that tests if the current state si better than an
    incumbent solution. *)
let state_is_better state =
  let make_span = state_make_span state in
    (fun saved -> make_span () < saved.make_span)


(** Makes a function that tests if the current state should be
    pruned. *)
let state_should_prune state =
  let make_span = state_make_span state in
    (fun saved -> make_span () >= saved.make_span)


(** Makes a function that reverts to the state at the given depth. *)
let state_revert state =
  let pairs_stk = state.pairs in
  let min_pair_stk = state.min_pair in
  let stn_depth_stk = state.stn_depth in
  let stn = state.stn in
    (fun fun_name depth ->
       try
	 assert (depth <= state.depth);
	 if depth >= 0 then begin
	   let nundo = state.depth - depth in
	   let stn_depth = Stack.top stn_depth_stk in
	     for i = 1 to nundo do
	       ignore (Stack.pop pairs_stk);
	       ignore (Stack.pop min_pair_stk);
	       ignore (Stack.pop stn_depth_stk);
	     done;
	     let stn_depth' = Stack.top stn_depth_stk in
	     let nundo_stn = stn_depth - stn_depth' in
	       for i = 1 to nundo_stn do ignore (Stn.undo stn) done;
	       state.depth <- depth
	 end
       with
	 | Stack.Empty ->
	     failwith (sprintf "%s: Failed to revert to depth %d\n"
			 fun_name depth)
	 | Assert_failure _ ->
	     failwith (sprintf "%s: Asked for depth %d, at depth %d\n"
			 fun_name depth state.depth))


(** {6 Search interface} The search interface represents nodes in the
    search space as integers.  Each integer is the depth along the
    current path.  When the search back-tracks, it gives a new depth
    value which must be less than the current depth.  The search pops
    changes off of the stacks in the main [state] to get to the
    desired depth for the backtrack. *)


let make_num_children revert state =
  let num_children = state_num_children state in
    (fun n ->
       if n < 0 then
	 0
       else begin
	 revert "num_children" n;
	 num_children ()
       end)


let make_nth_child revert f state =
  let nth_child = state_nth_child f state in
    (fun n rank ->
       assert (n >= 0);
       revert "nth_child" n;
       try
	 nth_child rank;
	 state.depth
       with Simple_temp_net.Inconsistent ->
	 ~-1)


let make_is_leaf revert state =
  let is_leaf = state_is_leaf state in
    (fun n ->
       revert "is_leaf" n;
       is_leaf ())


let make_is_better revert state =
  let is_better = state_is_better state in
    (fun n saved ->
       revert "is_better" n;
       is_better saved)


let make_copy_state revert state =
  let copy = state_copy state in
    (fun n ->
       revert "copy_state" n;
       copy ())


let make_should_prune revert state =
  let should_prune = state_should_prune state in
    (fun saved n ->
       revert "should_prune" n;
       should_prune saved)


let make_leaf_cost revert state =
  let make_span = state_make_span state in
    (fun n ->
       revert "leaf_cost" n;
       float (make_span ()))


let make_child_costs revert state =
  let child_costs = state_child_costs state in
    (fun n ->
       revert "child_costs" n;
       child_costs ())


(** Checks that the solution times are actually a valid solution to
    the problem. *)
let check_times inst stn =
  let confs = Inst.conflicting_op_ids inst in
  let not_conflicting (i, j) =
    let t_i, _ = Stn.bounds stn i and t_j, _ = Stn.bounds stn j in
      if t_i < t_j then
	t_i + (Inst.duration inst i) <= t_j
      else
	t_j + (Inst.duration inst j) <= t_i
  in
  let no_conflicts = List.for_all not_conflicting confs in
  let rec job_inline = function
    | i :: ((j :: _) as rest) ->
	let t_i, _ = Stn.bounds stn i and t_j, _ = Stn.bounds stn j in
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


(** Computes the make span from the times of each job.  This is just
    to verify that the STN is not giving back a bad make span. *)
let compute_make_span inst times =
  Array.fold_left (fun max ((job, op), st) ->
		     let id = Inst.id_of_op inst job op in
		     let dur = Inst.duration inst id in
		     let et = st + dur in
		       if et > max then et else max)
    ~-1 times



(** Gets a list of tuples ((job, op), est), which is a solution to
    the problem. *)
let make_start_times inst saved =
  let bounds = Stn.all_bounds saved.stn_copy in
  let n = Array.length bounds in
  let bounds = Array.sub bounds 1 (n - 2) in
    (* chop the dummy-0 node and the 'rest-node' *)
  let times =
    Array.map (fun (i, est_i, _) -> Inst.op_of_id inst i, est_i) bounds
  in
    check_times inst saved.stn_copy;
    if compute_make_span inst times <> saved.make_span then
      failwith "Computed make span does not match saved make span";
    times



let default_interface inst deadline =
  let module I = Bounded_depth_interface in
  let f = bslack2 3. 4. in
  let state = make_state f inst deadline in
  let revert = state_revert state in

  let num_children = make_num_children revert state in
  let nth_child = make_nth_child revert f state in
  let is_leaf = make_is_leaf revert state in
  let is_better = make_is_better revert state in
  let copy_state = make_copy_state revert state in
  let initial = state.depth in

  let max_depth = (List.length (Stack.top state.pairs)) + 2 in
    (* +1 for the 'min_pair', not on the list and apparently an extra
       +1 too. *)
  let should_prune = make_should_prune revert state in
  let leaf_cost = make_leaf_cost revert state in
  let child_costs = make_child_costs revert state in

  let iface =
    { (I.default num_children nth_child is_leaf is_better copy_state)
      with
	I.max_depth = max_depth;
	I.should_prune = should_prune;
	I.leaf_cost = leaf_cost;
	I.child_costs = child_costs;
    }
  in
    initial, iface, make_start_times inst
