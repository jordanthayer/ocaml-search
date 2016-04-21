(** Korf's frontier search using divide-and-conquer solution
    reconstruction.

    This may not be working totally correctly.

    @author eaburns
    @since 2010-06-28
*)

(** {1 Search nodes} ****************************************)


module Intset = Set.Make(struct
			   type t = int
			   let compare = compare
			 end)


type ('key, 'state) node = {
  key : 'key;
  mutable pruned_ops : Intset.t;

  mutable pq_pos : int;

  mutable state : 'state option;
  mutable relay : 'state option;
  mutable g : float;
  mutable f : float;
}


let dummy_node key =
  (** [dummy_node key] makes a dummy node for the given key. *)
  {
    key = key;
    pruned_ops = Intset.empty;
    pq_pos = Dpq.no_position;
    state = None;
    relay = None;
    g = infinity;
    f = infinity;
  }


let node h key relay state g =
  (** [node h key relay state g] makes a new node. *)
  {
    key = key;
    pruned_ops = Intset.empty;
    pq_pos = Dpq.no_position;
    state = Some state;
    relay = relay;
    g = g;
    f = g +. (h state);
  }


let get_relay parent state ~h ~g =
  (** [get_relay parent state ~h ~g] gets the relay node to use for
      the given node. *)
  let parent_h = parent.f -. parent.g in
    if parent_h > parent.g && h <= g then Some state else parent.relay



let undummy_node h state g ~parent ~node =
  (** [undummy_node h state g ~parent ~node] takes a dummy node and
      makes it a real node. *)
  let h = h state in
    node.g <- g;
    node.f <- g +. h;
    node.state <- Some state;
    node.relay <- get_relay parent state ~h ~g


let update_pq_pos n i = n.pq_pos <- i
  (** [update_pq_pos n i] updates the node's position in a priority
      queue. *)


let is_better a b = if a.f = b.f then a.g > b.g else a.f < b.f
  (** [is_better a b] compares two nodes, first on f then on g. *)


let is_dummy node = node.state = None
  (** [is_dummy node] tests if the node is a dummy node. *)


let get_state node = match node.state with
    (** [get_state node] gets the state. *)
  | Some st -> st
  | None -> invalid_arg "get_state: node has no state"


let update_node h reparent new_parent new_g state node =
  (** [update_node h reparent new_parent new_g state node]
      updates the parent of the given node. *)
  match node.state with
    | None ->
	undummy_node h state new_g ~parent:new_parent ~node
    | Some st ->
	reparent st (get_state new_parent);
	node.f <- node.f -. (node.g -. new_g);
	node.g <- new_g


let prune_operator node op =
  (** [prune_operator node op] prunes the given operator. *)
  node.pruned_ops <- Intset.add op node.pruned_ops


let is_pruned node op = Intset.mem op node.pruned_ops
  (** [is_pruned node op] tests if the given operator has been
      pruned. *)


let expand_node info expand gen_op node =
  (** [expand_node info expand gen_op node] expands the given node
      filtering out all children generated via a pruned operator. *)
  let state = get_state node in
    List.filter (fun (c, _) ->
		   Limit.incr_gen info;
		   let op = gen_op c in
		     not (is_pruned node op))
      (expand state node.g)


(** {1 Frontier search} ****************************************)

exception Halted

let prune_pred_ops pr_key nodes neighbor_ops state =
  (** [prune_pred_ops pr_key node neighbor_ops state] prunes the
      operator on each predecessor that will generate the given state.
      If the predecessor is not already opened then a dummy node is
      added for it. *)
  let preds = neighbor_ops state in
    List.iter (fun (k, op) ->
		 try
		   let n = Htable.find nodes k in
(*
		     Verb.pr Verb.debug "\t\tpruning operator %d on %s\n"
		       op (pr_key k);
*)
		     prune_operator n op
		 with Not_found ->
		   let d = dummy_node k in
(*
		     Verb.pr Verb.debug
		       "\t\tpruning operator %d on %s (dummy)\n"
		       op (pr_key k);
*)
		     prune_operator d op;
		     Htable.add nodes k d)
      preds


let handle_child info h reparent pr_key key neighbor_ops nodes open_list
    parent (state, g) =
  (** [handle_child info h reparent pr_key key neighbor_ops nodes
      open_list parent (state, g)] handles the generation of a new
      child. *)
  let k = key state in
(*
    Verb.pr Verb.debug "\tgenerated %s\n" (pr_key k);
*)
    prune_pred_ops pr_key nodes neighbor_ops state;
    try
      let node = Htable.find nodes k in
	Limit.incr_dups info;
	if node.g > g
	then begin
	  update_node h reparent parent g state node;
	  (* if the node was a dummy, then it goes on open here. *)
	  if node.pq_pos = Dpq.no_position
	  then Dpq.insert open_list node
	  else Dpq.see_update open_list node.pq_pos
	end
    with Not_found ->
      let node = node h k (get_relay parent state ~h:(h state) ~g) state g in
	Htable.add nodes k node;
	Dpq.insert open_list node



let astar info max_nodes pr_key hash eq key reparent neighbor_ops gen_op
    h expand is_goal root =
  (** [astar info max_nodes pr_key hash eq key reparent neighbor_ops
      gen_op h expand is_goal root] performs a frontier search. *)
  let root_node = node h (key root) (Some root) root 0. in
  let open_list = Dpq.create is_better update_pq_pos 100 root_node in
  let nodes = Htable.create hash eq 100 in
  let goal = ref None in
    Dpq.insert open_list root_node;
    Htable.add nodes root_node.key root_node;
    while not (Dpq.empty_p open_list) && !goal = None do
      if Limit.halt_p info then raise Halted;
      let n = Dpq.extract_first open_list in
      let nnodes = Htable.length nodes in
(*
	Verb.pr Verb.debug "expanding %s\n" (pr_key n.key);
*)
	if nnodes > !max_nodes then max_nodes := nnodes;
	if is_goal (get_state n)
	then goal := Some n
	else begin
	  Limit.incr_exp info;
	  let kids = expand_node info expand gen_op n in
	    List.iter (handle_child info h reparent pr_key key neighbor_ops
			 nodes open_list n)
	      kids;

	    (* Effectively deletes the node.  We do this after
	       iterating over the children because we don't want them to
	       regenerate a dummy version of the node that was just
	       expanded. *)
	    Htable.remove nodes n.key;
	end
    done;
    match !goal with
      | Some goal ->
	  begin match goal.relay with
	    | None -> None
	    | Some n -> Some (n, goal.g)
	  end
      | None -> None


let fix_parents reparent path init =
  (** [fix_parents reparent path init] sets the solution path by
      chaining the parent pointers of all nodes in the given path. *)
  List.fold_left (fun parent node ->
		    reparent node parent;
		    node)
    init path


let unwrap_sol = function
  | Limit.Nothing -> None
  | Limit.Incumbent (g, n) -> Some (n, g)


let astar_dups sface args =
  (** [dups sface args] divide-and-conquer frontier A* search. *)
  let max_nodes = ref 0 in
  let info = sface.Search_interface.info in
  let root = sface.Search_interface.initial in
  let reparent = sface.Search_interface.parent_update in
  let search sface =
    let hash = sface.Search_interface.hash
    and eq = sface.Search_interface.equals
    and key = sface.Search_interface.key
    and pr_key = sface.Search_interface.key_printer
    and reparent = sface.Search_interface.parent_update
    and neighbor_ops = sface.Search_interface.get_neighbor_operators
    and gen_op = sface.Search_interface.get_generating_operator
    and h = sface.Search_interface.h
    and expand = sface.Search_interface.domain_expand
    and is_goal = sface.Search_interface.goal_p
    and root = sface.Search_interface.initial
    in
      astar info max_nodes pr_key hash eq key reparent neighbor_ops gen_op
	h expand is_goal root
  in
    try
      let sol = Solution_reconstruct.divide_and_conquer search sface in
	begin match sol with
	  | None -> ()
	  | Some (path, cost) ->
	      let goal_node = fix_parents reparent path root in
		Limit.new_incumbent info (Limit.Incumbent (cost, goal_node));
	end;
	raise Halted
    with Halted ->
      Datafile.write_pairs stdout
	[ "max nodes in memory", string_of_int !max_nodes; ];
      Limit.unwrap_sol6 unwrap_sol (Limit.results6 info)
