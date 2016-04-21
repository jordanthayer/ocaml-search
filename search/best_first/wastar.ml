(* Comparison between wA* and some variant of wA* that is mostly the
   same, however, when re-encountering a node that has already been
   generated just trace through the tree of its children and update
   them instead of putting the duplicate back on OPEN and waiting for
   propagation. *)

type 'state node = {
  state : 'state;
  mutable g : float;
  mutable f : float;
  mutable parent : 'state node;
  mutable kids : 'state node array;
  mutable ops : float array;
  mutable pq_pos : int;
}

let is_better a b =
  if a.f = b.f then a.g > b.g else a.f < b.f

let pq_update n ind =
  n.pq_pos <- ind

let subtree_updates = ref 0

let node_updates = ref 0

let rec on_path eq hash ?(path=Htable.create hash eq 149) key node =
  if node.parent != node then begin
    let k = key node.state in
    if Htable.mem path k then
      true
    else begin
      Htable.add path k node;
      on_path eq hash ~path key node.parent
    end
  end else
    false

(** Update a node's g value and all of its kids.  Actually, since the
    'kids' array probably doesn't contain the generating parent (in
    domains that prune parents), this is probably technically incorrect
    compared to the version than re-opens nodes.  This version will not
    fix the inconsistency with respect to the old parent node which is not
    taken into account in 'kids'.  *)
let update_subtree parent_updt prkey key eq hash opn ~node ~parent g =
  let rec do_update ~node ~parent g =
    incr node_updates;
    node.f <- node.f -. node.g +. g;
    node.g <- g;
    node.parent <- parent;
    parent_updt node.state parent.state;
    if node.pq_pos <> Dpq.no_position then
      Dpq.see_update opn node.pq_pos
    else
      for i = 0 to Array.length node.kids - 1 do
	let k = node.kids.(i) in
	let g' = g +. node.ops.(i) in
	assert (node.ops.(i) >= 0.);
	if k.g > g' then
	  do_update ~node:k ~parent:node g'
      done in
  incr subtree_updates;
  do_update ~node ~parent g

(** Update a node's g value and then pop it back on open to lazily
    update the subtree beneath it. *)
let update_node parent_updt _prkey _key _eq _hash opn ~node ~parent g =
  incr node_updates;
  node.f <- node.f -. node.g +. g;
  node.g <- g;
  node.parent <- parent;
  parent_updt node.state parent.state;
  if node.pq_pos <> Dpq.no_position then
    Dpq.see_update opn node.pq_pos
  else
    Dpq.insert opn node

(** Expands a node, generating *all* of its children including the
    parent that generated its state.  *)
let expand_with_parent parent_updt domain_expand node =
  let state = node.state in
  let pstate = node.parent.state in
  parent_updt state state;
  let kids = domain_expand node.state node.g in
  parent_updt state pstate;
  kids

(** Expand a node, inserting the appropriate children into open.

    @param update is either [update_subtree] or [update_node].  The
    former will update the subtree immediately and the latter will
    just update the duplicate node and put it on the open list for
    lazily updating of the subtree.
*)
let expand subtree limits parent_updt  domain_expand h key update opn cls
    parent =
  let kids = expand_with_parent parent_updt domain_expand parent in
  Limit.incr_exp limits;
  Limit.incr_gen_n limits (List.length kids);
  let child_node (state, g) =
    let k = key state in
    try
      let node = Htable.find cls k in
      if node.g > g then update opn ~node ~parent g;
      node
    with Not_found ->
      let h = h state in
      let node =
	{ state = state;
	  g = g;
	  f = g +. h;
	  parent = parent;
	  kids = [||];
	  ops = [||];
	  pq_pos = Dpq.no_position; } in
      Htable.add cls k node;
      Dpq.insert opn node;
      node in
  let nodes = List.map child_node kids in
  (* only allocate the kids array if we are updating subtrees
     immediately. *)
  if subtree then begin
    parent.kids <- Array.of_list nodes;
    let op_costs = List.map (fun (_, g) -> g -. parent.g) kids in
    parent.ops <- Array.of_list op_costs
  end

let search subtree update parent_updt domain_expand h key eq hash is_goal
    limits init =
  let rec init_node =
    { state = init;
      g = 0.;
      f = h init;
      parent = init_node;
      kids = [||];
      ops = [||];
      pq_pos = Dpq.no_position; } in
  let opn = Dpq.create is_better pq_update 149 init_node in
  let cls = Htable.create hash eq 149 in
  let expand =
    expand subtree limits parent_updt domain_expand h key update opn cls in
  Dpq.insert opn init_node;
  Htable.add cls (key init) init_node;
  while
    not (Limit.has_incumbent limits)
    && not (Dpq.empty_p opn)
    && not (Limit.halt_p limits)
  do
    let n = Dpq.extract_first opn in
    if is_goal n.state then
      Limit.new_incumbent limits (Limit.Incumbent (n.g, n.state))
    else
      expand n
  done

let graphsearch results unwrap sface args =
  let wt = Search_args.get_float "Bugsy.bugsy" args 0 in
  let subtree = Search_args.get_bool "Bugsy.bugsy" args 1 in
  let update = if subtree then update_subtree else update_node in
  let h = sface.Search_interface.h in
  let h' s = wt *. h s in
  let limits = sface.Search_interface.info in
  let eq = sface.Search_interface.equals in
  let key = sface.Search_interface.key in
  let hash = sface.Search_interface.hash in
  let is_goal = sface.Search_interface.goal_p in
  let domain_expand = sface.Search_interface.domain_expand in
  let parent_updt = sface.Search_interface.parent_update in
  let prkey = sface.Search_interface.key_printer in
  let init = sface.Search_interface.initial in
  search subtree (update parent_updt prkey key eq hash) parent_updt
    domain_expand h' key eq hash is_goal limits init;
  Datafile.write_pairs stdout
    ["update subtree", string_of_bool subtree;
     "subtree updates", string_of_int !subtree_updates;
     "node updates", string_of_int !node_updates; ];
  unwrap
    (function Limit.Nothing -> None
      | Limit.Incumbent (g, state) -> Some (state, g))
    (results limits)

let dups sface args =
  graphsearch Limit.results6 Limit.unwrap_sol6 sface args

let no_dups sface args =
  graphsearch Limit.results5 Limit.unwrap_sol5 sface args
