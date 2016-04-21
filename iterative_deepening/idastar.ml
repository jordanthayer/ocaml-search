(** An iterative deepening A* implementation.

    @author eaburns
    @since 2009-06-18
*)

open Printf

type 'a t = {
  parent : 'a t;
  state : 'a;
  g : float;
  f : float;
}

let iter_col_name = "iter"
  (** The key for the iterations columns. *)

let iter_col_chan = stdout
  (** The channel to which the alternate column data for iterations will
      be output. *)


let output_iter_colnames () =
  (** [output_iter_colnames ()] outputs the alternate column data to
      stdout. *)
  Datafile.write_alt_colnames iter_col_chan iter_col_name
    ["iter num"; "iter bound"; "iter expanded"; "iter generated"]


let output_iter_row num bound info =
  (** [output_iter_row num bound exp gen] outputs a row of altcol
      data. *)
  Datafile.write_alt_row_prefix iter_col_chan iter_col_name;
  Verb.pf Verb.always iter_col_chan "%d\t%f\t%d\t%d\n"
    num bound info.Limit.expanded info.Limit.generated


let compare a b =
  (** [compare a b] compares [a] and [b], first on f-value and
      secondly on g-value.  The result is <0 if a is better than [b],
      >0 if [a] is worse than [b] and 0 if the nodes are of equal
      quality. *)
  if a.f = b.f
  then int_of_float (b.g -. a.g)
  else int_of_float (a.f -. b.f)


let make_sorted_expand compare info expand h =
  (** [make_sorted_expand expand h] creates a function that expands a
      node building a list of its children in order of lowest f
      first. *)
  (fun node ->
     Limit.incr_exp info;
     let children = expand node.state node.g in
       List.fast_sort
	 compare
	 (List.map (fun (child, g) ->
		      Limit.incr_gen info;
		      { parent = node;
			state = child;
			g = g;
			f = g +. (h child) })
	    children))


let make_expand info expand h =
  (** [make_expand expand h] creates a function that expands a node
      building a list of its children. *)
  (fun node ->
     Limit.incr_exp info;
     let children = expand node.state node.g in
       List.map (fun (child, g) ->
		   Limit.incr_gen info;
		   { parent = node;
		     state = child;
		     g = g;
		     f = g +. (h child) })
	 children)


let depth_first_search info is_goal expand h cost_bound init =
  (** A cost-bounded depth-first search.  The result is a (node option
      * float) where the fst is possibly the solution and the snd is
      the best out of bound cost value encountered. *)

  let rec dfs nnodes cost_bound best_pruned node =
    assert (nnodes > 0);
    Limit.curr_q info nnodes;
    if Limit.halt_p info then
      None, infinity
    else if node.f > cost_bound then begin
      Limit.incr_prune info;
      None, Math.fmin best_pruned node.f
    end else if is_goal node.state then
      Some node, best_pruned
    else
      let children = expand node in
      let nnodes = nnodes + (List.length children) in
      let solution, best_pruned =
	consider_children nnodes cost_bound best_pruned children
      in match solution with
	| (Some _) as sol -> sol, best_pruned
	| None -> None, best_pruned

  and consider_children nnodes cost_bound best_pruned children =
    (* Consider each child.  Stop early if a solution is found. *)
    match children with
      | [] ->
	  None, best_pruned
      | hd :: tl -> begin
	  let solution, best_pruned = dfs nnodes cost_bound best_pruned hd
	  in match solution with
	    | (Some _) as sol ->
		sol, best_pruned
	    | None ->
		consider_children (nnodes - 1) cost_bound best_pruned tl
	end
  in dfs 1 cost_bound infinity init


let search ?(limit = [Limit.Never]) is_goal expand h init =
  (** [search ?limit is_goal expand is_better h init] performs an IDA*
      search starting at the initial node [init], proceeding using the
      [expand] operation until a goal is found with the [is_goal]
      predicate.  [h] computes the heuristic value of a state. *)

  let info = Limit.make Limit.Nothing limit (fun _ _ -> assert false) in
  let expand = make_expand info expand h in
  let depth_first_search = depth_first_search info is_goal expand h in

  let rec do_iteration num init bound =
    (* Recursively perform iterations of the iterative deepening
       search until the solution is found. *)
    if Limit.halt_p info
    then ()
    else
      let solution, best_pruned = depth_first_search bound init in
	output_iter_row num bound info;
	match solution with
	  | Some node ->
	      Limit.new_incumbent info (Limit.Incumbent (0.0, node.state))
	  | None ->
	      do_iteration (num + 1) init best_pruned
  in

  let rec init_node = {
    parent = init_node;
    state = init;
    g = 0.0;
    f = h init;
  } in

    output_iter_colnames ();
    do_iteration 1 init_node (h init);
    Limit.results5 info
