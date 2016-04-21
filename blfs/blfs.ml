(* simple version of BLFS *)


(**
 * Interface to connect to the model being used.
 *
 *   node = tree node
 *   data = any path-dependent info needed by the model in iter_children
 *
 * in each iteration, iter_children returns the child node and its new data
 *)
type ('node, 'data) model_interface =
{
  initial_data  : 'node -> 'data;
  update_leaf   : 'node -> 'data -> unit;
  update_prune  : 'node -> 'data -> unit;
  (* take_child(index, next_data), notify_skipping(), node, prev_data, bound *)
  iter_children : ('node -> int -> 'data -> unit) -> (unit -> unit) -> 'node -> 'data -> float -> unit;
  (* calculates a bound to visit the desired nodes, returns (bound, predicted) *)
  next_bound : int -> (float * int);

  dump : unit -> unit;
  (* dump the model. *)
}

(**
 * creates the interface for BLFS to use to the model.
 *)
let make_interface ?(dump=(fun () -> ()))
    init_data up_leaf up_prune iter_child n_bound =
  {
    initial_data = init_data;
    update_leaf = up_leaf;
    update_prune = up_prune;
    iter_children = iter_child;
    next_bound = n_bound;
    dump = dump;
  }

(* raised if halt_p returns true or if solution found *)
exception Halted

(**
 * Performs a bounded depth-first search.
 *
 *   model = model used by blfs
 *   info  = search info
 *   bound = the cost bound to use
 *   root  = the root node
 *)
let bounded_dfs model info bound root =
  let complete = ref true in
    (* true if whole search space visited - init to true *)
  let skipped () = complete := false in
    (* called if any nodes skipped over *)
  let rec visit n data =
    (* returns true if no skips *)
    if (Info.halt_p info)
    then raise Halted
    else if (Info.leaf_p info n) then
      (if (Info.check_best_and_optimal info n)
       then raise Halted
       else model.update_leaf n data)
    else if (Info.prune_p info n) || (Info.num_children info n) = 0 then
      model.update_prune n data
    else
      (Info.incr_branches info;
       model.iter_children take_child skipped n data bound)

  and take_child n i data =
    (* used by iter_children to visit a child *)
    let c = Info.get_child info n i in
      visit c data
  in

    try
      visit root (model.initial_data root);
      !complete
    with Halted -> false


let output_iteration_col_hdr () =
  Datafile.write_alt_colnames stdout "iterations"
    ["iter no"; "iter bound"; "iter nodes"; "iter branches"; "iter leaves"; ]


let output_iteration iter_no cost_bound info =
  Datafile.write_alt_row_prefix stdout "iterations";
  Verb.pr Verb.always "%d\t%f\t%d\t%d\t%d\n%!"
    iter_no cost_bound info.Info.nodes info.Info.branches info.Info.leaves

let output_factor_col_hdr () =
  Datafile.write_alt_colnames stdout "factors"
    ["factor iteration"; "expected nodes"; "actual nodes"; "growth factor";
     "estimation factor"; "hit limit"; ]


let output_factors ~iter_no ~prev_nodes ~expected_nodes ~actual_nodes
    ~hit_limit =
  let actualf = float actual_nodes in
  let growth_factor = actualf /. float prev_nodes in
  let est_factor = actualf /. float expected_nodes in
  let hit_limit = if hit_limit then 1 else 0 in
    Datafile.write_alt_row_prefix stdout "factors";
    Verb.pr Verb.always "%d\t%d\t%d\t%g\t%g\t%d\n" iter_no expected_nodes
      actual_nodes growth_factor est_factor hit_limit


let output_estimation_col_hdr () =
  Datafile.write_alt_colnames stdout "estimation"
    ["estimation iteration"; "desired nodes"; "estimated bound";
     "bound limit"; ]


let output_estimation ~iter_no ~desired_nodes ~bound ~limit =
  Datafile.write_alt_row_prefix stdout "estimation";
  Verb.pr Verb.always "%d\t%d\t%g\t%d\n" iter_no desired_nodes bound limit


(**
   * Performs a Best-Leaf-First Search on the root.
   *
   *   ?limit = should a node limit be imposed to cut off large bounds.
   *   model = the model to use
   *   info  = search info
   *   bound = the previous bound used
   *   root  = the root node
*)
let best_leaf_first_search ?(limit=true) model info bound root =

  (* remember the initial halt criteria *)
  let original_halt = Info.get_halt info
    (* total number of nodes after previous iteration *)
  and prev_nodes = ref 0
    (* number of nodes desired on previous iteration *)
  and prev_desired = ref (let prev = Info.get_nodes info in
			    if prev = 0 then 1 else prev)
    (* bound for previous iteration *)
  and prev_bound = ref bound
    (* true if search is complete *)
  and complete = ref false
  and iter_no = ref 0 in

  let last_desired = ref 0 in
  let last_limit = ref 0 in

    output_iteration_col_hdr ();
    output_estimation_col_hdr ();
    output_factor_col_hdr ();

    while (
      (not (original_halt info)) (* has original halt criteria been met? *)
      && (not (Info.optimal_so_far info)) (* have we found a solution yet? *)
      && (not !complete) (* is search tree exausted? *)
    ) do
      let curr_nodes = Info.get_nodes info in
	(* total nodes visted so far *)
      let last_iter = curr_nodes - !prev_nodes in
	(* nodes visited by last iteration *)
      let desired = 2 * (max last_iter !prev_desired) in
        (* calculate cost bound *)
      let cost_bound, predicted = model.next_bound desired in
        (* this we be our upper node limit *)
      let node_limit = curr_nodes + (min (2 * predicted) (3 * desired)) in
      let node_limit = if not limit then max_int else node_limit in

	last_desired := desired;
        prev_nodes := curr_nodes;
        prev_desired := desired;

	output_estimation ~iter_no:!iter_no ~desired_nodes:desired
	  ~bound:cost_bound ~limit:node_limit;

	last_limit := node_limit;
        prev_bound := cost_bound;

        if Math.nan_p cost_bound then failwith "error - cost bound NaN!";
        (* added node limit to halt condition *)
        Info.set_halt_or info [Info.Expanded node_limit] original_halt;
        (* do a bounded dfs with cost_bound *)
        complete := bounded_dfs model info cost_bound root;

	(* output information for the previous iteration. *)
	output_iteration !iter_no cost_bound info;
	let curr_nodes' = Info.get_nodes info in

	  (* Output the factors if this is not the last iteration *)
	  if not (original_halt info) && not (Info.optimal_so_far info)
	    && not (!complete) then
	      output_factors ~iter_no:!iter_no ~prev_nodes:last_iter
		~expected_nodes:predicted
		~actual_nodes:(curr_nodes' - curr_nodes)
		~hit_limit:(curr_nodes' = node_limit);

	  incr iter_no;
    done;
    model.dump ();
    ((Info.curr_best info),
     (Info.stats info),
     (Info.optimal_so_far info),
     !complete)


(* EOF *)
