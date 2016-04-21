(******************************************************************************
 *
 * ~ Austin Dionne ~
 *
 *****************************************************************************)

(*******************************************************************************
 *
 * Speedy-First Search
 *
 *   The following functions define a Speedy algorithm which sorts the open list
 * on d and ignores all duplicate states generated. The open list is returned
 * along with the solution such that it can be reused in a future search.
 *
 ******************************************************************************)

type 'a node = {
  mutable q_pos : int;
  depth : int;
  d : float;
  d_hat : float;
  mutable local_one_step_d_error_total : float;
  g : float;
  h : float;
  f : float;
  data : 'a;
  parent_data : 'a;
}

let setpos node i =
(** Sets the q_pos of the specified node [node] to [i]. Used as the update
  function for the queue. *)
  node.q_pos <- i

let make_root hd_func initial =
(** Creates the root node for the speedy search given the hd function and the
  initial problem state. *)
  let h, d = hd_func initial in
    { q_pos = Dpq.no_position;
      depth = 0;
      d = d;
      d_hat = d;
      local_one_step_d_error_total = 0.;
      g = 0.;
      h = h;
      f = h;
      data = initial;
      parent_data = initial }

let compare_by_d_hat_then_by_f a b =
(** Returns true if a is better than b by comparing d first then f to break
  ties. *)
  (a.d_hat < b.d_hat) ||
  ((a.d_hat = b.d_hat) && (a.f < b.f))

let compare_by_d_then_by_f a b =
(** Returns true if a is better than b by comparing d first then f to break
  ties. *)
  (a.d < b.d) ||
  ((a.d = b.d) && (a.f < b.f))

let compare_by_g (g1, data1) (g2, data2) =
(** Compares 2 domain node pairs by the associated g values. *)
  g1 < g2

let initial_d_correction_delay = 200
let maximum_d_factor = 1000.

(*
let calculate_d_hat d_original node_depth total_error =
(** This method calculates the corrected d value d_hat using the local one step
  error approach given the current depth of a node and the total accumulated one
  step error along the path so far. The corrections will only be made if the
  depth of the node is greater than the previously defined
  [initial_d_correction_delay] and the maximum corrected d_hat will be a factor
  of [maximum_d_factor] of the original d, in order to prevent infinite values
  when paths have experienced a mean one step error greater than 1. *)
  (* Only correct d values if enough samples have been taken. *)
  if node_depth > initial_d_correction_delay then
    let one_step_error_mean = total_error /. (float node_depth)
    and maximum_d_hat = d_original *. maximum_d_factor in
      (* Mean one step error > 1 indicates that the error is not converging.
       This results in an invalid calculation and therefore the max is returned. *)
      if (one_step_error_mean >= 1.0) then
        maximum_d_hat
      else
        let d_hat = d_original /. (1. -. one_step_error_mean) in
          min d_hat maximum_d_hat
  else
    d_original *)

let calculate_d_hat d_original node_depth total_error =
(** This method calculates the corrected d value d_hat using the local one step
  error approach given the current depth of a node and the total accumulated one
  step error along the path so far. The corrections will only be made if the
  depth of the node is greater than the previously defined
  [initial_d_correction_delay] and the maximum corrected d_hat will be a factor
  of [maximum_d_factor] of the original d, in order to prevent infinite values
  when paths have experienced a mean one step error greater than 1. *)
  (* Only correct d values if enough samples have been taken. *)
  if node_depth > initial_d_correction_delay then
    let one_step_error_mean = total_error /. (float node_depth)
    and _maximum_d_hat = d_original *. maximum_d_factor in
      (* Mean one step error > 1 indicates that the error is not converging.
       This results in an invalid calculation and therefore the max is returned. *)
      if (one_step_error_mean >= 1.0) then
        infinity
      else
        let d_hat = d_original /. (1. -. one_step_error_mean) in
          d_hat
  else
    d_original

let speedy_first_search compare_nodes search_func sface args =
(** This method performs a speedy first search, finding a solution as fast as
  possible with no regards to quality, placing that solution into the search
  interface [sface] as the incumbent, then calling the specified search function
  [search_func] with the specified [args]. *)
  (* Extract all of the relevant parts of the search interface. *)
  let hd_func = sface.Search_interface.hd
  and initial = sface.Search_interface.initial
  and is_goal = sface.Search_interface.goal_p
  and expand = sface.Search_interface.domain_expand
  and key = sface.Search_interface.key in
  (* Initialize the open list.*)
  let root_node = make_root hd_func initial in
  let open_list = Dpq.create compare_nodes setpos 100 root_node
  and gen_table = Hashtbl.create 100 in
  let expand_node node =
  (** This subroutine will expand the specified node by generating all of its
    children, adding each to the open list, and reporting the one step errors
    in the heuristic estimates h and d. At each expansion, the best values of
    f, h, and d are maintained in order to determine the appropriate one step
    error for this expansion. A Queue of references to the new search nodes
    created is also maintained such that after the expansion is done the
    one-step error determined can be saved in each child node's total. *)
    Limit.incr_exp sface.Search_interface.info;
    let parent_parent_key = key node.parent_data in
    let node_queue = Queue.create () in
    let best_f, best_d = ref infinity, ref infinity in
      List.iter (fun (data, g) ->
                 let child_key = key data in
                 let h, d = hd_func data in
                 let d_hat = calculate_d_hat d node.depth node.local_one_step_d_error_total in
                 let child_node = { q_pos = Dpq.no_position;
                                    depth = node.depth + 1;
                                    d = d;
                                    d_hat = d_hat;
                                    local_one_step_d_error_total = node.local_one_step_d_error_total;
                                    g = g;
                                    h = h;
                                    f = (g +. h);
                                    data = data;
                                    parent_data = node.data; } in
                   Queue.add child_node node_queue;
                   if is_goal data then
                     Limit.new_incumbent sface.Search_interface.info (Limit.Incumbent (g, child_node.data));
                   Limit.incr_gen sface.Search_interface.info;
                   (* Keep track of the best child to report the one step
                     error in the heuristic. This best child can not include
                     the parent of the parent node. *)
                   if (child_key <> parent_parent_key) && (!best_f > (g +. h)) then
                     (best_f := g +. h;
                      best_d := d);
                   (* See if this node has already been generated before, and
                     if so, don't bother putting it in the open list. *)
                   try
                     let _prev = Hashtbl.find gen_table child_key in
                       Limit.incr_dups sface.Search_interface.info
                   with Not_found ->
                     Dpq.insert open_list child_node;
                     Hashtbl.add gen_table child_key child_node)
      (expand node.data node.g);
    (* If there was at least 1 child node, we saved the best child's h,d,op to
      report for the one step error in the heuristic. *)
    if !best_f < infinity then
      let one_step_d_error = !best_d -. node.d +. 1. in
         Queue.iter (fun child_node ->
                       child_node.local_one_step_d_error_total <- child_node.local_one_step_d_error_total +. one_step_d_error)
                    node_queue
  in
    (* Insert the root node into the open list and start searching. *)
    Dpq.insert open_list root_node;
    Hashtbl.add gen_table (key root_node.data) root_node;
    while not (Limit.has_incumbent sface.Search_interface.info) do
      let node = Dpq.extract_first open_list in
        expand_node node
    done;
    search_func sface args

(*******************************************************************************
 *
 * Speedy-First Anytime Search Instantiations
 *
 *   The following functions perform a speedy first search and then use that
 * result as the incumbent for various anytime algorithms.
 *
 ******************************************************************************)

let speedy_first_anytime_astar_dups sface args =
  speedy_first_search compare_by_d_hat_then_by_f Anytime_astar.dups sface args

let speedy_first_arastar_dups sface args =
  let bound = Search_args.get_float "Speedy First ARA* dups" args 0 in
    sface.Search_interface.info.Limit.start_time <- Sys.time ();
    speedy_first_search compare_by_d_hat_then_by_f Arastar.dups sface (Array.of_list (List.map string_of_float
		                                              			   (Arastar.mk_wtlist bound 0.2)))
let speedy_first_anytime_astar_dups2 sface args =
  speedy_first_search compare_by_d_then_by_f Anytime_astar.dups sface args

let speedy_first_arastar_dups2 sface args =
  let bound = Search_args.get_float "Speedy First ARA* dups" args 0 in
    sface.Search_interface.info.Limit.start_time <- Sys.time ();
    speedy_first_search compare_by_d_then_by_f Arastar.dups sface (Array.of_list (List.map string_of_float	                                              			   (Arastar.mk_wtlist bound 0.2)))


let speedy_first_acastar sface args =
  sface.Search_interface.info.Limit.start_time <- Sys.time ();
  speedy_first_search compare_by_d_then_by_f
    Speedy_first_acastar.sfa_dups_austin sface args



