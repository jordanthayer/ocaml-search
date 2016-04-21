(**
   Iterative deepening search interface.  Searches all nodes within the
   current bound, then proceeds to the next bound assuming time and memory
   are available, unless of course a solution was found
   Jordan July 2009 *)

let alt_col_name = "iterations"

let output_col_hdr () =
    Datafile.write_alt_colnames stdout alt_col_name ["iter no";
						     "iter bound";
						     "iter expanded"; ]
let output_row iter bound expansions =
    Datafile.write_alt_row_prefix stdout alt_col_name;
    Verb.pr Verb.always "%d\t%f\t%d\n" iter bound expansions

(************************************************************)
(* Shared.                                                  *)
(************************************************************)

let has_incumbent info =
  (** [has_incumbent info] test if there is an incumbent. *)
  match info.Limit.incumbent with
    | Limit.Nothing -> false
    | _ -> true

let is_incumbent_in_bound check_bound info =
  (** [is_incumbent_in_bound check_bound info] tests if the incumbent
      solution is within the bound. *)
  match info.Limit.incumbent with
    | Limit.Nothing -> false
    | Limit.Incumbent (_, n) -> check_bound n


let is_better_than_incumbent is_better info n =
  (** [is_better_than_incumbent is_better info n] tests if the given node
      is better than the incumbent solution. *)
  match info.Limit.incumbent with
    | Limit.Nothing -> true
    | Limit.Incumbent (_, inc) ->
	(* Apparently the convension is for better_p (is_better) to
	   return true if the nodes are of equal value.  This call is
	   negated to ensure that, if a node is of equal quality to the
	   incumbent solution, it can be pruned. *)
	not (is_better inc n)


let incumbent info n = Limit.new_incumbent info (Limit.Incumbent (0., n))
  (** Set a new incumbent solution. *)


(************************************************************)
(* DFS expansions.                                          *)
(************************************************************)

let dfs_default depth
    see_expansion check_bound consider_child
    is_better is_goal expand info n =
  (** [dfs_default depth check_bound consider_child is_better is_goal
      expand info n] performs the default depth-first search for IDA*,
      stopping when we have found the first in-bound incumbent. *)
  if is_goal n
  then incumbent info n
  else begin
    let children = expand n in
      if not (has_incumbent info)
      then see_expansion depth n children;
      List.iter
	(fun c ->
	   Limit.incr_gen info;
	   (* If the incumbent is within the bound then we are done so
	      don't recur. *)
	   if not (is_incumbent_in_bound check_bound info)
	   then consider_child c)
	children
  end


let dfs_bnb depth
    see_expansion check_bound consider_child
    is_better is_goal expand info n =
  (** [dfs_bnb check_bound consider_child is_better is_goal expand
      info n] performs a depth-first search doing branch-and-bound on
      the incumbent solution cost. *)
  if is_goal n
  then incumbent info n
  else begin
    let children = expand n in
      if not (has_incumbent info)
      then see_expansion depth n children;
      List.iter
	(fun c ->
	   Limit.incr_gen info;
	   (* If the child is better than the incumbent then continue
	      searching under it. *)
	   if (is_better_than_incumbent is_better info c)
	   then consider_child c)
	children
  end


let dfs_total depth
    see_expansion check_bound consider_child
    is_better is_goal expand info n =
  (** [dfs_total check_bound consider_child is_better is_goal expand
      info n] performs a depth-first search completing the entire
      final iteration before stopping. *)
  if is_goal n
  then incumbent info n
  else begin
    let children = expand n in
      if not (has_incumbent info)
      then see_expansion depth n children;
      List.iter (fun c ->
		   Limit.incr_gen info;
		   consider_child c)
	children
  end

(************************************************************)
(* Without duplicate detection.                             *)
(************************************************************)

let search_nosface ?(quiet = false) ?(dups=false) info is_goal expand init key hash equals
    better_p see_expansion check_bound iteration_complete dfs =
  (** [search ?dups sface better_p see_expansion check_bound
      iteration_complete dfs] is the general framework for an
      iterative deepening search without duplicate detection.  It
      takes a depth-first expansion function as the final argument to
      determine when to stop on the final iteration.

      If [dups] is true then the search attempts to search for
      duplicate states along its current path to avoid cycles.

      Model functions:

      [see_expansion] is called each time a node is expanded.

      [check_bound] tests if a node is within the bound.

      [iteration_complete] called at the end of an iteration.  It
      takes a boolean argument that is true if the last iteration was
      the final iteration. *)
  let path = Htable.create hash equals 100 in

  let see_expand depth node children =
    (* [see_expand depth node children] see a single expansion.  This
       takes cycle checking into account if it is enabled. *)
    let children' =
      if dups
      then List.filter (fun c -> not (Htable.mem path (key c))) children
      else children
    in see_expansion depth node children'
  in

  let max_depth = ref 0
  and nodes = ref 0
  and time = ref infinity
  and depth_tab = ref (Garray.make 0)
  and completed_depth = ref max_int in
  let opt_sol = ref false in

  let rec consider_child depth c =
    (* Consider a child for expansion. *)
    if check_bound c then begin
      if not dups || not (Htable.mem path (key c)) then begin
	if not (Limit.halt_p info) then begin
	  Limit.incr_exp info;
	  dfs_expand depth c
	end else
	  if is_goal c then incumbent info c
      end
    end else begin
      (* a node was pruned update the max completed depth counter. *)
      if (depth - 1) < !completed_depth then completed_depth := (depth - 1)
    end

  and dfs_expand depth n =
    (* Perform a depth-first expansion using the given dfs
       function. *)
    max_depth := max !max_depth depth;
    incr nodes;
    if Verb.level Verb.debug then
      Garray.set !depth_tab depth ((Garray.get !depth_tab depth) + 1);
    assert (not dups || (Htable.length path) = depth);
    assert (not dups || not (Htable.mem path (key n)));
    if dups then Htable.add path (key n) true;
    let r = (dfs depth
	       see_expand check_bound (consider_child (depth + 1))
	       better_p is_goal expand
	       info n)
    in
    if dups then Htable.remove path (key n);
    r
  in

  let rec do_search () =
    (* Perform the iterations of the search. *)
    if not (Limit.halt_p info) then begin
      dfs_expand 0 init;

      if Verb.level Verb.debug then begin
	(* If debugging is on then track the number of nodes expanded
	   at each depth. *)
	Verb.pr Verb.always "nodes expanded at each depth:\n";
	Garray.iteri
	  (fun i vl -> Verb.pr Verb.always "\t%3d: %8d\n" i vl)
	  !depth_tab;
	depth_tab := (Garray.make 0)
      end;
      Verb.pr Verb.toplvl "iteration max depth:       %d\n%!" !max_depth;
      Verb.pr Verb.toplvl "iteration completed depth: %d (%f%%)\n%!"
	!completed_depth ((float !completed_depth) /. (float !max_depth));

      if not (is_incumbent_in_bound check_bound info) then begin
	let delta_time = (Wrsys.walltime ()) -. !time in
	Verb.pr Verb.toplvl "%f nodes per second\n%!"
	  ((float !nodes) /. delta_time);
	iteration_complete !completed_depth false;
	completed_depth := max_int;
	nodes := 0;
	time := if Verb.level Verb.optional then Wrsys.walltime () else 0.;
	do_search ();
      end else begin
	(* get the next bound anyway to output iteration information,
	   but then don't recur. *)
	iteration_complete !completed_depth true;
	if not (Limit.halt_p info) then opt_sol := true;
	assert (not dups || (Htable.length path) = 0);
      end
    end in
  do_search ();
  if dups
  then (if not quiet
    then Datafile.write_pairs stdout [("cycle breaking", "yes")])
  else (if not quiet
    then Datafile.write_pairs stdout [("cycle breaking", "no")]);
  if not quiet
  then (Datafile.write_pairs stdout
	  [ "max depth", string_of_int !max_depth;
	    "optimal solution", string_of_bool !opt_sol;]);
  info


let search ?(quiet = false) ?(dups=false) sface better_p
    see_expansion check_bound iteration_complete dfs =
  (** [search ?dups sface better_p see_expansion check_bound
      iteration_complete dfs] is the general framework for an
      iterative deepening search without duplicate detection.  It
      takes a depth-first expansion function as the final argument to
      determine when to stop on the final iteration.

      If [dups] is true then the search attempts to search for
      duplicate states along its current path to avoid cycles.

      Model functions:

      [see_expansion] is called each time a node is expanded.

      [check_bound] tests if a node is within the bound.

      [iteration_complete] called at the end of an iteration.  It
      takes a boolean argument that is true if the last iteration was
      the final iteration. *)
  let info = sface.Search_interface.info
  and is_goal = sface.Search_interface.goal_p
  and expand = sface.Search_interface.node_expand
  and init = sface.Search_interface.initial
  and key  = sface.Search_interface.key
  and hash = sface.Search_interface.hash
  and eq = sface.Search_interface.equals in
  search_nosface ~quiet ~dups info is_goal expand init key hash eq
    better_p see_expansion check_bound
    iteration_complete dfs


let no_dups sface better_p see_expansion check_bound iteration_complete =
  (** [no_dups sface better_p see_expansion check_bound iteration_complete] is
      the default IDA* search, it stops when we find the first
      in-bound solution. *)
  Limit.results5
    (search ~dups:false sface better_p see_expansion check_bound
       iteration_complete dfs_default)


let no_dups_in_dups_dom_no_sface ?(quiet = false) info is_goal expand init key hash equals
    better_p see_expansion check_bound iteration_complete =
  (** [no_dups sface better_p see_expansion check_bound iteration_complete] is
      the default IDA* search, it stops when we find the first
      in-bound solution. *)
  Limit.results6
    (search_nosface ~quiet ~dups:true info is_goal expand init key hash equals
       better_p see_expansion check_bound iteration_complete dfs_default)


let no_dups_in_dups_dom ?(quiet = false) sface better_p see_expansion
    check_bound iteration_complete =
  (** [no_dups sface better_p see_expansion check_bound iteration_complete] is
      the default IDA* search, it stops when we find the first
      in-bound solution. *)
  Limit.results6
    (search ~quiet ~dups:true sface better_p see_expansion check_bound
       iteration_complete dfs_default)

let no_dups_in_dups_dom_total
    sface better_p see_expansion check_bound iteration_complete =
  (** [no_dups sface better_p see_expansion check_bound iteration_complete] is
      the default IDA* search, it stops when we find the first
      in-bound solution. *)
  Limit.results6
    (search ~dups:false sface better_p see_expansion check_bound
       iteration_complete dfs_total)


let no_dups_in_dups_dom_bnb
    sface better_p see_expansion check_bound iteration_complete =
  (** [no_dups sface better_p see_expansion check_bound iteration_complete] is
      the default IDA* search, it stops when we find the first
      in-bound solution. *)
  Limit.results6
    (search ~dups:true sface better_p see_expansion check_bound
       iteration_complete dfs_bnb)


let no_dups_bnb sface better_p see_expansion check_bound iteration_complete =
  (** [no_dups_bnb sface better_p see_expansion check_bound
      iteration_complete] is IDA* search with branch-and-bound on the final
      iteration. *)
  Limit.results5
    (search sface better_p see_expansion check_bound
       iteration_complete dfs_bnb)


let no_dups_total sface better_p see_expansion check_bound iteration_complete =
  (** [no_dups_total sface better_p see_expansion check_bound
      iteration_complete] is IDA* search that completes the entire
      final iteration. *)
  Limit.results5
    (search sface better_p see_expansion check_bound
       iteration_complete dfs_total)


(************************************************************)
(* Counting expanded nodes.                                 *)
(************************************************************)

let no_dups_count sface get_f get_h threshold =
  (** [no_dups_count sface better_p see_expansion check_bound
      iteration_complete dfs] count the number of expansions on an
      iteration of IDA* with a threshold of [threshold] *)
  let info = sface.Search_interface.info
  and expand = sface.Search_interface.node_expand
  and init = sface.Search_interface.initial in

  let exp = ref 0 in

  let rec dfs n =
    (* [dfs_total n] performs a depth-first search completing the
       entire final iteration before stopping. *)
    incr exp;
    let children = expand n in
      List.iter (fun c ->
		   Limit.incr_gen info;
		   if get_f c <= threshold then dfs c)
	children
  in
    dfs init;
    !exp

(************************************************************)
(* With duplicate detection.                                *)
(************************************************************)

let dups_nosface i goal_p initial node_expand better_p
    see_expansion check_bound iteration_complete =
  let rec expand_best n =
    if not (Limit.halt_p i)
    then
      (Limit.incr_exp i;
       if goal_p n
       then Limit.new_incumbent i (Limit.Incumbent (0.,n))
       else (let children = node_expand n in
 	       List.iter (fun c ->
			    Limit.incr_gen i;
			    see_expansion c;
 			    if check_bound c
 			    then expand_best c) children)) in
  let rec do_search () =
    if (not (Limit.halt_p i)) && (i.Limit.incumbent = Limit.Nothing)
    then (expand_best initial;
 	  iteration_complete ();
 	  do_search ()) in
    do_search();
    Limit.results6 i


let dups sface better_p see_expansion check_bound iteration_complete =
  dups_nosface sface.Search_interface.info sface.Search_interface.goal_p
    sface.Search_interface.initial sface.Search_interface.node_expand
    better_p see_expansion check_bound iteration_complete

(* EOF *)
