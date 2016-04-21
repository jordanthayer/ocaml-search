(**
    iDTA* search algorithm (calculating ideal values) as
        through the search interface
    Sofia Lemons - December 2009
*)


(******** ERROR FUNCTIONS AND STRUCTS *********)

(** Structure for learned error model. Stores error samples by feature
    value, search effort, error value, then count of times that
    error was seen. *)
type prob_table = {
  mutable branch_factor : float;
  mutable accuracy : int;
(* Search effort represents exponents for powers of 2 (e = 3 means
     2**3 nodes were expanded*)
  vcounts : (float, int) Hashtbl.t;
  (** Number of times we've seen each feature set value. *)
  counts : (float, (float list)) Hashtbl.t;
  (** Error values we've seen for this feature value. (feature
      (h-value), value) *)
  dists : (float, (float list) array) Hashtbl.t;
  (** Condensed distributions from second round of learning. Built
      from first-round distributions for frontier nodes after doing a
      set amount of search under a given node.  (feature (h-value),
      effort (with index 0 being effort 2^1), value) *)
}

let create_prob_table accuracy =
  (** Creates an empty error table. *)
  {
    branch_factor = 1.;
    accuracy = accuracy;
    vcounts = Hashtbl.create 0;
    counts = Hashtbl.create 0;
    dists = Hashtbl.create 0;
  }

let get_total tbl feature =
  (** Get the total number of times this feature value was seen. *)
  try
    Hashtbl.find tbl.vcounts feature
  with _ -> 0

let get_samples tbl feature =
  (** Get a list of error samples, the original data that
      was seen during learning *)
  try
    (Hashtbl.find tbl.counts feature)
  with _ ->
    []

let update_total tbl feature =
  (** Adds to total (vcounts) for feature. *)
  (try
     Hashtbl.replace tbl.vcounts feature
       ((Hashtbl.find tbl.vcounts feature)+1)
   with _ -> (
     Hashtbl.add tbl.vcounts feature 1;
     Hashtbl.add tbl.counts feature []
   )
  )

let add_sample tbl feature value =
  (** Checks whether table for feature exists and adds sample to
      counts. DOES NOT UPDATE TOTALS! *)
  (
    (try
       Hashtbl.replace tbl.counts feature ((get_samples tbl feature)@[value])
     with _ -> (
       (* Hashtable creation should always be done via update_total,
	  but just in case. *)
       Hashtbl.add tbl.counts feature [value]
     )
    )
  )

let add_dist tbl feature effort dist =
  (** Checks whether table for feature exists and adds to dists. DOES
      NOT UPDATE TOTALS! *)
  ()

let get_random_err tbl feature =
  (** Returns a random sample from the list for [feature] *)
  (try
     Wrlist.random_elt (get_samples tbl feature)
   with _ ->
     0.)

let load_table path =
  (** Loads in marshalled probability table from file. *)
  let in_file = open_in_bin path in
  let t = (Marshal.from_channel in_file : prob_table) in
    (close_in in_file;
     t)

let output_table path tbl =
  (** Outputs the contents of a prob_table [tbl] to a a file located
      at [path] *)
  let out_file = open_out_bin path in
    (Marshal.to_channel out_file tbl [];
     flush out_file;
     close_out out_file)


(****** NODE FUNCTIONS AND STRUCTS *******)

let closed_pos = -2

type ('a, 'b) node = {
  data : 'a;          (* Data Payload *)
  f : float;          (* Total cost of a node*)
  g : float;          (* Cost of reaching a node *)
  a_f : float;        (* Admissible f-value *)
  mutable backup_f : float;   (* Stored f-value of best descendent *)
  mutable backup_g : float;   (* Stored g-value of best descendent *)
  depth : int;        (* Depth of node in tree.  Root @ 0 *)
  mutable pos : int;  (* Position info for dpq *)
  rel : 'b;           (* Relevant ancestor *)
}


let wrap f =
  (** takes a function to be applied to the data payload such as the
      goal-test or the domain heuristic and wraps it so that it can be
      applied to the entire node *)
  (fun n -> f n.data)


let unwrap_sol s =
  (** Unwraps a solution which is in the form of a search node and presents
      it in the format the domain expects it, which is domain data followed
      by cost *)
  match s with
      Limit.Nothing -> None
    | Limit.Incumbent (q,n) -> Some (n.data, n.g)


let f_then_g a b =
  (** expansion ordering predicate, and also works for ordering duplicates
      assuming that h is the same for both
      (hence f will be lower when g is lower). *)
  ((a.f : float) < b.f) ||
  ((a.f = b.f) && (a.g >= b.g))

let ordered_p = f_then_g


let a_f_then_g a b =
  (** expansion ordering predicate, and also works for ordering duplicates
      assuming that h is the same for both
      (hence f will be lower when g is lower). *)
  ((a.a_f : float) < b.a_f) ||
  ((a.a_f = b.a_f) && (a.g >= b.g))

let a_ordered_p = a_f_then_g


let backup_f_then_g a b =
  (** expansion ordering function for list sorting, and also works for
      ordering duplicates assuming that h is the same for both (hence
      f will be lower when g is lower). *)
  if ((a.backup_f : float) < b.backup_f) ||
    ((a.backup_f = b.backup_f) &&
       (a.backup_g > b.backup_g)) then
      -1
  else if ((a.backup_f : float) > b.backup_f) ||
    ((a.backup_f = b.backup_f) &&
       (a.backup_g < b.backup_g)) then
      1
  else
    0


let just_f a b =
  (** Sorts nodes solely on total cost information *)
  (a.f : float) <= b.f

let better_p = just_f


let just_a_f a b =
  (** Sorts nodes solely on total cost information *)
  (a.a_f : float) <= b.a_f

let a_better_p = just_a_f


let just_g a b =
  (** Sorts nodes solely on incurred cost information *)
  (a.g : float) <= b.g


let setpos n i =
  (** Sets the location of a node, used by dpq's *)
  n.pos <- i


let getpos n =
  (** Returns the position of the node in its dpq.
      Useful for swapping nodes around on the open list *)
  n.pos


let update a =
  a *. 1.1


let make_learn_expand expand h a_h =
  (** Takes the domain expand function and a heuristic calculator
      and creates an expand function which returns search nodes. *)
  (fun n ->
     List.map (fun (d, g) ->
		 let calc_f = g +. (h d) in
		   { data = d;
		     f = calc_f;
		     g = g;
		     a_f = g +. (fst (a_h d));
		     backup_f = calc_f;
		     backup_g = g;
		     depth = n.depth + 1;
		     pos = Dpq.no_position;
		     rel = n.rel; }) (expand n.data n.g))


let make_learn_sface sface =
  let rec initial = { data = sface.Search_interface.initial;
		      f = sface.Search_interface.h
      sface.Search_interface.initial;
		      g = 0.;
		      a_f = 0.;
		      backup_f = neg_infinity;
		      backup_g = 0.;
		      depth = 0;
		      pos = Dpq.no_position;
		      rel = sface.Search_interface.key sface.Search_interface.initial; } in
  let def_log = Limit.make_default_logger (fun n -> n.f)
    (wrap sface.Search_interface.get_sol_length) in
    Search_interface.make
      ~node_expand:(make_learn_expand sface.Search_interface.domain_expand
		      sface.Search_interface.h sface.Search_interface.hd)
      ~goal_p:(wrap sface.Search_interface.goal_p)
      ~key:(wrap sface.Search_interface.key)
      ~hash:sface.Search_interface.hash
      ~equals:sface.Search_interface.equals
      ~halt_on:sface.Search_interface.halt_on
      sface.Search_interface.domain initial better_p
      (fun i ->
	 sface.Search_interface.info.Limit.log
	   (Limit.unwrap_info (fun n -> n.data) i);
	 def_log i)


let make_expand expand h a_h =
  (** Takes the domain expand function and a heuristic calculator
      and creates an expand function which returns search nodes. *)
  (fun n ->
     List.map (fun (d, g) ->
		 let calc_f = g +. (h d) in
		   { data = d;
		     f = calc_f;
		     g = g;
		     a_f = g;
		     backup_f = calc_f;
		     backup_g = g;
		     depth = n.depth + 1;
		     pos = Dpq.no_position;
		     rel = n.rel; }) (expand n.data n.g))

let make_sface sface =
  let rec initial = { data = sface.Search_interface.initial;
		      f = neg_infinity;
		      g = 0.;
		      a_f = neg_infinity;
		      backup_f = neg_infinity;
		      backup_g = 0.;
		      depth = 0;
		      pos = Dpq.no_position;
		      rel = sface.Search_interface.key sface.Search_interface.initial; } in
  let def_log = Limit.make_default_logger (fun n -> n.f)
    (wrap sface.Search_interface.get_sol_length) in
    Search_interface.make
      ~node_expand:(make_expand sface.Search_interface.domain_expand
		      sface.Search_interface.h sface.Search_interface.hd)
      ~goal_p:(wrap sface.Search_interface.goal_p)
      ~key:(wrap sface.Search_interface.key)
      ~hash:sface.Search_interface.hash
      ~equals:sface.Search_interface.equals
      ~halt_on:sface.Search_interface.halt_on
      sface.Search_interface.domain initial better_p
      (fun i ->
	 sface.Search_interface.info.Limit.log
	   (Limit.unwrap_info (fun n -> n.data) i);
	 def_log i)

let no_record = (fun _ _ -> ())

(************** ERROR LEARNING FUNCTIONS ************)

let learn_samples = 300

let expected_min frontier n_samples probs =
  (** Generates sample configurations for the frontier nodes and
      returns the set of minimum values from each config. *)
  let rec config_min i l =
    let config = Array.to_list (Dpq.map
      (fun n -> n.f -. (get_random_err probs n.f)) frontier) in
      if (i < n_samples) then
	let min_val = (List.fold_left min infinity config) in
	  (* minimum value in config *)
	  config_min (i+1) (l@[min_val])
      else
	l
  in
    config_min 0 []

let rec gather_sample dups outch round probs goal_p hash equals key expand
    horizon node =
  (** Perform A* to gather error data for a single node. On round 0
      it gathers perfect data (minimum cost to goal.) On round 1 it
      searches out to different effort levels and condenses
      information about the frontier using expected_min. *)
  let branch_factor = ref 0. in
  let openlist = Dpq.create a_ordered_p setpos 100 node
  and nodes = Htable.create hash equals 100 in
  let last_f = ref 0. in
  let n_exp = ref 0 in
  let next_effort = if (round == 1) then
    ref 2
  else
    ref (-1) in
    Dpq.insert openlist node;
    Htable.add nodes (key node) node;
    let consider_child n =
      let state = key n in
	if dups then
	  (try
	     let prev = Htable.find nodes state in
	       if not (a_better_p prev n) then
		 (* better path to previous state *)
		 (Htable.replace nodes state n;
		  let pos = getpos prev in
		    if pos == closed_pos
		    then Dpq.insert openlist n
		    else Dpq.swap openlist pos n)
	   with Not_found -> (* new state *)
	     Dpq.insert openlist n;
	     Htable.add nodes state n)
	else Dpq.insert openlist n in
    let rec expand_best () =
      if (not (Dpq.empty_p openlist))
      then
	(let n = Dpq.peek_first openlist in
	   if goal_p n then
	     ((if round == 0 then
		 Wrutils.pf outch " %f" (n.f -. node.f)
	       else
		 for i = !next_effort to horizon do
		   Wrutils.pf outch "\n%f" (n.f -. node.f)
		 done);
	      (*Output the final error values*)
	      !branch_factor, !n_exp)
	   else
	     ((if ((round = 1) && ((float(!n_exp)) =
		       (2.**(float(!next_effort))))) then
		 (Wrutils.pf outch "\n";
		  (List.iter (fun x -> Wrutils.pf outch "%f %!"
				(x -. node.f))
		     (expected_min openlist learn_samples probs));
		  incr next_effort));
	      ignore (Dpq.extract_first openlist);
	      if ((round = 1) && (!next_effort > horizon)) then
		!branch_factor, !n_exp
	      else
		((if (Dpq.empty_p openlist) then
		    last_f := infinity);
		 setpos n closed_pos;
		 let children = expand n in
		   branch_factor := (float (List.length children) +.
				       ((float !n_exp)
					*. !branch_factor)) /.
		     (float (!n_exp + 1)); (*Update branch_factor*)
		   incr n_exp;
		   List.iter consider_child children;
		   expand_best ())))
      else
	((for i = !next_effort to horizon do
	    Wrutils.pf outch " %f\n%!" (!last_f -. node.f)
	  done);
	 (*Output the final error values*)
	 !branch_factor, !n_exp)
    in
    let ret = expand_best () in
      Wrutils.pf outch "\n\n%!";
      ret

let idta_ideal_learn_run ?(dups = false) orig_sface args =
  (** Perform A* to gather error data for a given initial state. *)
  let horizon = Search_args.get_int "Idtastar_ideal.idta_ideal_learn" args 0 in
  let round = (Search_args.get_int
		 "Idtastar_ideal.idta_ideal_learn" args 1) in
  let path = (Search_args.get_string
		"Idtastar_ideal.idta_ideal_learn" args 2) in
  let outch = open_out (Search_args.get_string
			  "Idtastar_ideal.idta_ideal_learn" args 3) in
  let sface = make_learn_sface orig_sface in
  let expand = sface.Search_interface.node_expand
  and key = sface.Search_interface.key
  and hash = sface.Search_interface.hash
  and equals = sface.Search_interface.equals
  and info = sface.Search_interface.info
  and goal_p = sface.Search_interface.goal_p
  and initial = sface.Search_interface.initial in
  let ret_func =
    if dups then Limit.results6
    else (fun x ->
	    (fun (s, x2, x3, x4, x5) ->
	       (s, x2, x3, x4, x5, 0)) (Limit.results5 x)) in
  let j = ref 0
  and branch_factor = ref 0. in
  let openlist = Dpq.create ordered_p setpos 100 initial
  and nodes = Htable.create hash
    equals 100 in
  let probs = if (round = 1) then (load_table path)
  else create_prob_table 0 in
    (Dpq.insert openlist initial;
     Htable.add nodes (key initial) initial);
    let i = info in
    let consider_child n =
      Limit.incr_gen i;
      if not (Limit.promising_p i n) then
	Limit.incr_prune i
      else
	if dups then
	  (let state = sface.Search_interface.key n in
	     (* if heuristic is consistent (monotone) then
		first instance of node is always best path. *)
	     try
	       let prev = Htable.find nodes state in
		 Limit.incr_dups i;
		 if not (better_p prev n) then
		   (* better path to previous state *)
		   (Htable.replace nodes state n;
		    let pos = getpos prev in
		      if pos == closed_pos
		      then Dpq.insert openlist n
		      else Dpq.swap openlist pos n)
	     with Not_found -> (* new state *)
	       Dpq.insert openlist n;
	       Htable.add nodes state n)
	else
	  Dpq.insert openlist n
    in

    let rec expand_best () =
      if (not (Dpq.empty_p openlist)) && (not (Limit.halt_p i))
      then
	(let n = Dpq.extract_first openlist in
	   if not (Limit.promising_p i n) then
	     (Limit.incr_prune i;
	      Htable.remove nodes (sface.Search_interface.key n);
	      expand_best ())
	   else if sface.Search_interface.goal_p n then
	     Limit.new_incumbent i (Limit.Incumbent (0.,n))
	   else
	     (* Could save one expansion per call by handing the
		children to gather, along with the original node. Just
		have to make sure to start the branching factor at the
		number of children and its count at 1. *)
	     (setpos n closed_pos;
	      ((if true (*(Random.int 1) == 0*) then
		  (Wrutils.pf outch "%f%!" (n.f -. n.g);

		   let x, c = gather_sample dups outch round probs goal_p
		     hash equals key expand horizon n in
		     (branch_factor := ((x *. (float c)) +. ((float !j) *.
						!branch_factor))
		      /. (float (!j + c));
		      j := !j + c))));
	      let children = sface.Search_interface.node_expand n in
		Limit.incr_exp i;
		List.iter consider_child children;
		Limit.curr_q i (Dpq.count openlist);
		expand_best ()))
    in
    let ret = expand_best (); ret_func i in
      (Wrutils.pf outch "%f\n" !branch_factor;
       close_out outch;
       ret)

let idta_ideal_learn orig_sface args =
  Limit.unwrap_sol6_into_5 unwrap_sol
    (idta_ideal_learn_run orig_sface args ~dups:false)

let idta_ideal_learn_dups orig_sface args =
  Limit.unwrap_sol6 unwrap_sol
    (idta_ideal_learn_run orig_sface args ~dups:true)

(*********** PROBABILITY AND UNCERTAINTY FUNCTIONS *******)


let n_samples = 100

let sample_nodes hash key equals frontier n_samples probs =
  (** Generates sample configurations for the frontier nodes and
      returns table of number of times each node had the minimum value
      in those configs. *)
  let counts = Htable.create hash equals 0 in
  let rec config_min i =
    let config = List.map
      (fun n -> n, (n.f -. (get_random_err probs n.f))) frontier in
      if (i < n_samples) then
	let mins, _ = (Wrlist.mins_by (fun (n, v) -> v) config) in
	  (* minimum value in config *)
	  List.iter
	    (fun (n, _) -> let curr_count = (try
					       Htable.find counts (key n)
					     with _ ->
					       0) in
	       Htable.replace counts (key n) (curr_count + 1)) mins;
	  config_min (i+1)
      else
	()
  in
    config_min 0;
    counts

let best_action actions table n_samples probs =
  (** Draws samples from frontier nodes to determine which action is
      most often the best and returns that action. Also changes the
      action's backup_f to the average of the sampled minima for nodes
      belonging to it. *)
  let alpha = List.hd actions in
    alpha

let search_worthwhile openlists alpha horizon r info
    key probs =
  (** Determines whether search out to any number of expansions less
      than horizon would lead to a change in belief about the best
      action. *)
  true

let most_uncertain openlists alpha horizon r info
    key probs =
  (** Determines which node would have the most impact on uncertainty
      if it were expanded. *)
  ()

(*********** SEARCH FUNCTIONS ********)

let get_backups acts tbl key =
  (** Takes a list of nodes and returns them with their backed up cost
      values. *)
  List.map (fun a ->
	      (if Htable.mem tbl (key a) then
		 let stored = (Htable.find tbl (key a)) in
		   {a with backup_f = a.g +.
		       (fst stored);
		      backup_g = a.g +.
		       (snd stored)}
	       else a))
    acts

let set_rel_self acts key =
  (** Sets rel pointer for alist of actions to self. *)
  List.map (fun a -> ({a with rel = (key a)})) acts

let update_actions acts tbl key =
  (** Updates action list by setting rel pointers and getting backed
      up f-values. *)
  get_backups (set_rel_self acts key) tbl key

let search_step actions table grain_size horizon probs
    expand key info goal_p =
  (** Searches for a set number of expansions or until a goal is
      found.  Returns modified open/closed list by side-effect. *)
(*
  let i = info in
*)
  let rec expand_best curr_act n_exp =
    actions, false, (List.hd actions)
  in
    expand_best actions 0

let search_step_dups closedlist actions table grain_size horizon probs
    expand key info goal_p =
  (** Searches for a set number of expansions or until a goal is
      found.  Returns modified open/closed list by side-effect. Avoids
      placing generated duplicates on the open list if they do not
      represent a better path to the state.*)
(*
  let i = info
  and nodes = closedlist in
*)
    actions, false, (List.hd actions)

let idta_ideal_choose ?(record = no_record) expand key hash equals info
    goal_p backup_table curr grain_size horizon r
    dups probs =
  (** Selects the best action among those given using search_step and
      evaluating the benefit of further search after each call to
      search_step. *)
  let nt = Htable.create hash equals 0 in
  let search =
    if dups then (search_step_dups nt)
    else (search_step) in
  let choose actions =
    let table = (Htable.create hash equals 0) in
      (*print_endline "entering idta_choose";*)
      Htable.add nt (key curr) curr;
      List.iter (fun a ->
		   let nq = (Dpq.create ordered_p setpos 100 a) in
		     (if not (Htable.mem backup_table (key a)) then
			(Dpq.insert nq a;
			 Htable.add nt (key a) a));
		     Limit.incr_gen info;
		     (*Printf.printf "nq length: %d\n%!" (Dpq.count nq);*)
		     Htable.add table (key a) nq)
	actions;
      let rec evaluate (acts, goal_found, goal_node) =
	if ((List.length acts) == 0) then
	  (* unsolvable instance *)
	  curr
	else if ((List.length acts) == 1) then
	  List.hd acts
	else
	  let alpha = best_action acts table n_samples probs in
	    if goal_found && (goal_node.g <= alpha.backup_f) then
	      ((*print_endline "found goal in idta_choose";*)
		goal_node)
	    else if Limit.halt_p info then
	      ((*print_endline "halt hit in idta_choose";*)
		alpha)
	    else if not (search_worthwhile table
			   alpha horizon r info
			   key probs) then
	      alpha
	    else
	      ((*print_endline "search was worthwhile";*)
		evaluate (search acts table grain_size horizon probs
			    expand key info goal_p))
      in evaluate (actions, false, (List.hd actions))
  in ((*Printf.printf "number of actions: %d\n%!" (List.length (expand curr));*)
      choose (update_actions (expand curr) backup_table key))

let idta_ideal_search ?(record = no_record) sface grain_size horizon r
    learn_path ?(online = false) dups =
  (** Uses the action chosen by idta_ideal_choose to take the best action
      until the goal state is reached. *)
  let i = sface.Search_interface.info
  and backup_table = Htable.create sface.Search_interface.hash
    sface.Search_interface.equals 0
  and initial = sface.Search_interface.initial
  and probs = load_table learn_path in
  let ret_func =
    if dups then Limit.results6
    else (fun x -> (fun (s, x2, x3, x4, x5) ->
		      (s, x2, x3, x4, x5, 0)) (Limit.results5 x)) in
  let start = Wrsys.walltime () in
  let report_move curr next =
    if online then
      Online.output_row start
	(next.g -. curr.g);
    next in
  let rec move curr =
    if (sface.Search_interface.goal_p curr) then
      (Limit.new_incumbent i (Limit.Incumbent (0.,curr));
       ret_func i)
    else if Limit.halt_p i then
      ((*print_endline "halt hit in idta_search";*)
       ret_func i)
    else
      let alpha = idta_ideal_choose sface.Search_interface.node_expand
	sface.Search_interface.key sface.Search_interface.hash
	sface.Search_interface.equals sface.Search_interface.info
	sface.Search_interface.goal_p backup_table curr grain_size
	horizon r dups probs in
	(Limit.incr_exp i;
	 (*Printf.printf "f-value of curr: %f\n%!" curr.f;
	   Printf.printf "original f-value of alpha: %f\n%!" alpha.f;
	   Printf.printf "backed up f-value of alpha: %f\n%!" alpha.backup_f;*)
	 (*Printf.printf "original cost: %f\n%!" (alpha.backup_f -. curr.g);
	   Printf.printf "updated cost: %f\n%!"
	   (update (alpha.backup_f -. curr.g));*)
	 (if (alpha = curr) then
	    ret_func i
	  else
	    (Htable.replace backup_table
	       (sface.Search_interface.key curr)
	       ((update (alpha.backup_f -. curr.g)),
		(alpha.backup_g -. alpha.g));
	     move (report_move curr (alpha)))))
  in
    move initial


(************ FUNCTIONS FOR CALLING THE SEARCH **********)

  let no_dups sface args =
  (** Performs an iDTA* search from the initial state to a goal,
      for domains with no duplicates. *)
  let grain_size = Search_args.get_int "Idtastar_ideal.no_dups" args 0 in
  let horizon = Search_args.get_int "Idtastar_ideal.no_dups" args 1 in
  let r = Search_args.get_float "Idtastar_ideal.no_dups" args 2 in
  let learn_path = Search_args.get_string "Idtastar_ideal.dups" args 3 in
  let search_interface = make_sface sface in
    Limit.unwrap_sol6_into_5 unwrap_sol
      (idta_ideal_search
	 search_interface
	 grain_size
	 horizon
	 r
	 learn_path
	 false)


  let dups sface args =
  (** Performs an iDTA* search from the initial state to a goal,
      for domains where duplicates are frequently encountered. *)
  let grain_size = Search_args.get_int "Idtastar_ideal.dups" args 0 in
  let horizon = Search_args.get_int "Idtastar_ideal.dups" args 1 in
  let r = Search_args.get_float "Idtastar_ideal.dups" args 2 in
  let learn_path = Search_args.get_string "Idtastar_ideal.dups" args 3 in
  let search_interface = make_sface sface in
    Limit.unwrap_sol6 unwrap_sol
      (idta_ideal_search
	 (* must have g=0 as base for others, and
	    f<others to prevent re-opening *)
	 search_interface
	 grain_size
	 horizon
	 r
	 learn_path
	 true)


  let online_no_dups sface args =
  (** Performs an iDTA* search from the initial state to a goal,
      for domains with no duplicates. *)
  let grain_size = Search_args.get_int "Idtastar_ideal.no_dups" args 0 in
  let horizon = Search_args.get_int "Idtastar_ideal.no_dups" args 1 in
  let r = Search_args.get_float "Idtastar_ideal.no_dups" args 2 in
  let learn_path = Search_args.get_string "Idtastar_ideal.dups" args 3 in
  let search_interface = make_sface sface in
    Online.output_col_hdr ();
    Limit.unwrap_sol6_into_5 unwrap_sol
      (idta_ideal_search
	 search_interface
	 grain_size
	 horizon
	 r
	 learn_path ~online:true
	 false)


  let online_dups sface args =
  (** Performs an iDTA* search from the initial state to a goal,
      for domains where duplicates are frequently encountered. *)
  let grain_size = Search_args.get_int "Idtastar_ideal.dups" args 0 in
  let horizon = Search_args.get_int "Idtastar_ideal.dups" args 1 in
  let r = Search_args.get_float "Idtastar_ideal.dups" args 2 in
  let learn_path = Search_args.get_string "Idtastar_ideal.dups" args 3 in
  let search_interface = make_sface sface in
    Online.output_col_hdr ();
    Limit.unwrap_sol6 unwrap_sol
      (idta_ideal_search
	 (* must have g=0 as base for others, and
	    f<others to prevent re-opening *)
	 search_interface
	 grain_size
	 horizon
	 r
	 learn_path ~online:true
	 true)

(* Drop dups refers to whether duplicates are dropped in the
   action-selection search. Duplicates have to be reconsidered in the
   top-level search. *)
