(**
    DTA* search algorithm as through the search interface
    Sofia Lemons - December 2009
*)

(** Structure for learned error model. Stores error samples by feature
    value, depth, error value, then count of times that error was seen. *)
type prob_table = {
  mutable branch_factor : float;
  mutable accuracy : int;
  vcounts : (float, int) Hashtbl.t;
  (** Number of times we've seen each feature set value. *)
  counts : (float, (float, int) Hashtbl.t array) Hashtbl.t;
  (** Number of times we've seen each error value for this feature
      value, by depth of error calculation.  (feature (h-value), depth
      (with index 0 being depth 1), value, count) *)
}

let new_value horizon =
  (** Creates a new array to be added to counts. *)
  let a = Array.make horizon (fun () -> (Hashtbl.create 0)) in
    Array.map (fun f -> f ()) a

let create_prob_table accuracy =
  (** Creates an empty error table. *)
  {
    branch_factor = 1.;
    accuracy = accuracy;
    vcounts = Hashtbl.create 0;
    counts = Hashtbl.create 0;
  }

let get_total tbl feature =
  (** Get the total number of times this feature value was seen. *)
  try
    Hashtbl.find tbl.vcounts feature
  with _ -> 0

let get_counts tbl feature depth =
  (** Get the table of counts for this feature value and depth. *)
  try
    (Hashtbl.find tbl.counts feature).(depth-1)
  with _ -> Hashtbl.create 0

let get_count tbl feature depth value =
  (** Get the count for this feature value, depth, and error value. *)
  try
    (Hashtbl.find (Hashtbl.find tbl.counts feature).(depth-1) value)
  with _ -> 0

let update_total tbl feature horizon =
  (** Adds to total (vcounts) for feature, because we want that to be
      number of samples, not n_samples*horizon. *)
  (try
     Hashtbl.replace tbl.vcounts feature
       ((Hashtbl.find tbl.vcounts feature)+1)
   with _ -> (
     Hashtbl.add tbl.vcounts feature 1;
     Hashtbl.add tbl.counts feature (new_value horizon)
   )
  )

let add_sample tbl feature depth value =
  (** Checks whether value exists for feature, then depth, then value
      and adds to vcounts and/or counts as necessary. DOES NOT
      UPDATE TOTALS! *)
  (
    (try
       (Hashtbl.replace (get_counts tbl feature depth)
	  value ((get_count
		    tbl feature depth value)+1))
     with _ -> (
       (* used to have a hashtable creation here, but that should
	  always be done for all depths via new_value now *)
       Hashtbl.replace (get_counts tbl feature depth) value 1
     )
    )
  )

let tiles_path = ""
let vis_nav_path = ""

let load_table path =
  (** Loads in marshalled probability table from file. Some day this
      should be done more elegantly than hard-coding the path. *)
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

type ('a, 'b) node = {
  data : 'a;          (* Data Payload *)
  f : float;          (* Total cost of a node*)
  g : float;          (* Cost of reaching a node *)
  mutable backup_f : float;   (* Stored f-value of best descendent *)
  mutable backup_g : float;   (* Stored g-value of best descendent *)
  depth : int;        (* Depth of node in tree.  Root @ 0 *)
  mutable pos : int;  (* Position info for dpq *)
  rel : 'b;           (* Relevant ancestor *)
}


let wrap f =
  (** takes a function to be applied to the data payload
      such as the goal-test or the domain heuristic and
      wraps it so that it can be applied to the entire
      node *)
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


let make_expand expand h =
  (** Takes the domain expand function and a heuristic calculator
      and creates an expand function which returns search nodes. *)
  (fun n ->
     List.map (fun (d, g) -> { data = d;
			       f = g +. (h d);
			       g = g;
			       backup_f = g +. (h d);
			       backup_g = g;
			       depth = n.depth + 1;
			       pos = Dpq.no_position;
			       rel = n.rel; }) (expand n.data n.g))


let make_sface sface =
  let rec initial = { data = sface.Search_interface.initial;
		      f = neg_infinity;
		      g = 0.;
		      backup_f = neg_infinity;
		      backup_g = 0.;
		      depth = 0;
		      pos = Dpq.no_position;
		      rel = sface.Search_interface.key sface.Search_interface.initial; } in
  let def_log = Limit.make_default_logger (fun n -> n.f)
    (wrap sface.Search_interface.get_sol_length) in
    Search_interface.make
      ~node_expand:(make_expand sface.Search_interface.domain_expand
		      sface.Search_interface.h)
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

let rec gather_sample outch f_orig goal_p expand horizon curr_depth node =
  let alpha = ref infinity in
  let count = ref 0
  and branch_factor = ref 0. in
  let openlist = Stack.create ()
  and d = (node.depth) in
    (Stack.push node openlist;
     (while not (Stack.is_empty openlist) do
	let n = Stack.pop openlist in
	  if (n.f < !alpha) then
	    if (n.depth == (d + (curr_depth+1))) || (goal_p n) then
	      alpha := n.f
	    else
	      let children = (expand n) in
	      let x = float (List.length children) in
		branch_factor := (x +. ((float !count) *. !branch_factor))
		/. (float (!count + 1));
		incr count;
		List.iter
		  (fun i ->
		     Stack.push i openlist)
		  children
      done);
     let v = !alpha in
       ((** store this level's error value *)
	 Wrutils.pf outch " %f" (v -. f_orig);
	 if curr_depth < horizon then
	   gather_sample outch f_orig goal_p expand horizon (curr_depth+1) node
	 else
	   (Wrutils.pf outch "\n";
	    !branch_factor)))

let dta_learn_run ?(dups = false) orig_sface args =
  (** Perform SRTA* to gather error data for a given initial state. *)
  let horizon = Search_args.get_int "Dtastar.dta_learn" args 0 in
  let outch = open_out (Search_args.get_string "Dtastar.dta_learn" args 1) in
  let sface = make_sface orig_sface in
  let expand = sface.Search_interface.node_expand
  and key = sface.Search_interface.key
  and hash = sface.Search_interface.hash
  and equals = sface.Search_interface.equals
  and info = sface.Search_interface.info
  and goal_p = sface.Search_interface.goal_p
  and initial = sface.Search_interface.initial in
  let table = Htable.create hash equals 0 in
  let ret_func =
    if dups then Limit.results6
    else (fun x ->
	    (fun (s, x2, x3, x4, x5) ->
	       (s, x2, x3, x4, x5, 0)) (Limit.results5 x)) in
  let j = ref 0
  and branch_factor = ref 0. in
  let rec move curr =
    let s_choose () =
      let alpha = ref infinity
      and closedlist = Htable.create hash equals 0 in
      let s_eval action =
	let openlist = Dblq.create ()
	and d = (action.depth) in
	let consider_child new_node =
	  if dups then
	    (let state = key new_node in
	       try
 		 let prev = Htable.find closedlist state in
		   Limit.incr_dups info;
		   let p_node = Dblq.data prev in
		     if ((p_node.g > new_node.g) ||
			   (p_node.f > new_node.f) ||
			   (new_node.depth > p_node.depth)) then
		       (* better path to previous state *)
		       (* can't go on current g or f alone, because
			  update procedure means that heuristic isn't
			  consistent OR admissible *)
		       (* a node with more depth to explore may hit a
			  node with updated h, leading to a higher
			  f-value at depth bound than another copy of
			  the state at lower depth *)
		       (* will never replace any nodes in unit cost
			  domains except duplicates at same depth as each
			  other *)
		       (* remove old version and insert new at front *)
		       Limit.incr_prune info;
		     let replacing_item =
		       (if not (Dblq.in_q prev) then
			  Dblq.push_front_cons openlist new_node
			else
			  (if (p_node.g > new_node.g) ||
			     (p_node.f > new_node.f) then
			       ignore (Dblq.remove openlist prev);
			   Dblq.push_front_cons openlist new_node)) in
		       Htable.replace closedlist state replacing_item;
	       with Not_found -> (* new state *)
		 let new_item = Dblq.push_front_cons openlist new_node in
		   Htable.replace closedlist state new_item)
	  else
	    Dblq.push_front openlist new_node
	in

	  if Htable.mem table (key action) then
	    (Limit.incr_gen info;
	     alpha := (min (action.g +. (Htable.find table (key action)))
			 !alpha);
	     action.g +. (Htable.find table (key action)))
	  else
	    ((if not (Limit.halt_p info) then
		(consider_child action;
		 Limit.incr_gen info));
	     (while not (Dblq.is_empty openlist ||
			   Limit.halt_p info) do
		let n = Dblq.pop_front openlist in
		  if (n.f < !alpha) then
		    ((if (*(Random.int 10 == 0) &&*) not (goal_p n) then
			(Wrutils.pf outch "%f"
			   (action.f -. action.g);
			 let x = gather_sample outch
			   n.f goal_p expand horizon 0 n in
			   branch_factor := (x +. ((float !j) *.
						     !branch_factor))
			   /. (float (!j + 1));
			   incr j));
		     if (n.depth == (d + (horizon-1))) || (goal_p n) then
		       (alpha := n.f)
		     else
		       (Limit.incr_exp info;
			List.iter
			  (fun i ->
			     (consider_child i;
			      Limit.incr_gen info))
			  (expand n)))
		  else
		    Limit.incr_prune info
	      done);
	     !alpha)
      in
	Wrlist.min_by s_eval (expand curr)
    in
      if (goal_p curr) then
	(Limit.new_incumbent info (Limit.Incumbent (0.,curr));
	 ret_func info)
      else if (Limit.halt_p info) then
	ret_func info
      else
	(let alpha = s_choose () in
	   Htable.replace table
	     (key curr)
	     (update ((snd alpha) -. curr.g));
	   Limit.incr_exp info;
	   move (fst alpha)
	)
  in
  let ret = (move initial) in
    (Wrutils.pf outch "%f\n" !branch_factor;
     close_out outch;
     ret)

let dta_learn orig_sface args =
  Limit.unwrap_sol6_into_5 unwrap_sol
    (dta_learn_run orig_sface args ~dups:false)

let dta_learn_dups orig_sface args =
  Limit.unwrap_sol6 unwrap_sol
    (dta_learn_run orig_sface args ~dups:true)


let closed_pos = -2

let search_step actions table grain_size horizon expand key info goal_p =
  (** Searches for a set number of expansions or until a goal is
      found.  Returns modified open/closed list by side-effect. *)
  let i = info in
  let rec expand_best a e =
    let rec alpha = List.hd (List.sort backup_f_then_g a)
    in
    let q = Htable.find table (key alpha)
    in
      (if (Limit.halt_p i) then
	 ((*print_endline "halt hit in expand_best (no dups)";*)
	  (a, false))
       else if (not (Dpq.empty_p q)) && (e < grain_size) then
	 let n = Dpq.extract_first q in
	   if not (Limit.promising_p i n) then
	     ((* this should never happen, since we go straight to
		 solutions in the top-level search *)
	       failwith "dta.search_step: searched after solution found";
	       (*Limit.incr_prune i; print_endline "pruned a node";
	       (* don't modify alpha's f value. we've pruned based on
		 an incumbent which could only have been found under
		 alpha or one of its ancestors. *) expand_best (a) t
		 (e)*))
	   else if goal_p n then
	     (Limit.new_incumbent i (Limit.Incumbent (0.,n));
	      (*print_endline "goal found in search_step";*)
	      (* goal was found under alpha, so it must be best *)
	      (n::(List.tl a), true))
	   else
	     (Limit.incr_exp i;
	      (try
		 List.iter
		   (fun n ->
		      Limit.incr_gen i;
		      (if not (Limit.promising_p i n) then
			 failwith "dta.search_step: searched after solution found");
		      Dpq.insert q n)
		   (expand n);
	       with _ -> failwith "dta.search_step");
	      (*Printf.printf "open list length: %d\n%!" (Dpq.count q);*)
	      Limit.curr_q i (Dpq.count q);
	      if not (Dpq.empty_p q) then
		(alpha.backup_f <- (Dpq.peek_first q).f;
		 alpha.backup_g <- (Dpq.peek_first q).g;
		 (expand_best a (e+1)))
	      else
		(alpha.backup_f <- infinity;
		 alpha.backup_g <- infinity;
		 expand_best a (e+1)))
       else
	 (a, false))
  in
    expand_best actions 0

let search_step_dups closedlist actions table grain_size horizon expand key info goal_p =
  (** Searches for a set number of expansions or until a goal is
      found.  Returns modified open/closed list by side-effect. Avoids
      placing generated duplicates on the open list if they do not
      represent a better path to the state.*)
  let i = info
  and nodes = closedlist in
  let consider_child openlist n =
    Limit.incr_gen i;
    if not (Limit.promising_p i n) then
      failwith "dta.search_step: searched after solution found"
    else
      let state = key n in
	(* if heuristic is consistent (monotone) then
	   first instance of node is always best path. *)
	try
	  let prev = Htable.find nodes state in
	    Limit.incr_dups i;
	    if not (just_g prev n) then
	      (* better path to previous state *)
	      (Htable.replace nodes state n;
	       let pos = getpos prev in
		 if pos == closed_pos
		 then ((*print_endline "SHOULD NOT GET HERE!";*)
		       Dpq.insert openlist n)
		 else (if (n.rel = prev.rel) then
			 (Dpq.swap openlist pos n;
			  if pos = 1 then
			    let b = List.find (fun a -> (key a) = n.rel)
			      actions in
			      (b.backup_f <- (Dpq.peek_first openlist).f;
			       b.backup_g <- (Dpq.peek_first openlist).g))
		       else
			 let other_open = (Htable.find table prev.rel) and
			     b = List.find (fun a -> (key a) = prev.rel)
			   actions in
			   (Dpq.remove other_open pos;
			    (if pos = 1 then
			       if not (Dpq.empty_p other_open) then
				 (b.backup_f <- (Dpq.peek_first other_open).f;
				  b.backup_g <- (Dpq.peek_first other_open).g)
			       else
				 (b.backup_f <- infinity;
				  b.backup_g <- infinity));
			    Dpq.insert openlist n;)))
	with Not_found -> (* new state *)
	  Dpq.insert openlist n;
	  Htable.add nodes state n in

  let rec expand_best a e =
    let alpha = List.hd (List.sort backup_f_then_g a)
    in
    let q = Htable.find table (key alpha)
    in
      (if  (Limit.halt_p i) then
	 ((*print_endline "halt hit in expand_best (dups)";*)
	  (a, false))
       else if (not (Dpq.empty_p q)) && (e < grain_size) then
	 let n = Dpq.extract_first q in
	   if not (Limit.promising_p i n) then
	     ((* this should never happen, since we go straight to
		 solutions in the top-level search *)
	       failwith "dta.search_step: searched after solution found";)
	   else if goal_p n then
	     (Limit.new_incumbent i (Limit.Incumbent (0.,n));
	      (*print_endline "goal found in search_step";*)
	      (* goal was found under alpha, so it must be best *)
	      (n::(List.tl a), true))
	   else
	     (setpos n closed_pos;
	      Limit.incr_exp i;
	      (*Printf.printf "expanded node with f: %f and g: %f\n" n.f n.g;*)
	      List.iter (consider_child q) (expand n);
	      (*Printf.printf "open list length: %d\n%!" (Dpq.count q);*)
	      Limit.curr_q i (Dpq.count q);
	      if not (Dpq.empty_p q) then
		(alpha.backup_f <- (Dpq.peek_first q).f;
		 alpha.backup_g <- (Dpq.peek_first q).g;
		 (expand_best a (e+1)))
	      else
		(alpha.backup_f <- infinity;
		 alpha.backup_g <- infinity;
		 expand_best a (e+1)))
       else
	 (a, false))
  in
    expand_best actions 0

let get_backups acts tbl key =
  (** Takes a list of nodes and returns them with their backed up cost
      values. *)
  List.map (fun a ->
	      (if Htable.mem tbl (key a) then
		  {a with backup_f = a.g +.
		      (fst (Htable.find tbl (key a)));
		     backup_g = a.g +.
		      (snd (Htable.find tbl (key a)))}
	       else a))
    acts

let set_rel_self acts key =
  (** Sets rel pointer for alist of actions to self. *)
  List.map (fun a -> ({a with rel = (key a)})) acts

let update_actions acts tbl key =
  (** Updates action list by setting rel pointers and getting backed
      up f-values. *)
  get_backups (set_rel_self acts key) tbl key


let prob_greater probs info delta n d alpha_g beta_cost =
  (** Probabilty that the value of [n] will raise above
      [beta_cost]. *)
  let accuracy = probs.accuracy in
  let trunc x = Functions.trunc_to x accuracy in
  let incr_amt = 1. /. (10.**(float accuracy)) in
  let n_h = trunc (n.f -. n.g) in
  let n_g = n.g -. alpha_g in
  let total = get_total probs n_h
  and counts = Wrht.map (fun k v -> (k, v))
    (get_counts probs n_h d) in
    ((*Printf.printf "d: %d, beta_cost: %f, delta: %f, n_h: %f,
       incr_amt: %f\n" d beta_cost
       delta n_h incr_amt;*)
      if Limit.halt_p info then
	((*print_endline "halt hit in prob_greater";*)
	  0.)
      else if total != 0 then
	let get_probs i =
	  let c = (List.fold_left
		     (fun acc (k, v) -> (if (n_h +. n_g +.  k) > i then
					   acc +. (float v)
					 else
					   acc)) 0. counts) in
	    ((if c > 0. then
		((*Printf.printf "accuracy: %i\n" accuracy;
		 Printf.printf "seen values larger than %f for %f (%f + %f) %f times\n"
		   i (n_h+.n_g) n_h n_g c*)));
	     if c = 0. then
	       0.
	     else
	       c/.(float total)) in
	  ((*Printf.printf "number of items seen at this depth: %d\n" (List.length counts);*)
	    (*Printf.printf "probability of increasing: %f\n" !sum;*)
	    (*Verb.pe Verb.always "calculating probability\n";*)
	    let ret = List.fold_left (+.) 0. (Wrutils.map_for_f []
						(trunc beta_cost)
						((trunc delta)-.1.)
						incr_amt get_probs) in
	      ((*Verb.pe Verb.always "done calculating probability\n";*)
		ret))
      else
	((*Printf.printf "learning hadn't seen heuristic value %d\n" n_h;*)
	  (*Verb.pe Verb.always "learning hadn't seen heuristic value";*)
	  0.))

let search_cost branch_factor i r d =
  let ret = (r *. (float i)) *. (branch_factor ** (float d)) in
    (*Printf.printf "search cost: %f\n" ret;*)
    ret

let fake_infinity = 999.

let search_worthwhile openlists alpha beta horizon r info
    key probs =
  (** Determines whether search out to any depth less than horizon
      would lead to a change in belief about the best action. *)
  let openlist = (Htable.find openlists (key alpha)) in
  let len_alpha = Dpq.count openlist
  and ret = ref false in
    if len_alpha > 0 then
      let n = Dpq.peek_first openlist in
      let beta_cost = (beta.backup_f-.beta.g) in
      let get_elt = fst (Dpq.make_iterator_unsafe openlist) in
      let rec get_size acc elt =
	match elt with
	  | None -> len_alpha-1
	  | Some x -> (if (x.f-.x.g) >= beta_cost
		       then
			 acc
		       else
			 get_size (acc+1) (get_elt ())) in
      let len_m_zero = get_size 0 (get_elt ()) in
	((for d = 1 to horizon do
	    for i = len_m_zero to (len_alpha-1) do (* length of set m *)
	      let delta = (match get_elt () with
			     | Some delta_node -> delta_node.f -. delta_node.g
			     | None -> fake_infinity) in
		((*Printf.printf "length of M-0: %d, len_alpha-1: %d\n"
		   len_m_zero (len_alpha-1);*)
		  if ((prob_greater probs info delta n d alpha.g beta_cost) -.
			(search_cost probs.branch_factor i r d)) > 0. then
		    ((*print_endline "search was ACTUALLY worthwhile";*)
		      ret := true))
	    done
	  done);
	 (*Printf.printf "length of alpha's open list: %d\n" len_alpha;*)
	 !ret)
    else
      ((*print_endline "alpha's open list was empty. search is not worthwhile.";*)
	!ret)

let dta_choose ?(record = no_record) expand key hash equals info
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
      (*print_endline "entering dta_choose";*)
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
      let rec evaluate (acts, goal_found) =
	if ((List.length acts) == 0) then
	  (* unsolvable instance *)
	  curr
	else if ((List.length acts) == 1) then
	  List.hd acts
	else
	  let alpha = List.hd (List.sort backup_f_then_g acts)
	  and beta = List.hd (List.tl (List.sort backup_f_then_g acts)) in
	    if goal_found then
	      ((*print_endline "found goal in dta_choose";*) List.hd acts)
	    else if Limit.halt_p info then
	      ((*print_endline "halt hit in dta_choose";*)
	      alpha)
	    else if not (search_worthwhile table alpha beta horizon r info
			   key probs) then
	      ((*print_endline "search wasn't worthwhile";
		 Printf.printf "backed up f-value of alpha in dta_choose: %f\n%!" alpha.backup_f;*) alpha)
	    else
	      ((*print_endline "search was worthwhile";*)
		evaluate (search acts table grain_size horizon
			    expand key info goal_p))
      in evaluate (actions, false)
  in ((*Printf.printf "number of actions: %d\n%!" (List.length (expand curr));*)
      choose (update_actions (expand curr) backup_table key))


let dta_search ?(record = no_record) sface grain_size horizon r
    learn_path ?(online = false) dups =
  (** Uses the action chosen by dta_choose to take the best action
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
    if online then Online.output_row start
      (next.g -. curr.g);
    next in
  let rec move curr =
    if (sface.Search_interface.goal_p curr) then
      ((*print_endline "found a goal in move";*)
	(*Printf.printf "initial state's h-value: %f\n"
	  initial.data.Tiles.h;*)
	Limit.new_incumbent i (Limit.Incumbent (0.,curr));
	ret_func i)
    else if Limit.halt_p i then
      ((*print_endline "halt hit in dta_search";*)
       ret_func i)
    else
      let alpha = dta_choose sface.Search_interface.node_expand
	sface.Search_interface.key sface.Search_interface.hash
	sface.Search_interface.equals sface.Search_interface.info
	sface.Search_interface.goal_p backup_table curr grain_size
	horizon r dups probs in
	(Limit.incr_exp i;
	 (*Printf.printf "f-value of curr: %f\n%!" curr.f;
	   Printf.printf "original f-value of alpha: %f\n%!" alpha.f;
	   Printf.printf "backed up f-value of alpha: %f\n%!"
	   alpha.backup_f;*)
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


  let no_dups sface args =
  (** Performs a DTA* search from the initial state to a goal,
      for domains with no duplicates. *)
  let grain_size = Search_args.get_int "Dtastar.no_dups" args 0 in
  let horizon = Search_args.get_int "Dtastar.no_dups" args 1 in
  let r = Search_args.get_float "Dtastar.no_dups" args 2 in
  let learn_path = Search_args.get_string "Dtastar.dups" args 3 in
  let search_interface = make_sface sface in
    Limit.unwrap_sol6_into_5 unwrap_sol
      (dta_search
	 search_interface
	 grain_size
	 horizon
	 r
	 learn_path
	 false)


  let dups sface args =
  (** Performs a DTA* search from the initial state to a goal,
      for domains where duplicates are frequently encountered. *)
  let grain_size = Search_args.get_int "Dtastar.dups" args 0 in
  let horizon = Search_args.get_int "Dtastar.dups" args 1 in
  let r = Search_args.get_float "Dtastar.dups" args 2 in
  let learn_path = Search_args.get_string "Dtastar.dups" args 3 in
  let search_interface = make_sface sface in
    Limit.unwrap_sol6 unwrap_sol
      (dta_search
	 (* must have g=0 as base for others, and
	    f<others to prevent re-opening *)
	 search_interface
	 grain_size
	 horizon
	 r
	 learn_path
	 true)


  let online_no_dups sface args =
  (** Performs a DTA* search from the initial state to a goal,
      for domains with no duplicates. *)
  let grain_size = Search_args.get_int "Dtastar.no_dups" args 0 in
  let horizon = Search_args.get_int "Dtastar.no_dups" args 1 in
  let r = Search_args.get_float "Dtastar.no_dups" args 2 in
  let learn_path = Search_args.get_string "Dtastar.dups" args 3 in
  let search_interface = make_sface sface in
    Online.output_col_hdr ();
    Limit.unwrap_sol6_into_5 unwrap_sol
      (dta_search
	 search_interface
	 grain_size
	 horizon
	 r
	 learn_path ~online:true
	 false)


  let online_dups sface args =
  (** Performs a DTA* search from the initial state to a goal,
      for domains where duplicates are frequently encountered. *)
  let grain_size = Search_args.get_int "Dtastar.dups" args 0 in
  let horizon = Search_args.get_int "Dtastar.dups" args 1 in
  let r = Search_args.get_float "Dtastar.dups" args 2 in
  let learn_path = Search_args.get_string "Dtastar.dups" args 3 in
  let search_interface = make_sface sface in
    Online.output_col_hdr ();
    Limit.unwrap_sol6 unwrap_sol
      (dta_search
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

(* EOF *)
