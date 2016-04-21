(** A* search variant for recording search data online *)


type 'a node = {
  data : 'a;          (* Data Payload *)
  f : float;          (* Total cost of a node*)
  mutable est_f : float;
  g : float;          (* Cost of reaching a node *)
  h : float;
  d : float;
  depth : int;        (* Depth of node in tree.  Root @ 0 *)
  mutable q_pos : int;  (* Position info for dpq *)
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


let est_f_then_d_then_g a b =
  (** sorts nodes in order of estimated f, breaking ties in favor of
      low d values, and then in favor of high g *)
  (a.est_f < b.est_f) ||  (* sort by fhat *)
    ((a.est_f = b.est_f) &&
       a.d < b.d) ||
    (a.est_f = b.est_f && a.d = b.d &&
	((a.g >= b.g)))


let f_then_g a b =
  (** expansion ordering predicate, and also works for ordering duplicates
      assuming that h is the same for both
      (hence f will be lower when g is lower). *)
  (a.f < b.f) ||
  ((a.f = b.f) && (a.g >= b.g))


let just_f a b =
  (** Sorts nodes solely on total cost information *)
  a.f <= b.f


let setpos n i =
  (** Sets the location of a node, used by dpq's *)
  n.q_pos <- i


let getpos n =
  (** Returns the position of the node in its dpq.
      Useful for swapping nodes around on the open list *)
  n.q_pos


let make_expand expand hd timer calc_h_data f_calc node_record =
  (** [expand] is the domain expand
      [hd] is a cost and distance estimator
      [timer] returns true every so often, to tell the openlist to resort
      [calc_h_data] takes the parent, the best child, and all children
      in order to make a better h estimator
      [f_calc] uses the estimated h values to calculate the bounded f
      estimates *)
  (fun n ->
     let best_f = ref infinity
     and best_child = ref n
     and reorder = timer() in
     let children = (List.map (fun (s, g) ->
				 let h, d = hd s in
				 let f = g +. h in
				 let c =
				   { est_f = f;
				     f = f;
				     h = h;
				     d = d;
				     g = g;
				     depth = n.depth + 1;
				     q_pos = Dpq.no_position;
				     data = s;} in
				   if  f < !best_f then
				     (best_child := c;
				      best_f := f)
				   else if f = !best_f then
				     (if d < !best_child.d then
					(best_child := c;
					 best_f := f));
				   c)
		       (expand n.data n.g))
     in
       node_record n n children;
       if not ((List.length children) = 0)
       then
	 (calc_h_data n !best_child children;
	  List.iter (fun c -> c.est_f <- f_calc c) children);
       reorder, children)


let make_updater f_calc =
  (** Updates the estimated f values of all nodes in a given dpq *)
  (fun dpq ->
     Dpq.iter (fun n -> n.est_f <- f_calc n) dpq)



(************************* Searches Base ************************************)
let make_interface sface h_calc f_calc timer node_record =
  let init = Search_interface.make
    ~goal_p:(wrap sface.Search_interface.goal_p)
    ~halt_on:(sface.Search_interface.halt_on)
    ~hash:sface.Search_interface.hash
    ~equals:sface.Search_interface.equals
    sface.Search_interface.domain
    { est_f = neg_infinity;
      f = neg_infinity;
      h = neg_infinity;
      d = neg_infinity;
      g = 0.;
      depth = 0;
      q_pos = Dpq.no_position;
      data = sface.Search_interface.initial;}
    just_f
    (Limit.make_default_logger (fun n -> n.g +. n.h)
       (wrap sface.Search_interface.get_sol_length)) in
    Search_interface.alter
        ~resort_expand:(Some (make_expand sface.Search_interface.domain_expand
		    sface.Search_interface.hd timer h_calc f_calc
		    (node_record init.Search_interface.info)))
      init


let no_dups sface bound timer h_calc f_calc node_record queue_record =
  (** Performs an A* search from the initial state to a goal,
      for domains with no duplicates. *)
  let search_interface = make_interface sface h_calc f_calc timer node_record
  in
    Limit.unwrap_sol5 unwrap_sol
      (Reorderable_best_first.search
	 ~record:queue_record
	 search_interface
	 est_f_then_d_then_g
	 just_f
	 (make_updater f_calc))


let dups sface bound timer h_calc f_calc node_record queue_record =
  (** Performs an A* search from the initial state to a goal,
      for domains where duplicates are frequently encountered. *)
  (** Performs a search in domains where there are duplicates.
      [sface] is the domain's search interface
      [bound] is the desired quality bound
      [timer] is a timer from Timers.ml
      [h_calc] takes the parent, best child, and all children, returns unit
      [f_calc] takes a node and returns a float *)
  let search_interface = make_interface sface h_calc f_calc timer node_record
  in
    Limit.unwrap_sol6 unwrap_sol
      (Reorderable_best_first.search_dups
	 ~record:queue_record
	 search_interface
	 est_f_then_d_then_g
	 just_f
	 setpos
	 getpos
	 (make_updater f_calc))

(* TODO: We aren't using bound, why is it in the footprint? *)
let drop sface bound timer h_calc f_calc node_record queue_record =
  (** Performs an A* search from the initial state to a goal,
      for domains where duplicates are frequently encountered. *)
  (** Performs a search in domains where there are duplicates.
      [sface] is the domain's search interface
      [bound] is the desired quality bound
      [timer] is a timer from Timers.ml
      [h_calc] takes the parent, best child, and all children, returns unit
      [f_calc] takes a node and returns a float *)
  let search_interface = make_interface sface h_calc f_calc timer node_record
  in
    Limit.unwrap_sol6 unwrap_sol
      (Reorderable_best_first.search_drop_dups
	 ~record:queue_record
	 search_interface
	 est_f_then_d_then_g
	 just_f
	 setpos
	 getpos
	 (make_updater f_calc))


(****************************************************************************)

(*** Recorders ***)
let exp_rec key_printer key =
  Recorders.expansion_recorder key_printer (wrap key)
    (fun n -> n.g) (fun n -> n.depth) (fun n -> n.est_f)

let queue_rec key_printer key = Recorders.dpq_recorder key_printer (wrap key)

let truth_rec key_printer key =
  (* note that for simplicities sake, forward heuristics still point forward*)
  Recorders.truth_recorder key_printer (wrap key) (fun n -> n.g)
    (fun n -> n.depth) (fun n -> n.h) (fun n -> n.d)


(****************************************************************************)

let expand_recorder_nodups sface bound timer hcalc fcalc =
  no_dups sface bound timer hcalc fcalc
    (exp_rec sface.Search_interface.key_printer
       sface.Search_interface.key) Recorders.none

and expand_recorder_dups sface bound timer hcalc fcalc =
  dups sface bound timer hcalc fcalc
    (exp_rec sface.Search_interface.key_printer
       sface.Search_interface.key) Recorders.none

and expand_recorder_dd sface bound timer hcalc fcalc =
  drop sface bound timer hcalc fcalc
    (exp_rec sface.Search_interface.key_printer
       sface.Search_interface.key) Recorders.none

and queue_recorder_nodups sface bound timer hcalc fcalc =
  no_dups sface bound timer hcalc fcalc Recorders.no_node_record
    (queue_rec sface.Search_interface.key_printer
       sface.Search_interface.key)

and queue_recorder_dups sface bound timer hcalc fcalc =
  dups sface bound timer hcalc fcalc Recorders.no_node_record
    (queue_rec sface.Search_interface.key_printer
       sface.Search_interface.key)

and queue_recorder_dd sface bound timer hcalc fcalc =
  drop sface bound timer hcalc fcalc Recorders.no_node_record
    (queue_rec sface.Search_interface.key_printer sface.Search_interface.key)


(*****************************************************************************)

let h_ss_reckless_dups_exp sface args =
  (** perform a search based on this estimated f function on a domain
      with many duplicates.  Never update f estimates or resort queues *)
  let bound = Search_args.get_float "Recording_clamped.h_ss_reckless_dups_exp"
    args 0 in
  let h_calc,f_calc = Global_h_ss.make_clamped_correction (fun n -> n.g)
    (fun n -> n.h) (fun n -> n.d) bound in
    expand_recorder_dups sface bound Timers.reckless h_calc f_calc


let h_ss_reckless_dups_queue sface  args =
  (** perform a search based on this estimated f function on a domain
      with many duplicates.  Never update f estimates or resort queues *)
  let bound = Search_args.get_float
    "Recording_clamped.h_ss_reckless_dups_queue" args 0 in
  let h_calc,f_calc = Global_h_ss.make_clamped_correction (fun n -> n.g)
    (fun n -> n.h) (fun n -> n.d) bound in
    queue_recorder_dups sface bound Timers.reckless h_calc f_calc


let h_ss_reckless_dd_exp sface  args =
  (** perform a search based on this estimated f function on a domain
      with many duplicates.  Never update f estimates or resort queues *)
  let bound = Search_args.get_float "Recording_clamped.h_ss_reckless_dd_exp"
    args 0 in
  let h_calc,f_calc = Global_h_ss.make_clamped_correction (fun n -> n.g)
    (fun n -> n.h) (fun n -> n.d) bound in
    expand_recorder_dd sface bound Timers.reckless h_calc f_calc


let h_ss_reckless_dd_queue sface  args =
  (** perform a search based on this estimated f function on a domain
      with many duplicates.  Never update f estimates or resort queues *)
  let bound = Search_args.get_float "Recording_clamped.h_ss_reckless_dd_queue"
    args 0 in
  let h_calc,f_calc = Global_h_ss.make_clamped_correction (fun n -> n.g)
    (fun n -> n.h) (fun n -> n.d) bound in
    queue_recorder_dd sface bound Timers.reckless h_calc f_calc


(* EOF *)
