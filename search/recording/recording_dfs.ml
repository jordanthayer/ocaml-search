(**

    @author jtd7
    @since 2010-08-17

   Recording version of depth first search, for the kids
*)

type 'a node = {
  parent : 'a node;
  data : 'a;
  g : float;
  f : float;
  depth : int;
  h : float;
  d : float;
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


let better_p a b =
  (** Sorts nodes solely on total cost information *)
  a.f <= b.f

let ordered_p a b =
  (a.f -. a.g) < (b.f -. b.g)

let make_expand recorder expand hd =
  (** Takes the domain expand function and a heuristic calculator
      and creates an expand function which returns search nodes. *)
  (fun n ->
     let children =
       List.map (fun (dat, g) ->
		   let h,d = hd dat in
		     { parent = n;
		       data = dat;
		       g = g;
		       f = g +. h;
		       depth = n.depth + 1;
		       h = h;
		       d = d; }) (expand n.data n.g) in
       recorder n n.parent children;
       children)


let make_root dat f_hd =
  let h,d = f_hd dat in
  let rec root =
    { data = dat;
      parent = root;
      f = h;
      g = 0.;
      h = h;
      d = d;
      depth = 0;} in root


let make_iface node_record sface =
  let init = Search_interface.make
    ~goal_p:(fun n -> sface.Search_interface.goal_p n.data)
    ~halt_on:sface.Search_interface.halt_on
    ~hash:sface.Search_interface.hash
    ~key:(wrap sface.Search_interface.key)
    ~equals:sface.Search_interface.equals
    sface.Search_interface.domain
    (make_root sface.Search_interface.initial sface.Search_interface.hd)
    better_p
    (Limit.make_default_logger (fun n -> n.f)
       (wrap sface.Search_interface.get_sol_length)) in
  let recorder = node_record init.Search_interface.info in
    (Search_interface.alter
       ~node_expand:(Some (make_expand
			     recorder
			     sface.Search_interface.domain_expand
			     sface.Search_interface.hd))
       ~goal_p:(Some
		  (fun n ->
		     let goal = sface.Search_interface.goal_p n.data in
		       if goal then recorder n n.parent [];
		       goal))
       init)

let child_order better_p =
  (fun a b -> if better_p a b
   then 1
   else (if better_p b a
	 then -1
	 else 0))


let no_dups_checking ?(record_q = Fn.no_op2) sface better_p =
  let i = sface.Search_interface.info in
  let feasible_p n =
    (match i.Limit.incumbent with
	 Limit.Incumbent(float,m) -> better_p n m
       | _ -> true) in
  let rec expand_best n =
    if not (Limit.halt_p i)
    then
      (Limit.incr_exp i;
       if sface.Search_interface.goal_p n
       then Limit.new_incumbent i (Limit.Incumbent (0.,n))
       else (let children = sface.Search_interface.node_expand n in
	       List.iter (fun c ->
			    Limit.incr_gen i;
			    if feasible_p c
			    then expand_best c)
		 (List.sort (child_order better_p) children))) in
    expand_best sface.Search_interface.initial;
    Limit.results6 i


let dups_hash ?(continue = false) ?(record_q = Fn.no_op2)
    ?ordered_p sface better_p =
  let i = sface.Search_interface.info
  and closed = Htable.create sface.Search_interface.hash
    sface.Search_interface.equals 100
  and co = child_order (match ordered_p with None -> better_p
			  | Some pred -> pred)
  and solved = ref false
  and openlist = Stack.create() in
  let feasible_p n =
    try
      let best = Htable.find closed (sface.Search_interface.key n) in
	Limit.incr_dups i;
	if better_p n best && continue
	then (Htable.replace closed (sface.Search_interface.key n) n;
	      true)
	else false
    with Not_found ->
      Htable.replace closed (sface.Search_interface.key n) n;
      (match i.Limit.incumbent with
	   Limit.Incumbent(float,m) -> better_p n m
	 | _ -> true) in

  let rec expand_best () =
    if not (Limit.halt_p i)
    then
      (record_q i openlist;
       let n = Stack.pop openlist in
       Limit.incr_exp i;
       if sface.Search_interface.goal_p n
       then (solved := true;
	     Verb.pe Verb.always "Solved!\n";
	     Limit.new_incumbent i (Limit.Incumbent (0.,n)))
       else (let children = List.sort co
	       (sface.Search_interface.node_expand n)  in
	       List.iter (fun c ->
			    Limit.incr_gen i;
			    if feasible_p c
			    then Stack.push c openlist
			    else Limit.incr_prune i) children);
       if (not !solved) || continue then expand_best ()) in

    Stack.push sface.Search_interface.initial openlist;
    expand_best ();
    Limit.results6 i



(***************************************************************************)

let dups sface node_record queue_record =
  (** Performs an A* search from the initial state to a goal,
      for domains where duplicates are frequently encountered. *)
  let search_interface = make_iface node_record sface in
    Limit.unwrap_sol6 unwrap_sol
      (dups_hash
	 ~record_q:queue_record
	 search_interface
	 better_p)


let exp_rec key_printer key =
  Recorders.expansion_recorder key_printer (wrap key)
    (fun n -> n.g) (fun n -> n.depth) (fun n -> n.f)


let queue_rec key_printer key = Recorders.stack_recorder key_printer (wrap key)


let expand_recorder_dups sface args =
  Search_args.is_empty "Recording_dfs.expand_recorder_dups" args;
  let expand_rec =  (exp_rec sface.Search_interface.key_printer
		sface.Search_interface.key) in
  dups sface expand_rec Recorders.none


let stack_recorder_dups sface args =
  Search_args.is_empty "Recording_astar.queue_recorder_dups" args;
  dups sface Recorders.no_node_record
    (queue_rec sface.Search_interface.key_printer sface.Search_interface.key)


(* EOF *)
