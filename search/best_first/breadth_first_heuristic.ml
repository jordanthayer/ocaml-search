(** Breadth-first heuristic search.

    @author eaburns
    @since 2010-03-08
*)


type 'state node = {
  data : 'state;
  relay : 'state;
  h : float;
  mutable g : float;
  mutable on_q : bool;
}

exception Halted


module Layer = struct
  (* A depth layer with quick look-ups. *)
  type ('state, 'key) t = {
    q : 'state node Queue.t;
    tbl : ('key, 'state node) Htable.t;
  }

  let make hash eq =
    {
      q = Queue.create ();
      tbl = Htable.create hash eq 100;
    }


  let insert key t node =
    try
      let real_n = Htable.find t.tbl (key node.data) in
	if real_n.g > node.g
	then begin
	  real_n.g <- node.g;
	  if not real_n.on_q
	  then begin
	    Queue.push real_n t.q;
	    real_n.on_q <- true
	  end
	end
    with Not_found ->
      Queue.push node t.q;
      node.on_q <- true;
      Htable.add t.tbl (key node.data) node


  let take key t =
    let node = Queue.take t.q in
      node.on_q <- false;
      node


  let mem key t state = Htable.mem t.tbl (key state)


  let is_empty t = Queue.is_empty t.q

end


let new_layers k layers cur =
  if (List.length layers) = k
  then (List.tl layers) @ [cur]
  else layers @ [cur]


let get_relay parent state ~h ~g =
  if parent.h > parent.g && h <= g then state else parent.relay


let bfhs info k bound pr_key key hash eq set_parent h expand is_goal init =
  (** [bfhs ...] does a single search returning the midpoint. *)
  let rec breadth_first depth layers cur =
    let next = Layer.make hash eq in
    let goal = ref None in
      while not (Layer.is_empty cur) && !goal = None do
	if Limit.halt_p info then raise Halted;
	let node = Layer.take key cur in
	  if is_goal node.data
	  then goal := Some node
	  else
	    List.iter
	      (fun (child, child_g) ->
		 let child_h = h child in
		 let relay = get_relay node child ~h:child_h ~g:child_g in
		   set_parent child child;
		   (* Make each child its own parent so that the path is not
		      preserved. *)

		   if child_g +. child_h < bound
		     && (List.for_all
			   (fun l -> not (Layer.mem key l child))
			   layers)
		   then
		     let child_node = { data = child;
					relay = relay;
					g = child_g;
					h = child_h;
					on_q = false;
				      }
		     in Layer.insert key next child_node)
	      (expand node.data node.g)
      done;
      match !goal with
	| None ->
	    if Layer.is_empty next
	    then None
	    else breadth_first (depth + 1) (new_layers k layers cur) next
	| Some goal -> Some (goal.relay, goal.g)
  in

  let layer = Layer.make hash eq in
    Layer.insert key layer { data = init;
			     relay = init;
			     g = 0.;
			     h = h init;
			     on_q = false; };
    breadth_first 0 [ ] layer


let fix_parents set_parent path init =
  (** [fix_parents set_parent path init] sets the solution path by
      chaining the parent pointers of all nodes in the given path. *)
  List.fold_left (fun parent node ->
		    set_parent node parent;
		    node) init path


let unwrap_sol = function
  | Limit.Nothing -> None
  | Limit.Incumbent (g, n) -> Some (n, g)


let do_search sface k bound =
  (** [do_search sface k bound] *)
  let module I = Search_interface in
  let info = sface.I.info in
  let init = sface.I.initial in
  let set_parent n p = try sface.I.parent_update n p with _ -> () in
  let search sface =
    let expand = sface.I.domain_expand
    and set_parent = sface.I.parent_update
    and key = sface.I.key
    and h = sface.I.h
    and is_goal = sface.I.goal_p
    and init = sface.I.initial
    and hash = sface.I.hash
    and eq = sface.I.equals
    and pr_key = sface.I.key_printer
    and info = sface.I.info in
      bfhs info k bound pr_key key hash eq set_parent h expand is_goal init
  in
    try
      let sol = Solution_reconstruct.divide_and_conquer search sface in
	begin match sol with
	  | None -> ()
	  | Some (path, cost) ->
	      let goal_node = fix_parents set_parent path init in
		Limit.new_incumbent info (Limit.Incumbent (cost, goal_node));
	end;
	Limit.unwrap_sol6 unwrap_sol (Limit.results6 info);
    with Halted -> Limit.unwrap_sol6 unwrap_sol (Limit.results6 info)


let search sface args =
  (** [search sface args] is the standard interface to the search. *)
  let k = Search_args.get_int "breadth first heuristic search" args 0 in
    do_search sface k infinity
