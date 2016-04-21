(**

    @author jtd7
    @since 2011-01-10

   Contract A* as described in Heuristic Search Under Contract
   Aine, Chakrabarti, Kumar Computational Intelligence
*)

type 'a node = {
  data : 'a;
  f : float;
  g : float;
  depth : int;
  mutable q_pos : int;
}

let ordered_p a b =
  a.f < b.f ||
    ((a.f = b.f) && (a.g > b.g)) ||
    ((a.f = b.f) && (a.g = b.g))

let setpos n i = n.q_pos <- i

let getpos n = n.q_pos


type 'a layer = {
  open_list : 'a node Dpq.t;
  max_count : int;
  mutable count : int;
  mutable lqp : int;
}

let better_layer layer1 layer2 =
  let empty1 = Dpq.empty_p layer1.open_list
  and empty2 = Dpq.empty_p layer2.open_list in
    match empty1, empty2 with
      | true, true -> true
      | true, false -> false
      | false, true -> true
      | false, false -> (let has_nodes1 = layer1.count <= layer1.max_count
			 and has_nodes2 = layer2.count <= layer2.max_count in
			   match has_nodes1, has_nodes2 with
			     | false, false -> (* the backup strategy *)
				 (let n1 = Dpq.peek_first layer1.open_list
				  and n2 = Dpq.peek_first layer2.open_list in
				    ordered_p n1 n2)
			     | true, false -> true
			     | false, true -> false
			     | true, true ->
				 (let n1 = Dpq.peek_first layer1.open_list
				  and n2 = Dpq.peek_first layer2.open_list in
				    ordered_p n1 n2))


let make_expand expand h =
  let make_child depth (data, g) =
    { data = data;
      f = g +. (h data);
      g = g;
      depth = depth;
      q_pos = Dpq.no_position;} in
  (fun n ->
     let next_depth = n.depth + 1 in
       List.map (make_child next_depth)
	 (expand n.data n.g))


let make_initial sface =
  { data = sface.Search_interface.initial;
    f = sface.Search_interface.h sface.Search_interface.initial;
    g = 0.;
    depth = 0;
    q_pos = Dpq.no_position; }


type 'a layers = {
  mutable by_depth : 'a layer array;
  by_best : 'a layer Dpq.t;
}


let make_layers first_layer depth_est =
  let depth_est = depth_est + 1 in
  let by_depth = Array.create depth_est first_layer in
  let by_best = (Dpq.create better_layer (fun a i -> a.lqp <- i) depth_est
		   first_layer) in
    Dpq.insert by_best first_layer;
    { by_depth = by_depth;
      by_best = by_best; }


let get_best_node layers =
  let best_layer = Dpq.extract_first layers.by_best in
  let best_node = Dpq.extract_first best_layer.open_list in
    best_node.q_pos <- Dpq.no_position;
    Dpq.insert layers.by_best best_layer;
    best_node


exception Layer_too_deep of int
exception Node_too_deep of int

let make_dpq size init = Dpq.create ordered_p setpos size init

let make_new_layer size first_node =
  (*Verb.pe Verb.always "Layer %i size %i\n%!" first_node.depth size;*)
  let q = make_dpq size first_node in
    Dpq.insert q first_node;
    {open_list = q;
     max_count = size;
     count = 0;
     lqp = Dpq.no_position; }

let insert_layer depth layers layer =
    if depth >= (Array.length layers.by_depth)
    then layers.by_depth <- Wrarray.extend layers.by_depth depth layer;
    layers.by_depth.(depth) <- layer;
    Dpq.insert layers.by_best layer


let insert_node layers n =
  let depth = n.depth in
    if depth < Dpq.count layers.by_best
    then (let layer = layers.by_depth.(depth) in
	    Dpq.insert layer.open_list n;
	    Dpq.see_update layers.by_best layer.lqp)
    else raise (Node_too_deep depth) (* add layer here *)


let estimate_prp ?(alpha = 1.) ?(beta = 1.) ?(gamma = 0.) height_est bfactor =
  (* prp is P(S | l, k(l)) *)
  assert (alpha >= 1.);
  assert (beta <= 1.);
  assert (gamma >= 0.);
  (fun depth nodes ->
     let depth = float depth
     and nodes = float nodes in
       Math.fmin 1.
	 (((nodes /. (bfactor ** depth)) *. alpha) **
	    (((height_est -. depth) /. height_est)) *. beta -. gamma))


let goal_est mean =
  let fn = Normal.make_1d_pdf mean 1. in
    (fun depth -> (fn (float depth)))


let goal_unif start stop =
  let range = float ((stop - start) + 1) in
  let step = 1. /. range in
    (fun depth -> if depth >= start && depth <= stop then step else 0.)


let remaining_nodes layers =
  Array.fold_left (fun accum e -> accum || not (Dpq.empty_p e.open_list))
    false layers.by_depth

let dups ar layer_est bfactor expand root goal info key eq hash =
  let initial_layer = make_new_layer 1 root
  and closed = Htable.create hash eq 100 in
  let layers = make_layers initial_layer layer_est in
  let assigned = ref 0 in

  let consider_child c =
    if not (Limit.promising_p info c)
    then (Limit.incr_prune info;
	  false)
    else (let kval = key c in
	    try
	      let prev = Htable.find closed kval in
		if prev.g > c.g
		then (Htable.replace closed kval c;
		      if prev.q_pos <> Dpq.no_position
		      then (let layer_at_depth = layers.by_depth.(prev.depth) in
			      let p' = Dpq.remove_ret
				layer_at_depth.open_list prev.q_pos in
				Dpq.see_update layers.by_best
				  layer_at_depth.lqp;
				assert (eq (key p') (key prev));
			      prev.q_pos <- Dpq.no_position);
		      true)
		else false
	    with Not_found -> (Htable.add closed kval c;
			       true)) in

  let rec expand_eval () =
    if not (Limit.halt_p info) && (remaining_nodes layers)
      (* just check the best one for nodes *)
      (*(not (Dpq.empty_p (Dpq.peek_first layers.by_best).open_list))*)
    then
      (let next_node = get_best_node layers in
	 if not (Limit.promising_p info next_node)
	 then (Limit.incr_prune info;
	       expand_eval ())
	 else if goal next_node
	 then (Limit.new_incumbent info (Limit.Incumbent (0., next_node));
	       expand_eval ())
	 else (let children = expand next_node in
	       let last_layer = layers.by_depth.(next_node.depth) in
		 last_layer.count <- last_layer.count + 1;
		 if last_layer.count >= last_layer.max_count
		 then Dpq.see_update layers.by_best last_layer.lqp;
		 Limit.incr_exp info;
		 List.iter
		   (fun child ->
		      if consider_child child
		      then
			(Limit.incr_gen info;
			 (let dep = child.depth in
			    if dep >= (Dpq.count layers.by_best)
			    then (let depth = Math.imin dep (layer_est-1) in
				  let size = ar.(depth) in
				    assigned := size + !assigned;
				    let nlayer = make_new_layer size child in
				      insert_layer dep layers nlayer));
			 insert_node layers child)) children;
		 expand_eval ())) in
    insert_node layers root;
    expand_eval ()
    (*
    Array.iteri (fun i e ->
		   Verb.pe Verb.always "Depth %i expanded %i\n%!"
		     i e.count) layers.by_depth*)


let unwrap_sol s =
  match s with
      Limit.Incumbent (q, n) -> Some (n.data, n.g)
    | _ -> None


let do_dups sface args =
  let expand = (make_expand sface.Search_interface.domain_expand
		  sface.Search_interface.h)
  and contract = Search_args.get_int "Contract_astar.dups" args 0
  and root = make_initial sface
  and goal = (fun n -> sface.Search_interface.goal_p n.data)
  and key = (fun n -> sface.Search_interface.key n.data)
  and eq = sface.Search_interface.equals
  and hash = sface.Search_interface.hash
  and info = (Limit.make Limit.Nothing sface.Search_interface.halt_on ordered_p
		(Limit.make_default_logger (fun n -> n.f)
		   (fun n -> sface.Search_interface.get_sol_length n.data))) in
  let ar = Eq_38_tables.get_kl sface.Search_interface.domain contract in
    dups ar
      (truncate (Eq_38_tables.depth_est sface.Search_interface.domain))
      (Eq_38_tables.bfactor sface.Search_interface.domain)
      expand root goal info key eq hash;
    Limit.unwrap_sol6 unwrap_sol (Limit.results6 info)


let approx_dups sface args =
  let expand = (make_expand sface.Search_interface.domain_expand
		  sface.Search_interface.h)
  and contract = truncate (Search_args.get_float
			     "Contract_astar.approx_dups" args 0)
  and res = Search_args.get_int "Contract_astar.approx_dups" args 1
  and root = make_initial sface
  and goal = (fun n -> sface.Search_interface.goal_p n.data)
  and key = (fun n -> sface.Search_interface.key n.data)
  and eq = sface.Search_interface.equals
  and hash = sface.Search_interface.hash
  and info = (Limit.make Limit.Nothing sface.Search_interface.halt_on ordered_p
		(Limit.make_default_logger (fun n -> n.f)
		   (fun n -> sface.Search_interface.get_sol_length n.data))) in
  let ar = Approx_eq_38.get_kl res sface.Search_interface.domain contract in
    dups ar
      (truncate (Eq_38_tables.depth_est sface.Search_interface.domain))
      (Eq_38_tables.bfactor sface.Search_interface.domain)
      expand root goal info key eq hash;
    Limit.unwrap_sol6 unwrap_sol (Limit.results6 info)


let make_table_tlvl domain contract =
  let bfactor = Eq_38_tables.bfactor domain
  and depth_est = Eq_38_tables.depth_est domain in
  let max_depth = truncate (1.5 *. depth_est) in
  let goal_est = goal_est depth_est in
  let prp = estimate_prp depth_est bfactor in
    Eq_38_tables.solve_38 domain prp goal_est bfactor max_depth contract


let make_table domain contract =
  let outfile = Eq_38_tables.get_path domain contract in
  let table = make_table_tlvl domain contract in
    Wrio.with_outfile outfile
      (fun ch -> Marshal.to_channel ch table [])


let approx_table_tlvl res domain contract =
  let bfactor = Eq_38_tables.bfactor domain
  and depth_est = Eq_38_tables.depth_est domain in
  (*let goal_est = goal_est depth_est in*)
  let goal_est = goal_unif (truncate depth_est) (truncate (depth_est *. 1.5)) in
  let prp = estimate_prp depth_est bfactor in
    Approx_eq_38.calc_table ~res domain prp goal_est contract


let make_approx_table ?(res = 100) domain contract =
  let outfile = Approx_eq_38.get_path domain res in
  let table = approx_table_tlvl res domain contract in
    Wrio.with_outfile outfile
      (fun ch -> Marshal.to_channel ch table [])


(* EOF *)
