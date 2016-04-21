(** Partial A* estimates the number of nodes that A* will expand
    within a given bound.

    @author eaburns
    @since 2010-05-26
*)

(*
let seed = Some  1275397707103097
*)
let seed = None
let random_state =
  Random.State.make
    (match seed with
       | None ->
	   let seed = truncate ((Unix.gettimeofday ()) *. 1000000.) in
(*
	     Printf.printf "seed: %d\n%!" seed;
*)
	     [| seed |]
       | Some s -> [| s |])

let random_float bound = Random.State.float random_state bound


(** {1 Partial A* Trees} ****************************************)


type 'state node = {
  state : 'state;
  h : float;
  mutable g : float;
  mutable f : float;

  p : float;
  (** The p-value at which this state was generated. *)

  mutable pq_pos : int;

  mutable expanded : bool;
  (** Has this node been expanded? *)

  mutable parent : 'state node option;
  (** The current best parent. *)

  mutable children : 'state node list;
  (** The current children. *)
}

let node ~h ~g ?p parent state =
  (** [node ~h ~g ?p parent state] creates a new node. *)
  {
    state = state;
    p = (match p with None -> Random.float 1. | Some p -> p);
    h = h;
    g = g;
    f = g +. h;
    expanded = false;
    pq_pos = Dpq.no_position;
    parent = parent;
    children = [];
  }


let update_parent reparent ~g ~parent ~n =
  (** [update_parent reparent ~g ~parent ~n] re-parents [n]
      to [parent] with path-cost [g]. *)
  begin match n.parent with
    | None -> ()
    | Some p ->
	assert (List.memq n p.children);
	p.children <- List.filter ((!=) n) p.children;
	assert (not (List.memq n p.children));
  end;
  assert (g < n.g);
  n.g <- g;
  n.f <- n.h +. g;
  n.parent <- Some parent;
  reparent n.state parent.state;
  parent.children <- n :: parent.children


let update_pq_pos n i = n.pq_pos <- i
  (** [update_pq_pos n i] updates the open list position field. *)


let has_better_f a b = if a.f = b.f then a.g > b.g else  a.f < b.f
  (** [has_better_f a b] compares the quality of two nodes based on
      their f-values, then their g-values. *)


let has_better_p a b = a.p > b.p
  (** [has_better_p a b] compares the quality of two nodes based on their
      p-values. *)


let is_delayed p n = n.p < p
  (** [is_delayed p n] tests if the given node is delayed based on the
      current p-value, [p]. *)


let is_parent ~p ~n = match n.parent with
    (** [is_parent ~p ~n] tests if the [p] is the parent of [n]. *)
  | None -> false
  | Some parent -> p == parent


let out_of_bound f_max n  = f_max <= n.f
  (** [out_of_bound f_max n] tests if the node should be pruned by
      the bound. *)


let cost_estimator d root_f root_d =
  (** [cost_estimator d root_f root_d] makes a get and update function for
      estimating the cost of the goal given single-step error in f and
      d. *)
  let mean_f_err = ref 0.
  and mean_d_err = ref 0.
  and num = ref 0. in
    ((fun () -> root_f +. (!mean_f_err *. root_d /. !mean_d_err)),
     (fun n n' ->
	let df = n'.f -. n.f
	and dd = 1. -. ((d n.state) -. (d n'.state)) in
	  assert (df >= 0.);
	  mean_f_err := !mean_f_err +. ((df -. !mean_f_err) /. (!num +. 1.));
	  mean_d_err := !mean_d_err +. ((dd -. !mean_d_err) /. (!num +. 1.));
	  num := !num +. 1.;))


let extract_first info opened =
  (** [extract_first info opened] extracts the first node from the
      open list.  *)
  let n = Dpq.extract_first opened in
    n.expanded <- true;
    Limit.incr_exp info;
    n


let handle_old_child
    info seen ~opened ~delayed key h reparent p parent (state, g) =
  (** [handle_old_child info seen ~opened ~delayed key h reparent p
      parent (state, g)] handles the generation of a duplicate
      child. *)
  let dup = Htable.find seen (key state) in
    Limit.incr_dups info;
    if dup.g > g
    then begin
      update_parent reparent ~g ~parent ~n:dup;
      if is_delayed p dup
      then begin
	assert (not dup.expanded);
	if dup.pq_pos = Dpq.no_position then Dpq.insert delayed dup
      end else begin
	if dup.pq_pos = Dpq.no_position
	then Dpq.insert opened dup
	else Dpq.see_update opened dup.pq_pos
      end
    end


let handle_new_child
    forced seen ~opened ~delayed key h reparent ~f_max ~p parent (state, g) =
  (** [handle_new_child forced seen ~opened ~delayed key h reparent ~p
      ~f_max parent (state, g)] handles the generation of a brand new
      child.  If [forced] is true then the child is not delayed. *)
  let child =
    if forced
    then node ~h:(h state) ~g ~p:1. (Some parent) state
    else node ~h:(h state) ~g (Some parent) state
  in
    parent.children <- child :: parent.children;
    Htable.add seen (key state) child;
    if not (out_of_bound f_max child)
    then begin
      if is_delayed p child
      then Dpq.insert delayed child
      else Dpq.insert opened child
    end


let choose_child best_f h new_children =
  (** [choose_child best_f h new_chidren] choose the child that will be
      forced to be expanded. *)
  let nnew = List.length new_children in
    if nnew > 0
    then
      if best_f
      then begin
	let best_f = ref infinity in
	let best_i = ref ~-1 in
	  Wrlist.iteri (fun i (state, g) ->
			  let f = g +. (h state) in
			    if f < !best_f
			    then begin
			      best_f := f;
			      best_i := i;
			    end)
	    new_children;
	  !best_i
      end else Random.int nnew
    else 0


let extend_partial_tree
    ?(best_f=false) info key h reparent expand is_goal ~f_max ~p seen
    ~opened ~delayed =
  (** [extend_partial_tree ?best_f info key h reparent expand is_goal
      ~f_max ~p seen ~opened ~delayed] extends a partial A* tree.
      Nodes from [opened] are expanded and children with p values
      smaller than [p] are added to [delayed] whereas children with p
      values larger than [p] are added back to [opened].

      If [best_f] is true then the choosen child is the one with the
      best f value among the siblings.  Otherwise it is choosen at
      random. *)
  while not (Dpq.empty_p opened) && not (Limit.halt_p info) do
    let n = extract_first info opened in
      if not (out_of_bound f_max n)
      then begin
	if (is_goal n.state)
	then Limit.new_incumbent info (Limit.Incumbent (n.g, n.state))
	else
	  let children = expand n.state n.g in
	  let old_children, new_children =
	    List.partition (fun (s, _) -> Htable.mem seen (key s)) children
	  in
	  let choosen = choose_child best_f h new_children in
	    Limit.incr_gen_n info (List.length children);
	    List.iter
	      (handle_old_child info seen ~opened ~delayed key h reparent p n)
	      old_children;
	    Wrlist.iteri
	      (fun i c ->
		 handle_new_child (i = choosen) seen ~opened ~delayed key
		   h reparent ~f_max ~p n c)
	      new_children;
      end
  done


let rec estimate_size f_max root =
  (** [estimate_size f_max root] estimates the size of the full
      tree. *)
  let expd, unexpd = List.partition (fun n -> n.expanded) root.children in
  let size_sum =
    List.fold_left (fun ss n ->
		      if not (out_of_bound f_max n)
		      then ss +. (estimate_size f_max n)
		      else ss)
      0. expd
  in
  let nexpd = List.length expd in
  let size_mean = if nexpd > 0 then size_sum /. (float nexpd) else 0. in
    List.fold_left (fun s n ->
		      if not (out_of_bound f_max n)
		      then s +. size_mean
		      else s)
      (size_sum +. 1.) unexpd


(************************************************************)

(* Estimating the size of the A* tree by assuming nodes with similar f
   values have similar sized subtrees. *)


let plot_function ~min ~max pts =
  if Verb.level Verb.toplvl
  then begin
    Verb.pr Verb.toplvl "Fitting for plot\n%!";
    let ig_size, conf =
      Nonparam_regression.nadaraya_watson_with_confidence
	~xvalidate_size:100
	~tolerance:0.1
	Nonparam_regression.Kernel.gaussian pts
	min max
    in
    let ig_size = Cache.memoize ig_size 100 in
    let x_jitter = 0.2 in
      Verb.pr Verb.toplvl "Fit complete\n%!";
      Wrio.with_outfile "fit.spt"
	(fun ochan ->
	   Printf.fprintf ochan "(let* ((fit (";
	   for i = (truncate min) to (truncate max) do
	     Printf.fprintf ochan "(%d %f)" i (ig_size (float i));
	   done;
	   Printf.fprintf ochan "))\n";
	   Printf.fprintf ochan "(conf-low (";
	   for i = (truncate min) to (truncate max) do
	     Printf.fprintf ochan "(%d %f)" i
	       (ig_size (float i) -. (conf (float i)));
	   done;
	   Printf.fprintf ochan "))\n";
	   Printf.fprintf ochan "(conf-high (";
	   for i = (truncate min) to (truncate max) do
	     Printf.fprintf ochan "(%d %f)" i
	       (ig_size (float i) +. (conf (float i)));
	   done;
	   Printf.fprintf ochan "))\n";
	   Printf.fprintf ochan "(samples (";
	   Array.iter
	     (fun (x, y) ->
		let x = ((Random.float (x_jitter *. 2.)) -. x_jitter) +. x in
		  Printf.fprintf ochan "(%f %f)" x y)
	     pts;
	   Printf.fprintf ochan "))\n";
	   Printf.fprintf ochan "(fit (line-dataset :name \"fit\"\n";
	   Printf.fprintf ochan ":points fit))\n";
	   Printf.fprintf ochan "(cl (line-dataset :name \"conf-lower\"\n";
	   Printf.fprintf ochan ":points conf-low))\n";
	   Printf.fprintf ochan "(ch (line-dataset :name \"conf-upper\"\n";
	   Printf.fprintf ochan ":points conf-high))\n";
	   Printf.fprintf ochan "(samp (scatter-dataset :name \"samples\"\n";
	   Printf.fprintf ochan ":points samples))\n";
	   Printf.fprintf ochan "(plot0 (num-by-num-plot :dataset fit\n";
	   Printf.fprintf ochan ":title \"subtree size by f (fit)\"\n";
	   Printf.fprintf ochan ":dataset cl\n";
	   Printf.fprintf ochan ":dataset ch\n";
	   Printf.fprintf ochan ":dataset samp\n";
	   Printf.fprintf ochan ":x-label \"f\"\n";
	   Printf.fprintf ochan ":y-label \"estimated size\"))\n";
	   Printf.fprintf ochan ")\n(display plot0))\n";
	)
  end


let fit_function pts =
  (** [fit_function pts] fits a function to the points and returns
      it. *)
  let kern = Nonparam_regression.Kernel.gaussian in
    Nonparam_regression.nadaraya_watson ~tolerance:0.1 kern pts


let get_fit sample_size nconsidered cur_func pts = match !cur_func with
  | None ->
      let f = fit_function pts in
	cur_func := Some f;
	f
  | Some f when (!nconsidered < 10 * sample_size)
      && (Array.length pts) >= sample_size -> f
  | Some _ ->
      let f' = fit_function pts in
	cur_func := Some f';
	nconsidered := 0;
	f'

let estimate_size_with_f f_max root =
  (** [estimate_size_with_f f_max root] estimates the size of the A*
      tree by assuming that ignored subtrees have a size that is based
      on a function fit to the (f, size) pairs from the non-ignored
      subtrees. *)
  let sample_size = 100 in
  let nconsidered = ref 0 in
  let f_function = ref None in
  let consider, get_sample, reset = Sample.make_stream_sampler sample_size in
  let rec do_estimate root =
    let expd, unexpd = List.partition (fun n -> n.expanded) root.children in
    let sum =
      List.fold_left (fun s k ->
			if not (out_of_bound f_max k)
			then begin
			  let size = do_estimate k in
			    consider (k.f, size);
			    incr nconsidered;
			    s +. size
			end else s)
	1. expd
    in
      if unexpd <> []
      then begin
	let f = get_fit sample_size nconsidered f_function (get_sample ()) in
	  List.fold_left (fun s k -> (f k.f) +. s) sum unexpd
      end else sum
  in
  let size = do_estimate root in
    plot_function ~min:root.f ~max:f_max (get_sample ());
    size

(************************************************************)

let rec count_ignored_expanded f_max root =
  (** [count_ignored_expanded f_max root] counts the number of
      nodes in the partial tree that were ignored versus expanded
      within the f_max bound. *)
  let expd, unexpd = List.partition (fun n -> n.expanded) root.children in
  let nignored, nexpanded =
    List.fold_left (fun (nignored, nexpanded) n ->
		      if not (out_of_bound f_max n)
		      then
			let ign, exp = count_ignored_expanded f_max n in
			  nignored + ign, nexpanded + exp
		      else nignored, nexpanded)
      (0, 0) expd
  in nignored + (List.length unexpd), nexpanded + 1


let sanity_check hash eq key f_max root =
  (** [sanity_check hash eq key f_max root] performs some sanity
      checks on the partial tree. *)
  let seen = Htable.create hash eq 1000 in
  let rec depth_first root =
    assert (not (Htable.mem seen (key root.state)));
    Htable.add seen (key root.state) root;
    List.iter (fun child ->
		 assert (is_parent ~p:root ~n:child);
		 if not (out_of_bound f_max child)
		 then depth_first child)
      root.children
  in depth_first root


(** {1 Visualization} ****************************************)

let output_keys key pr f_max root =
  (** [output_keys key pr f_max root] outputs to a file 'keys' the
      keys with a color in the format: <r>, <g>, <b>: <key>

      Green for ignored nodes, red for expanded nodes and blue for
      nodes that are out of the f-bound. *)
  let rec do_output_keys outchan key pr f_max root =
    if out_of_bound f_max root
    then Printf.fprintf outchan "0, 0, 255: %s" (pr (key root.state))
    else begin
      if root.expanded
      then Printf.fprintf outchan "255, 0, 0: %s" (pr (key root.state))
      else Printf.fprintf outchan "0, 255, 0: %s" (pr (key root.state));
      List.iter (do_output_keys outchan key pr f_max) root.children
    end
  in
    if Verb.level Verb.toplvl
    then begin
      let outchan = open_out "keys" in
	do_output_keys outchan key pr f_max root;
	close_out outchan
    end

let astar_tree hash eq key reparent h expand f_max root_state =
  (** [astar_tree hash eq key reparent h expand f_max root_state]
      computes the nodes that A* would expand *)
  let root_h = h root_state in
  let root_node = node ~h:root_h ~g:0. None root_state in
  let open_nodes = Dpq.create_with has_better_f root_node in
  let closed_nodes = Htable.create hash eq 100 in
  let nexp = ref 0 in
    Dpq.insert open_nodes root_node;
    Htable.add closed_nodes (key root_state) root_node;
    while not (Dpq.empty_p open_nodes) do
      let n = Dpq.extract_first open_nodes in
	if n.f < f_max then begin
	  let children = expand n.state n.g in
	    incr nexp;
	    List.iter (fun (c, g) ->
			 let ck = key c in
			   try
			     let c_node = Htable.find closed_nodes ck in
			       if c_node.g > g
			       then begin
				 update_parent reparent ~g ~parent:n ~n:c_node;
				 if c_node.pq_pos = Dpq.no_position
				 then Dpq.insert open_nodes c_node
				 else Dpq.see_update open_nodes c_node.pq_pos
			       end
			   with Not_found ->
			     let ch = h c in
			     let c_node = node ~h:ch ~g (Some n) c in
			       n.children <- c_node :: n.children;
			       Dpq.insert open_nodes c_node;
			       Htable.add closed_nodes ck c_node;)
	      children
	end
    done;
    Printf.printf "astar expands %d nodes\n" !nexp;
    root_node


let rec output_both eq key ochan f_max pnode anode =
  let color = if pnode.expanded then "0.5" else "1" in
    Printf.fprintf ochan "((color :r %s :b %s)" color color;
    let pnode_children =
      List.filter (fun n -> n.f < f_max) pnode.children
    and anode_children =
      List.filter (fun n -> n.f < f_max) anode.children
    in
    let psuccs = ref pnode_children in
    let asuccs = ref anode_children in
    let leaf = !psuccs = [] && !asuccs = [] in
      if not leaf then Printf.fprintf ochan "(";
      while not (!psuccs = [] && !asuccs = []) do
	begin match !asuccs, !psuccs with
	  | [], pnodes ->
	      List.iter (output_partial key ochan f_max) pnodes;
	      psuccs := [];
	  | anodes, pnodes ->
	      List.iter
		(fun anode' ->
		   let anode_key = key anode'.state in
		   let pnode', pnodes =
		     List.partition (fun n -> eq anode_key (key n.state))
		       !psuccs
		   in
		     begin match pnode' with
		       | [] ->
			   output_astar ochan f_max anode'
		       | pnode' :: [] ->
			   output_both eq key ochan f_max pnode' anode'
		       | _ -> failwith "Multiple matches: impossible"
		     end;
		     psuccs := pnodes;)
		anodes;
	      asuccs := [];
	end;
      done;
      if not leaf
      then Printf.fprintf ochan "))\n"
      else Printf.fprintf ochan ")\n"


and output_partial key ochan f_max pnode =
  let kids = List.filter (fun n -> n.f < f_max) pnode.children in
  let color = if pnode.expanded then ":r 0.5" else ":r 1" in
    Printf.fprintf ochan "((color %s)" color;
    if kids <> []
    then begin
      Printf.fprintf ochan "(";
      List.iter (output_partial key ochan f_max) kids;
      Printf.fprintf ochan ")";
    end;
    Printf.fprintf ochan ")\n";


and output_astar ochan f_max anode =
  let kids = List.filter (fun n -> n.f < f_max) anode.children in
    Printf.fprintf ochan "((color :b 1)";
    if kids <> []
    then begin
      Printf.fprintf ochan "(";
      List.iter (output_astar ochan f_max) kids;
      Printf.fprintf ochan ")";
    end;
    Printf.fprintf ochan ")\n"


let jitter x =
  (** [jitter x] jitters the value x by some small amount. *)
  let j = 0.2 in
    x +. ((Random.float (j *. 2.)) -. j)

let rec output_astar_f_vs_subtree ochan f_max anode =
  let kids = List.filter (fun n -> n.f < f_max) anode.children in
  let size = (List.fold_left
		(fun s k -> s + (output_astar_f_vs_subtree ochan f_max k))
		1 kids)
  in
    Printf.fprintf ochan "(%f %f)" (jitter anode.f) (log10 (float size));
    size

let rec output_astar_depth_vs_subtree ?(depth=0) ochan f_max anode =
  let kids = List.filter (fun n -> n.f < f_max) anode.children in
  let size = (List.fold_left
		(fun s k -> s + (output_astar_depth_vs_subtree
				   ~depth:(depth + 1) ochan f_max k))
		1 kids)
  in
    Printf.fprintf ochan "(%d %f)"
      (truncate (jitter (float depth))) (log10 (float size));
    size


let compare_to_astar hash eq key reparent h expand f_max partial_root =
  if Verb.level Verb.toplvl
  then begin
    let astar_root =
      astar_tree hash eq key reparent  h expand f_max partial_root.state in
    let ochan = open_out "tree.spt" in
      Printf.fprintf ochan "(let* ((root ";
      output_both eq key ochan f_max partial_root astar_root;
      Printf.fprintf ochan
	")\n(plot0 (sunburst-tree-plot :outlined true :tree root)))\n";
      Printf.fprintf ochan "(display plot0))\n";
      close_out ochan;
      let ochan = open_out "astar_tree.spt" in
	Printf.fprintf ochan "(let* ((root ";
	output_astar ochan f_max astar_root;
	Printf.fprintf ochan
	  ")\n(plot0 (sunburst-tree-plot :outlined true :tree root)))\n";
	Printf.fprintf ochan "(display plot0))\n";
	close_out ochan;
	let ochan = open_out "astar_f_vs_subtree_size.spt" in
	  Printf.fprintf ochan "(let* ((pts (";
	  ignore (output_astar_f_vs_subtree ochan f_max astar_root);
	  Printf.fprintf ochan "))\n(scatter0 (scatter-dataset ";
	  Printf.fprintf ochan ":points pts))\n";
	  Printf.fprintf ochan "(plot0 (num-by-num-plot :x-label \"f\" ";
	  Printf.fprintf ochan ":y-label \"log10 subtree size\" ";
	  Printf.fprintf ochan ":dataset scatter0 )))\n";
	  Printf.fprintf ochan "(display plot0))\n";
	  close_out ochan;
	  let ochan = open_out "astar_depth_vs_subtree_size.spt" in
	    Printf.fprintf ochan "(let* ((pts (";
	    ignore (output_astar_depth_vs_subtree ochan f_max astar_root);
	    Printf.fprintf ochan "))\n(scatter0 (scatter-dataset ";
	    Printf.fprintf ochan ":points pts))\n";
	    Printf.fprintf ochan "(plot0 (num-by-num-plot :x-label \"depth\" ";
	    Printf.fprintf ochan ":y-label \"log10 subtree size\" ";
	    Printf.fprintf ochan ":dataset scatter0 )))\n";
	    Printf.fprintf ochan "(display plot0))\n";
	    close_out ochan;
  end


(** {1 Call search} ****************************************)


let make_partial_astar_dups anytime_fun size_fun sface argv =
  (** [make_partial_astar_dups anytime_fun size_fun sface argv] makes
      a function that estimates in a duplicates domain.

      [anytime_fun] determines how the anytime behavior of the
      algorithm is implemented, be it continuing or restarting,
      etc.  *)
  let p0 = Search_args.get_float "par_astar" argv 0
  and p_rate = Search_args.get_float "par_astar" argv 1
  and f_max = Search_args.get_float "par_astar" argv 2 in

  let expand = sface.Search_interface.domain_expand
  and key = sface.Search_interface.key
  and pr = sface.Search_interface.key_printer
  and hash = sface.Search_interface.hash
  and eq = sface.Search_interface.equals
  and h = sface.Search_interface.h
  and d = sface.Search_interface.h
  and reparent = sface.Search_interface.parent_update
  and is_goal = sface.Search_interface.goal_p
  and info = sface.Search_interface.info
  and root = sface.Search_interface.initial in
  let pn, nroot =
    anytime_fun info hash eq key h d reparent
      expand is_goal ~f_max ~p0 ~p_rate root
  in
  let size = size_fun f_max nroot in
  let nignored, nexpanded = count_ignored_expanded f_max nroot in
    compare_to_astar hash eq key reparent h expand f_max nroot;
    output_keys key pr f_max nroot;
    sanity_check hash eq key f_max nroot;
    Datafile.write_pairs stdout
      [ "unexpanded in partial tree", string_of_int nignored;
	"expanded in partial tree", string_of_int nexpanded;
	"maximum f", string_of_float f_max;
	"final estimation", string_of_float size;
	"final probability", string_of_float pn;
      ];
    Limit.unwrap_sol6
      (function
	 | Limit.Nothing -> None
	 | Limit.Incumbent (f, state) -> Some (state, f))
      (Limit.results6 info)



(** {1 Continued Partial A*} ****************************************)

let rec next_p ~opened ~delayed ~f_max ~p ~p_rate =
  (** [next_p ~opened ~delayed ~f_max ~p ~p_rate] moves to the next
      p-value. *)
  assert (Dpq.empty_p opened);
  let p' = p *. p_rate in
    while not (Dpq.empty_p delayed)
      && not (is_delayed p' (Dpq.peek_first delayed))
    do
      let n = Dpq.extract_first delayed in
	assert (n.pq_pos = Dpq.no_position);
	if not (out_of_bound f_max n) then Dpq.insert opened n
    done;
    if (Dpq.empty_p opened) && not (Dpq.empty_p delayed)
    then next_p ~opened ~delayed ~f_max ~p:p' ~p_rate
    else p'


let continuing_partial_tree
    ?(best_f=false) info hash eq key h d reparent expand is_goal ~f_max
    ~p0 ~p_rate root =
  (** [continuing_partial_tree ?best_f info hash eq key h d reparent
      expand is_goal ~f_max ~p0 ~p_rate root] builds a partial tree by
      continually extending a single partial tree. *)
  let p = ref p0 in
  let nroot = node ~h:(h root) ~g:0. ~p:1. None root in
  let opened = Dpq.create has_better_f update_pq_pos 100 nroot in
  let delayed = Dpq.create has_better_p update_pq_pos 100 nroot in
  let seen = Htable.create hash eq 100 in
    Htable.add seen (key root) nroot;
    Dpq.insert opened nroot;
    while not (Limit.halt_p info) && not (Dpq.empty_p opened) do
      extend_partial_tree ~best_f info key h reparent expand is_goal
	~f_max ~p:!p seen ~opened ~delayed;
      if not (Limit.halt_p info)
      then p := next_p ~opened ~delayed ~f_max ~p:!p ~p_rate;
    done;
    !p, nroot



let continuing_dups sface argv =
  (** [continuing_dups sface argv] estimates in a duplicates
      domain. *)
  let cont = continuing_partial_tree ~best_f:false in
    make_partial_astar_dups cont estimate_size sface argv


let continuing_with_f_dups sface argv =
  (** [continuing_with_f_dups sface argv] estimates in a duplicates
      domain. *)
  let cont = continuing_partial_tree ~best_f:false in
    make_partial_astar_dups cont estimate_size_with_f
      sface argv


let continuing_with_bestf_dups sface argv =
  (** [continuing_with_bestf_dups sface argv] estimates in a duplicates
      domain. *)
  let cont = continuing_partial_tree ~best_f:true in
    make_partial_astar_dups cont estimate_size_with_f
      sface argv


(** {1 Restarting Partial A*} ****************************************)


let restarting_partial_tree
    info hash eq key h d reparent expand is_goal ~f_max ~p0 ~p_rate root =
  (** [restarting_partial_tree info hash eq key h d reparent expand
      is_goal ~f_max ~p0 ~p_rate root] builds a series of partial
      trees with decreasing p values. *)
  let p = ref p0 in
  let nroot = ref (node ~h:(h root) ~g:0. ~p:1. None root) in
    while not (Limit.halt_p info) do
      let nroot' = node ~h:(h root) ~g:0. ~p:1. None root in
      let opened = Dpq.create has_better_f update_pq_pos 100 nroot' in
      let delayed = Dpq.create has_better_p update_pq_pos 100 nroot' in
      let seen = Htable.create hash eq 100 in
	Htable.add seen (key root) nroot';
	Dpq.insert opened nroot';
	extend_partial_tree info key h reparent expand is_goal
	  ~f_max ~p:!p seen ~opened ~delayed;
	p := !p *. p_rate;
	if Dpq.empty_p opened then nroot := nroot';
    done;
    !p, !nroot


let restarting_dups sface argv =
  (** [restarting_dups sface argv] estimates in a duplicates domain. *)
  make_partial_astar_dups restarting_partial_tree estimate_size sface argv


(** {1 Counting Subtree Sizes} ****************************************)


let rec output_relative_sizes ?(depth=0) f_max root =
  (** [output_relative_sizes f_max root] Outputs the size of sibling
      subtrees relative to the smallest sibling subtree. *)
  if out_of_bound f_max root
  then 0.
  else begin
    let child_sizes =
      List.map (output_relative_sizes ~depth:(depth+1) f_max) root.children in
    let sorted =
      List.sort compare (List.filter (fun s -> s > 0.) child_sizes)
    in
      begin match sorted with
	| [] -> ()
	| hd :: [] -> ()
	| hd :: tl ->
	    List.iter
	      (fun s ->
		 Datafile.write_alt_row_prefix stdout "subtree sizes";
		 Printf.printf "%d\t%f\n" depth (s /. hd))
	      tl;
      end;
      List.fold_left (+.) 1. child_sizes
  end


let dups_count_subtrees sface argv =
  (** [dups_count_subtrees sface argv] makes a search interface that
      counts the relative size of sibling subtrees. *)
  let f_max = Search_args.get_float "par_astar" argv 0 in

  let expand = sface.Search_interface.domain_expand
  and key = sface.Search_interface.key
  and pr = sface.Search_interface.key_printer
  and hash = sface.Search_interface.hash
  and eq = sface.Search_interface.equals
  and h = sface.Search_interface.h
  and d = sface.Search_interface.h
  and reparent = sface.Search_interface.parent_update
  and is_goal = sface.Search_interface.goal_p
  and info = sface.Search_interface.info
  and root = sface.Search_interface.initial in
  let _, nroot =
    continuing_partial_tree info hash eq key h d reparent
      expand is_goal ~f_max ~p0:0. ~p_rate:0. root
  in
  let size = estimate_size f_max nroot in
  let nignored, nexpanded = count_ignored_expanded f_max nroot in
    output_keys key pr f_max nroot;
    sanity_check hash eq key f_max nroot;
    Datafile.write_alt_colnames stdout "subtree sizes"
      ["subtree depth"; "subtree relative size"];
    ignore (output_relative_sizes f_max nroot);
    Datafile.write_pairs stdout
      [ "unexpanded in partial tree", string_of_int nignored;
	"expanded in partial tree", string_of_int nexpanded;
	"maximum f", string_of_float f_max;
	"final estimation", string_of_float size;
      ];
    Limit.unwrap_sol6
      (function
	 | Limit.Nothing -> None
	 | Limit.Incumbent (f, state) -> Some (state, f))
      (Limit.results6 info)
