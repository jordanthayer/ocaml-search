(** A simple approach to estimating the size of a search tree based on
    counting the number of duplicate states and counting the number of
    nodes in the brute-force search tree.

    @author eaburns
    @since 2010-06-17
*)

open Num

let zero_num = num_of_int 0
let one_num = num_of_int 1

(** {1 Kilby-based} ****************************************)
(* Kilby-based duplicate counting uses a tree estimator that is
   similar to Kilby's weighted backtracking estimator (but fixed up to
   work on trees with non-uniform branching). *)

let rec depth_first
    info path see_branch see_leaf key h expand f_max cost depth node =
  (** [depth_first info path see_branch see_leaf key h expand f_max
      cost depth node] performs a depth first search beneath a node.
      Each time a leaf or pruned branch is encountered the [see_leaf]
      function is called.  Each time a branching point is encountered
      the see_branch function is called.  [path] is a Htable used for
      cycle detection. *)
  if not (Limit.halt_p info)
  then begin
    if Htable.mem path (key node) || (h node) +. cost >= f_max
    then see_leaf depth
    else begin
      Htable.add path (key node) true;
      let children = expand node cost in
	Limit.incr_exp info;
	see_branch depth node children;
	List.iter (fun (c, g) ->
		     Limit.incr_gen info;
		     depth_first info path see_branch see_leaf key h
		       expand f_max g (depth + 1) c;)
	  children;
	Htable.remove path (key node);
    end
  end


let track_mean_branching () =
  (** [track_mean_branching ()] make a pair of functions for computing
      and retrieving the mean branching factor at each depth. *)
  let count = Garray.make zero_num and branching = Garray.make zero_num in
  let see_branch depth children =
    let nchildren = num_of_int (List.length children) in
    let n = Garray.nth count depth
    and b = Garray.nth branching depth in
    let n' = n +/ one_num in
      Garray.set branching depth (b +/ (nchildren -/ b) // n');
      Garray.set count depth n';
  and mean_branching depth =
    Garray.nth branching depth
  in see_branch, mean_branching


let track_leaf_depths () =
  (** [track_leaf_depths ()] makes a pair of functions for building
      and iterating over a multi-set of depths.  [f] is a function
      from depth and number of times encounting a leaf at the given
      depth to unit. *)
  let depths = Garray.make 0 in
  let see_leaf depth =
    let n = Garray.get depths depth in
      Garray.set depths depth (n + 1)
  and iter_depths f =
    for d = 0 to (Garray.get_fill depths) - 1 do
      let n = Garray.get depths d in
	if n > 0 then f ~d ~n
    done
  and max_depth () = Garray.get_fill depths
  in see_leaf, iter_depths, max_depth


let duplicate_estimator hash key eq =
  (** [duplicate_estimator hash key eq] makes a pair of functions for
      estimating the number of duplicate states and accessing the
      estimation. *)
  let states = Htable.create hash eq 100 in
  let expansions = ref 0 in
  let see_state state =
    let k = key state in
      incr expansions;
      try
	let count = Htable.find states k in
	  Htable.replace states k (count + 1)
      with Not_found -> Htable.add states k 1
  in
  let estimate_dups () =
    let exps = float !expansions in
      (float (Htable.length states)) /. exps
  in see_state, estimate_dups


let estimate_tree_size max_depth iter_depths mean_branching =
  (** [estimate_tree_size max_depth iter_depths mean_branching]
      estimates the size of a search tree. *)
  let numerator = ref zero_num and denominator = ref zero_num in
  let size = Array.create (max_depth + 1) one_num in
  let prob = Array.create (max_depth + 1) one_num in
    for d = 1 to max_depth do
      if (mean_branching d) >/ zero_num then begin
	size.(d) <- size.(d - 1) */ (mean_branching d);
	prob.(d) <- prob.(d - 1) // (mean_branching d);
      end;
      if d > 1 then size.(d - 1) <- size.(d - 1) +/ size.(d - 2);
    done;
    iter_depths (fun ~d ~n ->
		   let nf = num_of_int n in
		     (*
		       if prob.(d) then begin
		     *)
		     numerator := !numerator +/ nf */ prob.(d) */ size.(d);
		     denominator := !denominator +/ (nf */ prob.(d));
		     (* end;*));
    if !denominator >/ zero_num
    then float_of_num (!numerator // !denominator)
    else infinity


let estimate info hash eq key expand h f_max root =
  (** [estimate info hash eq key expand h f_max root] estimates the
      number of states in a search space by estimating the size of the
      tree expansion and the fraction of duplicate nodes. *)
    let see_state, estimate_dups = duplicate_estimator hash key eq in
    let see_leaf, iter_depths, max_depth = track_leaf_depths () in
    let see_branch, mean_branching = track_mean_branching () in
    let see_branch' depth node children =
      see_branch depth children;
      see_state node;
    in
    let path = Htable.create hash eq 100 in
      depth_first info path see_branch' see_leaf key h expand f_max 0. 0 root;
      (estimate_tree_size (max_depth ()) iter_depths mean_branching,
       estimate_dups ())


let kilby_dups sface argv =
  (** [kilby_dups sface argv] makes a function that estimates in a
      duplicates domain using a version of Kilby's weighted
      backtracking estimator that is adapted for non-uniform branching
      trees. *)
  let f_max = Search_args.get_float "kilby_dups" argv 0 in
  let expand = sface.Search_interface.domain_expand
  and h = sface.Search_interface.h
  and hash = sface.Search_interface.hash
  and key = sface.Search_interface.key
  and eq = sface.Search_interface.equals
  and info = sface.Search_interface.info
  and root = sface.Search_interface.initial in
  let nnodes, dup_frac = estimate info hash eq key expand h f_max root in
    Datafile.write_pairs stdout
      [
	"maximum f", string_of_float f_max;
	"tree size estimation", string_of_float nnodes;
	"duplicate fraction", string_of_float dup_frac;
	"final estimation", string_of_float (dup_frac *. nnodes);
      ];
    Limit.unwrap_sol6
      (function
	 | Limit.Nothing -> None
	 | Limit.Incumbent (f, state) -> Some (state, f))
      (Limit.results6 info)


(** {1 Knuth-based} ****************************************)
(* Knuth-based counting uses Knuth's tree estimator until the time
   limit runs out.  Each probe is added to a single tree of all probes
   (the "probe tree").  Once the time limit has been reached the
   number of nodes in the probe tree are counted and so are the number
   of unique states.  These counts form the ratio used to decrease the
   mean tree-estimation of all of the probes.  *)


type ('state, 'key) node = {
  (* A node in the probe tree. *)

  state : 'state;
  key : 'key list;
  children : ('key list, ('state, 'key) node) Htable.t;
}


let find_or_create nodes h hash eq key path child_state =
  (** [find_or_create nodes h hash eq key path child_state] either
      finds the pre-existing child node in the probe tree or it creates a
      new child node. *)
  let child_key = (key child_state) :: path in
    try Htable.find nodes child_key
    with Not_found ->
      let child_node = { state = child_state;
			 key = child_key;
			 children = Htable.create hash eq 5;
		       }
      in
	Htable.add nodes child_key child_node;
	child_node


let add_child_once nodes child_node =
  (** [add_child_once nodes child_node] if [child_node] is not in the
      [nodes] hash table then add it. *)
  let key = child_node.key in
    if not (Htable.mem nodes key)
    then Htable.add nodes key child_node


let rec probe info nodes expand h hash eq key f_max ?(br=one_num)
    ?(cost=0.) path root =
  (** [probe info nodes expand h hash eq key f_max ?br ?cost path
      root] performs a single probe. *)
  if Limit.halt_p info
  then zero_num
  else begin
    let children = expand root.state cost in
    let nchildren = List.length children in
      Limit.incr_exp info;
      Limit.incr_gen_n info nchildren;
      if nchildren > 0
      then begin
	let choosen = Random.int nchildren in
	let br' = (num_of_int nchildren) */ br in
	let child_state, child_cost = List.nth children choosen in
	  if child_cost +. (h child_state) > f_max
	  then zero_num
	  else begin
	    let child_node =
	      find_or_create nodes h hash eq key path child_state
	    in
	      add_child_once root.children child_node;
	      let ch =
		probe info nodes expand h hash eq key f_max ~br:br'
		  ~cost:child_cost ((key root.state) :: path) child_node
	      in ch +/ br
	  end
      end else br
  end


let build_probe_tree info expand h key f_max root =
  (** [build_probe_tree info expand h key f_max root] builds a probe
      tree until the time limit has been reached. *)
  let node_hash = Hashtbl.hash_param 1000 1000 in
  let node_eq = (=) in
  let root_key = [key root] in
  let root_node = { state = root;
		    key = root_key;
		    children = Htable.create node_hash node_eq 5;
		  }
  and nodes = Htable.create node_hash node_eq 100
  and avg = ref zero_num and count = ref zero_num in
    Htable.add nodes root_key root_node;
    while not (Limit.halt_p info) do
      let est =
	probe info nodes expand h node_hash node_eq key f_max [] root_node
      in
	if not (Limit.halt_p info) then begin
	  let count' = !count +/ one_num in
	    avg := !avg +/ ((est -/ !avg) // count');
	    count := count';
	end
    done;
    let avg' = if !count >/ zero_num then !avg else one_num in
      root_node, avg', !count


let count_dups hash eq key root_node =
  (** [count_dups hash eq key root_node] counts the number of nodes
      and states in the probe tree, returning the ratio. *)
  let rec do_count seen node =
    let key = key node.state in
    let seen_before = Htable.mem seen key in
      if not seen_before then Htable.add seen key node;
      let unique = if seen_before then zero_num else one_num in
	Htable.fold (fun _ child (nodes, states) ->
		       let n, s = do_count seen child in
			 nodes +/ n, states +/ s)
	  node.children (one_num, unique)
  in
  let seen = Htable.create hash eq 100 in
    Htable.add seen (key root_node.state) root_node;
    let nodes, states = do_count seen root_node in
      states // nodes


let knuth_dups sface argv =
  let f_max = Search_args.get_float "knuth_counting" argv 0 in
  let expand = sface.Search_interface.domain_expand
  and h = sface.Search_interface.h
  and info = sface.Search_interface.info
  and key = sface.Search_interface.key
  and eq = sface.Search_interface.equals
  and hash = sface.Search_interface.hash
  and root = sface.Search_interface.initial in
  let root_node, tree_size, nsamples =
    build_probe_tree info expand h key f_max root in
  let ratio = count_dups hash eq key root_node in
  let graph_size = tree_size */ ratio in
    Datafile.write_pairs stdout
      [ "num samples", string_of_float (float_of_num nsamples);
	"maximum f", string_of_float f_max;
	"tree size estimation", string_of_float (float_of_num tree_size);
	"duplicate fraction", string_of_float (float_of_num ratio);
	"final estimation", string_of_float (float_of_num graph_size);
      ];
    Limit.unwrap_sol6
      (function
	 | Limit.Nothing -> None
	 | Limit.Incumbent (f, state) -> Some (state, f))
      (Limit.results6 info)
