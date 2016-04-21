(** Ruml, Silvia, Thayer Trees *)

(* Assumes fixed branching factor *)

type data = {
  cost  : float;
  depth : int;
  key : int;
}


type scale =
  | Uniform
  | Linear_Increase
  | Linear_Decrease


type rst_tree = {
  scale : scale;
  max_edge_cost : float;
  max_depth : int;
  branch : int;
  t : data Random_tree.tree;
}


(************************* Printing functions ********************************)

let data_to_string d =
  Wrutils.str "depth: %i\t cost: %f\t key: %i\n" d.depth d.cost d.key

let print_tree t = Random_tree.print_tree data_to_string t.t

let print_key k = Printf.eprintf "%i" k

(*****************************************************************************)

let get_path branch =
  (** Calculates the path from the root to the leaf node *)
  let rec gp key = if key = 0 then [0]
  else key::(gp ((key - 1) / branch)) in
    gp

let path_to_string p =
  let rec fn str p =
    match p with
	[] -> str
      | hd::tl -> fn (str ^ (Wrutils.str ", %i" hd)) tl in
    fn "" p

let calculate_key b parent offset =
  (** Calculates the rank of a current node as defined by the order in which it
      would be touched in a breadth first traversal. *)
(* $ b \cdot [k_p \cdot \frac{1-b^(d-1)}{1 - b) + \frac{1-b^d}{1-b} + offset$*)
  b * (parent.key - ((1 - (Math.int_exp b (parent.depth))) / ( 1 - b))) +
    ((1 - (Math.int_exp b (parent.depth + 1))) / (1 - b)) + offset


let size_of_tree t =
  let v = (1 - (Math.int_exp t.branch (t.max_depth + 1))) / (1 - t.branch) in
    Verb.pe Verb.debug "tree size: %i\n" v;
    v


let uniform _ = (fun _ -> 1.)


let make_scaling slope mdepth =
  assert (slope >= (-.mdepth /. 2.));
  assert (slope <= (mdepth /. 2.));
  let b = 1. -. (2. /. mdepth) *. slope in
    (fun node -> (float_of_int node) *. slope +. b)


let make_scaling_fslope slope mdepth =
  assert (slope >= -.1.);
  assert (slope <= 1.);
  make_scaling (slope *. mdepth /. 2.) mdepth


let scale_to_fun s =
  match s with
    | Uniform -> uniform
    | Linear_Increase -> (make_scaling_fslope 1.)
    | Linear_Decrease -> (make_scaling_fslope (-.1.))


let scale_to_string s =
  match s with
    | Uniform -> "uniform"
    | Linear_Increase -> "linear_increasing"
    | Linear_Decrease -> "linear_decreasing"


let string_to_scale str =
  if str = (scale_to_string Uniform)
  then Uniform
  else (if str = (scale_to_string Linear_Increase)
	then Linear_Increase
	else (if str = (scale_to_string Linear_Decrease)
	      then Linear_Decrease
	      else failwith "str doesn't matcha model!"))


let make_expand ?(dfun = uniform) max_depth max_edge_cost
    branch =
  (** Geneartes the expand function used to *)
  let scale = dfun (float_of_int max_depth) in
  (fun node -> (* Random_tree.wrap_expand does the seed init for us! *)
     let data = node.Random_tree.data in
       if max_depth > data.depth
       then (let nd = data.depth + 1 in
	     let next i = {depth = nd;
			    cost = (data.cost +.
				      (scale nd) *.
				      (Random.float max_edge_cost));
			    key = calculate_key branch data i; } in
	       Array.to_list (Array.init branch next))
       else [])


let make_goal t =
  (** generates a goal predicate for the given pearl tree *)
  (fun node -> if node.Random_tree.data.depth == t.max_depth
   then (Verb.pe Verb.debug "goal @ %i\n" node.Random_tree.data.key;
	 true)
   else false)


let make_tree scale max_edge_cost branch depth seed =
  (** generates a new pearl tree from a probability, maximum depth, and a seed.
      seed comes last so that we can generate many similar trees from a
      list or an array of seeds *)
  {scale = scale;
   max_edge_cost = max_edge_cost;
   max_depth = depth;
   branch = branch;
   t =  (Random_tree.make_tree seed {depth = 0; cost = 0.; key = 0}
	   (make_expand ~dfun:(scale_to_fun scale)
	      depth max_edge_cost branch));}


let wrap_random_tree_expand exp =
  (** Takes the random tree expand function and manipulates it into something
      that the searches are expecting *)
  (fun n _ -> let children = exp n in
     List.map (fun c -> c,c.Random_tree.data.cost) children)



(******************************** IO ****************************************)

let config_to_pairs t =
  (** Converts an rst tree into a set of datafile pairs *)
  [("model", "rst");
   ("scale", scale_to_string t.scale);
   ("branching factor", string_of_int t.branch);
   ("tree depth", string_of_int t.max_depth);
   ("maximum edge cost", string_of_float t.max_edge_cost);
   ("seed", string_of_int t.t.Random_tree.root.Random_tree.seed);]


let write_pairs t ch =
  (** Writes an rst tree into the channel as a datfile *)
  Datafile.write_header_pairs ch;
  Datafile.write_pairs ch (config_to_pairs t);
  Datafile.write_trailer_pairs ch


let read_instance file =
  (** Reads a datafile repersenting an rst tree into memory *)
  let df = Datafile.load file in
  let mec = float_of_string (Datafile.get_val df "maximum edge cost")
  and dep = int_of_string (Datafile.get_val df "tree depth")
  and b = int_of_string (Datafile.get_val df "branching factor")
  and seed = int_of_string (Datafile.get_val df "seed")
  and scale = string_to_scale (Datafile.get_val df "scale")  in
    make_tree scale mec b dep seed


let read_instance_ch file_name ch =
  (** Reads a datafile repersenting an rst tree into memory *)
  let df = Datafile.read file_name ch in
  let mec = float_of_string (Datafile.get_val df "maximum edge cost")
  and dep = int_of_string (Datafile.get_val df "tree depth")
  and b = int_of_string (Datafile.get_val df "branching factor")
  and seed = int_of_string (Datafile.get_val df "seed")
  and scale = string_to_scale (Datafile.get_val df "scale")  in
    make_tree scale mec b dep seed


let read_heuristic_values df =
  let h_vals = Datafile.get_col df "cost to go" in
    (fun n -> h_vals.(n.Random_tree.data.key))


let make_true_heuristic cache ch t =
  (** Traverses the tree [t], calculating the true heuristic value of the nodes
      in said tree. Requires the use of a post order traversal*)
  let h_vals = Array.create (size_of_tree t) 0. in
    Verb.pe Verb.debug "Allocating array of length %i\n" (Array.length h_vals);
    let calc_true_h node =
      let v =
	if node.Random_tree.data.depth == t.max_depth
	then 0. (* Leaves have heuristics value = 0 *)
	else (let children = t.t.Random_tree.expand node in
	      let heuristics = List.map
		(fun c -> (* cost of getting to child + true h of child *)
		   Verb.pe Verb.debug "Getting h of %i\n" c.Random_tree.data.key;
		   (c.Random_tree.data.cost -.
		      node.Random_tree.data.cost) +.
		     (h_vals.(c.Random_tree.data.key))) children
	      in
		List.fold_left  (* get minimum value and pass back *)
		  (fun accum cur -> min accum cur) infinity heuristics) in
	Verb.pe Verb.debug "Setting h of %i\n" node.Random_tree.data.key;
	h_vals.(node.Random_tree.data.key) <- v
    in
      if cache
      then (Random_tree.postorder_traversal calc_true_h t.t;
	    Datafile.write_header_pairs ch;
	    Datafile.write_colnames ch ["cost to go"];
	    Array.iter (fun v -> Printf.fprintf ch "%f\n" v) h_vals;
	    Datafile.write_pairs ch (config_to_pairs t);
	    Datafile.write_trailer_pairs ch;
	    flush ch);
      (fun n -> h_vals.(n.Random_tree.data.key))


(***************************** Heuristics *********************************)

let get_true_heuristic cache data_root t =
  let h_pairs = ("type", "heuristic")::(config_to_pairs t) in
  let paths = Rdb.matching_paths data_root h_pairs in
    match paths with
      | [path] -> read_heuristic_values (Datafile.load path)
      | _ -> (if cache
	      then (Verb.pe Verb.toplvl "Heuristic Not Cached, Recording\n";
		    Wrio.with_outfile (Rdb.path_for data_root h_pairs)
		      (fun ch -> make_true_heuristic cache ch t))
	      else make_true_heuristic cache stderr t)


let malte_roeger_h c cache data_root t =
  let truth = get_true_heuristic cache data_root t in
    (fun node -> Math.fmax 0. ((truth node) -. c))


let constant_percent_scale p cache data_root t =
  assert (p <= 1.);
  let truth = get_true_heuristic cache data_root t in
    (fun node -> (truth node) *. p)


let random_percent_scale max_p cache data_root t =
  assert (max_p <= 1.);
  let truth = get_true_heuristic cache data_root t in
    (fun node ->
       Random.set_state !(Math.random_state_from node.Random_tree.seed);
       (truth node) *. (Random.float max_p))


let distance t =
  (fun n -> float_of_int (t.max_depth - n.Random_tree.data.depth))


let truth_scaling_error_decreasing _ cache data_root t =
  let truth = get_true_heuristic cache data_root t in
    (fun node ->
       ((float_of_int node.Random_tree.data.depth) /.
	  (float_of_int t.max_depth)) *. (truth node))


let truth_scaling_error_increasing _ cache data_root t =
  let truth = get_true_heuristic cache data_root t in
    (fun node ->
       (1. -. ((float_of_int node.Random_tree.data.depth) /.
		 (float_of_int t.max_depth))) *. (truth node))


let constant_scaling_error_decreasing max_p_error cache data_root t =
  assert (max_p_error <= 1.);
  assert (max_p_error >= 0.);
  let truth = get_true_heuristic cache data_root t in
    (fun node ->
       ((float_of_int node.Random_tree.data.depth) /.
	  (float_of_int t.max_depth)) *. (truth node) *. max_p_error)


let constant_scaling_error_increasing max_p_error cache data_root t =
  assert (max_p_error <= 1.);
  assert (max_p_error >= 0.);
  let truth = get_true_heuristic cache data_root t in
    (fun node ->
       (1. -. ((float_of_int node.Random_tree.data.depth) /.
		 (float_of_int t.max_depth))) *.
	 (truth node) *. max_p_error)


let random_scaling_error_decreasing max_p_error cache data_root t =
  assert (max_p_error <= 1.);
  assert (max_p_error >= 0.);
  let truth = get_true_heuristic cache data_root t in
    (fun node ->
       Random.set_state !(Math.random_state_from node.Random_tree.seed);
       ((float_of_int node.Random_tree.data.depth) /.
	  (float_of_int t.max_depth)) *.
	 (truth node) *. (Random.float max_p_error))


let random_scaling_error_increasing max_p_error cache data_root t =
  assert (max_p_error <= 1.);
  assert (max_p_error >= 0.);
  let truth = get_true_heuristic cache data_root t in
    (fun node ->
       Random.set_state !(Math.random_state_from node.Random_tree.seed);
       (1. -. ((float_of_int node.Random_tree.data.depth) /.
		 (float_of_int t.max_depth))) *.
	 (truth node) *. (Random.float max_p_error))


let string_to_heuristic ?(opt_arg = 0.) str =
  match str with
    | "truth" -> get_true_heuristic
    | "helmert" -> malte_roeger_h opt_arg
    | "constant_percent" -> constant_percent_scale opt_arg
    | "random_percent" -> random_percent_scale opt_arg
    | "truth_increase" -> truth_scaling_error_increasing opt_arg
    | "truth_decrease" -> truth_scaling_error_decreasing opt_arg
    | "constant_increase" -> constant_scaling_error_increasing opt_arg
    | "constant_decrease" -> constant_scaling_error_decreasing opt_arg
    | "random_increase" -> random_scaling_error_increasing opt_arg
    | "random_decrease" -> random_scaling_error_decreasing opt_arg
    | _ -> failwith (Wrutils.str "%s not recognized!" str)


(***************************** Interfaces *********************************)

let alt_col_name = "solution_id"
let output_header () = Datafile.write_alt_colnames stdout alt_col_name
  ["id";"quality";]

let make_interface ?(cache = false) ?(h = get_true_heuristic)
    data_root t limit =
  output_header();
  let hfun = (h cache data_root t)
  and dfun = (distance t) in
  Search_interface.make
    ~h:hfun
    ~d:dfun
    ~hd:(fun n -> hfun n, dfun n)
    ~rev_hd:(fun n -> n.Random_tree.data.cost, float_of_int n.Random_tree.data.depth)
    ~domain_expand:(wrap_random_tree_expand t.t.Random_tree.expand)
    ~key:(fun n -> n.Random_tree.data.key)
    ~key_print:string_of_int
    ~goal_p:(make_goal t)
    ~get_sol_length:(fun _ -> -1)
    ~halt_on:limit
    ~equals:(=)
    Search_interface.Synthetic
    t.t.Random_tree.root
    (fun _ _ -> false)
    (fun sol_info ->
       match sol_info.Limit.incumbent with
	   Limit.Nothing -> ()
	 | Limit.Incumbent (q,node) ->
	     Datafile.write_alt_row_prefix stdout alt_col_name;
	     Verb.pr Verb.always "%i\t%f\n" node.Random_tree.data.key q)


(* eof *)
