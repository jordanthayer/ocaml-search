(** Second iteration of the rst tree, very similar in form to the
    synthetic trees described in "The LAMA Planner: Using Landmark Counting
    in Heuristic Search" *)


type data = {
  mutable p_acc : float;
  g  : float;
  remaining : float;
  depth : int;
  key : float;
}


type scale =
  | Uniform
  | Linear_Increase
  | Linear_Decrease


type stu_tree = {
  scale : scale;
  max_edge_cost : float;
  branch : int;
  decisive : float;
  root : data Random_tree.tree;
}


(************************* Printing functions ********************************)
let data_to_string d =
  Wrutils.str "depth: %i\t cost: %f\t key: %f\n" d.depth d.g d.key

let print_tree t = Random_tree.print_tree data_to_string t.root

let print_key k = Printf.eprintf "%f" k

(*****************************************************************************)

let uniform _ = (fun _ -> 1.)


let make_scaling ?(epsilon = 0.1) slope mdepth =
  assert (slope >= (-.mdepth /. 2.));
  assert (slope <= (mdepth /. 2.));
  let b = 1. -. (2. /. mdepth) *. slope in
  let max = 2. in
    (* scaling must return at least epsilon (no zero cost edges)
       and no more than 2 *)
    (fun node ->
       Math.fmin (Math.fmax epsilon ((float_of_int node) *. slope +. b))
	 max)


let make_scaling_fslope slope mdepth =
  assert (slope >= -.1.);
  assert (slope <= 1.);
  make_scaling (slope *. mdepth /. 2.) mdepth

let approximate_min_depth opt max_edge_cost =
  (* assumes edge costs are drawn uniformly at random from interval
     0 .. max_edge cost *)
    opt /. (max_edge_cost /. 2.)


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


(*
let calculate_key b parent offset =
  (** Calculates the rank of a current node as defined by the order in which it
      would be touched in a breadth first traversal. *)
  (* $ b \cdot [k_p \cdot \frac{1-b^(d-1)}{1 - b) + \frac{1-b^d}{1-b} + offset$*)
  let b = Big_int.big_int_of_int b
  and depth = Big_int.big_int_of_int parent.depth
  and offset = Big_int.big_int_of_int offset
  and one = Big_int.unit_big_int in
  let one_minus_b = Big_int.sub_big_int one b
  and b_to_d = Big_int.power_big_int_positive_big_int b depth in
    Big_int.add_big_int
      offset
      (Big_int.add_big_int
	 (Big_int.mult_big_int
	    b
	    (Big_int.sub_big_int
	       parent.key
	       (Big_int.div_big_int
		  (Big_int.sub_big_int
		     one
		     b_to_d)
		  one_minus_b)))
	 (Big_int.div_big_int
	    (Big_int.sub_big_int
	       one
	       (Big_int.mult_big_int b_to_d b))
	    one_minus_b))
      (*
      b * (parent.key -
	     ((1 -
		 (Math.int_exp b (parent.depth))) / ( 1 - b))) +
      ((1 - (Math.int_exp b (parent.depth + 1))) / (1 - b)) + offset*)
*)

let calculate_key b parent offset =
  (** Calculates the rank of a current node as defined by the order in which it
      would be touched in a breadth first traversal. *)
  (* $ b \cdot [k_p \cdot \frac{1-b^(d-1)}{1 - b)] + \frac{1-b^d}{1-b} + offset$*)
  let b = float_of_int b
  and offset = float_of_int offset
  and depth = float_of_int parent.depth in
  let one_minus_b = 1. -. b
  and b_to_d =  b ** depth in
    b *. (parent.key -. ((1. -.	b_to_d) /. one_minus_b)) +.
      ((1. -. (b *. b_to_d)) /. one_minus_b) +. offset


let make_goal t =
  (** generates a goal predicate for the given pearl tree *)
  (fun node -> node.Random_tree.data.remaining = 0.)


let make_expand ?(dfun = uniform) max_depth max_edge_cost branch decisive =
  (** Geneartes the expand function used to *)
  assert (decisive >= 0.);
  let scale = dfun max_depth in
  (fun node -> (* Random_tree.wrap_expand does the seed init for us! *)
     let data = node.Random_tree.data in
       if  data.remaining > 0.
       then (let nd = data.depth + 1 in
	     let scaler = (scale nd) in
	     let best_arc = Math.fmin data.remaining
	       (scaler *. (Park_miller.next() *. max_edge_cost)) in
	     let best_remaining = data.remaining -. best_arc in
	       assert (best_arc > 0.);
	       Verb.pe Verb.debug "best_arc %f\t" best_arc;
	       Verb.pe Verb.debug "remaining %f\n" data.remaining;
	     let next i =
	       let edge_to_node_i =
		 Math.fmax best_arc
		   (scaler *.
		      (Park_miller.next() *. max_edge_cost)) in
		 { p_acc = data.p_acc;
		   g = data.g +. edge_to_node_i;
		   remaining  = best_remaining *.
		     (1. +. Park_miller.next() *. decisive);
		   depth = nd;
		   key = calculate_key branch data (i+1); } in
	    {g = data.g +. best_arc;
	     p_acc = data.p_acc;
	     remaining = best_remaining;
	     depth = nd;
	     key = calculate_key branch data 0;}
	       ::(Array.to_list (Array.init (branch - 1) next)))
       else [])


let make_tree scale max_edge_cost branch opt_cost seed decisive =
  (** generates a new pearl tree from a probability, maximum depth, and a seed.
      seed comes last so that we can generate many similar trees from a
      list or an array of seeds *)
  Park_miller.init seed;
  {scale = scale;
   max_edge_cost = max_edge_cost;
   branch = branch;
   decisive = decisive;
   root =  (Random_tree.make_tree seed
	      {g = 0.;
	       p_acc = Park_miller.next();
	       remaining = opt_cost; depth = 0; key = 0.}
	      (make_expand ~dfun:(scale_to_fun scale)
		 (approximate_min_depth opt_cost max_edge_cost)
		 max_edge_cost branch decisive));}


let wrap_random_tree_expand exp =
  (** Takes the random tree expand function and manipulates it into something
      that the searches are expecting *)
  (fun n _ -> let children = exp n in
     List.map (fun c -> c, c.Random_tree.data.g) children)


(******************************** IO ****************************************)

let config_to_pairs t =
  (** Converts an rst tree into a set of datafile pairs *)
  [("model", "stu");
   ("scale", scale_to_string t.scale);
   ("branching factor", string_of_int t.branch);
   ("optimal", string_of_float t.root.Random_tree.root.Random_tree.data.remaining);
   ("maximum edge cost", string_of_float t.max_edge_cost);
   ("decisive", string_of_float t.decisive);
   ("seed", string_of_int t.root.Random_tree.root.Random_tree.seed);]


let write_pairs t ch =
  (** Writes an rst tree into the channel as a datfile *)
  Datafile.write_header_pairs ch;
  Datafile.write_pairs ch (config_to_pairs t);
  Datafile.write_trailer_pairs ch


let read_instance file =
  (** Reads a datafile repersenting an rst tree into memory *)
  let df = Datafile.load file in
  let mec = float_of_string (Datafile.get_val df "maximum edge cost")
  and opt = float_of_string (Datafile.get_val df "optimal")
  and b = int_of_string (Datafile.get_val df "branching factor")
  and seed = int_of_string (Datafile.get_val df "seed")
  and dec = float_of_string (Datafile.get_val df "decisive")
  and scale = string_to_scale (Datafile.get_val df "scale")  in
    make_tree scale mec b opt seed dec


let read_instance_ch file_name ch =
  (** Reads a datafile repersenting an rst tree into memory *)
  let df = Datafile.read file_name ch in
  let mec = float_of_string (Datafile.get_val df "maximum edge cost")
  and opt = float_of_string (Datafile.get_val df "optimal")
  and b = int_of_string (Datafile.get_val df "branching factor")
  and seed = int_of_string (Datafile.get_val df "seed")
  and dec = float_of_string (Datafile.get_val df "decisive")
  and scale = string_to_scale (Datafile.get_val df "scale")  in
    make_tree scale mec b opt seed dec


(***************************** Heuristics *********************************)

let truth n = n.Random_tree.data.remaining


let distance t =
  (* Admissible distance function *)
  (fun n -> n.Random_tree.data.remaining /. t.max_edge_cost)

let distance_inadmissible t =
  (* inadmissible, but more reasonable *)
  let avg_edge = t.max_edge_cost /. 2. in
  (fun n -> n.Random_tree.data.remaining /. avg_edge)

let int_distance t =
  let dfun = distance t in
    (fun n -> floor (dfun n))

let int_distance_inadmissible t =
  let dfun = distance_inadmissible t in
    (fun n -> ceil (dfun n))


let get_true_heuristic cache data_root t = truth

let malte_roeger_h c cache data_root t =
  (fun node -> Math.fmax 0. ((truth node) -. c))


let constant_percent_scale p cache data_root t =
  assert (p <= 1.);
  (fun node -> (truth node) *. p)


let random_percent_scale max_p cache data_root t =
  assert (max_p <= 1.);
  (fun node ->
     let data = node.Random_tree.data in
       Park_miller.init node.Random_tree.seed;
       data.p_acc <- Park_miller.next();
       let truth = truth node in
	 (truth -. (truth *.(data.p_acc *. max_p))))


let random_median_scale max_p cache data_root t =
  assert (max_p <= 1.);
  (fun node ->
     let data = node.Random_tree.data in
       Park_miller.init node.Random_tree.seed;
       data.p_acc <- (data.p_acc +. Park_miller.next()) /. 2.;
       let truth = truth node in
	 (truth -. (truth *.(data.p_acc *. max_p))))


let scaled_error max_p cache data_root t =
  assert (max_p <= 1.);
  (fun node ->
     let data = node.Random_tree.data in
     let p_err = (max_p *.
		    (1. -. (data.g /. (data.remaining +. data.g)))) in
       Verb.pe Verb.debug "%f\n" p_err;
       Park_miller.init node.Random_tree.seed;
       data.p_acc <- Park_miller.next();
       let truth = truth node in
	 (truth -. (truth *.(data.p_acc *. p_err))))


let string_to_heuristic ?(opt_arg = 0.) str =
  match str with
    | "truth" -> get_true_heuristic
    | "helmert" -> malte_roeger_h opt_arg
    | "constant_percent" -> constant_percent_scale opt_arg
    | "random_percent" -> random_percent_scale opt_arg
    | "random_median" -> random_median_scale opt_arg
    | "scaled_error" -> scaled_error opt_arg
(*  | "truth_increase" -> truth_scaling_error_increasing opt_arg
    | "truth_decrease" -> truth_scaling_error_decreasing opt_arg
    | "constant_increase" -> constant_scaling_error_increasing opt_arg
    | "constant_decrease" -> constant_scaling_error_decreasing opt_arg
    | "random_increase" -> random_scaling_error_increasing opt_arg
    | "random_decrease" -> random_scaling_error_decreasing opt_arg*)
    | _ -> failwith (Wrutils.str "%s not recognized!" str)


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
      ~domain_expand:(wrap_random_tree_expand t.root.Random_tree.expand)
      ~key:(fun n -> n.Random_tree.data.key)
      ~key_print:string_of_float
      ~goal_p:(make_goal t)
      ~get_sol_length:(fun _ -> -1)
      ~halt_on:limit
      ~equals:(==)
      Search_interface.Synthetic
      t.root.Random_tree.root
      (fun _ _ -> false)
      (fun sol_info ->
	 match sol_info.Limit.incumbent with
	     Limit.Nothing -> ()
	   | Limit.Incumbent (q,node) ->
	       Datafile.write_alt_row_prefix stdout alt_col_name;
	       Verb.pr Verb.always "%f\t%f\n"node.Random_tree.data.key q)


let make_hdhat_interface ?(cache = false) ?(h = get_true_heuristic)
    ?(hd = get_true_heuristic) data_root t limit =
  output_header();
  let hfun = (h cache data_root t)
  and dfun = (distance t)
  and hdfun = (hd cache data_root t) in
    Search_interface.make
      ~h:hfun
      ~d:dfun
      ~hd:(fun n -> hdfun n, dfun n)
      ~domain_expand:(wrap_random_tree_expand t.root.Random_tree.expand)
      ~key:(fun n -> n.Random_tree.data.key)
      ~key_print:string_of_float
      ~goal_p:(make_goal t)
      ~get_sol_length:(fun _ -> -1)
      ~halt_on:limit
      ~equals:(=)
      Search_interface.Synthetic
      t.root.Random_tree.root
      (fun _ _ -> false)
      (fun sol_info ->
	 match sol_info.Limit.incumbent with
	     Limit.Nothing -> ()
	   | Limit.Incumbent (q,node) ->
	       Datafile.write_alt_row_prefix stdout alt_col_name;
	       Verb.pr Verb.always "%f\t%f\n"node.Random_tree.data.key q)

(*
let online_model_interface ?(cache = false) ?(h = get_true_heuristic)
    data_root t limit =
  let get_d n = int_of_float (distance t n) in
  let get_t n = 0
  and type_max = 0 in
  let d_max = 110 in
  let dd_range = ~-10, 10 in
  let bound_model = Incremental_model.make type_max d_max dd_range in
  let iterative_model =
    Idastar_learned_model.make
      (Incremental_model.reset_model bound_model)
      (Incremental_model.update_model bound_model)
      (fun root_h root_t children nodes ->
	 (* might need to clamp this if we are weighted. *)
	 (Incremental_model.find_bound bound_model [root_h] children nodes))
      (Incremental_model.count_nodes bound_model)
      get_t get_d 2.
  in
    { (make_interface ~cache:cache ~h:h data_root t limit) with
	Search_interface.iterative_model = iterative_model }
*)


(****** Testing trees ******)
let test branch cost seed decisive =
  let t = make_tree Uniform 1. branch cost seed decisive in
    print_tree t (* this will never end because trees will be infinite. *)


let test_2 branch cost seed decisive =
  let t = make_tree Uniform 1. branch cost seed decisive in
    t,make_interface "" t [Limit.Never]


(* EOF *)
