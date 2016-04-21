(** Second iteration of the rst tree, very similar in form to the
    synthetic trees described in "The LAMA Planner: Using Landmark Counting
    in Heuristic Search" *)


type data = {
  mutable p_acc : float;
  g  : float;
  remaining : float;
  depth : int;
  key : int;
}


type t = {
  branch : int;
  root : data Random_tree.tree;
}


(************************* Printing functions ********************************)
let data_to_string d =
  Wrutils.str "depth: %i\t cost: %f\t key: %i\n" d.depth d.g d.key

let print_tree t = Random_tree.print_tree data_to_string t.root

let print_key k = Printf.eprintf "%i" k

(*****************************************************************************)

let uniform _ = (fun _ -> 1.)


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


let make_goal t =
  (** generates a goal predicate for the given pearl tree *)
  (fun node -> if node.Random_tree.data.remaining = 0.
   then (Verb.pe Verb.debug "goal @ %i\n" node.Random_tree.data.key;
	 true)
   else false)


let make_expand_dep ?(fs = 0.05) branch =
  let epsilon = 1.
  and p_progress = 1. /. (float_of_int branch) in
  let make_node data nd max_edge i =
    if Park_miller.next() > p_progress
    then {p_acc = data.p_acc;
	  g = data.g +. (Math.fmax epsilon (Park_miller.next() *. max_edge));
	  remaining = data.remaining;
	  depth = nd;
	  key = calculate_key branch data (i+1);}
    else (let arc = Math.fmin data.remaining
	    (Math.fmax epsilon (Park_miller.next() *. max_edge)) in
	    {p_acc = data.p_acc;
	     g = data.g +. arc;
	     remaining = data.remaining -. arc;
	     depth = nd;
	     key = calculate_key branch data (i+1);})
  in
    (fun node ->
       let data = node.Random_tree.data in
	 Verb.pe Verb.debug "%f\n" data.remaining;
	 if data.remaining <= 0.
	 then []
	 else
	   (let nd = data.depth + 1
	    and max_edge_cost = Math.fmax epsilon (fs *. data.remaining) in
	      Array.to_list (Array.init branch
			      (make_node data nd max_edge_cost))))

let make_expand ?(fs = 0.05) branch =
  let epsilon = 1.
  and p_progress = 1. /. (float_of_int branch) in
  let make_node data nd max_edge i =
    if Park_miller.next() > p_progress
    then {p_acc = data.p_acc;
	  g = data.g +. 1.;
	  remaining = data.remaining;
	  depth = nd;
	  key = calculate_key branch data (i+1);}
    else ({p_acc = data.p_acc;
	   g = data.g +. 1.;
	   remaining = data.remaining -.
	      (floor (Math.fmin data.remaining
			(Math.fmax epsilon
			   (Park_miller.next() *. max_edge))));
	   depth = nd;
	   key = calculate_key branch data (i+1);})
  in
    (fun node ->
       let data = node.Random_tree.data in
	 Verb.pe Verb.debug "%f\n" data.remaining;
	 if data.remaining <= 0.
	 then []
	 else
	   (let nd = data.depth + 1
	    and max_edge_cost = Math.fmax epsilon (fs *. data.remaining) in
	      Array.to_list (Array.init branch
			      (make_node data nd max_edge_cost))))


let make_tree branch opt_cost seed =
  (** generates a new pearl tree from a probability, maximum depth, and a seed.
      seed comes last so that we can generate many similar trees from a
      list or an array of seeds *)
  Park_miller.init seed;
  {branch = branch;
   root =  (Random_tree.make_tree seed
	      {g = 0.;
	       p_acc = Park_miller.next();
	       remaining = opt_cost; depth = 0; key = 0}
	      (make_expand branch));}


let wrap_random_tree_expand exp =
  (** Takes the random tree expand function and manipulates it into something
      that the searches are expecting *)
  (fun n _ -> let children = exp n in
     List.map (fun c -> c, c.Random_tree.data.g) children)


(******************************** IO ****************************************)

let config_to_pairs t =
  (** Converts an rst tree into a set of datafile pairs *)
  [("model", "silvia");
   ("branching factor", string_of_int t.branch);
   ("optimal", string_of_float t.root.Random_tree.root.Random_tree.data.remaining);
   ("seed", string_of_int t.root.Random_tree.root.Random_tree.seed);]


let write_pairs t ch =
  (** Writes an rst tree into the channel as a datfile *)
  Datafile.write_header_pairs ch;
  Datafile.write_pairs ch (config_to_pairs t);
  Datafile.write_trailer_pairs ch


let read_instance file =
  (** Reads a datafile repersenting an rst tree into memory *)
  let df = Datafile.load file in
  let opt = float_of_string (Datafile.get_val df "optimal")
  and b = int_of_string (Datafile.get_val df "branching factor")
  and seed = int_of_string (Datafile.get_val df "seed")  in
    make_tree b opt seed


let read_instance_ch file_name ch =
  (** Reads a datafile repersenting an rst tree into memory *)
  let df = Datafile.read file_name ch in
  let opt = float_of_string (Datafile.get_val df "optimal")
  and b = int_of_string (Datafile.get_val df "branching factor")
  and seed = int_of_string (Datafile.get_val df "seed")  in
    make_tree b opt seed


(***************************** Heuristics *********************************)

let truth n = n.Random_tree.data.remaining


let distance t =
  (fun n -> n.Random_tree.data.remaining)



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



let string_to_heuristic ?(opt_arg = 0.) str =
  match str with
    | "truth" -> get_true_heuristic
    | "random_percent" -> random_percent_scale opt_arg
    | "random_median" -> random_median_scale opt_arg
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
      ~key_print:string_of_int
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
	       Verb.pr Verb.always "%i\t%f\n" node.Random_tree.data.key q)

(* EOF *)
