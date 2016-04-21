(* Stored version of a run of an algorithm *)
Random.self_init()

(* Final data structures *)
type 'a node = {
  key : 'a;
  parent : 'a;
  truth : float list;
  admiss : float list;
  g : float;
  depth : float;
  cost: float;
  children : 'a list
}

type 'a t = {
  sequence : 'a array;
  run: ('a, 'a node) Hashtbl.t;
  sol_cost : float;
  solution : 'a list;
  structs : ('a Structure.t) list;
  key_print: 'a -> string;
  max_g : float;
  max_depth : float;
  max_h : float;
  max_d : float;
}


let structs_to_val run step fn =
  let fn k = fn (Hashtbl.find run.run k)
  and structs_at = List.map (fun s -> Structure.get_entry s step) run.structs in
    List.map (fun s -> Array.map fn s.Structure.keys) structs_at


let has_node t key =
  Hashtbl.mem t.run key

let get_node t key =
  try
    Hashtbl.find t.run key
  with Not_found -> failwith (Wrutils.str "%s not found" (t.key_print key))

let sorted run get_val =
  let structs = Hashtbl.fold
    (fun key ele accum -> ele.Structure.keys::accum)
    (List.hd run.structs).Structure.structure [] in
    List.map
      (fun ar ->
	 let ab = Array.copy ar in
	   Array.sort (fun a b ->
			 let nodea = get_node run a
			 and nodeb = get_node run b in
			   compare (get_val nodea)(get_val nodeb)) ab;
	   ab) structs

let hmin_dist on_hstar other =
  List.map2 (fun seq_h seq_hstar ->
	       Wrarray.index seq_h seq_hstar.(0)) other on_hstar

let get_path run node =
  (* returns a set of nodes associated with the partial solution of node *)
  let rec recur n =
    if n.parent = n.key
    then [n.key]
    else n.key :: (recur (Hashtbl.find run n.parent)) in
    List.rev (recur node)


let get_paths run nodes =
  List.map (get_path run) nodes


let path_accum base_val accum_fn fn path =
  let rec helper accum = function
      | [] -> accum
      | [_] -> accum
      | parent::child::tl ->
	  accum_fn (fn parent child) (helper accum tl) in
    helper base_val path



let find_fringe t =
  (* Returns a list of all nodes in the frindge of the search space at
     time of solving (exactly those nodes without children *)
  let rec get_frindge node accum =
    if node.children = []
    then node::accum
    else (List.fold_left (fun ac ele ->
			    if has_node t ele.key
			    then (get_frindge ele ac)
			    else ac) accum
	    (List.fold_left
	       (fun ac ele ->
		  if has_node t ele
		  then (get_node t ele)::ac
		  else ac) []
	       node.children)) in

  let root = get_node t t.sequence.(0) in
    get_frindge root []


let all_paths t = get_paths t.run (find_fringe t)


let iter fn t =
  Hashtbl.iter (fun key n -> fn n) t.run

(* Fetches data from the run *)
let get_parent_child run key =
  if not (Hashtbl.mem run.run key)
  then None
  else (let parent = Hashtbl.find run.run key in
	  Some (parent,
		(List.fold_left (fun accum c ->
				   try (Hashtbl.find run.run c)::accum
				   with _ -> accum) []  parent.children)))

let get_struct_during run step =
  (fun n ->
     Structure.get_entry (List.nth run.structs n) step)

(* calculates the f value *)
let get_g exp =
  exp.g

let get_depth exp =
  exp.depth

let get_h exp =
  try
    List.nth exp.admiss 0
  with _ -> -.1.

let get_ht exp =
  try
    List.nth exp.truth 0
  with _ -> -.1.

let get_d exp =
  try
    List.nth exp.admiss 1
  with _ -> -.1.

let get_dt exp =
  try
    List.nth exp.truth 1
  with _ -> -.1.

let get_rh exp =
  try
    List.nth exp.admiss 2
  with _ -> -.1.

let get_rd exp =
  try
    List.nth exp.admiss 3
  with _ -> -.1.

let calc_f exp =
  let ret = exp.g +. (get_h exp) in
    assert (ret <> infinity);
    ret

let calc_gf de he exp =
  exp.g +. (get_h exp) +. ((de *. (get_d exp)) *. he)

let get_cost exp =
  exp.cost


let calc_fp w exp =
  exp.g +. w *. (get_h exp)

let calc_ft exp =
  let ret = exp.g +. (get_ht exp) in
    assert (ret <> infinity);
    ret


let get_hp w exp =
  w *. (get_h exp)


let single_step_f (parent,child) =
  (calc_f child) -. (calc_f parent)


let single_step_d (parent,child) =
  (get_d child) -. (get_d parent) +. 1.


(* Grabs the best child (based on f) from a list of children *)
let select_best children =
  assert (children <> []);
  List.fold_left (fun cb x ->
		    if (get_h cb) < (get_h x)
		    then cb
		    else x) (List.hd children) children


(* Takes a random child of a node *)
let select_random children =
  match children with
      [c] -> c
    | _ ->
	let ind = Random.int ((List.length children) - 1) in
	  List.nth children ind


(* Gets all possible pairings of nodes and children for a run *)
let get_all_pairs run =
  let c_gpc = get_parent_child run in
    Array.fold_left
      (fun accum x ->
	 match c_gpc x with
	     None -> accum
	   | Some (parent,children) ->
	       if (List.length children) > 0
	       then accum @ (List.map (fun y -> parent,y) children)
	       else accum)
      [] run.sequence


(* Gets best pairings (based on f) of nodes and children for a run *)
let get_best_pairs run =
  let c_gpc = get_parent_child run in
    Array.fold_left
      (fun accum x ->
	 match c_gpc x with
	     None -> accum
	   | Some (parent,children) ->
	       if (List.length children) > 0
	       then (parent, (select_best children))::accum
	       else accum) [] run.sequence


(* Gets all pairings of nodes and a random child *)
let get_random_pairs run =
  Random.self_init();
  let c_gpc = get_parent_child run in
    Array.fold_left
      (fun accum x ->
	 match c_gpc x with
	     None -> accum
	   | Some (parent,children) ->
	       if (List.length children) > 0
	       then (parent, (select_random children))::accum
	       else accum) [] run.sequence


(* Gets all pairings of nodes and a random child *)
let get_solution_pairs run =
  let rec gp accum lst =
    match lst with
      | [] -> accum
      | [last] -> accum
      | p::c::tl -> gp ((p,c)::accum) (c::tl) in
    List.map (fun (a,b) ->
		Hashtbl.find run.run a,
		Hashtbl.find run.run b)	(gp [] run.solution)


let get_sequence_pairs run =
  List.fold_left
    (fun accum ele -> match ele with None -> accum | Some e -> e::accum) []
    (Array.to_list
       (Array.init  ((Array.length run.sequence))
	  (fun i ->
	     let k = run.sequence.(i) in
	       match get_parent_child run k with
		 | Some (n,c) -> (if c = [] then None
				  else Some (n, select_best c))
		 | None -> failwith "?!")))


(* Subsamples a set of pairs in order to reduce its size *)
let rec subsample_pairs p pairs =
  match pairs with
      [] -> []
    | hd::tl -> (if (Random.float) > p
		 then subsample_pairs p tl
		 else hd :: (subsample_pairs p tl))


(* Build matrix builds a set of feature matrix.
   One for parent, one for child
   get_features must return an array
*)
let build_matrix get_features pairs =
  let pairs = Array.of_list pairs
  and mag = List.length pairs in
    (Array.init mag (fun index -> get_features pairs.(index)))


(* Builds a single matrix, so that the features applied
   to the vector should always equal zero
   get_conv must return an array
*)
let rec t_append l1 l2 =
  match l1 with
      [] -> l2
    | h::tl -> t_append tl (h::l2)



let build_conv_matrix get_conv p =
  let to_ret = Array.create (List.length p) [||] in
    ignore (List.fold_left (fun i x ->
			      to_ret.(i) <- (get_conv x);
			      i+1) 0 p);
    to_ret


(* Returns a length x 3 matrix where length is the length of
   the solution.
   each row is then f , f^, f*
o*)
let do_solution run fhat =
  let ar = Array.of_list run.solution in
    Array.init (List.length run.solution)
      (fun key ->
	 let exp = Hashtbl.find run.run ar.(key) in
	   [| exp.g +. (get_h exp); (fhat exp); run.sol_cost|])


(* Rattle off some relevant statistics about the solution array *)
let process_solution_array ar =
  let cci = ref 0
  and wci = ref 0
  and negative_correction = ref 0 in
    Array.iteri
      (fun i ae ->
	 let df_i = ar.(i).(2) -. ar.(i).(0)
	 and dfh_i = ar.(i).(2) -. ar.(i).(1)
	 and cfh_i = ar.(!wci).(2) -. ar.(!wci).(1) in
	   if dfh_i > df_i
	   then negative_correction := !negative_correction + 1;
	   if (abs_float dfh_i) < (abs_float cfh_i)
	   then cci := i;
	   if (abs_float dfh_i) > (abs_float cfh_i)
	   then wci := i) ar;
    Verb.pe Verb.toplvl "Negative Correction: %d\n%!" !negative_correction;
    Verb.pe Verb.toplvl "Worst Correction:    %f\n%!"
      (ar.(!wci).(2) -. ar.(!wci).(1));
    Verb.pe Verb.toplvl "Best Correction:     %f\n%!"
      (ar.(!cci).(2) -. ar.(!cci).(1))


let average_pair_values fn pairs =
  let nelms = List.length pairs in
    (List.fold_left (fun accum ele ->
		       accum +. (fn ele)) 0. pairs) /. (float nelms)


let get_optimal_nodes run =
  (** fetches all nodes on an optimal path *)
  Hashtbl.fold (fun key ele accum ->
		  if (calc_ft ele) = run.sol_cost then ele::accum else accum)
    run.run []

(* EOF *)
