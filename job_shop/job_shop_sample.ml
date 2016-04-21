(** Random probing into the search tree.

    @author eaburns
    @since 2010-03-01
*)

let compute_indecisions kid_costs node =
  (** [compute_indecisions kid_costs node] computes the indecision
      values for each child. *)
  let costs = kid_costs node in
  let best_cost = List.hd costs in
  let indecision = Array.of_list costs in
    Array.iteri (fun i vl -> indecision.(i) <- vl -. best_cost) indecision;
    indecision


let rec probe num_kids nth_kid kid_costs is_leaf leaf_cost edge d root =
  (** [probe num_kids nth_kid kid_costs is_leaf leaf_cost edge d root]
      a random probe in the tree. *)
  if is_leaf root
  then Some (leaf_cost root)
  else
    let nchildren = num_kids root in
      if nchildren = 0
      then
	assert false
	  (*
	    None				(* dead end. *)
	  *)
      else begin
	let indecision = compute_indecisions kid_costs root in
	let r = Random.int nchildren in
	let c = nth_kid root r in
	  edge ~depth:d ~rank:r indecision.(r);
	  probe num_kids nth_kid kid_costs is_leaf leaf_cost edge (d + 1) c;
      end

let rec dfs
    num_kids nth_kid kid_costs is_leaf leaf_cost see_leaf edge unedge d root =
  (** [dfs num_kids nth_kid kid_costs is_leaf leaf_cost see_leaf edge
      unedge d root] do a full enumeration of the given search space
      and save each leaf that is encountered for offline training. *)
  if is_leaf root
  then see_leaf (leaf_cost root)
  else begin
    let nchildren = num_kids root in
    let indecision =
      if nchildren > 0
      then compute_indecisions kid_costs root
      else [| |]
    in
      for r = 0 to nchildren - 1 do
	let child = nth_kid root r in
	  edge ~depth:d ~rank:r indecision.(r);
	  dfs num_kids nth_kid kid_costs is_leaf
	    leaf_cost see_leaf edge unedge (d + 1) child;
	  unedge ~depth:d ~rank:r indecision.(r);
      done
  end


let quad_model max_depth =
  (** [quad_model max_depth] gets a set of functions for learning a
      quadratic model. *)
  let num = ref 0 in
  let examples = ref [] in
  let cur = Array.create 9 0. in
  let max_depthf = float max_depth in
  let reset () =
    Array.iteri (fun i _ -> cur.(i) <- 0.) cur;
    cur.(0) <- cur.(0) +. 1.0;
  and see_edge  ~depth ~rank indecision =
    let pdepth = (float depth) /. max_depthf in
    let pdepth2 = pdepth *. pdepth in
    let right = if rank <> 0 then 1. else 0. in
    let left = if rank = 0 then 1. else 0. in
      cur.(0) <- cur.(0) +. 1.0 *. left;
      cur.(1) <- cur.(1) +. pdepth *. left;
      cur.(2) <- cur.(2) +. pdepth2 *. left;
      cur.(3) <- cur.(3) +. 1.0 *. right;
      cur.(4) <- cur.(4) +. pdepth *. right;
      cur.(5) <- cur.(5) +. pdepth2 *. right;
      cur.(6) <- cur.(6) +. indecision;
      cur.(7) <- cur.(8) +. pdepth *. indecision;
      cur.(8) <- cur.(8) +. pdepth2 *. indecision
  and see_unedge  ~depth ~rank indecision =
    let pdepth = (float depth) /. max_depthf in
    let pdepth2 = pdepth *. pdepth in
    let right = if rank <> 0 then 1. else 0. in
    let left = if rank = 0 then 1. else 0. in
      cur.(0) <- cur.(0) -. 1.0 *. left;
      cur.(1) <- cur.(1) -. pdepth *. left;
      cur.(2) <- cur.(2) -. pdepth2 *. left;
      cur.(3) <- cur.(3) -. 1.0 *. right;
      cur.(4) <- cur.(4) -. pdepth *. right;
      cur.(5) <- cur.(5) -. pdepth2 *. right;
      cur.(6) <- cur.(6) -. indecision;
      cur.(7) <- cur.(8) -. pdepth *. indecision;
      cur.(8) <- cur.(8) -. pdepth2 *. indecision
  and see_leaf cost =
    let vec = Array.copy cur in
(*
      Array.iter (Printf.printf "%f ") vec;
      Printf.printf "\n";
*)
      incr num;
      if Math.divisible !num 1_000
      then Printf.printf "%d\n%!" !num;
      examples := (vec, cost) :: !examples
  and get_coeffs () =
    let feat, tar = List.split !examples in
      Offline_lms.lms (Array.of_list feat) (Array.of_list tar)
  in reset, see_edge, see_unedge, see_leaf, get_coeffs


let spinner () =
  let spinner = [| '|'; '/'; '-'; '\\'; |] in
  let i = ref ~-1 in
  let next () =
    i := (!i + 1) mod (Array.length spinner);
    spinner.(!i)
  in next

let learn_model t num =
  let spin_next = spinner () in
  let var_order = Job_shop.min_bslack2_pairs 3. 4. in
(*
  let nth_child = Job_shop.make_nth_child var_order t in
*)
  let nth_child = Job_shop.make_nth_child_with_greedy_pruning var_order t in
    (*
      let num_children = Job_shop.num_children
    *)
  let num_children = Job_shop.make_num_children nth_child
  and child_costs = Job_shop.make_log_child_costs t
  and is_leaf = Job_shop.is_leaf
  and leaf_cost = Job_shop.make_leaf_cost t
  and root = Job_shop.initial_node var_order t in
  let max_depth = List.length root.Job_shop.pairs in
  let reset, see_edge, see_unedge, see_leaf, get_coeffs =
    quad_model max_depth in
  let probe =
    probe
      num_children
      (*
	nth_child
      *)
      Job_shop.get_nth_child
      child_costs is_leaf leaf_cost see_edge
  in
  let rec do_probes ?(retry=false) left =
    if left = 0
    then get_coeffs ()
    else begin
      if not retry then Verb.pr Verb.toplvl "Probe %d\n%!" (num - left);
      reset ();
      match probe 0 root with
	| None ->
	    Verb.pr Verb.toplvl "\r\tfailed %c%!" (spin_next());
	    do_probes ~retry:true left
	| Some cost ->
	    Verb.pr Verb.toplvl "\r\tsucceeded, cost=%f\n%!" cost;
	    see_leaf cost;
	    do_probes (left - 1)
    end
  in
    let model = do_probes num in
      Array.iter (Printf.printf "%f\n") model

let enumerate_leaves t =
  let var_order = Job_shop.min_bslack2_pairs 3. 4. in
    (*
      let nth_child = Job_shop.make_nth_child var_order t in
    *)
  let nth_child = Job_shop.make_nth_child_with_greedy_pruning var_order t in
    (*
      let num_children = Job_shop.num_children
    *)
  let num_children = Job_shop.make_num_children nth_child
  and child_costs = Job_shop.make_log_child_costs t
  and is_leaf = Job_shop.is_leaf
  and leaf_cost = Job_shop.make_leaf_cost t
  and root = Job_shop.initial_node var_order t in
  let max_depth = List.length root.Job_shop.pairs in
  let _, see_edge, see_unedge, see_leaf, get_coeffs = quad_model max_depth in
    dfs num_children nth_child child_costs is_leaf leaf_cost
      see_leaf see_edge see_unedge 0 root;
    let model = get_coeffs () in
      Array.iter (Printf.printf "%f\n") model

