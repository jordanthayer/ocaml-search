(** Weighted indecision search.  The weights are trained off-line.

    Learns/uses a polynomial model for the weight of the indecision
    values at each depth given the leaf cost:

    {% \mathit{cost} = c_0 + c_1 (i_0 (d_0/\mathit{dmax})^k + i_1
    (d_1/d_{\mathit{max}})^k + ...)  + c_2 (i_0
    (d_0/d_{\mathit{max}})^(k-1) + i_1 (d_1/d_{\mathit{max}})^(k-1) +
    ...)  + ... + c_k (i_0 + i_1 + ...) %},

    where cost is the leaf cost, c_x for 0 <= x <= k are the learned
    coefficients, i_x for x >= 0 are the indecisions at each depth
    d_x, dmax is the max depth and k is the degree of the polynomial
    being fit.

    @author eaburns
    @since 2010-12-14
*)

open Printf
open Float_ref
open Fn

(************************************************************)
(** {6 Traversal patterns} *)

type ('node, 'saved) traversal =
    ('node, 'saved) Info.info
    -> down:(depth:int -> 'node -> int -> unit)
      -> up:(depth:int -> 'node -> int -> unit)
  -> leaf:('node -> unit)
  -> 'node
  -> unit


let probe_traversal_str = "probe"

(** [probe_traversal info ~down ~up ~leaf root] traverses the tree
    with random probes. *)
let probe_traversal : ('n,'s) traversal =
  let rec probe info ~down ~up ~leaf depth root =
    if not (Info.halt_p info) then begin
      if Info.leaf_p info root then begin
	leaf root;
	true
      end else
	next_node info ~down ~up ~leaf depth root
    end else
      true
  and next_node info ~down ~up ~leaf depth root =
    let nkids = Info.num_children info root in
      if nkids > 0 then begin
	let kids = Array.init nkids identity in
	  go_to_leaf info ~down ~up ~leaf depth root kids
      end else
	false
  and go_to_leaf info ~down ~up ~leaf depth root kids =
    Wrarray.permute kids;
    let left = ref (Array.to_list kids) in
    let hit_leaf = ref false in
      while !left <> [] && not !hit_leaf do
	let i = List.hd !left in
	  left := List.tl !left;
	  down ~depth root i;
	  (* Order here is important.  In an in-place domain,
	     [get_child] changes the in-place state so we must make sure
	     to use [down] {e before} getting the child because [down]
	     may access information about the current state. *)
	  let root' = Info.get_child info root i in
	    hit_leaf := probe info ~down ~up ~leaf (depth + 1) root';
	    up ~depth root i;
      done;
      !hit_leaf
  in
    (fun info ~down ~up ~leaf root ->
       while not (Info.halt_p info) do
	 ignore (probe info ~down ~up ~leaf 0 root)
       done)


let df_traversal_str = "df"


(** [df_traversal info ~down ~up ~leaf root] traverses the tree with
    depth first search. *)
let df_traversal : ('n,'s) traversal =
  let rec dfs info ~down ~up ~leaf depth root =
    if not (Info.halt_p info) then begin
      if Info.leaf_p info root then
	leaf root
      else
	for i = 0 to (Info.num_children info root) - 1 do
	  down ~depth root i;
	  dfs info ~down ~up ~leaf (depth + 1) (Info.get_child info root i);
	  up ~depth root i;
	done
    end
  in
    (fun info ~down ~up ~leaf root -> dfs info ~down ~up ~leaf 0 root)


let discr_traversal_str = "discr"

(** [discr_traversal info ~down ~up ~leaf root] traverses the
    tree using as discrepancy search style of traversal. *)
let discr_traversal : int -> ('n,'s) traversal =
  let rec ilds info ~down ~up ~leaf ~max_depth ~k ~depth root =
    if not (Info.halt_p info) then begin
      if Info.leaf_p info root then
	leaf root
      else
	let depth' = depth + 1 in
	  for i = 0 to (Info.num_children info root) - 1 do
	    if (i = 0 && max_depth - depth > k) || (i > 0 && k > 0) then begin
	      down ~depth root i;
	      let kid = Info.get_child info root i in
	      let k' = if i = 0 then k else k + 1 in
		ilds info ~down ~up ~leaf ~max_depth ~depth:depth' ~k:k' kid;
		up ~depth root i;
	    end
	  done
    end
  in
  let rec do_ilds_probes info ~down ~up ~leaf ~max_depth ?(k=0) root =
    ilds info ~down ~up ~leaf ~max_depth ~k:(k+1) ~depth:0 root;
    if not (Info.halt_p info) then
      do_ilds_probes info ~down ~up ~leaf ~max_depth ~k:(k+1) root
  in
    (fun max_depth info ~down ~up ~leaf root ->
       do_ilds_probes info ~down ~up ~leaf ~max_depth root)


let get_traversal max_depth = function
  | s when s = probe_traversal_str -> probe_traversal
  | s when s = df_traversal_str -> df_traversal
  | s when s = discr_traversal_str -> discr_traversal max_depth
  | x -> invalid_arg ("Unknown traversal scheme: " ^ x)

(************************************************************)
(** {6 Offline training} *)

(** [make_update_indecision vec max_depth] makes functions that
    add/subtract indecision at a given depth to/from the model. *)
let make_update_indecision vec max_depth =
  (* The code duplication is terrible here (just change + to -) but I
     am not sure of another way to do it without effecting run-time
     performance. *)
  let k = (Array.length vec) - 1 in
  let max_depth = float max_depth in
  let add ~depth ~ind =
    let dnorm = float depth /. max_depth in
    let x = dnorm *. ind in
    let t = float_ref x in
      for i = 1 to k do
	vec.(i) <- vec.(i) +. !!t;
	t <-- !!t *. x;
      done
  and sub ~depth ~ind =
    let dnorm = float depth /. max_depth in
    let x = dnorm *. ind in
    let t = float_ref x in
      for i = 1 to k do
	vec.(i) <- vec.(i) -. !!t;
	t <-- !!t *. x;
      done
  in add, sub


(** [visit_leaves k nleaves info lc costs max_depth root] visits a
    bunch of leaves on the given instance.  [k] is the degree of the
    polynomial. *)
let visit_leaves k nleaves (info : ('n,'s) Info.info) lc costs
    (traverse : ('n,'s) traversal) max_depth root =
  let vec = Array.init (k + 1) (function 0 -> 1. | _ -> 0.) in
  let add, sub = make_update_indecision vec max_depth in
  let ind n i = match costs n with
    | [] -> failwith "No children"
    | (b :: _) as cs -> (List.nth cs i) -. b in
  let down ~depth n i = add depth (ind n i) in
  let up ~depth n i = sub depth (ind n i) in
  let consider, sample, _ = Sample.make_stream_sampler nleaves in
  let copy (c, v) = c, Array.copy v in
  let leaf node =
    let c = lc node in
      consider ~copy (c, vec)
  in
    traverse info ~down ~up ~leaf root;
    sample ()


(** [store_sample ?overwrite f s] appends the sample to the given
    file *)
let store_sample ?(overwrite=false) f s =
  let flags =
    if overwrite then
      [ Open_creat ]
    else
      [ Open_append; Open_creat ]
  in
  let o = open_out_gen flags 0o664 f in
  let output (c, vec) =
    fprintf o "%g %d" c (Array.length vec);
    Array.iter (fprintf o " %g") vec;
    fprintf o "\n"
  in
    try
      Array.iter output s;
      close_out o;
    with e ->
      close_out o;
      raise e


(** [load_sample f] loads the sampled leaves from the given file. *)
let load_sample f =
  let fl i =
    let t = Wrio.read_token i in
      if t = "" then raise End_of_file else float_of_string t
  in
  let read_leaf inch =
    let cost = fl inch in
    let n = Wrio.input_int inch in
    let vec = Array.init (n + 1) (function 0 -> 1. | _ -> fl inch) in
      cost, vec
  in
  let read_sample inch =
    let leaves = ref [] in
      try
	while true do leaves := (read_leaf inch) :: !leaves done;
	failwith "No End_of_file";
      with End_of_file ->
	let cs, vecs = List.split !leaves in
	  Array.of_list cs, Array.of_list vecs
  in
    Verb.pr Verb.debug "reading sample from [%s]\n" f;
    Wrio.with_infile f read_sample


(** [learn_coeffs f] learns the coefficient vector from the given
    set of samples. *)
let learn_coeffs f =
  let cs, vecs = load_sample f in
  let coeffs = Offline_lms.lms vecs cs in
  let n = Array.length coeffs in
    Array.sub coeffs 1 (n - 1)


(** [coeffs_of_string s] reads the coefficients from the given
    string. *)
let coeffs_of_string s =
  let strm = Wrstr.strm s in
  let n = int_of_string (Wrstr.next strm) in
    Array.init n (fun _ -> float_of_string (Wrstr.next strm))


(** [string_of_coeffs cs] creates a string of the coefficients. *)
let string_of_coeffs cs =
  let b = Buffer.create 20 in
  let n = Array.length cs in
    Buffer.add_string b (string_of_int n);
    Buffer.add_char b ' ';
    Array.iteri (fun i f ->
		   Buffer.add_string b (string_of_float f);
		   if i < n - 1 then Buffer.add_char b ' ';)
      cs;
    Buffer.contents b


(** [get_sample ?k ?nleaves ?halt ?prune ?is_opt sample_file
    traversal copy is_better is_leaf nkids kid_costs nth_kid
    leaf_cost max_depth root].  This is a pseudo-search algorithm
    that just generates a sample of leaves for the given problem
    along with a vector for learning weighted indecision. *)
let get_sample ?(k=2) ?(nleaves=512) ?(halt=[Info.Never]) ?(log=no_op2)
    ?(prune=constantly2 false) ?(is_opt=constantly1 false)
    ?(overwrite=false) sample_file traversal
    copy is_better is_leaf nkids kid_costs nth_kid leaf_cost max_depth root =
  let prev_best = None in
  let info =
    Info.make nkids nth_kid is_leaf is_better is_opt copy prune
      log prev_best halt
  in
  let traverse = get_traversal max_depth traversal in
  let sample =
    visit_leaves k nleaves info leaf_cost kid_costs traverse max_depth root
  in
    store_sample ~overwrite sample_file sample;
    (Info.curr_best info), (Info.stats info), false, false

(************************************************************)
(** {6 Searching} *)


(** [make_indecision_wt coeffs max_depth] makes a function that
    computes the weight of and indecision at a given depth. *)
let make_indecision_wt coeffs max_depth =
  let max_depth = float max_depth in
  let k = (Array.length coeffs) - 1 in
  let t = float_ref nan and vl = float_ref nan in
    (* don't bother allocating these references each time wt_d is called. *)
    (fun d ->
       let x = float d /. max_depth in
	 t <-- x;
	 vl <-- coeffs.(0);
	 for i = 1 to k do
	   vl <-- !!vl +. coeffs.(i) *. !!t;
	   t <-- !!t *. x;
	 done;
	 !!vl)


(** RBFS on weighted indecision sum.

    @param coeffs is a string contaiting the coefficients for the
    indecision weight polynomial.  The format for this string is:
    <num> <coeff>_1 <coeff>_2 ... <coeff>_<num> where <coeff>_k is the
    coefficient for the (d/dmax)^k term of the polynomial. *)
let rbfs ?halt ?log ?prev_best ?is_opt ?prune coeffs
    max_depth copy is_better is_leaf nkids kid_costs nth_kid root =
  let coeffs = coeffs_of_string coeffs in
  let indecision_wt = make_indecision_wt coeffs max_depth in
    Indecision_rbfs.rbfs ~norm:false ?halt ?log ?prev_best ?is_opt ?prune
      ~indecision_wt copy is_better is_leaf nkids kid_costs nth_kid root


(** IDA* on weighted indecision sum preferring bottom indecisions

    @param coeffs is a string contaiting the coefficients for the
    indecision weight polynomial.  The format for this string is:
    <num> <coeff>_1 <coeff>_2 ... <coeff>_<num> where <coeff>_k is the
    coefficient for the (d/dmax)^k term of the polynomial. *)
let sum_bottom ?halt ?log ?prev_best ?is_opt ?prune ?bins coeffs
    max_depth max_children kid_costs copy is_better is_leaf nkids
    nth_kid root =
  let coeffs = coeffs_of_string coeffs in
  let indecision_wt = make_indecision_wt coeffs max_depth in
    Indecision.indecision_search ~norm:false ?halt ?log ?prev_best
      ?optimal_p:is_opt ?max_bins:bins ~bottom_first:true ?prune_p:prune
      ~indecision_wt max_depth max_children kid_costs copy is_better
      is_leaf nkids nth_kid root


(** IDA* on weighted indecision sum preferring top indecisions

    @param coeffs is a string contaiting the coefficients for the
    indecision weight polynomial.  The format for this string is:
    <num> <coeff>_1 <coeff>_2 ... <coeff>_<num> where <coeff>_k is the
    coefficient for the (d/dmax)^k term of the polynomial. *)
let sum_top ?halt ?log ?prev_best ?is_opt ?prune ?bins coeffs
    max_depth max_children kid_costs copy is_better is_leaf nkids
    nth_kid root =
  let coeffs = coeffs_of_string coeffs in
  let indecision_wt = make_indecision_wt coeffs max_depth in
    Indecision.indecision_search ~norm:false ?halt ?log ?prev_best
      ?optimal_p:is_opt ?max_bins:bins ~bottom_first:false ?prune_p:prune
      ~indecision_wt max_depth max_children kid_costs copy is_better
      is_leaf nkids nth_kid root
