(** A greenhouse permutation problem.

    From: "The Scanalyzer Domain: Greenhous Logistics as a Planning
    Problem," Malte Helmert, ICAPS 2010.

    @author eaburns
    @since 2011-02-25
*)

open Printf
open Fn
open Scanalyzer_inst

(** An array of non-zero integers.  Negative values represent batches
    that have already been scanned. *)
type state = int array

(************************************************************)
(** {1 Cycling batches} *)

(** Rotates the elements in [ary] between index [start] and [finish]
    to the left by 1.  This means that the element at [start] is
    "shifted off" and the elements from [start+1] to [finish] are
    moved to 1 index lower.  Then the element at that was shifted off
    is placed in the index [finish]. *)
let rotate_left ary start finish =
  let start_entry = ary.(start) in
    for i = start + 1 to finish do ary.(i - 1) <- ary.(i) done;
    ary.(finish) <- start_entry

(** Rotates the elements in [ary] between index [start] and [finish]
    to the right by 1.  This means that the element at [finish] is
    "shifted off" and the elements from [start+1] to [finish] are
    moved to 1 index higher.  Then the element at that was shifted off
    is placed in the index [start]. *)
let rotate_right ary start finish =
  let finish_entry = ary.(finish) in
    for i = finish - 1 downto start do ary.(i + 1) <- ary.(i) done;
    ary.(start) <- finish_entry

(** Performs a single cycle of two subsets of the array.  This
    effectively swaps a batch from the conveyer belt specified by
    indices [s0]-[f0] and the belt specified by [s1]-[f1].  [s0]-[f0]
    is rotated left and [s1]-[f1] is rotated right. *)
let cycle_once ary s0 f0 s1 f1 =
  rotate_left ary s0 f0;
  rotate_right ary s1 f1;
  let zero_batch = ary.(f0) and one_batch = ary.(s1) in
    ary.(f0) <- one_batch;
    ary.(s1) <- zero_batch


(************************************************************)
(** {1 Expanding} *)

(** Applies the given operator [ntimes] to the given state.  The
    result is a brand new state and the cost of the action.

    @param mult_cost If true, the input [cost] is multiplied by
    [ntimes] for the resulting operation cost, else it is just passed
    on through.
*)
let apply_operator scan mult_cost state ntimes (s0, f0, s1, f1, cost) =
  let state' = Array.copy state in
  let cost' = if mult_cost then cost *. float ntimes else cost in
    for i = 1 to ntimes do cycle_once state' s0 f0 s1 f1 done;
    if scan then begin
      let scanned = state'.(f1) in
	if scanned > 0 then state'.(f1) <- ~- scanned;
    end;
    state', cost'

(** Apply an operator in reverse. *)
let apply_operator_rev scan mult_cost state ntimes (s0, f0, s1, f1, cost) =
  let state' = Array.copy state in
  let cost' = if mult_cost then cost *. float ntimes else cost in
    for i = 1 to ntimes do cycle_once state' s1 f1 s0 f0 done;
    if scan then begin
      let scanned = state'.(f0) in
	if scanned < 0 then state'.(f0) <- ~- scanned;
    end;
    state', cost'

(** Builds an expand function in either the forward or reverse
    direction.  The derection depends on the [apply] function. *)
let make_expand_aux apply inst mult_costs (cycles, scans) =
  let ints = Wrutils.map_for [] 1 inst.batches_per_belt 1 identity in
  let for_op g scan state ntimes accum op =
    let s', c = apply scan mult_costs state ntimes op in
      (s', c +. g) :: accum in
  let for_ntimes g state accum ntimes =
    let accum' = List.fold_left (for_op g true state ntimes) accum scans in
      List.fold_left (for_op g false state ntimes) accum' cycles
  in
    (fun state (g : float) -> List.fold_left (for_ntimes g state) [] ints)

(** Builds an expand function for [inst] using [ops] as the primitive
    operators.

    @param mult_cost If true, then the cost of each operation is
    multiplied by the number of times that it is applied in a single
    generation, otherewise it is just charged once.  For example, if
    you have 2 batches per conveyer, you can cycle either 1 batch or
    2.  If [mult_cost] is true then cycling 2 batches costs twice as
    much as cycling 1, otherwise they both cost the same. *)
let make_expand inst mult_costs ops =
  make_expand_aux apply_operator inst mult_costs ops

(** Reverse expand. *)
let make_expand_rev inst mult_costs ops =
  make_expand_aux apply_operator_rev inst mult_costs ops

(************************************************************)
(** {1 Heuristic} *)

let int_array_equal (a : int array) b =
  Wrarray.for_alli (fun i a -> b.(i) = a) a

(** An int array hash table. *)
module H = Hashtbl.Make(struct
			  type t = int array
			  let equal a b = int_array_equal a b
			  let hash = Hashtbl.hash
			end)

(** Format an int array to the given formatter. *)
let format_int_array fmt ints =
  for i = 0 to Array.length ints - 1 do
    if i = Array.length ints - 1 then
      Format.fprintf fmt "%d" ints.(i)
    else
      Format.fprintf fmt "%d@ " ints.(i)
  done

(** Get a string from an array of ints.  We don't want to use
    [format_int_array] here because this requires that there are no
    newlines. *)
let string_of_ints ints =
  let b = Buffer.create 100 in
    Buffer.add_char b '<';
    for i = 0 to Array.length ints - 1 do
      let str =
	if i = Array.length ints - 1 then
	  sprintf "%d>" ints.(i)
	else
	  sprintf "%d, " ints.(i)
      in
	Buffer.add_string b str
    done;
    Buffer.contents b

(** Make an abstraction function and an unabstraction function that
    pays attention to the batches specified in the [batches] array. {b
    NOTE} that the unabstraction function returns a state array with
    many of the entries filled in with zeroes with exception of the
    batches that were not abstracted away. *)
let make_abstraction inst (batches : int array) =
  let nbatches = nbatches inst in
  let positions = Array.create nbatches ~-1 in
  let abst state =
    try
      for i = 0 to nbatches - 1 do
	let batch = state.(i) in
	  if batch <> 0 then positions.((abs batch) - 1) <- i
      done;
      Array.map (fun p ->
		   let pos = positions.(p - 1) in
		     if state.(pos) < 0 then ~-(pos + 1) else pos)
	batches
    with e ->
      Format.printf "@[<4>abst:@\nbatch=%a@\nstate=%a@."
	format_int_array batches format_int_array state;
      raise e in
  let unabst abst =
    let state = Array.create nbatches 0 in
    let set_batch i pos =
      let batch = batches.(i) in
	if pos < 0 then
	  state.((abs pos) - 1) <- ~-batch
	else
	  state.(pos) <- batch
    in
      Array.iteri set_batch abst;
      state
  in
    abst, unabst

(** Expand an abstract state. *)
let make_expand_abst inst mult_costs ops abst unabst =
  let expand = make_expand_rev inst mult_costs ops in
    (fun abst_state g ->
       let state = unabst abst_state in
       let kids = expand state g in
	 List.map (fun (kid, cost) -> abst kid, cost) kids)

(** Enumerate an entire abstraction.  The result is a hash table of
    abstract states to their goal cost. *)
let enumerate_abst inst mult_costs ops goal batches =
  let abst, unabst = make_abstraction inst batches in
  let expand = make_expand_abst inst mult_costs ops abst unabst in
  let abst_goal = abst goal in
  let opn = Queue.create () in
  let closed = H.create 149 in
  let count = ref 0 in
    Queue.push (abst_goal, 0.) opn;
    while not (Queue.is_empty opn) do
      let abst_state, cost = Queue.take opn in
	if not (H.mem closed abst_state) then begin
	  incr count;
	  H.add closed abst_state cost;
	  List.iter (fun ((kid, _cost) as node) ->
		       if not (H.mem closed kid) then
			 Queue.push node opn)
	    (expand abst_state cost)
	end
    done;
    Datafile.write_pairs stdout
      [ "abst state " ^ (string_of_ints batches), string_of_int !count];
    closed

(** Make the admissible all pairs heuristic. *)
let make_all_pairs_heuristic inst mult_costs ops goal =
  let nbatches = nbatches inst in
  let compute_pair_costs b0 b1 =
    enumerate_abst inst mult_costs ops goal [| b0 + 1; b1 + 1; |] in
  let costs = Wrarray.init_matrix nbatches nbatches compute_pair_costs in
  let pair_abstraction b0 b1 =
    fst (make_abstraction inst [| b0 + 1; b1 + 1; |]) in
  let absts = Wrarray.init_matrix nbatches nbatches pair_abstraction in
  let pair_cost b0 b1 state =
    let abst = absts.(b0).(b1) state in
    let tbl = costs.(b0).(b1) in
      try
	H.find tbl abst
      with Not_found ->
	eprintf "<%d, %d> is not found\n" b0 b1;
	raise Not_found
  in
    (fun state ->
       let max = ref 0. in
	 for i = 0 to nbatches - 2 do
	   for j = i to nbatches - 1 do
	     let c = pair_cost i j state in
	       if c > !max then max := c
	   done
	 done;
	 !max)

(** Make the admissible all triples heuristic. *)
let make_all_triples_heuristic inst mult_costs ops goal =
  let nbatches = nbatches inst in
  let compute_trip_costs b0 b1 b2 =
    enumerate_abst inst mult_costs ops goal [| b0 + 1; b1 + 1; b2 + 1 |] in
  let costs = Wrarray.init_3d nbatches nbatches nbatches compute_trip_costs in
  let trip_abstraction b0 b1 b2 =
    fst (make_abstraction inst [| b0 + 1; b1 + 1; b2 + 1 |]) in
  let absts = Wrarray.init_3d nbatches nbatches nbatches trip_abstraction in
  let trip_cost b0 b1 b2 state =
    let abst = absts.(b0).(b1).(b2) state in
    let tbl = costs.(b0).(b1).(b2) in
      try
	H.find tbl abst
      with Not_found ->
	eprintf "<%d, %d, %d> is not found\n" b0 b1 b2;
	raise Not_found
  in
    (fun state ->
       let max = ref 0. in
	 for i = 0 to nbatches - 3 do
	   for j = i to nbatches - 2 do
	     for k = j to nbatches - 1 do
	       let c = trip_cost i j k state in
		 if c > !max then max := c
	     done
	   done
	 done;
	 !max)
