(** Reading/writing scanalyzer instances.

    @author eaburns
    @since 2011-02-28
*)

open Printf

type inst = {
  num_belts : int;
  (** Total number of conveyer belts. *)
  batches_per_belt : int;
  (** Number of batches on each belt. *)
  left_belts : int list;
  (** Belts that are going in the left direction.  All other belts go right. *)
  scan_cycles : (int * int) list;
  (** Pairs of belts that can be used for scanning. *)
}

(** The example instance from Malte's paper. *)
let ex_instance =
  {
    num_belts = 6;
    batches_per_belt = 1;
    left_belts = [ 0; 1; 2 ];
    scan_cycles = [ 0, 5 ];
  }

(** Total number of batches in the given instance. *)
let nbatches inst =
  inst.num_belts * inst.batches_per_belt

(** The initial state of the problem. *)
let initial_state inst =
  Array.init (nbatches inst) (fun i -> i + 1)

(** The canonical goal is to have the batches in the original order
    but everything is scanned. *)
let canonical_goal inst =
  Array.init (nbatches inst) (fun i -> ~-(i + 1))

(** Get the list of right belts for the instance. *)
let right_belts inst =
  let is_right = Array.create inst.num_belts true in
    List.iter (fun left_belt -> is_right.(left_belt) <- false) inst.left_belts;
    Wrarray.fold_lefti
      (fun lst i right -> if right then i :: lst else lst)
      [] is_right

(** Get the possible cycle operations from the instance. *)
let possible_cycles inst =
  let right_belts = right_belts inst in
  let consider_cycle lbelt lst rbelt =
    if rbelt > lbelt then (lbelt, rbelt) :: lst else lst in
  let rec cycles accum = function
    | [] -> accum
    | lbelt :: left_belts ->
	let accum' = List.fold_left (consider_cycle lbelt) accum right_belts in
	  cycles accum' left_belts
  in
    cycles [] inst.left_belts

(** Get the list of possible scanning cycles.  This really just
    verifies that the scan cycles in the instance are valid cycles
    then returns them. *)
let scan_cycles inst cycles =
  let is_cycle cyc = List.mem cyc cycles in
  let check_valid ((l, r) as cyc) =
    if not (is_cycle cyc) then
      failwith (sprintf "scan cycle (%d, %d) is not a valid cycle" l r)
  in
    List.iter check_valid inst.scan_cycles;
    inst.scan_cycles

(** Get the indices of a belt within a state array. *)
let belt_indices inst belt =
  let per_belt = inst.batches_per_belt in
  let start = belt * per_belt in
  let finish = start + per_belt - 1 in
    start, finish

(** Gets the cycles along with the cost of each cycle given the cost
    function for normal and scan cycles. *)
let operators_with_cost inst norm_cycle_cost scan_cycle_cost =
  let cycles = possible_cycles inst in
  let scans = scan_cycles inst cycles in
  let op cost_fun (l, r) =
    let cost : float = cost_fun l r in
    let left_start, left_finish = belt_indices inst l in
    let right_start, right_finish = belt_indices inst r in
      left_start, left_finish, right_start, right_finish, cost
  in
    (List.map (op norm_cycle_cost) cycles,
     List.map (op scan_cycle_cost) scans)

(* The canonical cost functions from Malte's paper *)
let canonical_norm_cycle_cost _ _ = 1.
let canonical_scan_cycle_cost _ _ = 3.
let canonical_mult_costs = false

(** Reads an instance from a channel. *)
let read inch =
  let num_belts = Wrio.input_int inch in
  let batches_per_belt = Wrio.input_int inch in
  let left_belts = Wrio.read_ints inch in
  let scan_cycle_ints = Wrio.read_ints inch in
    if not (Math.even_p (List.length scan_cycle_ints)) then
      failwith "Scanalyzer_inst.read: invalid scan cycle.";
    let count = ref ~-1 in
    let l, r =
      List.partition (fun _ -> incr count; Math.even_p !count) scan_cycle_ints
    in
    let scan_cycles = List.combine l r in
      { num_belts = num_belts;
	batches_per_belt = batches_per_belt;
	left_belts = left_belts;
	scan_cycles = scan_cycles }

(** Load an instance from the given file. *)
let load file =
  Wrio.with_infile file read

(** Writes an instance to the given channel. *)
let write inst outch =
  fprintf outch "%d\n" inst.num_belts;
  fprintf outch "%d\n" inst.batches_per_belt;
  List.iter (fprintf outch " %d") inst.left_belts;
  fprintf outch "\n";
  List.iter (fun (l, r) -> fprintf outch " %d %d" l r) inst.scan_cycles;
  fprintf outch "\n"

(** Save the instance to the given file. *)
let save inst file =
  Wrio.with_outfile file (write inst)
