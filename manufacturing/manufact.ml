(** A manufacturing domain that is suspiciously similar to the
    internals of a very large and dynamic printer.

    Each action has a fixed duration and therefore the only 'floating'
    time points are the start times for each material.  These are
    implemented as time points in an STN.

*)

open Format
open Manufact_inst

module Stn = D_stn
type stn = Stn.t
type time_pt = Stn.node

(* Because actions have fixed durations, a machine allocation can be
   uniquely specified by a material starting time point, offset from
   that time point and the allocation duration. *)
type mach_alloc = {
  material : int;
  offs : int;
  dur : int;
}

type action =
  | Process of int * int * machine * attr
  | Transport of int * int * machine * transport

type state = {
  stn : stn;
  mach_allocs : mach_alloc list array;
  plan : action list;
  (* mat_num is also the time point *)
  mat_num : int;
  attrs : int list;
  loc : mach_id;
  time : int;
  (* Current time offset from the staret of the plan for this
     material. *)
  entered_at : int;
  (* Time that the job entered the current machine. *)
  exiting : bool;

  (* The following fields are updated in Manufact_intf and
     Manufact_heur. *)
  mutable f : float;
  mutable h : float;
  mutable d : float;
  mutable parent : state;
}

let init_stn inst =
  let stn = Stn.create inst.nmaterial in
  for i = 1 to inst.nmaterial do
    ignore (Stn.add_constraint stn (Stn.not_earlier_than i 0))
  done;
  stn

let time_pt mat_id =
  mat_id + 1

let initial_state inst =
  let stn = init_stn inst in
  let rec init = { stn = stn;
                   mach_allocs = Array.create inst.layout.nmachs [];
                   plan = [];
                   mat_num = 0;
                   attrs = [];
                   loc = inst.srcs.(0);
                   time = 0;
                   entered_at = 0;
                   exiting = true;
                   f = ~-.1.;
                   h = nan;
		   d = nan;
                   parent = init; }
  in init

let machine_allocation state =
  { material = state.mat_num;
    offs = state.entered_at;
    dur = state.time - state.entered_at; }

let with_cnsts state allocs cnsts =
  let stn = Stn.copy state.stn in
  try [{ state with
    stn = Stn.add_constraints stn cnsts;
    mach_allocs = allocs }]
  with Simple_temp_net.Inconsistent -> []

let order_first state alloc = function
  | (a :: _) as rest ->
    let dur_before = Math.imax 0 (state.time - a.offs) in
    let before =
      Stn.before (time_pt state.mat_num) (time_pt a.material) dur_before in
    let allocs = Array.copy state.mach_allocs in
    allocs.(state.loc) <- alloc :: rest;
    with_cnsts state allocs [before]
  | [] ->
    let allocs = Array.copy state.mach_allocs in
    allocs.(state.loc) <- [alloc];
    [{ state with mach_allocs = allocs }]

let rec order_between state prefix alloc = function
  | a :: ((b :: _) as rest) ->
    let dur_after = Math.imax 0 (a.offs + a.dur - state.entered_at) in
    let after =
      Stn.after (time_pt state.mat_num) (time_pt a.material) dur_after in
    let dur_before = Math.imax 0 (state.time - b.offs) in
    let before =
      Stn.before (time_pt state.mat_num) (time_pt b.material) dur_before in
    let allocs = Array.copy state.mach_allocs in
    let prefix' = a :: prefix in
    allocs.(state.loc) <- List.rev_append prefix' (alloc :: rest);
    (with_cnsts state allocs [before; after]
     @ order_between state prefix' alloc rest)
  | _ -> []

let rec order_last state prefix alloc = function
  | a :: ((b :: _) as rest) -> order_last state (a :: prefix) alloc rest
  | a :: [] ->
    let dur_after = Math.imax 0 (a.offs + a.dur - state.entered_at) in
    let after =
      Stn.after (time_pt state.mat_num) (time_pt a.material) dur_after in
    let allocs = Array.copy state.mach_allocs in
    allocs.(state.loc) <- List.rev (alloc :: a :: prefix);
    with_cnsts state allocs [after]
  | [] -> []

let orderings state =
  let alloc = machine_allocation state in
  let allocs = state.mach_allocs.(state.loc) in
  order_first state alloc allocs
  @ order_last state [] alloc allocs
  @ order_between state [] alloc allocs

let possible_exits inst mach state =
  let transport lst tr =
    let time = state.time + tr.trans_time in
    let act = Transport (state.mat_num, state.time, mach, tr) in
    let state' =
      { state with
	plan = act :: state.plan;
	loc = tr.dest_id;
	time = time;
	entered_at = time;
	exiting = false; } in
    state' :: lst in
  Array.fold_left transport [] mach.out

let exit_machine inst state mach =
  let ordered = orderings state in
  Wrlist.mapcan (possible_exits inst mach) ordered

let enter_machine state mach =
  let process lst attr =
    let time = state.time + attr.proc_time in
    let act = Process (state.mat_num, state.time, mach, attr) in
    let state' =
      { state with
	plan = act :: state.plan;
	attrs = attr.attr_id :: state.attrs;
	time = time;
	exiting = true; } in
    state' :: lst in
  let pass_thru = { state with exiting = true; } in
  Array.fold_left process [pass_thru] mach.Manufact_inst.attrs

let is_destination inst state =
  inst.dests.(state.mat_num) = state.loc

let has_desired_attrs inst state =
  List.for_all
    (fun a -> List.mem a state.attrs)
    inst.desired_attrs.(state.mat_num)

let next_material inst state =
  let next_mat = state.mat_num + 1 in
  let loc = if next_mat < inst.nmaterial then inst.srcs.(next_mat) else ~-1 in
  { state with
    mat_num = next_mat;
    attrs = [];
    loc = loc;
    time = 0;
    entered_at = 0;
    exiting = true; }

(* This is the core expand function.  It does not update the
   'search-related' fields of the state (parent, h, d and f).  Those
   fields are updated by the caller.

   For the search expansion function see: Manufact_inst.domain_expand. *)
let expand inst state =
  if state.mat_num >= inst.nmaterial then
    [] (* goal state and thus kids *)
  else
    let mach = inst.layout.machs.(state.loc) in
    if is_destination inst state && has_desired_attrs inst state then
      [next_material inst state]
    else if state.exiting then
      exit_machine inst state mach
    else
      enter_machine state mach

let is_goal inst state =
  state.mat_num >= inst.nmaterial

let current_plan_duration state =
  let latest = ref 0 in
  let consider_action act =
    let mat, latest_offs = match act with
      | Process (mat, offs, _, attr) -> mat, offs + attr.proc_time
      | Transport (mat, offs, _, tr) -> mat, offs + tr.trans_time in
    let est, _ = Stn.bounds state.stn (time_pt mat) in
    let time = est + latest_offs in
    if time > !latest then latest := time
  in
  List.iter consider_action state.plan;
  !latest

(*
  #use "use.ml";;
  let s :: _ = expand example (initial_state example) ;;
  let s' :: _ = expand example s;;
  let s'' :: _ = expand example s';;
  let s''' :: _ = expand example s'';;
  let _:: s'''' :: [] = expand example s''';;
*)
