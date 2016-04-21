(** The heuristic works by computing for all possible locations in an
    empty machine and attribute sets the quickest way to get the
    desired attributes and be at the desired destination machine.

    Each (attribute set, location) pair is mapped to a minimal set of
    integers and then we run Floyd-Warshall to find all shortest
    paths. *)

open Format
open Fn
open Manufact
open Manufact_inst
open Manufact_fmt

(** Build a pair of functions that map an (attribute set, location)
    pair to a unique integer ID and back.  The result is a triple
    (node -> id, id -> node, max id value).  The mapping is to a
    minimal set of integers so that they can safely be used as array
    indices and all indices represent a valid (attribute set,
    location) pair.*)
let make_id_map layout =
  let nattrs = layout.nattrs and nmachs = layout.nmachs in
  let mach_bits = truncate (ceil (Math.log2 (float nmachs))) in
  let attr_bits = 1 lsl (nattrs - 1) in
  let attr_mask = lnot ((lnot 0) lsl attr_bits) in
    Verb.pr Verb.debug "mach_bits=%d, attr_bits=%d, attr_mask=%x\n"
      mach_bits attr_bits attr_mask;
    if mach_bits + attr_bits > Sys.word_size - 1 then
      failwith "Manufact_heur: Instance is too large";
    let node_id attrs loc =
      begin try
        assert (List.for_all (fun a -> a < nattrs) attrs);
        assert (loc < nmachs);
      with _ ->
        printf "nattrs=@[%a@]@\nloc=%d@."
          (format_list (fun fmt i -> fprintf fmt "%d" i)) attrs
          loc;
        assert false;
      end;
      let abits = List.fold_left (fun w a -> w lor (1 lsl a)) 0 attrs in
        (loc lsl attr_bits) lor abits in
    let id_node id =
      let loc = id lsr attr_bits in
      let abits = ref (id land attr_mask) in
      let attrs = ref [] in
      let i = ref 0 in
        while !abits <> 0 do
          if !abits land 0x1 <> 0 then attrs := !i :: !attrs;
          abits := !abits lsr 1;
          incr i;
        done;
        !attrs, loc in
    let max_id =
      node_id (Array.to_list (Array.init nattrs identity)) (nmachs - 1)
    in
      node_id, id_node, max_id

let format_node fmt (attrs, loc) =
  fprintf fmt "(%a, %d)" (format_list (fun fmt i -> fprintf fmt "%d" i))
    attrs loc

let attr_diff a b =
  let in_b = Hashtbl.create 149 in
    List.iter (fun b -> Hashtbl.replace in_b b b) b;
    List.filter (fun a -> not (Hashtbl.mem in_b a)) a

let proc_time mach attr =
  let attrs = mach.Manufact_inst.attrs in
  let ind = Wrarray.find (fun at -> at.attr_id = attr) attrs in
    attrs.(ind).proc_time

let transport_time mach dest_id =
  let out = mach.out in
  let ind = Wrarray.find (fun tr -> tr.dest_id = dest_id) out in
    out.(ind).trans_time

let edge_cost layout id_node id0 id1 =
  let attrs0, mach0_id = id_node id0 in
  let attrs1, mach1_id = id_node id1 in
  let mach0 = layout.machs.(mach0_id) in
    try match attr_diff attrs1 attrs0 with
      | [] when mach0_id = mach1_id -> 0.
      | [] -> float (transport_time mach0 mach1_id)
      | a :: [] when mach0_id = mach1_id -> float (proc_time mach0 a)
      | _ -> infinity
    with Not_found ->
      infinity

let cost_matrix layout id_node max_id =
  let edge_cost = edge_cost layout id_node in
  let nids = max_id + 1 in
  let m = Wrarray.init_matrix nids nids edge_cost in
  let d =
    Wrarray.init_matrix nids nids
      (fun i j ->
         if i = j then 0.
         else if Math.finite_p m.(i).(j) then 1.
         else infinity)
  in
    for k = 0 to max_id do
      for i = 0 to max_id do
        for j = 0 to max_id do
          let cost_via_k = m.(i).(k) +. m.(k).(j) in
            if cost_via_k < m.(i).(j) then begin
              d.(i).(j) <- d.(i).(k) +. d.(k).(j);
              m.(i).(j) <- cost_via_k
            end
        done
      done
    done;
    m, d

let rec latest_action_time est mat_num = function
  | Process (m, offs, _, attr) :: acts when m = mat_num ->
      let next_time = latest_action_time est mat_num acts in
      let this_time = est + offs + attr.proc_time in
        if this_time > next_time then this_time else next_time
  | Transport (m, offs, _, tr) :: acts when m = mat_num ->
      let next_time = latest_action_time est mat_num acts in
      let this_time = est + offs + tr.trans_time in
        if this_time > next_time then this_time else next_time
  | _ :: acts -> latest_action_time est mat_num acts
  | [] -> 0

let heuristic_remaining_time inst costs node_id state mat_num =
  let desired_loc = inst.dests.(mat_num) in
  let desired_attrs = inst.desired_attrs.(mat_num) in
  let dst_id = node_id desired_attrs desired_loc in
    if state.mat_num > mat_num then
      0.
    else if state.mat_num = mat_num then
      let src_id = node_id state.Manufact.attrs state.Manufact.loc in
        costs.(src_id).(dst_id)
    else
      let src_id = node_id [] inst.srcs.(mat_num) in
        costs.(src_id).(dst_id)

let current_plan_duration state mat_num =
  let est, _ = Stn.bounds state.stn (time_pt mat_num) in
    latest_action_time est mat_num state.plan

let g_and_f inst costs node_id state mat_num =
  let duration = float (current_plan_duration state mat_num) in
  let rem_time = heuristic_remaining_time inst costs node_id state mat_num in
    duration, duration +. rem_time

(* Compute the current plan duration (g) and the current duration
   plus heuristic estimate (f) for each material.  The g value of
   this state is the maximum current duration so the h value is the
   maximum f minus the current g value.  *)
let make_h inst node_id costs state =
  if Math.nan_p state.h then begin
    let max_g = ref 0. in
    let max_f = ref state.parent.f in
    for i = 0 to inst.nmaterial - 1 do
      let g, f = g_and_f inst costs node_id state i in
      if g > !max_g then max_g := g;
      if f > !max_f then max_f := f;
    done;
    assert (!max_f >= !max_g);
    state.f <- !max_f;
    state.h <- !max_f -. !max_g;
  end;
  state.h

let make_d inst node_id dist state =
  if Math.nan_p state.d then begin
    if state.mat_num >= inst.nmaterial then
      state.d <- 0.
    else
      let src = node_id state.Manufact.attrs state.loc in
      let dst_attr = inst.desired_attrs.(state.mat_num) in
      let dst_mach = inst.dests.(state.mat_num) in
      let dst = node_id dst_attr dst_mach in
      let sum = ref (dist.(src).(dst)) in
      for i = state.mat_num + 1 to inst.nmaterial - 1 do
	let src_mach = inst.srcs.(i) and dst_mach = inst.dests.(i) in
	let src_attr = [] and dst_attr = inst.desired_attrs.(i) in
	let src = node_id src_attr src_mach in
	let dst = node_id dst_attr dst_mach in
	let d = dist.(src).(dst) in
	sum := !sum +. d
      done;
      state.d <- !sum
  end;
  state.d

let make_heuristic inst =
  let layout = inst.layout in
  let node_id, id_node, max_id = make_id_map layout in
  let costs, dist = cost_matrix layout id_node max_id in
  for i = 0 to inst.nmaterial - 1 do
    let src_mach = inst.srcs.(i) and dst_mach = inst.dests.(i) in
    let src_attr = [] and dst_attr = inst.desired_attrs.(i) in
    let src = node_id src_attr src_mach in
    let dst = node_id dst_attr dst_mach in
    if not (Math.finite_p costs.(src).(dst)) then begin
      eprintf "%d: cost from %d to %d is infinite\n%!" i src dst;
      eprintf "%d: @[%a@]@." src format_node (id_node src);
      eprintf "%d: @[%a@]@." dst format_node (id_node dst);
    end;
    assert (Math.finite_p costs.(src).(dst));
    assert (Math.finite_p dist.(src).(dst));
  done;
  make_h inst node_id costs, make_d inst node_id dist
