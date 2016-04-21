open Format
open Manufact_inst
open Manufact

let format_list format_ent fmt lst =
  let rec format_ents fmt = function
    | [] -> ()
    | e :: [] -> format_ent fmt e
    | e :: es -> fprintf fmt "%a;@ %a" format_ent e format_ents es
  in
    fprintf fmt "[@[%a@]]" format_ents lst

let format_stn fmt stn =
  let format_bounds fmt (n, s, e) = fprintf fmt "<%d: %d-%d>" n s e in
  let bounds = Stn.all_bounds stn in
    format_list format_bounds fmt (Array.to_list bounds)

let format_alloc fmt alloc =
  fprintf fmt "{@[material = %d@ " alloc.material;
  fprintf fmt "offs = %d@ " alloc.offs;
  fprintf fmt "dur = %d" alloc.dur;
  fprintf fmt "@]}"

let format_allocs fmt allocs =
  fprintf fmt "[|@[";
  for i = 0 to Array.length allocs - 1 do
    fprintf fmt "%a" (format_list format_alloc) allocs.(i);
    if i < Array.length allocs - 1 then fprintf fmt ";@ ";
  done;
  fprintf fmt "@]|]"

let format_plan fmt plan =
  let format_action fmt = function
    | Process (mat, offs, mach, attr) ->
        fprintf fmt "Process (%d, %d, %d, %d)" mat offs
          mach.mach_id attr.attr_id
    | Transport (mat, offs, src, tr) ->
        fprintf fmt "Transport (%d, %d, %d, %d)" mat offs
          src.mach_id tr.dest_id
  in
    format_list format_action fmt plan

let format_state fmt state =
  fprintf fmt "{@[stn = @[%a@];@\n" format_stn state.stn;
  fprintf fmt "mach_allocs = @[%a@];@\n" format_allocs state.mach_allocs;
  fprintf fmt "plan = @[%a@];@\n" format_plan state.plan;
  fprintf fmt "mat_num = %d;@\n" state.mat_num;
  fprintf fmt "attrs = @[%a@];@\n"
    (format_list (fun fmt i -> fprintf fmt "%d" i)) state.attrs;
  fprintf fmt "loc = %d@\n" state.loc;
  fprintf fmt "time = %d@\n" state.time;
  fprintf fmt "entered_at = %d@\n" state.entered_at;
  fprintf fmt "exiting = %s" (string_of_bool state.exiting);
  fprintf fmt "@]}"

let plan_with_times stn plan =
  let with_time act =
    let mat, offs = match act with
      | Process (mat, offs, _, _) | Transport (mat, offs, _, _) -> mat, offs in
    let est, _ = Stn.bounds stn (time_pt mat) in
      est + offs, act
  in
    List.map with_time plan

let format_solution fmt goal_node =
  let timed_plan = plan_with_times goal_node.stn goal_node.plan in
  let cmp_times (a, _) (b, _) = compare a b in
  let sorted = List.sort cmp_times timed_plan in
    fprintf fmt "@[";
    List.iter
      (function
         | (t, Process (mat, _, mach, attr)) ->
             fprintf fmt "%3d - %-3d:   Process %d   at %d for attr %d@\n"
               t (t + attr.proc_time) mat mach.mach_id attr.attr_id
         | (t, Transport (mat, _, src, tr)) ->
             fprintf fmt "%3d - %-3d: Transport %d from %d to %d@\n"
               t (t + tr.trans_time) mat src.mach_id tr.dest_id)
      sorted;
    fprintf fmt "@]@."
