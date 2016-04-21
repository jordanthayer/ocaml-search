(* Another implementation of ARA*. *)

open Float_ref

type 'state node = {
  mutable state : 'state;
  h : float;
  g : float_ref;
  f : float_ref;
  mutable parent : 'state node;
  mutable pq_pos : int;
}

type ('key, 'state) incons = {
  elms : ('key, 'state node) Htable.t;
  mutable lst : 'state node list;
  minf : float_ref;
}

let incons_insert incons key node =
  let nf = !!(node.g) +. node.h in
  if nf < !!(incons.minf) then incons.minf <-- nf;
  if not (Htable.mem incons.elms key) then begin
    Htable.add incons.elms key node;
    incons.lst <- node :: incons.lst;
  end

let handle_kid key info h eps incons opn cls parent (state, g) =
  let k = key state in
  try
    let n = Htable.find cls k in
    if !!(n.g) > g then begin
      n.g <-- g;
      n.f <-- g +. eps *. n.h;
      n.parent <- parent;
      n.state <- state;
      if n.pq_pos = Dpq.no_position then
	incons_insert incons k n
      else
	Dpq.see_update opn n.pq_pos;
    end
  with Not_found ->
    let h = h state in
    let n =
      { state = state; h = h; g = float_ref g; f = float_ref (g +. eps *. h);
	parent = parent; pq_pos = Dpq.no_position; } in
    Htable.add cls k n;
    Dpq.insert opn n

let incumbent info incf n =
  Limit.new_incumbent info (Limit.Incumbent (!!(n.g), n.state));
  incf <-- !!(n.f)

let improve stop info incf key succs is_goal h eps incons opn cls =
  while
    not (Dpq.empty_p opn)
    && !!incf > !!((Dpq.peek_first opn).f)
    && not (Limit.halt_p info)
    && not (stop !!incf)
  do
    let n = Dpq.extract_first opn in
    if is_goal n.state then
      incumbent info incf n
    else begin
      let kids = succs n.state !!(n.g) in
      Limit.incr_exp info;
      Limit.incr_gen_n info (List.length kids);
      List.iter (handle_kid key info h eps incons opn cls n) kids
    end
  done

let is_better a b =
  let af = !!(a.f) and bf = !!(b.f) in
  if af = bf then !!(a.g) > !!(b.g) else af < bf

let pq_update n ind =
  n.pq_pos <- ind

let open_incons opn incons eps =
  let open_node n =
    n.f <-- !!(n.g) +. eps *. n.h;
    Dpq.insert opn n; in
  List.iter open_node incons.lst;
  Htable.clear incons.elms;
  incons.lst <- [];
  incons.minf <-- infinity

let update_open opn eps =
  let eval_f n = n.f <-- !!(n.g) +. eps *. n.h in
  Dpq.update_all opn eval_f

let min_f opn incons =
  let opn_f = float_ref infinity in
  let consider n =
    let f = !!(n.g) +. n.h in
    if f < !!opn_f then opn_f <-- f in
  Dpq.iter consider opn;
  Math.fmin !!opn_f !!(incons.minf)

let make_publish_sol info =
  Datafile.write_colnames stdout
    ["sol cost"; "sol length"; "nodes expanded"; "nodes generated";
     "duplicates encountered"; "raw cpu time"; "guaranteed quality"; ];
  (fun f eps ->
    let t = Sys.time () -. info.Limit.start_time in
    Printf.printf "%g" f;
    Printf.printf "\t%d" ~-1;		(* len, not computed *)
    Printf.printf "\t%d" info.Limit.expanded;
    Printf.printf "\t%d" info.Limit.generated;
    Printf.printf "\t%d" info.Limit.duplicates;
    Printf.printf "\t%g" t;
    Printf.printf "\t%g" eps;
    Printf.printf "\n")

let arastar stop info hash eq key succs is_goal h eps0 delta init =
  let h0 = h init in
  let rec init_node =
    { state = init; g = float_ref 0.; f = float_ref (eps0 *. h0);
      h = h0; parent = init_node; pq_pos = Dpq.no_position; } in
  let cls = Htable.create hash eq 149 in
  let opn = Dpq.create is_better pq_update 100 init_node in
  let incons =
    { elms = Htable.create hash eq 149; lst = []; minf = float_ref infinity} in
  let incf = float_ref infinity in
  let publish_sol = make_publish_sol info in
  Dpq.insert opn init_node;
  Htable.add cls (key init) init_node;
  let rec loop eps =
    if eps > 1. && not (stop !!incf) && not (Limit.halt_p info) then begin
      let eps = Math.fmax 1. (eps -. delta) in
      update_open opn eps;
      open_incons opn incons eps;
      Htable.clear cls;
      improve stop info incf key succs is_goal h eps incons opn cls;
      let eps' = Math.fmin eps (!!incf /. min_f opn incons) in
      publish_sol !!incf eps';
      loop eps'
    end in
  improve stop info incf key succs is_goal h eps0 incons opn cls;
  let eps' = Math.fmin eps0 (!!incf /. min_f opn incons) in
  publish_sol !!incf eps';
  loop eps'

let search stop results unwrap sface args =
  let eps0 = Search_args.get_float "Aras.search" args 0 in
  let delta = Search_args.get_float "Aras.search" args 1 in
  let h = sface.Search_interface.h in
  let info = sface.Search_interface.info in
  let eq = sface.Search_interface.equals in
  let key = sface.Search_interface.key in
  let hash = sface.Search_interface.hash in
  let is_goal = sface.Search_interface.goal_p in
  let succs = sface.Search_interface.domain_expand in
  let init = sface.Search_interface.initial in
  arastar stop info hash eq key succs is_goal h eps0 delta init;
  Datafile.write_pairs stdout
    ["eps0", string_of_float eps0; "delta", string_of_float delta; ];
  unwrap
    (function Limit.Nothing -> None
      | Limit.Incumbent (g, state) -> Some (state, g))
    (results info)

let dups sface args =
  let stop _ = false in
  search stop Limit.results6 Limit.unwrap_sol6 sface args

let no_dups sface args =
  let stop _ = false in
  search stop Limit.results5 Limit.unwrap_sol5 sface args

let mon_dups sface args =
  let wf = Search_args.get_float "Aras.mon_dups" args 2 in
  let wt = Search_args.get_float "Aras.mon_dups" args 3 in
  let pfile = Search_args.get_string "Aras.mon_dups" args 4 in
  let (mon, step), t =
    Wrsys.with_time
      (fun () -> Aprof.mon pfile (fun ~q ~t -> ~-. (wf *. q +. wt *. t))) in
  let stime = Sys.time () in
  let next = ref step in
  let stop = ref false in
  let stop q =
    let t = Sys.time () -. stime in
    if not (Math.finite_p q) then
      false
    else if not !stop && t >= !next then begin
      next := t +. step;
      stop := mon q t;
      (*
	Printf.printf "Testing at q=%g, t=%g, stop=%s\n%!" q t
	(string_of_bool !stop);
      *)
      !stop
    end else
      !stop in
  (* reset the clock so that we don't take into account the time to
     solve for the monitor policy. *)
  Datafile.write_pairs stdout
    [ "monitor policy sol time", string_of_float t;
      "monitor time step", string_of_float step ];
  sface.Search_interface.info.Limit.start_time <- Sys.time ();
  search stop Limit.results6 Limit.unwrap_sol6 sface args
