(** Best-first Utility Guided Search Yes! as described in:

    Wheeler Ruml and Minh B. Do, Best-first Utility-Guided Search,
    Proceedings of the Twentieth International Joint Conference on
    Artificial Intelligence (IJCAI-07), 2007 *)
open Float_ref

type 'node sinfo = {
  wf : float;
  wt : float;
  herrs : float array;
  derrs : float array;
  hd_fns : ('node -> float * float) array;
  correcth : bool;
  dropdups: bool;
  estdelay : bool;
  mutable time_per : float;
  mutable delay : float;
  mutable num : int;
}

type node_util = {
  g : float;
  f : float;
  t : float;
  u : float;
}

type 'state node = {
  hs : float array;
  ds : float array;
  mutable state : 'state;
  mutable us : float array;
  mutable util : node_util;
  mutable parent : 'state node;
  mutable exp : int;			(* expansion number. *)
  mutable pq_pos : int;
}

let pq_update node ind =
  node.pq_pos <- ind

let is_better a b =
  let a = a.util and b = b.util in
  if a.u = b.u then
    if a.t = b.t then
      if a.f = b.f then
	a.g > b.g
      else
	a.f < b.f
    else
      a.t < b.t
  else
    a.u > b.u

(** Compute the utility values for a node.  This mutates the [us]
    input array. *)
let compute_util sinfo =
  let num = Array.length sinfo.hd_fns in
  let u_best = float_ref neg_infinity in
  let t_best = float_ref neg_infinity in
  let f_best = float_ref neg_infinity in
  let herrs = sinfo.herrs and derrs = sinfo.derrs in
  let wf = sinfo.wf and wt = sinfo.wt in
  (fun ~hs ~ds ~us g ->
    u_best <-- neg_infinity;
    t_best <-- neg_infinity;
    f_best <-- neg_infinity;
    for i = 0 to num - 1 do
      let dhat =
	if derrs.(i) > 1. then infinity else ds.(i) /. (1. -. derrs.(i)) in
      let hhat =
	if sinfo.correcth then hs.(i) +. (herrs.(i) *. dhat) else hs.(i) in
      let t =
	if wt = 0. || sinfo.time_per = 0. then 0.
	else wt *. sinfo.time_per *. dhat in
      let t = if sinfo.estdelay then t *. sinfo.delay else t in
      let f = if wf = 0. then 0. else wf *. (g +. hhat) in
      let u = ~-. (f +. t) in
      us.(i) <- u;
      if u > !!u_best then begin
	u_best <-- u;
	t_best <-- t;
	f_best <-- f;
      end
    done;
    { g = g; t = !!t_best; f = !!f_best; u = !!u_best })

let make_node sinfo =
  let compute_util = compute_util sinfo in
  (fun parent state g ->
    let ds = Array.create (Array.length sinfo.hd_fns) nan in
    let h_set_d i hd = let h, d = hd state in ds.(i) <- d; h in
    let hs = Array.mapi h_set_d sinfo.hd_fns in
    let us = Array.create (Array.length sinfo.hd_fns) nan in
    let node =
      { state = state;
	hs = hs;
	ds = ds;
	us = us;
	util = compute_util ~hs ~ds ~us g;
	parent = parent;
	exp = ~-1;
	pq_pos = Dpq.no_position } in
    node)

(** Get the best child node according to the [ind]th hd function. *)
let best_by ind kids = match kids with
  | [] -> invalid_arg "Bugsy.best_by: no kids"
  | k :: ks ->
    let best = ref k in
    let best_u = float_ref k.us.(ind) in
    let best_h = float_ref k.hs.(ind) in
    let best_d = float_ref k.ds.(ind) in
    let consider k =
      if k.us.(ind) > !!best_u then begin
	best := k;
	best_u <-- k.us.(ind);
	best_h <-- k.hs.(ind);
	best_d <-- k.ds.(ind);
      end in
    List.iter consider ks;
    !best, !!best_h, !!best_d

(** Update the global single-step errors for d and possibly h too. *)
let update_err sinfo node kids i =
  let best, best_h, best_d = best_by i kids in
  let cost = best.util.g -. node.util.g in
  let n = float sinfo.num in
  let herr = best.hs.(i) +. cost -. node.hs.(i) in
  sinfo.herrs.(i) <- sinfo.herrs.(i) +. (herr -. sinfo.herrs.(i)) /. n;
  let derr = best.ds.(i) +. 1. -. node.ds.(i) in
  sinfo.derrs.(i) <- sinfo.derrs.(i) +. (derr -. sinfo.derrs.(i)) /. n

(** Expand a node generating all its kids.  If a kid has already been
    generated then don't recompute the [hs] and [ds] arrays. *)
let child_nodes sinfo domain_expand limits cls key =
  let compute_util = compute_util sinfo and make_node = make_node sinfo in
  (fun parent ->
    let handle_kid (state, g) =
      Limit.incr_gen limits;
      try
	let node = Htable.find cls (key state) in
	let us = Array.copy node.us in
	let util = compute_util ~hs:node.hs ~ds:node.ds ~us g in
	let node' =
	  { node with
	    state = state;
	    us = us;
	    util = util;
	    parent = parent;
	    pq_pos = Dpq.no_position } in
	node'
      with Not_found ->
	let node = make_node parent state g in
	node in
    let kids = domain_expand parent.state parent.util.g in
    Limit.incr_exp limits;
    List.map handle_kid kids)

let estimate_delay sinfo limits node =
  node.exp <- limits.Limit.expanded;
  let delay = node.exp - node.parent.exp in
  let prev = sinfo.delay in
  sinfo.delay <- prev +. (float delay -. prev) /. float sinfo.num

(** Expand a node generating nodes for all of the children. *)
let expand sinfo domain_expand limits cls key =
  let child_nodes = child_nodes sinfo domain_expand limits cls key in
  (fun node ->
    let kids = child_nodes node in
    sinfo.num <- sinfo.num + 1;
    if sinfo.estdelay then estimate_delay sinfo limits node;
    if List.length kids > 0 then begin
      for i = 0 to Array.length sinfo.herrs - 1 do
	update_err sinfo node kids i;
      done;
    end;
    kids)

(** Consider adding or updating a child in the open list. *)
let consider_child sinfo opn cls key kid =
  let key = key kid.state in
  try
    let other = Htable.find cls key in
    if not sinfo.dropdups && other.util.g > kid.util.g then begin
      other.us <- kid.us;
      other.util <- kid.util;
      other.parent <- kid.parent;
      other.state <- kid.state;
      if other.pq_pos = Dpq.no_position then
	Dpq.insert opn other
      else
	Dpq.see_update opn other.pq_pos
    end
  with Not_found ->
    Htable.add cls key kid;
    Dpq.insert opn kid


type state =
  | Init
  | Wait_tick
  | Expand_some
  | Wait_expand

let update_time sinfo opn =
  let state = ref Init in
  let start_time = float_ref nan and last_tick = float_ref nan in
  let nexp = ref 0 and expands_per_tick = ref 20 in
  let update_u =
    let compute = compute_util sinfo in
    (fun n -> n.util <- compute ~hs:n.hs ~ds:n.ds ~us:n.us n.util.g) in
  (fun () ->
    incr nexp;
    match !state with
      | Init ->
	last_tick <-- Sys.time ();
	state := Wait_tick;
      | Wait_tick ->
	let now = Sys.time () in
	if now > !!last_tick then begin
	  start_time <-- now;
	  state := Expand_some
	end
      | Expand_some ->
	if !nexp > !expands_per_tick then begin
	  last_tick <-- Sys.time ();
	  state := Wait_expand
	end
      | Wait_expand ->
	let now = Sys.time () in
	if now > !!last_tick then begin
	  Dpq.update_all opn update_u;
	  sinfo.time_per <- (now -. !!start_time) /. float !nexp;
	  (* check ticks again in (1.8 * nexp) expansions (this is
	     what Wheeler does in bugsy_old.ml). *)
	  expands_per_tick := (!nexp * 9) / 5;
	  nexp := 0;
	  start_time <-- now;
	  state := Expand_some;
	end)

let make_init_node sinfo init =
  let compute_util = compute_util sinfo in
  let ds = Array.create (Array.length sinfo.hd_fns) nan in
  let h_set_d i hd = let h, d = hd init in ds.(i) <- d; h in
  let hs = Array.mapi h_set_d sinfo.hd_fns in
  let us = Array.create (Array.length sinfo.hd_fns) nan in
  let rec node =
    { state = init;
      hs = hs;
      ds = ds;
      us = us;
      util = compute_util ~hs ~ds ~us 0.;
      parent = node;
      exp = ~-1;
      pq_pos = Dpq.no_position } in
  node

let search sinfo domain_expand key eq hash is_goal limits init =
  let init_node = make_init_node sinfo init in
  let opn = Dpq.create is_better pq_update 149 init_node in
  let cls = Htable.create hash eq 149 in
  let expand = expand sinfo domain_expand limits cls key in
  let consider_child = consider_child sinfo opn cls key in
  let update_time = update_time sinfo opn in
  Dpq.insert opn init_node;
  Htable.add cls (key init) init_node;
  while
    not (Limit.has_incumbent limits)
    && not (Dpq.empty_p opn)
    && not (Limit.halt_p limits)
  do
    update_time ();
    let n = Dpq.extract_first opn in
    if is_goal n.state then
      Limit.new_incumbent limits (Limit.Incumbent (n.util.g, n.state))
    else
      List.iter consider_child (expand n)
  done;
  Datafile.write_pairs stdout
    [ "sec per expansion", string_of_float sinfo.time_per; ]

let make_sinfo ~wf ~wt hd_fns ~correcth ~dropdups ~estdelay =
  { wf = wf;
    wt = wt;
    herrs = Array.create (Array.length hd_fns) 0.;
    derrs = Array.create (Array.length hd_fns) 0.;
    hd_fns = hd_fns;
    correcth = correcth;
    dropdups = dropdups;
    estdelay = estdelay;
    time_per = 0.;
    delay = 0.;
    num = 0 }

(** Get the heuristics.  If there is both a cheapest and nearest then
    use them both otherwise just use cheapest. *)
let heuristics sface =
  let module H = Heuristic in
  let h = sface.Search_interface.heuristics in
  match H.get_hd h [ H.Cheapest; ], H.get_hd h [ H.Nearest; ] with
    | [], _ ->
      Datafile.write_pairs stdout ["h cheapest name", "<unknown>"];
      [| sface.Search_interface.hd |]
    | c :: _, [] ->
      Datafile.write_pairs stdout ["h cheapest name", c.H.name];
      [| c.H.heuristic |]
    | c :: _, n :: _ ->
      Datafile.write_pairs stdout ["h cheapest name", c.H.name];
      Datafile.write_pairs stdout ["h nearest name", n.H.name];
      [| c.H.heuristic; n.H.heuristic |]

let bugsy results unwrap sface args =
  let wf = Search_args.get_float "Bugsy.bugsy" args 0 in
  let wt = Search_args.get_float "Bugsy.bugsy" args 1 in
  let correcth = Search_args.get_bool "Bugsy.bugsy" args 2 in
  let dropdups = Search_args.get_bool "Bugsy.bugsy" args 3 in
  let estdelay = Search_args.get_bool "Bugsy.bugsy" args 4 in
  let hd_fns = heuristics sface in
  let sinfo = make_sinfo ~wf ~wt hd_fns ~correcth ~dropdups ~estdelay in
  let limits = sface.Search_interface.info in
  let eq = sface.Search_interface.equals in
  let key = sface.Search_interface.key in
  let hash = sface.Search_interface.hash in
  let is_goal = sface.Search_interface.goal_p in
  let domain_expand = sface.Search_interface.domain_expand in
  let init = sface.Search_interface.initial in
  search sinfo domain_expand key eq hash is_goal limits init;
  unwrap
    (function Limit.Nothing -> None
      | Limit.Incumbent (g, state) -> Some (state, g))
    (results limits)

let dups sface args =
  bugsy Limit.results6 Limit.unwrap_sol6 sface args

let no_dups sface args =
  bugsy Limit.results5 Limit.unwrap_sol5 sface args
