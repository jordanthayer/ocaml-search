(** A very simple implementation of BUGYS!.  This implementation
    doesn't include multiple heuristics or heuristic correction. *)


type util = {
  g : float;
  d : float;
  h : float;
  t : float;
  f : float;
  u : float;
}

type 'state node = {
  mutable util : util;
  mutable state : 'state;
  mutable parent : 'state node;
  mutable exp_num : int;
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

let utility ~wt ~wf time_per delay ~h ~d state g =
  let t = !time_per *. (d *. !delay) in
  let f = g +. h in
  let u = ~-. (wf *. f +. wt *. t) in
  { g = g; d = d; h = h; t = t; f = f; u = u }

let consider_child ~wt ~wf time_per delay hd opn cls key parent (s, g) =
  let key = key s in
  try
    let node = Htable.find cls key in
    if node.util.g > g then begin
      let h = node.util.h and d = node.util.d and pq_pos = node.pq_pos in
      let u = utility ~wt ~wf time_per delay ~h ~d s g in
      node.util <- u;
      node.state <- s;
      node.parent <- parent;
      if pq_pos = Dpq.no_position then
	Dpq.insert opn node
      else
	Dpq.see_update opn pq_pos
    end
  with Not_found ->
    let h, d = hd s in
    let u = utility ~wt ~wf time_per delay ~h ~d s g in
    let node =
      { util = u; state = s; parent = parent;
	exp_num = ~-1; pq_pos = Dpq.no_position; } in
    Dpq.insert opn node;
    Htable.add cls key node

let update_time time_per =
  let batch_size = 5000 and max_batch = 50. in
  let nbatchs = ref (~-.2.) and stime = ref nan and num = ref 0 in
  let batch_sizef = float batch_size in
  (fun () ->
    if !nbatchs < max_batch then begin
      if !num = batch_size && !stime = !stime then begin
	if !nbatchs > 0. then begin
	  let time = (Sys.time () -. !stime) /. batch_sizef in
	  time_per := !time_per +. (time -. !time_per) /. !nbatchs;
	end;
	nbatchs := !nbatchs +. 1.;
	num := 0;
      end;
      if !num = 0 then stime := Sys.time ();
      incr num
    end)

let update_exp_delay delay =
  let num = ref 1. in
  let settle = 200. in
  (fun n ->
    if !num > settle then begin
      let ed = !num -. float n.parent.exp_num in
      delay := !delay +. (ed -. !delay) /. !num;
      num := !num +. 1.;
    end)

let search limits ~wt ~wf hd key eq hash is_goal expand init =
  let h0, d0 = hd init in
  let time_per = ref 0. and delay = ref 1. in
  let update_time = update_time time_per in
  let update_exp_delay = update_exp_delay delay in
  let u0 = utility ~wt ~wf time_per delay ~h:h0 ~d:d0 init 0. in
  let rec node0 =
    { util = u0; state = init; parent = node0;
      exp_num = 1; pq_pos = Dpq.no_position } in
  let opn = Dpq.create is_better pq_update 149 node0 in
  let cls = Htable.create hash eq 149 in
  Dpq.insert opn node0;
  Htable.add cls (key init) node0;
  while
    not (Limit.halt_p limits)
    && not (Limit.has_incumbent limits)
    && not (Dpq.empty_p opn)
  do
    update_time ();
    let n = Dpq.extract_first opn in
    n.exp_num <- limits.Limit.expanded;
    update_exp_delay n;
    if is_goal n.state then
      Limit.new_incumbent limits (Limit.Incumbent (n.util.g, n.state))
    else begin
      let kids = expand n.state n.util.g in
      Limit.incr_exp limits;
      Limit.incr_gen_n limits (List.length kids);
      List.iter (consider_child ~wt ~wf time_per delay hd opn cls key n) kids
    end;
  done;
  Datafile.write_pairs stdout ["time per expand", string_of_float !time_per]

let bugsy results unwrap sface args =
  let wf = Search_args.get_float "Bugsy.bugsy" args 0 in
  let wt = Search_args.get_float "Bugsy.bugsy" args 1 in
  let limits = sface.Search_interface.info in
  let eq = sface.Search_interface.equals in
  let key = sface.Search_interface.key in
  let hash = sface.Search_interface.hash in
  let is_goal = sface.Search_interface.goal_p in
  let expand = sface.Search_interface.domain_expand in
  let hd = sface.Search_interface.hd in
  let init = sface.Search_interface.initial in
  search limits ~wt ~wf hd key eq hash is_goal expand init;
  unwrap
    (function Limit.Nothing -> None
      | Limit.Incumbent (g, state) -> Some (state, g))
    (results limits)

let dups sface args =
  bugsy Limit.results6 Limit.unwrap_sol6 sface args

let no_dups sface args =
  bugsy Limit.results5 Limit.unwrap_sol5 sface args
