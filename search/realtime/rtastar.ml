let best_kid h kids =
  let mins = ref [] and minf = ref infinity and sndf = ref infinity in
  let consider_kid ((k, c) as kid) =
    let f = h k +. c in
    if f < !minf then begin
      sndf := !minf;
      minf := f;
      mins := [kid];
    end else if f = !minf then
	mins := kid :: !mins
      else if f < !sndf then
	sndf := f in
  List.iter consider_kid kids;
  !mins, !sndf

let rec rtastar pr lim h succs key is_goal seen cost state =
  if not (Limit.halt_p lim) then
    if is_goal state then
      Limit.new_incumbent lim (Limit.Incumbent (cost, state))
    else begin
      let kids = succs state 0. in
      Limit.incr_exp lim;
      Limit.incr_gen_n lim (List.length kids);
      let mins, sndf = best_kid h kids in
      if mins <> [] then begin
	Htable.replace seen (key state) sndf;
	let len = List.length mins in
	let n = if len = 1 then 0 else Random.int len in
	let k, c = List.nth mins n in
	rtastar pr lim h succs key is_goal seen (cost +. c) k
      end
    end

let eval pr key is_goal succs h lim dlim state =

  let rec dfs ~alpha ~g depth ~state =
    if is_goal state then
      g
    else begin
      let f = g +. h state in
      if depth < dlim && f < alpha && not (Limit.halt_p lim) then begin
	let ss = succs state g in
	Limit.incr_exp lim;
	Limit.incr_gen_n lim (List.length ss);
	best_kid ~alpha ~g (depth + 1) ss
      end else
	Math.fmin alpha f
    end

  and best_kid ~alpha ~g depth' = function
    | (k, g') :: ks ->
      let v = dfs ~alpha ~g:g' depth' ~state:k in
      best_kid ~alpha:(Math.fmin alpha v) ~g depth' ks
    | [] -> alpha in

  dfs ~alpha:infinity ~g:0. 0 ~state

let make_h pr is_goal succs h lim dlim seen key state =
  try
    let h = Htable.find seen (key state) in
    Limit.incr_dups lim;
    h
  with Not_found ->
    let h = eval pr key is_goal succs h lim dlim state in
    Htable.add seen (key state) h;
    h

let search results unwrap sface args =
  let dlim = Search_args.get_int "Rtastar.search" args 0 in
  Random.self_init ();
  let h = sface.Search_interface.h in
  let limits = sface.Search_interface.info in
  let eq = sface.Search_interface.equals in
  let key = sface.Search_interface.key in
  let hash = sface.Search_interface.hash in
  let is_goal = sface.Search_interface.goal_p in
  let succs = sface.Search_interface.domain_expand in
  let init = sface.Search_interface.initial in
  let seen = Htable.create hash eq 149 in
  let pr = sface.Search_interface.key_printer in
  rtastar pr limits (make_h pr is_goal succs h limits dlim seen key)
    succs key is_goal seen 0. init;
  unwrap
    (function Limit.Nothing -> None
      | Limit.Incumbent (g, state) -> Some (state, g))
    (results limits)

let dups sface args =
  search Limit.results6 Limit.unwrap_sol6 sface args

let no_dups sface args =
  search Limit.results5 Limit.unwrap_sol5 sface args
