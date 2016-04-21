(**

    @author jordan
    @since 2011-11-26

   Anytime non-parametric astar,
*)

type 'a node = {
  data : 'a;
  mutable qpos : int;
  g : float;
  h : float;
  mutable e : float;
}

let ordered_p a b =
  a.e >= b.e ||
    (a.e = b.e && a.h < b.h) ||
    (a.e = b.e && a.h = b.h && a.g < b.g)


let better_p a b = (a.g +. a.h) <= (b.g +. b.h)
let setpos n i = n.qpos <- i


let compute_e ic g hval =
  (ic -. g) /. hval

let make_expand exp h =
  let incumbent_cost = ref infinity in
  (fun n ->
    let ce = (if Math.finite_p !incumbent_cost
              then compute_e !incumbent_cost
              else (fun _ h -> -.h)) in
    List.map (fun (d,g) ->
      let hval = h d in
      { data = d;
	g = g;
	h = hval;
	e = ce g hval;
        qpos = Dpq.no_position;}) (exp n.data n.g)), incumbent_cost


let search root key hash equals i goal_p expand incumbent_cost =
  let openlist = Dpq.create ordered_p setpos 100 root
  and nodes = Htable.create hash equals 100 in
  let consider_child n =
    Limit.incr_gen i;
    if not (Limit.promising_p i n)
    then Limit.incr_prune i
    else (let state = key n in
	  try
	    let prev = Htable.find nodes state in
	    Limit.incr_dups i;
	    if not (better_p prev n)
	    then (Htable.replace nodes state n;
		  let pos = prev.qpos in
		  if pos == Dpq.no_position
		  then Dpq.insert openlist n
		  else Dpq.swap openlist pos n)
	  with Not_found ->
	    Dpq.insert openlist n;
	    Htable.add nodes state n) in
  let rec expand_best () =
    if not ((Dpq.empty_p openlist) || Limit.halt_p i)
    then (let n = Dpq.extract_first openlist in
	  n.qpos <- Dpq.no_position;
	  if not (Limit.promising_p i n)
	  then (Limit.incr_prune i;
		Htable.remove nodes (key n);
		expand_best())
	  else if goal_p n
	  then (Limit.new_incumbent i (Limit.Incumbent (0.,n));
		let nic = n.g in
		let ue = compute_e nic in
		incumbent_cost := nic;
		Dpq.update_all openlist (fun n-> n.e <- ue n.g n.h);
		expand_best () (*anytime alg *))
	  else (let children = expand n in
		Limit.incr_exp i;
		List.iter consider_child children;
		Limit.curr_q i (Dpq.count openlist);
		expand_best())) in
  Dpq.insert openlist root;
  Htable.add nodes (key root) root;
  expand_best()

let wrap fn n =
  fn n.data

let unwrap_sol s =
  (** takes the solution found by the search and returns something that the
      domains can easily operate on *)
  match s with
    Limit.Incumbent (q,n) -> Some (n.data, n.g)
    | _ -> None


let dups sface _ =
  let module SI = Search_interface in
  let key = wrap sface.SI.key
  and hash = sface.SI.hash
  and equals = sface.SI.equals
  and goal_p = wrap sface.SI.goal_p
  and expand,inc_cost_ref = make_expand sface.SI.domain_expand sface.SI.h
  and root = { data = sface.SI.initial;
	       g = 0.;
	       h = sface.SI.h sface.SI.initial;
	       e = 0.;
	       qpos = Dpq.no_position; } in
  let i = (Limit.make Limit.Nothing sface.SI.halt_on better_p
	     (Limit.make_default_logger (fun n -> n.g) (fun n -> -1))) in
  search root key hash equals i goal_p expand inc_cost_ref;
  Limit.unwrap_sol6 unwrap_sol (Limit.results6 i)


(* EOF *)
