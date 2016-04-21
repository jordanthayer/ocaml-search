(**

    @author jordan
    @since 2011-07-11

   A Reimplementation of the DAS framework that austin first proposed
   back in '08, that didn't actually get published until the SoCS 2011
   paper Deadline Aware Search using Measurements of Search Behavior,
   Dionne, Thayer, Ruml
*)

type node_type =
  | Open
  | Reserve
  | Closed

type floats = {
  g : float;
  f : float;
  d : float;
  depth : float;
  generated : float; (* float so it can be time or exp count *)
}

type 'a node = {
  data : 'a;
  fp : floats;
  mutable qpos : int;
  mutable ntype : node_type;
}

(* Comparitors *)

let ordered_f a b =
  let afp = a.fp
  and bfp = b.fp in
  let af = afp.f
  and bf = bfp.f in
  af < bf ||
    (af = bf && afp.d < bfp.d) ||
    (af = bf && afp.d = bfp.d && afp.g >= bfp.g)


let speedy_order a b =
  let afp = a.fp
  and bfp = b.fp in
  let ad = afp.d
  and bd = bfp.d in
  ad < bd || (ad = bd && afp.g >= bfp.g)

(* utils *)
let wrap f =
  (** takes a function to be applied to the data payload
      such as the goal-test or the domain heuristic and
      wraps it so that it can be applied to the entire
      node *)
  (fun n -> f n.data)


let unwrap_sol s =
  (** Unwraps a solution which is in the form of a search node and presents
      it in the format the domain expects it, which is domain data followed
      by cost *)
  match s with
    | Limit.Nothing -> None
    | Limit.Incumbent (q,n) -> Some (n.data, n.fp.g)


let set_pos n i =
  (** Sets the location of a node, used by dpq's *)
  n.qpos <- i


let delay_exp info child =
  ((float info.Limit.expanded) -. child.fp.generated) *. child.fp.d

let exp_rate_time info () =
  (float info.Limit.expanded) /. (Sys.time() -. info.Limit.start_time)

let exp_rate _ () = 1.

(* Expand function *)
let make_expand expand hd info =
  let expand_das_node n =
    Limit.incr_exp info;
    let depth' = n.fp.depth +. 1. in
    List.map (fun (data, g) ->
      let h,d = hd data
      and _ = g -. n.fp.g in
      Limit.incr_gen info;
      let flt_data = { g = g; f = g +. h; d = d; depth = depth';
		       (* this will be done with a function call later
			  to allow time based generated instead of
			  expansion based generated *)
		       generated = float (info.Limit.expanded); } in
      { data = data; fp = flt_data; qpos = Dpq.no_position; ntype = Open })
      (expand n.data n.fp.g) in
  expand_das_node


let default_recover reserve openlist remaining =
  let r = ref remaining in
  while (!r > 0.) do
    (let n = Dpq.extract_first reserve in
     n.ntype <- Open;
     r := !r -. n.fp.d;
     Dpq.insert openlist n)
  done


let consider_child closed info key make_decision reserve openlist child =
  let insert decis =
    (match decis with
      | Open -> Dpq.insert openlist child
      | Reserve -> Dpq.insert reserve child
      | _ -> failwith "Bad decision!") in
  if not (Limit.promising_p info child) then Limit.incr_prune info
  else (let state = key child
        and decis = make_decision child in
	try
	  let prev = Htable.find closed state in
	  Limit.incr_dups info;
	  if (child.fp.f < prev.fp.f)
	  then (Htable.replace closed state child;
		let pos = prev.qpos in
		if (pos = Dpq.no_position) then Dpq.insert openlist child
		else (match prev.ntype with
		    (* prev doesn't need updated here because it is gone *)
		  | Open -> Dpq.remove openlist pos
		  | Reserve -> Dpq.remove reserve pos
		  | _ -> failwith "should have caught close in if state");
		insert decis)
	with Not_found ->
	  child.ntype <- decis;
	  insert decis;
	  Htable.add closed state child)


let speedy_search_phase closed_list info expand goal_p key root =
  let openlist = Dpq.create speedy_order set_pos 100 root in
  let make_decision _ = Open in
  let consider_kid = (consider_child closed_list info key
			make_decision openlist openlist) in
  let rec next () =
    if (not (Dpq.empty_p openlist)) && (not (Limit.halt_p info)) then
      let n = Dpq.extract_first openlist in
      n.qpos <- Dpq.no_position;
      n.ntype <- Closed;
      if not (Limit.promising_p info n) then (Limit.incr_prune info; next())
      else if goal_p n then Limit.new_incumbent info (Limit.Incumbent (0.,n))
      else (let kids = expand n in
	    List.iter consider_kid kids;
	    Limit.curr_q info (Dpq.count openlist);
	    next()) in
  Dpq.insert openlist root;
  next();
  openlist


let das_search_phase closed_list info root expand key goal_p
    prev_open deadline =
  let reserve = Dpq.create ordered_f set_pos 100 root
  and openlist = Dpq.create ordered_f set_pos 100 root in
  let get_delay = delay_exp info in
  let exp_rate = exp_rate info in
  let remaining () = float (deadline - info.Limit.expanded) in
  let make_decision node =
    let delay = get_delay node
    and rate = exp_rate() in
    if delay *. rate < (remaining()) then Open else Reserve in
  let consider_kid = (consider_child closed_list info key make_decision
			reserve openlist) in
  let init () =
    while (not (Dpq.empty_p prev_open)) do
      (let n = Dpq.extract_first prev_open in
       if Limit.promising_p info n then Dpq.insert openlist n
       else Limit.incr_prune info)
    done in
  let recover () =
    default_recover reserve openlist (remaining ()) in
  let rec next () =
    let empty_op = Dpq.empty_p openlist
    and empty_re = Dpq.empty_p reserve in
    let halt_b = Limit.halt_p info
    and rem_b = remaining() <= 0. in
    if (not (halt_b || (empty_op && empty_re) || rem_b)) then
      if empty_op then (recover (); next ())
      else (let n = Dpq.extract_first openlist in
	    n.qpos <- Dpq.no_position;
	    n.ntype <- Closed;
	    if not (Limit.promising_p info n)
	    then (Limit.incr_prune info;
		  next())
	    else if goal_p n
	    then (Limit.new_incumbent info (Limit.Incumbent (0.,n));
		  next ())
	    else (let kids = expand n in
		  List.iter consider_kid kids;
		  Limit.curr_q info ((Dpq.count openlist)+(Dpq.count reserve));
		  next())) in
  init ();
  next ()


let search hash eq key goal_p info expand root deadline =
  (* make the closed list *)
  let closed = Htable.create hash eq 100 in
  let sopen = speedy_search_phase closed info expand goal_p key root in
  let deadline' = deadline - (info.Limit.expanded) in
  Verb.pe Verb.always "Speedy phase finished w %i remaining\n%!" deadline';
  das_search_phase closed info root expand key goal_p sopen deadline

let dups sface args =
  let deadline = Search_args.get_int "Contract_astar.dups" args 0 in
  let key = wrap sface.Search_interface.key
  and hash = sface.Search_interface.hash
  and eq = sface.Search_interface.equals
  and goal = wrap sface.Search_interface.goal_p
  and hd = sface.Search_interface.hd
  and init_state = sface.Search_interface.initial in
  let hi, di = hd init_state in
  let init_fp = { g = 0.; f = hi; d = di; depth = 0.; generated = 0.; } in
  let root = { data = init_state; fp = init_fp;
	       qpos = Dpq.no_position; ntype = Open }
  and info = (Limit.make Limit.Nothing sface.Search_interface.halt_on ordered_f
		(Limit.make_default_logger (fun n -> n.fp.f)
		   (fun n -> sface.Search_interface.get_sol_length n.data))) in
  let expand = make_expand sface.Search_interface.domain_expand hd info in
  search hash eq key goal info expand root deadline;
  Limit.unwrap_sol6 unwrap_sol (Limit.results6 info)


(* EOF *)
