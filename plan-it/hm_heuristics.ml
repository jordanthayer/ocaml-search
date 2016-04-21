(** Some of the h^m heuristics.

    @author eaburns
    @since 2010-07-23
*)

open Sas
open Float_ref

type achiever = {
  op : operator option;
  precs : int list;
  effs : int list;
}

type float_ref = { mutable fl_val : float }

let achiever_cost a =
  (** [achiever_cost a] gets the cost of the given achiever.  Axioms
      are free. *)
  match a.op with | Some o -> (* o.o_cost *) 1. | None -> 0.


let prop props (vr, vl) = props.(vr).(vl)
  (** [prop props assign] gets the propositionalized form of the given
      assignment. *)


let achievers_of_operator props op =
  (** [achievers_of_operator props op] make a list of achiever
      structures from the effects of [op]. *)
  let precs = List.map  (prop props) op.o_conds in
  let ucond_effs = List.map (prop props) op.o_uncond_effs in
  let cond_effs =
    List.map (fun (effs, (vr, vl)) ->
		let effs = List.map (fun (vr, vl) -> props.(vr).(vl)) effs in
		  { op = Some op;
		    precs = props.(vr).(vl) :: precs;
		    effs = effs @ ucond_effs;
		  })
      op.o_cond_effs
  in { op = Some op; precs = precs; effs = ucond_effs; } :: cond_effs


let achiever_of_axiom props ax =
  (** [achiever_of_axiom props ax] makes an achiever from the given
      axiom. *)
  { op = None;
    precs = List.map (prop props) ax.a_conds;
    effs = [ props.(ax.a_var).(ax.a_val) ];
  }


let for_each_achiever props operators axioms f =
  (** [for_each_achiever props operators axioms f] evaluate [f] on each
      achiever. *)
  List.iter
    (fun op ->
       let achievers = achievers_of_operator props op in
	 List.iter (fun a -> f a) achievers)
    operators;
  Array.iter
    (fun axs ->
       List.iter (fun ax -> f (achiever_of_axiom props ax)) axs)
    axioms


let make_p_achievers nprops props operators axioms =
  (** [make_p_achievers nprops props operators axioms] makes an array
      indexed on a proposition that gives a list of the operators and
      axioms that achieve the proposition. *)
  let ps = Array.create nprops [] in
  let add_effs a = List.iter (fun p -> ps.(p) <- a :: ps.(p)) a.effs in
    for_each_achiever props operators axioms add_effs;
    ps


let union lista listb =
  (** [union lista listb] creates a list that is the union of the two
      lists. *)
  (List.filter (fun e -> not (List.mem e listb)) lista) @ listb


let make_p_graph nprops props operators axioms =
  (** [make_p_graph nprops props operators axioms] makes a graph
      specified in two parts: 1) for each proposition, what are its
      achievers and 2) for each proposition, what other propositions
      'depend' on it.  A proposition p depends on q if there is an
      operator where p is a precondition and q is an effect. *)
  let achievers = Array.create nprops [] in
  let dependents = Array.create nprops [] in
  let handle_achiever a =
    List.iter (fun e -> achievers.(e) <- a :: achievers.(e);) a.effs;
    List.iter
      (fun p -> dependents.(p) <- union dependents.(p) a.effs)
      a.precs
  in
    for_each_achiever props operators axioms handle_achiever;
    achievers, dependents


(** {1 The h^1 heuristic} ****************************************)

module H1 = struct
  (** The h^1 heuristic.  This should be the same as h_max. *)

  let reset dependents props nprops state g s0 =
    (** [reset dependents props nprops state g s0] resets the [g]
	function and sets the bitset [s0] to contain only those
	propositions thate are true in the initial state.  The result
	is a queue of all of the propositions that may need to be
	updated based on the initial state. *)
    let nprops = Array.length g in
    let queue = Queue.create () in
    let in_queue = Bitset.create nprops in
      for i = 0 to nprops - 1 do g.(i) <- infinity done;
      Bitset.clear s0;
      Array.iteri
	(fun vr vl ->
	   let p = props.(vr).(vl) in
	     g.(p) <- 0.;
	     Bitset.insert s0 p;
	     List.iter (fun p ->
			  Queue.push p queue;
			  Bitset.insert in_queue p;)
	       dependents.(p);)
	state;
      queue, in_queue


  let prec_g_value g a =
    (** [prec_g_value g a] gets the G value for the given set of
	preconditions. *)
    let g' = { fl_val = neg_infinity } in
      List.iter (fun p ->
		   let gvl = g.(p) in
		     if gvl > g'.fl_val then g'.fl_val <- gvl)
	a.precs;
      g'.fl_val


  let update achievers dependents props g queue in_queue p =
    (** [update achievers dependents props g queue in_queue p] updates
	the proposition [p]. *)
    let g' = { fl_val = infinity } in
      List.iter (fun a ->
		   let op_cost = achiever_cost a in
		   let c = op_cost +. (prec_g_value g a) in
		     if c < g'.fl_val then g'.fl_val <- c)
	achievers.(p);
      if g'.fl_val <> g.(p) then begin
	g.(p) <- g'.fl_val;
	List.iter (fun p' ->
		     if not (Bitset.mem in_queue p')
		     then begin
		       Queue.add p' queue;
		       Bitset.insert in_queue p';
		     end)
	  dependents.(p);
      end


  let update_all achievers dependents nprops props g queue in_queue s0 =
    (** [update_all achievers dependents nprops props g queue in_queue s0]
	performs the dynamic programming updates until nothing
	changes. *)
    while not (Queue.is_empty queue) do
      let p = Queue.take queue in
	Bitset.remove in_queue p;
	if not (Bitset.mem s0 p)
	then update achievers dependents props g queue in_queue p
    done


  let make_progression domain goal =
    (** [make_progression domain goal] makes the h^1 heuristic
	function for progression. *)
    let props = domain.props and prop_invs = domain.prop_invs in
    let nprops = Array.length prop_invs in
    let ops = domain.operators and axs = domain.axioms in
    let achievers, dependents = make_p_graph nprops props ops axs in
    let g = Array.create nprops infinity in
    let goal_props =
      List.map (fun (vr, vl) -> props.(vr).(vl)) goal.s_assigns
    in
    let s0 = Bitset.create nprops in
      (fun state ->
	 let h = { fl_val = neg_infinity } in
	 let queue, in_queue =
	   reset dependents props nprops state.s_vars g s0
	 in
	   update_all achievers dependents nprops props g queue in_queue s0;
	   List.iter (fun p ->
			let gvl = g.(p) in
			  if gvl > h.fl_val then h.fl_val <- gvl)
	     goal_props;
	   h.fl_val)
end

(** {1 The h^2 heuristic} ****************************************)

let negates prop_invs q p =
  (** [negates prop_invs q p] tests if [p] negates [q]. *)
  if p <> q
  then begin
    let vrp, _ = prop_invs.(p) and vrq, _ = prop_invs.(q) in
      vrp = vrq
  end else false


let rec pair_up = function
    (** [pair_up ps] get all pairs of propositions in the list. *)
  | [] -> []
  | p :: ps -> (List.map (fun q -> p, q) ps) @ (pair_up ps)


module H2 = struct

  type pair = {
    p : int;
    q : int;
    id : int;
    mutable incoming : in_arc list;
    mutable outgoing : out_arc list;
    mutable g : float;
  }

  and in_arc = {
    sources : pair list;
    achiever : achiever;

    mutable max_source : pair;
    mutable max_source_vl : float;
  }

  and out_arc = {
    target : pair;
    rev : in_arc;
  }


  let no_pair = { p = ~-1;
		  q = ~-1;
		  id = ~-1;
		  incoming = [];
		  outgoing = [];
		  g = nan; }


  let get_pair pairs p q =
    (** [get_pair pairs p q] gets the pair from the pair set. *)
    if p < q then pairs.(p).(q - p) else pairs.(q).(p - q)


  let pairs_of_props pairs = function
      (** [g_value pairs pairs props] gets the g-value of the given
	  propositions. *)
    | p :: [] -> [ pairs.(p).(p) ]
    | p :: q :: [] -> [ get_pair pairs p q ]
    | ps -> List.map (fun (p, q) -> get_pair pairs p q) (pair_up ps)


  let add_arc ?pre_add pairs achiever dst =
    (** [add_arc ?pre_add pairs achiever dst] adds arcs from the
	precondition pairs of [achiever] to [dst].  *)
    let precs = match pre_add with
      | None -> achiever.precs
      | Some p -> p :: achiever.precs
    in
    let sources = pairs_of_props pairs precs in
    let in_arc = { sources = sources;
		   achiever = achiever;
		   max_source = List.hd sources;
		   max_source_vl = infinity;
		 }
    in
      dst.incoming <- in_arc :: dst.incoming;
      List.iter (fun src ->
		   let out_arc = { target = dst; rev = in_arc } in
		     src.outgoing <- out_arc :: src.outgoing)
	sources


  let pair_array nprops =
    (** [pair_array nprops] creates an initial pair array. *)
    let new_pair id_base p q =
      { p = p; q = q; id = id_base + q;
	incoming = []; outgoing = []; g = nan; }
    in
      Array.init nprops
	(fun p ->
	   let id_base = p * nprops in
	     Array.init (nprops - p) (fun q -> new_pair id_base p (q - p)))


  let make_pairs nprops negs achievers =
    (** [make_pairs nprops negs achievers] make all pairs. *)
    let pairs = pair_array nprops in
      for p = 0 to nprops - 1 do
	let p_achs = achievers.(p) in
	  for q = p to nprops - 1 do
	    let pair = pairs.(p).(q - p) in
	      List.iter
		(fun a ->
		   if List.mem q a.effs
		   then add_arc pairs a pair
		   else
		     if not (List.exists (fun r -> r = q || negs q r) a.effs)
		     then add_arc ~pre_add:q pairs a pair;)
		p_achs;
	      List.iter
		(fun a ->
		   if not (List.exists (fun r -> r = p || negs p r) a.effs)
		   then add_arc ~pre_add:p pairs a pair;)
		achievers.(q);
	  done
      done;
      pairs


  let max_pair pairs =
    (** [max_pair pairs] get the value and pair that has the max cost
	among [pairs]. *)
    let max_p = ref (List.hd pairs) in
    let max_vl = { fl_val = !max_p.g } in
    let rec get_max_pair = function
      | [] -> max_vl.fl_val, !max_p
      | p :: _ when p.g = infinity -> infinity, p
      | p :: ps ->
	  let g = p.g in
	    if g > max_vl.fl_val
	    then begin
	      max_vl.fl_val <- g;
	      max_p := p;
	    end;
	    get_max_pair ps
    in get_max_pair (List.tl pairs)


  let update queue in_queue pairs pair =
    (** [update queue in_queue pairs pair] updates the value of the
	given pair. *)
    let g' = { fl_val = infinity } in
      List.iter (fun arc ->
		   let op_cost = achiever_cost arc.achiever in
		   let max_vl, max_src = max_pair arc.sources in
		   let c = op_cost +. max_vl in
		     arc.max_source <- max_src;
		     arc.max_source_vl <- max_vl;
		     if c < g'.fl_val then g'.fl_val <- c)
	pair.incoming;
      let g' = g'.fl_val in
	if g' <> pair.g
	then begin
	  pair.g <- g';
	  List.iter (fun arc ->
		       let target = arc.target in
		       let tid = target.id in
		       let rev = arc.rev in
			 if (g' > rev.max_source_vl || rev.max_source == pair)
			   && not (Bitset.mem in_queue tid)
			 then begin
			   Queue.push target queue;
			   Bitset.insert in_queue tid;
			 end)
	    pair.outgoing
	end


  let update_all queue in_queue pairs s0 =
    (** [update_all queue in_queue pairs s0] updates the g
	values of the pairs. *)
    let count = ref 0 in
      while not (Queue.is_empty queue) do
	let pair = Queue.take queue in
	  Bitset.remove in_queue pair.id;
	  if not (Bitset.mem s0 pair.id)
	  then begin
	    update queue in_queue pairs pair;
	    incr count
	  end
      done;
      !count


  let reset pairs npairs s0 =
    (** [reset pairs npairs s0] resets the cost values for the pairs
	given the new start state [s0].  The result is the queue of
	pairs that need updating. *)
    let queue = Queue.create () in
    let in_queue = Bitset.create npairs in
      for i = 0 to (Array.length pairs) - 1 do
	let pairs = pairs.(i) in
	  for j = 0 to (Array.length pairs) - 1 do
	    let pair = pairs.(j) in
	      if Bitset.mem s0 pair.id
	      then begin
		pair.g <- 0.;
		(* no need to update the reverses of the outgoing arcs
		   here (max_source and max_source_vl) because they will
		   be updated soon anyway. *)
		List.iter
		  (fun arc ->
		     let target = arc.target in
		       Queue.push target queue;
		       Bitset.insert in_queue target.id;)
		  pair.outgoing;
	      end else pair.g <- infinity
	  done
      done;
      queue, in_queue


  let make_s0 nprops props pairs state =
    (** [make_s0 nprops props state] makes a bitset containing each
	pair of propositions that are true in the given state. *)
    let s0 = Bitset.create (nprops * nprops) in
    let ps = ref [] in
      for vr = 0 to (Array.length state) - 1 do
	let p = props.(vr).(state.(vr)) in
	  Bitset.insert s0 (get_pair pairs p p).id;
	  List.iter (fun q -> Bitset.insert s0 (get_pair pairs p q).id) !ps;
	  ps := p :: !ps;
      done;
      s0


  let rec goal_pairs ?(accum=[]) ?(gprops=[]) pairs props = function
      (** [goal_pairs ?accum ?gprops pairs props] get the list of
	  pairs that are associated with the goal. *)
    | [] -> List.map (fun (p, q) -> get_pair pairs p q) accum
    | (vr, vl) :: assigns ->
	let p = props.(vr).(vl) in
	  goal_pairs
	    ~accum:((p, p) :: (List.map (fun q -> p, q) gprops) @ accum)
	    ~gprops:(p :: gprops)
	    pairs props assigns


  let make_progression domain goal =
    (** [make_progression domain goal] makes a function that computes
	the h2 heuristic value of a state.  This is to be used with
	progression search. *)
    let ops = domain.operators and axs = domain.axioms in
    let props = domain.props and prop_invs = domain.prop_invs in
    let nprops = Array.length prop_invs in
    let npairs = nprops * nprops in
    let negates = negates prop_invs in
    let achievers, _ = make_p_graph nprops props ops axs in
    let pairs = make_pairs nprops negates achievers in
    let goal_pairs = goal_pairs pairs props goal.s_assigns in
      Verb.pr Verb.debug "created h2, %d props, %d pairs, %d goal pairs\n%!"
	nprops npairs (List.length goal_pairs);
      (fun state ->
	 let s0 = make_s0 nprops props pairs state.s_vars in
	 let queue, in_queue = reset pairs npairs s0 in
	 let h = { fl_val = neg_infinity } in
(*
	 let count, time =
	   Wrsys.with_time (fun () -> update_all queue in_queue pairs s0)
	 in
	   Verb.pr Verb.debug "%d updates in %f seconds\n%!" count time;
*)
	   ignore (update_all queue in_queue pairs s0);
	   List.iter (fun p ->
			let g = p.g in
			  if g > h.fl_val then h.fl_val <- g)
	     goal_pairs;
	   h.fl_val)
end
