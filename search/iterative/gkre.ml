(** The generalized KRE formula.  Computing the number of nodes that
    IDA* will expand given a start state and a threshold (cost bound) by
    using conditional distributions.

    @author eaburns
    @since 2009-09-28
*)

type t = {
  hmax : int;
  (* The maximum heuristic value. *)

  ntypes : int;
  (* The number of different state types. *)

  p_ht : float array array array array array;
  (* p_ht.(t * hmax + v).(vp).(tp).(vgp).(tgp) *)

  b : float array array array array;
  (* Avg branching factor for nodes
     b.(v_p).(t_p).(v_{gp}).(t_{gp}). *)
}

(************************************************************)
(* Learning GKRE 2-step model with node types.              *)
(************************************************************)


let create hmax ntypes =
  (** [create hmax ntypes] creates a new 2-step model with node types
      given the max heuristic value and the number of node types. *)
  let nheuristics = hmax + 1 in
    {
      hmax = hmax;
      ntypes = ntypes;
      p_ht =
	Wrarray.make_5d
	  (nheuristics * ntypes)
	  nheuristics
	  ntypes
	  nheuristics
	  ntypes
	  0.;
      b = Wrarray.make_4d nheuristics ntypes nheuristics ntypes 0.
    }


let no_model = create 0 0
  (** A place-holder. *)


let make_next rand h t expand =
  (** [make_next rand h t expand] makes a 'next state function'
      suitable for [learn].  [rand] generates random instances of a
      domain, [h] computes the heuristic value on an instance of the
      domain, [t] gives the type of an instance and [expand] generates
      a list of children states.  This only throws End_of_file if the
      random instance generator does. *)
  let gps = ref 0 in
  let fill_queue q =
    (* Take a random instance and populate the queue with the grand
       chidren. *)
    try
      let h n = int_of_float (h n) in
      let gp = rand () in
      let vgp = h gp
      and tgp = t gp in
      let parents = expand gp in
	incr gps;
	List.iter (fun p ->
		     let vp = h p
		     and tp = t p in
		     let nodes = expand p in
		       List.iter (fun n ->
				    let v = h n
				    and t = t n
				    and b = float (List.length (expand n)) in
				      Queue.push (b, v, t, vp, tp, vgp, tgp) q)
			 nodes)
	  parents
    with End_of_file -> begin
      Verb.pr Verb.debug "%d grand parents seen\n%!" !gps;
      raise End_of_file
    end
  in
  let q = Queue.create () in
    (fun () ->
       if Queue.is_empty q then fill_queue q;
       Queue.take q)


let make_next_num rand h t expand num =
  (** [make_next_num rand h t expand num] makes a 'next state
      function' that generates [num] next states. *)
  let next = make_next rand h t expand in
  let count = ref 0 in
    (fun () ->
       if !count = num
       then raise End_of_file
       else begin
	 incr count;
	 if !count mod 100_000 = 0
	 then Verb.pr Verb.optional "%d\n" (num - !count);
	 next ()
       end)


let vt_val hmax v t = t * (hmax + 1) + v
  (** [vt_val hmax v t] get the index into the heuristic value and
      type array. *)


let vt_max hmax ntypes = ((hmax + 1) * ntypes) - 1


let learn hmax ntypes next =
  (** [learn hmax ntypes next] learns a GKRE model given an instance
      generator function.  The learning stops when the [next] function
      throws an End_of_file exception.

      [next] is a funciton that returns a tuple: (b, v, t, vp, tp,
      vgp, tgp) where b is the number of children of the generated
      instance, v is the heuristic of the instance, t is the type of
      the instance, vp is the heuristic of the parent, tp is the type
      of the parent and vgp and tgp are the heuristic and type of the
      grand parent respectively. *)
  let m = create hmax ntypes in
  let nheuristics = hmax + 1 in
  let b_count = Wrarray.make_4d nheuristics ntypes nheuristics ntypes 0 in
  let sums = Wrarray.make_4d nheuristics ntypes nheuristics ntypes 0. in
  let counts = Array.make nheuristics 0 in
  let num_insts = ref 0 in
    try
      while true do
	let b, v, t, vp, tp, vgp, tgp = next () in

	  counts.(v) <- counts.(v) + 1;

	  incr num_insts;

	  m.p_ht.(vt_val hmax v t).(vp).(tp).(vgp).(tgp) <-
	    m.p_ht.(vt_val hmax v t).(vp).(tp).(vgp).(tgp) +. 1.;

	  sums.(vp).(tp).(vgp).(tgp) <- sums.(vp).(tp).(vgp).(tgp) +. 1.;

	  (* compute the avg branching factor. *)
	  let avg = m.b.(v).(t).(vp).(tp) in
	  let count = b_count.(v).(t).(vp).(tp) in
	  let b_avg = avg +. ((b -. avg) /. ((float count) +. 1.)) in
	    m.b.(v).(t).(vp).(tp) <- b_avg;
	    b_count.(v).(t).(vp).(tp) <- count + 1;
      done;
      failwith "Gkre.learn: should never reach here"
    with End_of_file ->
      Verb.pr Verb.debug "%d instances\n%!" !num_insts;
      for vp = 0 to hmax do
	if counts.(vp) = 0
	then Verb.pr Verb.optional "No nodes with h=%d were seen\n%!" vp
	else
	  Verb.pr Verb.debug "%d nodes with h=%d were seen\n%!"
	    counts.(vp) vp;

	(* in retrospect, this is a terrible loop with no invariants
	   pulled out... *)
	for tp = 0 to ntypes - 1 do
	  for vgp = 0 to hmax do
	    for tgp = 0 to ntypes - 1 do
	      for vt = 0 to vt_max hmax ntypes do
		if m.p_ht.(vt).(vp).(tp).(vgp).(tgp) = 0.
		  || sums.(vp).(tp).(vgp).(tgp) = 0.
		then
		  m.p_ht.(vt).(vp).(tp).(vgp).(tgp) <- 0.
		else
		  m.p_ht.(vt).(vp).(tp).(vgp).(tgp) <-
		    (m.p_ht.(vt).(vp).(tp).(vgp).(tgp)
		     /. sums.(vp).(tp).(vgp).(tgp))
	      done;
	    done;
	  done;
	done;
      done;
      m


(************************************************************)
(* Computing GKRE 2-step model with node types.             *)
(************************************************************)

let prob model v t vp tp vgp tgp =
  (** [prob model v t vp tp vgp tgp] computes p(v,t|vp,tp,vgp,tgp). *)
  try
    model.p_ht.(vt_val model.hmax v t).(vp).(tp).(vgp).(tgp)
  with e -> failwith (Wrutils.str "prob: %s" (Printexc.to_string e))


let rec do_ngen ?(br=None) cache model i hs ts children d v t vp tp =
  (** [do_ngen ?br cache model i hs ts children d v t vp tp] gives the
      estimated number of generation with heuristic value [v] and type
      [t] using the 2-step model with node types.

      [nget_cache] -- cache of the result of calls to ngen with other
      values.

      [i] -- the level

      [hs] -- the heuristic value of the start state

      [ts] -- the type of the start state

      [children] -- an array indexed on heuristic and type where the
      entry is the number of children of the root node with that
      heuristic value.

      [d] -- the cost-bound (threshold)

      [v] -- the heuristic value of the node

      [t] -- the type of the node

      [vp] -- the heuristic of the parent

      [tp] -- the type of the parent
  *)
  match i with
    | 0 -> 0.
    | 1 ->
	(* The base case is for i = 1 *)
	(try
	   if vp = hs && tp = ts then children.(v).(t) else 0.
	 with e -> failwith (Wrutils.str "ngen_base v=%d t=%d: %s"
			       v t (Printexc.to_string e)))
    | _ ->
	let sum = ref 0. in
	  for vgp = 0 to d - (i - 2) do
	    for tgp = 0 to model.ntypes - 1 do
	      let vgp = min vgp model.hmax in
	      let b = match br with
		| None ->
		    begin try
		      model.b.(vp).(tp).(vgp).(tgp)
		    with e ->
		      failwith (Wrutils.str
				  "ngen b vp=%d tp=%d vgp=%d tgp=%d: %s"
				  vp tp vgp tgp (Printexc.to_string e))
		    end
		| Some f -> f vp tp vgp tgp
	      in
	      let p = if b > 0. then prob model v t vp tp vgp tgp else 0. in
		(* Try to avoid doing computation when it is zero. *)
		if b > 0. && p > 0.
		then begin
		  let n_prev =
		    ngen cache model (i - 1) hs ts children d vp tp vgp tgp
		  in
		    sum := !sum +. (n_prev *. b *. p);
		    if p > 1.0 then failwith (Printf.sprintf "p=%f" p);
		end
	    done;
	  done;
	  !sum


and ngen ?(br=None) cache model i hs ts children d v t vp tp =
  (** [ngen br cache model i hs hcs_ary d v t vp tp] tries to lookup
      the ngen value in the cache, if it is not there then it is
      computed.  Without this caching the method is pretty much
      unusable since it spends so much time re-computing the same
      values over and over again. *)
  Cache.find_or_compute
    cache
    (fun (model, i, hs, ts, children, d, v, t, vp, tp) ->
       do_ngen ~br:br cache model i hs ts children d v t vp tp)
    (model, i, hs, ts, children, d, v, t, vp, tp)


let cdp ?br model hs ts ht_cs =
  (** [cdp ?br model hs ts ht_cs d] computes the conditional
      distribution prediction (number of nodes expanded by IDA* ) for
      a given start state and threshold (cost-bound).  [br] is an
      optional function that will accept all of the features and
      return the branching factor.

      The start state is given in two pieces: [hs] is the heuristic of
      the start state and [ht_cs] is a list of the (heuristic * type)
      tuples of the children of the start state. *)
  let children = Array.make_matrix (model.hmax + 1) model.ntypes 0. in
  let sum = ref 0. in
  let cache = Cache.create 100_000 in (* cache of ngen values *)
    List.iter (fun (h, t) ->
		 try
		   children.(h).(t) <- children.(h).(t) +. 1.
		 with e ->
		   Printf.printf "hmax=%d, ntypes=%d\n%!"
		     model.hmax model.ntypes;
		   failwith (Wrutils.str "cdp iter: h=%d t=%d: %s"
				       h t (Printexc.to_string e)))
      ht_cs;
    (fun d ->
       for i = 0 to d do
	 for v = 0 to d - i do
	   for t = 0 to model.ntypes - 1 do
	     for vp = 0 to d - (i - 1) do
	       for tp = 0 to model.ntypes - 1 do
		 let v = min v model.hmax
		 and vp = min vp model.hmax in
		   sum :=
		     !sum +.
		       (ngen ~br:br cache model i hs ts children d v t vp tp)
	       done;
	     done;
	   done;
	 done;
       done;
       Verb.pr Verb.debug "\n";
       !sum)
