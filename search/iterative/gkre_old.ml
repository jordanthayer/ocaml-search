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

  p_h : Nb.t;
  (* The learned conditional heuristic distribution.
     p(v|vp,tp,vgp,tgp) *)

  p_t : Nb.t;
  (* The learned conditional type distribution.
     p(t|v,vp,tp,vgp,tgp) *)

  (*
    We can compute:

    p(v,t|vp,tp,vgp,tgp) = p(v|vp,tp,vgp,tgp) * p(t|v,vp,tp,vgp,tgp)

    using p_h and p_t.
  *)

  b : float array array array array;
  (* Avg branching factor for nodes
     b.(v_p).(t_p).(v_{gp}).(t_{gp}). *)
}

(************************************************************)
(* Learning GKRE 2-step model with node types.              *)
(************************************************************)


let make_4d_array one two three four init =
  (** [make_4d_array one two three four init] make a 4d array with the
      dimensions [one]x[two]x[three]x[four] given the initial value
      [init]. *)
  Array.init
    one
    (fun _ ->
       Array.init
	 two
	 (fun _ ->
	    Array.init
	      three
	      (fun _ -> Array.make four init)))


let create hmax ntypes =
  (** [create hmax ntypes] creates a new 2-step model with node types
      given the max heuristic value and the number of node types. *)
  let f_max = max hmax ntypes		(* max feature value. *)
  and nheuristics = hmax + 1 in
    {
      hmax = hmax;
      ntypes = ntypes;
      p_h = Nb.make_no_smoothing 4 (* vp, tp, vgp, tgp    *) f_max nheuristics;
      p_t = Nb.make_no_smoothing 5 (* v, vp, tp, vgp, tgp *) f_max ntypes;
      b = make_4d_array nheuristics ntypes nheuristics ntypes 0.
    }


let make_next rand h t expand =
  (** [make_next rand h t expand] makes a 'next state function'
      suitable for [learn].  [rand] generates random instances of a
      domain, [h] computes the heuristic value on an instance of the
      domain, [t] gives the type of an instance and [expand] generates
      a list of children states.  This only throws End_of_file if the
      random instance generator does. *)
  let fill_queue q =
    (* Take a random instance and populate the queue with the grand
       chidren. *)
    let gp = rand () in
    let vgp = h gp
    and tgp = t gp in
    let parents = expand gp in
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
	 if !count mod 10_000 = 0 then Verb.pr Verb.optional "%d\n" !count;
	 next ()
       end)


let learn hmax ntypes next =
  (** [learn hmax ntypes next] learns a GKRE model given an instance
      generator function.  The learning stops when the [next] function
      throws an End_of_file exception.

      [inst] is a funciton that returns a tuple: (b, v, t, vp, tp,
      vgp, tgp) where b is the number of children of the generated
      instance, v is the heuristic of the instance, t is the type of
      the instance, vp is the heuristic of the parent, tp is the type
      of the parent and vgp and tgp are the heuristic and type of the
      grand parent respectively. *)
  let m = create hmax ntypes in
  let nheuristics = hmax + 1 in
  let b_count = make_4d_array nheuristics ntypes nheuristics ntypes 0 in
    try
      while true do
	let b, v, t, vp, tp, vgp, tgp = next () in
	  Nb.train m.p_h (v, [| vp; tp; vgp; tgp |]);
	  Nb.train m.p_t (t, [| v; vp; tp; vgp; tgp |]);
	  (* compute the avg branching factor. *)
	  let avg = m.b.(vp).(tp).(vgp).(tgp) in
	  let count = b_count.(vp).(tp).(vgp).(tgp) in
	  let b_avg = avg +. ((b -. avg) /. ((float count) +. 1.)) in
	    m.b.(vp).(tp).(vgp).(tgp) <- b_avg;
	    b_count.(vp).(tp).(vgp).(tgp) <- count + 1;
      done;
      m
    with End_of_file -> m


(************************************************************)
(* Computing GKRE 2-step model with node types.             *)
(************************************************************)


let prob model v t vp tp vgp tgp =
  (** [prob model v t vp tp vgp tgp] computes p(v,t|vp,tp,vgp,tgp). *)
  (Nb.prob model.p_h [| vp; tp; vgp; tgp |] v)
  *. (Nb.prob model.p_t [| v; vp; tp; vgp; tgp |] t)

let rec do_ngen cache model i hs hcs_ary d v t vp tp =
  (** [do_ngen cache model i hs hcs_ary d v t vp tp] gives the
      estimated number of generation with heuristic value [v] and type
      [t] using the 2-step model with node types.

      [nget_cache] -- cache of the result of calls to ngen with other
      values.

      [i] -- the level

      [hs] -- the heuristic value of the start state

      [hcs_ary] -- an array indexed on heuristic value where the entry
      is the number of children of the root node with
      that heuristic value.

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
	if vp = hs then hcs_ary.(v) else 0.
    | _ ->
	let sum = ref 0. in
	  for vgp = 0 to d - (i - 2) do
	    for tgp = 0 to model.ntypes - 1 do
	      let n_prev =
		ngen cache model (i - 1) hs hcs_ary d vp tp vgp tgp
	      and b = model.b.(vp).(tp).(vgp).(tgp)
	      and p = prob model v t vp tp vgp tgp
	      in sum := !sum +. (n_prev *. b *. p);
		if p > 1.0 then failwith (Printf.sprintf "p=%f" p);
	    done;
	  done;
	  !sum

and ngen cache model i hs hcs_ary d v t vp tp =
  (** [ngen cache model i hs hcs_ary d v t vp tp] tries to lookup the
      ngen value in the cache, if it is not there then it is computed.
      Without this caching the method is pretty much unusable since it
      spends so much time re-computing the same values over and over
      again. *)
  Cache.find_or_compute
    cache
    (fun (model, i, hs, hcs_ary, d, v, t, vp, tp) ->
       do_ngen cache model i hs hcs_ary d v t vp tp)
    (model, i, hs, hcs_ary, d, v, t, vp, tp)


let cdp model hs hcs =
  (** [cdp model hs hcs d] computes the conditional distribution
      prediction (number of nodes expanded by IDA* ) for a given start
      state and threshold (cost-bound).

      The start state is given in two pieces: [hs] is the heuristic of
      the start state and [hcs] is a list of the heuristic values of
      the children of the start state. *)
  let hcs_ary = Array.make (model.hmax + 1) 0. in
  let sum = ref 0. in
  let cache = Cache.create 100_000 in (* cache of ngen values *)
    (fun d ->
       List.iter (fun ch -> hcs_ary.(ch) <- hcs_ary.(ch) +. 1.) hcs;
       for i = 0 to d do
	 for v = 0 to d - i do
	   for t = 0 to model.ntypes - 1 do
	     for vp = 0 to d - (i - 1) do
	       for tp = 0 to model.ntypes - 1 do
		 Verb.pr Verb.debug "i=%02d/%02d " i d;
		 Verb.pr Verb.debug "v=%02d/%02d " v (d - i);
		 Verb.pr Verb.debug "t=%02d/%02d " t (model.ntypes - 1);
		 Verb.pr Verb.debug "vp=%02d/%02d " vp (d - (i - 1));
		 Verb.pr Verb.debug "tp=%02d/%02d%!\r" tp (model.ntypes - 1);
		 sum := !sum +. (ngen cache model i hs hcs_ary d v t vp tp)
	       done;
	     done;
	   done;
	 done;
       done;
       Verb.pr Verb.debug "\n";
       !sum)
