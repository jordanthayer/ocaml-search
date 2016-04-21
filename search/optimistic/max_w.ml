(** Max w Search:
    Uses the optimistic framework, but must maintain both lists at all times.
    sets the current weight on nodes to be
    bound * f(f_min) = g(f_min) + w * h(f_min) *)

type 'a node = {
  data: 'a;
  g : float;
  h : float;
  depth : int;
  mutable wf : float;
  mutable agg_pos : int;
  mutable clean_pos : int}

let set_agg_pos n i =
  n.agg_pos <- i

let set_clean_pos n i =
  n.clean_pos <- i

let get_agg_pos n =
  n.agg_pos

let get_clean_pos n =
  n.clean_pos

let ordered_wf a b =
  a.wf < b.wf ||
    (a.wf = b.wf &&
	a.g >= b.g)

let ordered_f a b =
  let af = a.g +. a.h
  and bf = b.g +. b.h in
    af < bf ||
      (af = bf && a.g >= b.g)

let just_f a b =
  (a.g +. a.h) <= (b.g +. b.h)


let alt_col_name = "max_wt"

let output_col_hdr () =
  Datafile.write_alt_colnames stdout alt_col_name ["current weight"; "step";]

let output_row v s =
  Datafile.write_alt_row_prefix stdout alt_col_name;
  Verb.pr Verb.always "%f\t%i\n" v s

let output_geometric_sched ?(duration = 2) output =
  let i = ref 0
  and next = ref duration in
    (fun v s ->
       if !i >= !next
       then (i := !i + 1;
	     next := (!next * 15) / 10;
	     output v s)
       else i := !i + 1)


let make_expand bound expand hfun =
  output_col_hdr();
  let wt = ref bound
  and ls = ref 0
  and output = output_geometric_sched output_row in
    output_row !wt !ls;
    (fun n ->
       List.map (fun (dat,g) ->
		     let h = hfun dat in
		       { data = dat;
			 g = g;
			 h = h;
			 wf = g +. !wt *. h;
			 depth = n.depth + 1;
			 agg_pos = Dpq.no_position;
			 clean_pos = Dpq.no_position;}) (expand n.data n.g)),

  (fun agg_queue f_queue i ->
     let f_min = Dpq.peek_first f_queue in
     let b_times_f_min = bound *. (f_min.g +. f_min.h) in
     let new_wt = (b_times_f_min -. f_min.g) /. (f_min.h)
     and step = i.Limit.expanded in
(*       try (* I can arrive at a node by a suboptimal path, then rearrive
	      allowing for f_min drops.  assert can fail *)
	 assert (new_wt >= !wt);*)
       if !wt <> new_wt then output !wt step;
       wt := new_wt)

       (*with Assert_failure str -> failwith (Wrutils.str "%f >= %f\n" new_wt !wt))*)


let wrap f =
  (fun n -> f n.data)


let make_get_node update_wt bound =
  (fun agg_queue clean_queue i ->
     update_wt agg_queue clean_queue i;
     match i.Limit.incumbent with
	 Limit.Nothing ->
	   (let n = Dpq.extract_first agg_queue in
	      (*Verb.pe Verb.debug "Get Node: No Incumbent...";*)
	      Dpq.remove clean_queue n.clean_pos;
	      (*Verb.pe Verb.debug " Done\n";*)
	      n)
       | Limit.Incumbent (qual,inc) ->
	   let fn = Dpq.peek_first clean_queue
	   and fpn = Dpq.peek_first agg_queue in
	     if (( fn.g +. fn.h) *. bound) >= inc.g
	     then raise Two_queue_framework.Done;
	     if fpn.wf < (inc.g)
	     then ((*Verb.pe Verb.debug "Got Clean Node...";*)
		   Dpq.remove agg_queue fpn.agg_pos;
		   Dpq.remove clean_queue fpn.clean_pos;
		   (*Verb.pe Verb.debug " Done\n";*)
		   fpn)
	     else ((*Verb.pe Verb.debug "Got Prime Node...";*)
		   Dpq.remove agg_queue fn.agg_pos;
		   Dpq.remove clean_queue fn.clean_pos;
		   (*Verb.pe Verb.debug " Done\n";*)
		   fn))


let unwrap_sol s =
  match s with
      Limit.Nothing -> None
    | Limit.Incumbent (q,n) -> Some (n.data, n.g)


let no_dups sface args =
  let bound = Search_args.get_float "Max_w.no_dups" args 0 in
  let expand,update_wt =
    make_expand
      bound
      sface.Search_interface.domain_expand
      sface.Search_interface.h
  and h_init = sface.Search_interface.h (sface.Search_interface.initial) in
  let search_interface = Search_interface.make
    ~node_expand:expand
    ~goal_p:(wrap sface.Search_interface.goal_p)
    ~key:(wrap sface.Search_interface.key)
    ~hash:sface.Search_interface.hash
    ~equals:(sface.Search_interface.equals)
    ~halt_on:sface.Search_interface.halt_on
    sface.Search_interface.domain
    { data = sface.Search_interface.initial;
      g = 0.;
      h = h_init;
      wf = bound *. h_init;
      depth = 0;
      agg_pos = Dpq.no_position;
      clean_pos = Dpq.no_position}
    just_f
    (Limit.make_default_logger (fun n -> n.g +. n.h)
       (wrap sface.Search_interface.get_sol_length)) in
    Limit.unwrap_sol5 unwrap_sol
      (Two_queue_framework.search
	 ordered_wf
	 ordered_f
	 set_agg_pos
	 set_clean_pos
	 (make_get_node update_wt bound)
	 search_interface.Search_interface.info
	 search_interface
	 bound)


let dups sface args =
  let bound = Search_args.get_float "Max_w.dups" args 0 in
  let expand,update_wt =
    make_expand
      bound
      sface.Search_interface.domain_expand
      sface.Search_interface.h
  and h_init = sface.Search_interface.h (sface.Search_interface.initial) in
  let search_interface = Search_interface.make
    ~node_expand:expand
    ~goal_p:(wrap sface.Search_interface.goal_p)
    ~key:(wrap sface.Search_interface.key)
    ~hash:sface.Search_interface.hash
    ~equals:(sface.Search_interface.equals)
    ~halt_on:sface.Search_interface.halt_on
    sface.Search_interface.domain
    { data = sface.Search_interface.initial;
      g = 0.;
      h = h_init;
      wf = bound *. h_init;
      depth = 0;
      agg_pos = Dpq.no_position;
      clean_pos = Dpq.no_position}
    just_f
    (Limit.make_default_logger (fun n -> n.g +. n.h)
       (wrap sface.Search_interface.get_sol_length)) in
    Limit.unwrap_sol6 unwrap_sol
      (Two_queue_framework.search_dups
	 ordered_wf
	 ordered_f
	 just_f
	 set_agg_pos
	 set_clean_pos
	 get_agg_pos
	 get_clean_pos
	 (make_get_node update_wt bound)
	 search_interface.Search_interface.info
	 search_interface
	 bound)
(* EOF *)
