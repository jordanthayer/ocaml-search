(** Another re-write of the incremental model. *)

(*module Hist = Histogram*)
(*
*)

module Int_tbl = Hashtbl.Make(struct
				type t = int
				let equal (a:t) b = a = b
				let hash = Hashtbl.hash
			      end)

module Two_int_tbl = Hashtbl.Make(struct
				      type t = int * int
				      let equal ((a, b):t) (x, y) =
					a = x && b = y
				      let hash = Hashtbl.hash
				    end)

module Three_int_tbl = Hashtbl.Make(struct
				      type t = int * int * int
				      let equal ((a, b, c):t) (x, y, z) =
					a = x && b = y && c = z
				      let hash = Hashtbl.hash
				    end)

module Two_int_tbl_window = Sliding_window.Make(struct
						  type t = Hist.t Two_int_tbl.t
						  let init _ =
						    Two_int_tbl.create 100
						end)


type t = {
  parents : parent Three_int_tbl.t;

  mutable prev_bound : float;
  warm_start : warm_start;
  max_bins : int;
}

and parent = {
  mutable pcount : int;
  depth : int;
  pd : int;
  pt : int;
  child_types : child_type Int_tbl.t;
  (* children by type. *)
}

and child_type = {
  mutable ccount : int;
  dds : Hist.t Int_tbl.t;
}

and warm_start = {
  mutable curr_depth : int;
  mutable warm_fs : Hist.t;
  warm_nodes : Hist.t Two_int_tbl.t;
  warm_f_dists : Hist.t Two_int_tbl.t Two_int_tbl_window.t;
  (* The child f distributions at each incomplete depth. *)
}

let backoff = ~-1

let cutoff_weight = 0.001
  (* Minimum weight in a histogram where we will assume that
     simulation should continue. *)

let verb_update = Verb.toplvl

let verb_hists = Verb.debug

let verb_simulate = Verb.debug

let verb_summary = Verb.toplvl

(** {6 Accessing Hash Tables} ****************************************)

let get_parent model ~pd ~pt ~depth =
  (** [get_parent model ~pd ~pt ~depth] gets the parent structure. *)
  let key = pt, pd, depth in
    try Three_int_tbl.find model.parents key
    with Not_found ->
      let parent = { pcount = 0;
		     depth = depth;
		     pd = pd;
		     pt = pt;
		     child_types = Int_tbl.create 100; } in
	Three_int_tbl.add model.parents key parent;
	parent

let get_parent_or_backoff_full model ~pd ~pt ~depth =
  (** [get_parent_or_backoff_full model ~pd ~pt ~depth] gets the
      parent structure or a backed off model.  This function will back
      off in three stages to the most general of all models. *)
  let parents = model.parents in
  let key = pt, pd, depth in
    begin try (Three_int_tbl.find parents key)
    with Not_found ->
      let key' = pt, pd, backoff in
        begin try (Three_int_tbl.find parents key')
        with Not_found ->
          let key'' = pt, backoff, backoff in
            begin try (Three_int_tbl.find parents key'')
            with Not_found ->
              let key''' = backoff, backoff, backoff in
                try (Three_int_tbl.find parents key''')
                with Not_found ->
                  failwith
                    (Printf.sprintf
                       "unable to find a model for pd=%d, pt=%d, depth=%d\n"
                       pd pt depth)
            end
        end
    end

let get_parent_or_backoff_one model ~pd ~pt ~depth =
  (** [get_parent_or_backoff_one model ~pd ~pt ~depth] gets the
      parent structure or a backed off model.  This function will back
      off depth only. *)
  let parents = model.parents in
  let key = pt, pd, depth in
    begin try (Three_int_tbl.find parents key)
    with Not_found ->
      let key' = pt, pd, backoff in
        begin try (Three_int_tbl.find parents key')
        with Not_found ->
	  { pcount = 0;
	    depth = ~-1;
	    pd = ~-1;
	    pt = ~-1;
	    child_types = Int_tbl.create 1;
	  }
        end
    end


let get_parent_or_backoff_two model ~pd ~pt ~depth =
  (** [get_parent_or_backoff_two model ~pd ~pt ~depth] gets the
      parent structure or a backed off model.  This function will back
      off depth then parent d-estimate. *)
  let parents = model.parents in
  let key = pt, pd, depth in
    begin try (Three_int_tbl.find parents key)
    with Not_found ->
      let key' = pt, pd, backoff in
        begin try (Three_int_tbl.find parents key')
        with Not_found ->
	  let key' = pt, backoff, backoff in
            begin try (Three_int_tbl.find parents key')
            with Not_found -> { pcount = 0;
				depth = ~-1;
				pd = ~-1;
				pt = ~-1;
				child_types = Int_tbl.create 1;
			      }
            end
	end
    end


let rec get_parent_or_backoff_up model ~pd ~pt ~depth =
  (** [get_parent_or_backoff_up model ~pd ~pt ~depth] gets the parent
      structure or a backed off model.  This function will back off by
      checking the previous depth up to the completed depth. *)
  let parents = model.parents in
  let key = pt, pd, depth in
    begin try (Three_int_tbl.find parents key)
    with Not_found ->
      if depth < model.warm_start.curr_depth
      then { pcount = 0;
	     depth = ~-1;
	     pd = ~-1;
	     pt = ~-1;
	     child_types = Int_tbl.create 1;
	   }
      else get_parent_or_backoff_up model ~pd ~pt ~depth:(depth - 1)
    end


let rec get_parent_or_nothing model ~pd ~pt ~depth =
  (** [get_parent_or_nothing model ~pd ~pt ~depth] gets the parent
      structure or nothing. *)
  let parents = model.parents in
  let key = pt, pd, depth in
    begin try (Three_int_tbl.find parents key)
    with Not_found -> { pcount = 0;
			depth = ~-1;
			pd = ~-1;
			pt = ~-1;
			child_types = Int_tbl.create 1;
		      }
    end


let get_parent_or_backoff model ~pd ~pt ~depth =
  (** [get_parent_or_backoff model ~pd ~pt ~depth] gets the parent
      structure or a backed off model. *)
  get_parent_or_backoff_one model ~pd ~pt ~depth
(*
  get_parent_or_backoff_full model ~pd ~pt ~depth

  get_parent_or_backoff_two model ~pd ~pt ~depth
  get_parent_or_nothing model ~pd ~pt ~depth
  get_parent_or_backoff_up model ~pd ~pt ~depth
*)


let get_child_type model parent ct =
    (** [get_child model parent ct] gets the child_type structure given
	the delta d value and the parent. *)
  try Int_tbl.find parent.child_types ct
  with Not_found ->
    let child = { ccount = 0;
		  dds = Int_tbl.create 100 } in
    Int_tbl.add parent.child_types ct child;
    child


let get_child_hist model childt cdd =
  (** [get_child_hist model childt cdd] gets the child's histogram
      structure for the given delta d. *)
  try Int_tbl.find childt.dds cdd
  with Not_found ->
    let h = Hist.make model.max_bins in
    Int_tbl.add childt.dds cdd h;
    h


let get_nodes model tbl ~t ~d =
  try Two_int_tbl.find tbl (t, d)
  with Not_found ->
    let h = Hist.make model.max_bins in
      Two_int_tbl.add tbl (t, d) h;
      h


let set_nodes tbl ~t ~d v =
  Two_int_tbl.replace tbl (t, d) v


(** {6 Histogram Convenience} ****************************************)


let add_hists max_bins lst =
  (** [add_hists max_bins lst] adds a (possibly empty) list of
      histograms. *)
  if lst = [] then Hist.make max_bins else Hist.add lst


let add_two a b =
  (** [add_two a b] adds the two histograms. *)
  if Hist.has_mass a
  then begin
    if Hist.has_mass b
    then Hist.add [a; b]
    else a
  end else b


let add_to a b = a := add_two !a b
  (** [add_two a b] adds [b] to the histogram reference [a]. *)


let prune_histograms a b bound desired =
  (** [prune_histograms a b bound desired] tries to find a better
      pruning bound.  Prunes the histograms and returns the new
      bound. *)
  let bound' = (if desired < infinity
		then (Hist.bound_for_wted_combo desired a b 1.0)
		else bound) in
  let bound'' = Math.fmin bound' bound in
    Hist.prune_value_right bound'' a;
    Hist.prune_value_right bound'' b;
    bound''


(** Prints a histogram if debugging is enabled.

    NOTE: this function does not work with the Histogram module, only
    the Hist module. *)
let debug_print_hist lvl name hist =
(*
  if Verb.level lvl
  then begin
  Wrutils.pr "-------------------- %s --------------------\n" name;
  Hist.fprint stdout hist;
  Wrutils.pr "----------------------------------------\n";
  flush stdout
  end
*)
()


(** {6 Warm starting} ****************************************)


let make_warm_start max_bins =
  {
    curr_depth = ~-1;
    warm_fs = Hist.make (max_bins * 10);
    warm_nodes = Two_int_tbl.create 100;
    warm_f_dists = Two_int_tbl_window.init 0 ~-100;
  }


let reset_warm_start max_bins ws =
  ws.curr_depth <- ~-1;
  Two_int_tbl_window.reset ws.warm_f_dists 0


(** Updates the warmstart model with a new f value. *)
let warm_start_f model ~depth ~d ~t f =
  let ws = model.warm_start in
  let tbl = Two_int_tbl_window.get ws.warm_f_dists depth in
  let fs = get_nodes model tbl ~t ~d in
  Hist.add_mass f 1.0 fs



(** Transitions the warm start for the new completed depth. *)
let transition_warm_start model complete_depth =
  let ws = model.warm_start in
  let next_depth = complete_depth + 1 in
  if ws.curr_depth < next_depth
  then begin
    let nodes = ws.warm_nodes in
    Two_int_tbl.clear nodes;
    for depth = Math.imax ws.curr_depth 0 to next_depth do
      Two_int_tbl.iter
	(fun (t, d) fs ->
	  if depth > ws.curr_depth
	  then begin
	    if depth = next_depth
	    then begin
	      let wn = get_nodes model ws.warm_nodes ~t ~d in
	      set_nodes ws.warm_nodes ~t ~d (add_two fs wn)
	    end;
	    ws.warm_fs <- add_two ws.warm_fs fs;
	  end;
	  if depth < next_depth
	  then Three_int_tbl.remove model.parents (t, d, depth);
	) (Two_int_tbl_window.get ws.warm_f_dists depth);
    done;
    Two_int_tbl_window.slide ws.warm_f_dists
      (next_depth - ws.curr_depth - 1);
    ws.curr_depth <- next_depth;
  end


let warm_start_nodes max_bins ws =
  (** [warm_start_nodes max_bins ws] get the set of nodes for warm
      starting the model. *)
  ws.warm_nodes, ws.warm_fs, ws.curr_depth


(** {6 Modifying} ****************************************)

let make max_bins =
  {
    parents = Three_int_tbl.create 100;
    warm_start = make_warm_start max_bins;
    prev_bound = neg_infinity;
    max_bins = max_bins;
  }


let reset model =
  reset_warm_start model.max_bins model.warm_start;
  model.prev_bound <- neg_infinity;
  Three_int_tbl.clear model.parents


(** Called when an iteration is completed.

    NOTE: This is called before finding the bound so it must not clear
    important information. *)
let iteration_complete model depth bound =
  model.prev_bound <- bound;
  transition_warm_start model depth


(** Tracks info a child of the given parent.  *)
let see_child model parent ~ct ~cdd cdf =
  let childt = get_child_type model parent ct in
  let child_hist = get_child_hist model childt cdd in
  childt.ccount <- childt.ccount + 1;
  Hist.add_mass cdf 1.0 child_hist


let rec iter_succs model get_t get_d get_f ~parent
    ~b0 (* ~b1 ~b2 *) ~pf ~pd ~depth =
  function
    | s :: ss ->
      let ct = get_t s and cd = get_d s and cf = get_f s in
      if Math.finite_p cf then begin
	let cdf = cf -. pf and cdd = cd - pd in
	warm_start_f model ~depth:(depth + 1) ~d:cd ~t:ct cf;
	see_child model parent ~ct ~cdd cdf;
        see_child model b0 ~ct ~cdd cdf;

	  (*
            see_child model b1 ~ct ~cdd cdf;
            see_child model b2 ~ct ~cdd cdf;
	  *)
      end;
      iter_succs model get_t get_d get_f ~parent
	~b0 (* ~b1 ~b2 *) ~pf ~pd ~depth ss
    | [] -> ()


let learn model get_t get_d get_f depth node successors =
  let pt = get_t node
  and pd = get_d node
  and pf = get_f node in
  let parent = get_parent model ~pd ~pt ~depth in
  let b0 = get_parent model ~pd ~pt ~depth:backoff in
(*
  let b1 = get_parent model ~pd:backoff ~pt ~depth:backoff in
  let b2 = get_parent model ~pd:backoff ~pt:backoff ~depth:backoff in
*)
    if depth = 0 then warm_start_f model ~depth ~d:pd ~t:pt pf;
    parent.pcount <- parent.pcount + 1;
    b0.pcount <- b0.pcount + 1;
(*
    b1.pcount <- b1.pcount + 1;
    b2.pcount <- b2.pcount + 1;
*)
    iter_succs model get_t get_d get_f ~parent
      ~b0 (* ~b1 ~b2 *) ~pf ~pd ~depth successors


(** Updates the model from the given expansion. *)
let update model get_t get_d get_f bound depth node successors =
  let pf = get_f node in
    if pf > model.prev_bound then
      learn model get_t get_d get_f depth node successors


(** {6 Simulating} ****************************************)

(** Generates the children from a parent.

    [nparents] is for normalizing the histograms. *)
let gen_children model ~bound accum next nparents child_types
    ~depth ~pt ~pd pf =
  (* *)
    Verb.pr verb_simulate "parents seen for model = %f\n" nparents;
    Verb.pr verb_simulate "pt=%d, pd=%d, estimated parents=%f\n"
    pt pd (Hist.total_weight pf);
    debug_print_hist verb_simulate "parent fs" pf;
 (*  *)
  Int_tbl.iter
    (fun ct childt ->
       let ccount = childt.ccount in
	 if ccount > 0
	 then begin
	   let nchildren = float ccount in
	   let branching = nchildren /. nparents in
(* *)
	     Verb.pr verb_simulate
	       "\tct=%d, branching=%f, children_to_norm=%f\n%!"
	       ct branching nchildren;
(* *)
	     Int_tbl.iter
	       (fun cdd cdf ->
		  let cd = pd + cdd in
		    if cd >= 0 && Hist.has_mass cdf
		    then begin
		      let fs = get_nodes model next ~t:ct ~d:cd in
		      let frac = (Hist.total_weight cdf) /. nchildren in
		      let norm = Hist.copy cdf in
			Hist.normalize (branching *. frac) norm;
			let fs' = Hist.convolve_pruning norm pf bound in
(* *)
			  Verb.pr verb_simulate
			    "\t\tcdd=%d cd=%d, frac=%0.3f, cd_to_norm=%0.3f, "
			    cdd cd frac (Hist.total_weight fs');
			  Verb.pr verb_simulate "estimated children=%f\n%!"
			    (Hist.total_weight cdf);
			  debug_print_hist verb_simulate "child df" norm;
(* *)
			  set_nodes next ~t:ct ~d:cd (add_two fs fs');
			  add_to accum fs';
		    end)
	       childt.dds
	 end)
    child_types


(** Simulates the expansion of the given parents.  The result is a
    new 2d-Garray of the children and a count of the children
    added. *)
let expand_parents model ~bound depth nodes =
  let next = Two_int_tbl.create 100 in
  let mbins = model.max_bins in
  let accum = ref (Hist.make mbins) in
  Two_int_tbl.iter
    (fun (pt, pd) pf ->
      if pd > 0 && Hist.has_mass pf
	&& (Hist.total_weight pf) > cutoff_weight
      then begin
	let parent = get_parent_or_backoff model ~pd ~pt ~depth in
	let nparents = float parent.pcount in
	let child_types = parent.child_types in
	gen_children model ~bound accum next
	  nparents child_types ~depth ~pt ~pd pf
      end)
    nodes;
    (*
      debug_print_hist verb_simulate "next" !accum;
    *)
    next, !accum

let rec simulate model
    ?(depths=Garray.init (fun _ -> Hist.make model.max_bins))
    ?(bound=infinity) desired depth accum nodes =
 (* *)
    if Verb.level Verb.debug then begin
    Wrutils.pr "--------------------------------------------------\n%!";
    Wrutils.pr "-------------------- depth %d --------------------\n%!" depth;
    Wrutils.pr "--------------------------------------------------\n%!";
    end;
  (* *)
  let nodes', next = expand_parents model ~bound depth nodes in
  let bound' = prune_histograms accum next bound desired in
  let accum' = add_two accum next in
  let wt = Hist.weight_left_of bound' next in
(* *)
  if Verb.level Verb.debug then begin
    if depth = model.warm_start.curr_depth then
      Garray.set depths depth accum;
    Garray.set depths (depth + 1) next;
    Wrutils.pr "next weight: %f\n" wt;
    Wrutils.pr "total weight: %f\n" (Hist.total_weight accum');
    Wrutils.pr "bound: %f\n%!" bound';
  end;
(* *)
  if wt > cutoff_weight then begin
    simulate model
      ~depths
      ~bound:bound' desired (depth + 1) accum' nodes'
  end else begin
    if Verb.level Verb.debug then begin
      let bound =
	if desired < infinity then Hist.val_for_weight desired accum'
	else bound in
      let depth0 = model.warm_start.curr_depth in
      Garray.iteri
	(fun d h ->
	  Hist.prune_value_right bound h;
	  if Hist.has_mass h then begin
	    let wt = Hist.total_weight h in
	    let ch = if d = depth0 then '*' else ' ' in
	    Wrutils.pr "%c%2d: %0.2f\n%!" ch d wt
	  end)
	depths;
    end;
    accum', depth
  end


(** Finds the bound that will give the desired number of nodes. *)
let find_bound model desired =
  let layer, accum, depth = warm_start_nodes model.max_bins model.warm_start in
  Verb.pr verb_summary "completed depth: %d\n" model.warm_start.curr_depth;
  Verb.pr verb_summary "complete nodes: %f\n" (Hist.total_weight accum);
  let init_accum = Hist.copy accum in
  let fs, depth = simulate model desired depth accum layer in
  let bound = Hist.val_for_weight desired fs in
  let bound = bound +. (bound *. 10. *. epsilon_float) in
  let expected = Hist.weight_left_of bound fs in
  if Verb.level verb_summary then begin
    debug_print_hist verb_summary "final" fs;
    Wrutils.pr "     desired = %f\n" desired;
    Wrutils.pr "  init depth = %d\n" model.warm_start.curr_depth;
    Wrutils.pr "  init accum = %.2f\n" (Hist.total_weight init_accum);
    Wrutils.pr " final depth = %d\n" depth;
    Wrutils.pr "final weight = %.2f\n" (Hist.total_weight fs);
    Wrutils.pr " final bound = %.2f\n" bound;
    Wrutils.pr "    expected = %.2f\n" expected;
  end;
  bound, expected


(** Finds the number of nodes that will be expanded at the given
    bound. *)
let count_nodes model bound =
  let layer, accum, depth = warm_start_nodes model.max_bins model.warm_start in
  Verb.pr Verb.debug "completed depth: %d\n" model.warm_start.curr_depth;
  Verb.pr Verb.debug "complete nodes: %f\n" (Hist.total_weight accum);
  let fs, depth = simulate model ~bound infinity depth accum layer in
  let expected = Hist.weight_left_of bound fs in
  if Verb.level verb_summary then begin
    debug_print_hist verb_summary "final" fs;
    Wrutils.pr "  init depth = %d\n" model.warm_start.curr_depth;
    Wrutils.pr "       bound = %.2f\n" bound;
    Wrutils.pr " final depth = %d\n" depth;
    Wrutils.pr "final weight = %.2f\n" (Hist.total_weight fs);
    Wrutils.pr "    expected = %.2f\n" expected;
  end;
  bound, expected


(** Finds the number of nodes that will be expanded at the given
    bound from the given initial node (along with its kids). *)
let count_nodes_from model root_f kids_tdf bound =
  failwith "C hist and add full backoff"
(*
  let model = { model with max_bins = 1000 } in
  let accum = Hist.make model.max_bins in
  let layer = Two_int_tbl.create 100 in
  Hist.add_mass root_f 1.0 accum;
  List.iter (fun (t, d, f) ->
  let hist = get_nodes model layer ~t ~d in
  Hist.add_mass f 1. accum;
  Hist.add_mass f 1. hist;)
  kids_tdf;
  let init_depth = 0 in
  let fs, depth = simulate model ~bound infinity init_depth accum layer in
  let expected = Hist.weight_left_of bound fs in
  if Verb.level verb_summary
  then begin
  debug_print_hist verb_summary "final" fs;
  Wrutils.pr "  init depth = %d\n" init_depth;
  Wrutils.pr "       bound = %.2f\n" bound;
  Wrutils.pr " final depth = %d\n" depth;
  Wrutils.pr "final weight = %.2f\n" (Hist.total_weight fs);
  Wrutils.pr "    expected = %.2f\n" expected;
  end;
  bound, expected
*)


(** Makes a function that trains the model.  This is for off-line
    training. *)
let make_train model get_t get_d get_f =
  let learn = learn model get_t get_d get_f in
  (fun depth node successors -> learn depth node successors)
