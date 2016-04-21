(** A model that can learn incrementally (either online or offline).

    @author eaburns
    @since 2009-10-05
*)

(*
module Hist = Histogram

let _ =
  try
    Histogram.set_debug_mask (Histogram.debug_sanity_check
				(*
				  lor Histogram.debug_add
				  lor Histogram.debug_bounds
				  lor Histogram.debug_bins
				  lor Histogram.debug_convolve
				*)
			     )
  with _ -> ()
*)

type t = {
  f : Hist.t array array array array;
  (* parent_d * parent_t -> child_t * child_delta_d * child_delta_f *)

  parent_counts : int array array;
  (* parent_d * parent_t -> count *)

  mutable flag : bool;

  child_counts : int array array array;
  (* parent_d * parent_t * child_t -> count *)

  max_bins : int;
  (* Max number of bins to use in a histogram. *)

  backoff_threshold : int;
  (* If there are [backoff_threshold] or fewer observed datapoints
     (parent_d_counts) then we back the model off and look at the
     global distribution. *)

  type_max : int;
  (* Maximum type-value a node can have. *)

  d_max : int;
  (* Maximum d (search distance to go) value a node can have. *)

  dd_min : int;
  (* delta_d minimum. *)

  dd_max : int;
  (* delta_d maximum. *)
}


let accum_bin_factor = 4
  (** [accum_bin_factor] is the factor multiplied to max_bins when
      creating an accumulation histogram.  These may need to hold more f
      values and might need to be larger than the standard histograms. *)


let backoff = ~-1
  (** A special parameter value that is used to backoff a parameter in
      the model. *)


let epsilon = 0.00001
  (** Used for comparing the equality of floats. *)


let cutoff_weight = epsilon
  (** Used to determine the minimum weight to allow to the left of the
      bound in the next layer's histogram.  This determines the stopping
      criteria of the simulation function.

      This can be thought of as the smallest weight that we will still
      consider a 'whole node'.  More formally, this is the infimum of
      the weights that we will consider at least one possible node. *)


let make max_bins backoff_threshold type_max d_max (dd_min, dd_max) =
  (** [make max_bins backoff_threshold type_max d_max delta_d_range]
      creates a new incremental model. *)
  let nds = d_max + 1
  and ntypes = type_max + 1
  and ndds = (dd_max - dd_min) + 1 in
    {
      flag = false;
      f = (Wrarray.init_4d (nds + 1) ntypes ntypes ndds
	     (fun _ _ _ _-> Hist.make max_bins));
      parent_counts = Array.make_matrix (nds + 1) ntypes 0;
      child_counts = Wrarray.init_3d (nds + 1) ntypes ntypes (fun _ _ _ -> 0);
      max_bins = max_bins;
      backoff_threshold=backoff_threshold;
      type_max = type_max;
      d_max = d_max;
      dd_min = dd_min;
      dd_max = dd_max;
    }


let no_model = make 1 0 0 0 (0, 0)
  (** A place-holder. *)


let dd_ind model dd = dd - model.dd_min
  (** [dd_ind model dd] gets an array index to use for a delta d
      value. *)


let add_mass model pt pd ct cdd delta_f =
  (** [add_mass model pt pd ct cdd delta_f] adds mass to the f
      model. *)
  Hist.add_mass delta_f 1.0  model.f.(pt).(pd).(ct).(dd_ind model cdd)

(************************************************************)
(* Modifying the model.                                     *)
(************************************************************)

let update_model model depth node children =
  (** [update_model model depth node children] updates the model by
      looking at the given expansion.  [node] is a tuple of the (t, d,
      g, h) of the parent node and children is a list of (t, d, g, h)
      tuples of the children. *)
  let pt, pd, pg, ph = node in
  let pf = pg +. ph in
    Verb.pr Verb.debug
      "-------------------- update model --------------------\n%!";
    Verb.pr Verb.debug "depth=%d parent_t=%d, parent_d=%d\n%!"
      depth pt pd;
    assert (pd <= model.d_max);
    assert (pt <= model.type_max);

    (let a = model.parent_counts.(pd + 1) in a.(pt) <- a.(pt) + 1);
    (let a = model.parent_counts.(0) in a.(pt) <- a.(pt) + 1);

    Verb.pr Verb.debug
      "-------------------- itering children --------------------\n%!";
    List.iter
      (fun (ct, cd, cg, ch) ->
	 let cf = cg +. ch in
	 let delta_f = cf -. pf in
	 let delta_d = cd - pd in
	   assert (delta_d >= model.dd_min);
	   assert (delta_d <= model.dd_max);
	   assert (ct <= model.type_max);
	   if not (delta_f > 0.) && not (Math.within delta_f 0. epsilon)
	   then	      (* check for inconsistent change in f *)
	     failwith
	       (Wrutils.str "\n%s: pg=%f ph=%f pf=%f\ncg=%f ch=%f cf=%f\n%!"
		  "Inconsistent change in f" pg ph pf cg ch cf);
	   (let a = model.child_counts.(pd + 1).(pt) in a.(ct) <- a.(ct) + 1);
	   (let a = model.child_counts.(0).(pt) in a.(ct) <- a.(ct) + 1);
	   let ddi = dd_ind model delta_d in
	     Hist.add_mass delta_f 1.0 model.f.(pd + 1).(pt).(ct).(ddi);
	     Hist.add_mass delta_f 1.0 model.f.(0).(pt).(ct).(ddi);)
      children


let reset_model model () =
  (** [reset_model model] completely resets the model to the state
      that it was in when it was first created. *)
  let max_bins = model.max_bins in
  let type_max = model.type_max in
  let ndds = model.dd_max - model.dd_min in
    for pd = 0 to model.d_max + 1 do
      let f = model.f.(pd)
      and pcounts = model.parent_counts.(pd)
      and ccounts = model.child_counts.(pd) in
	for pt = 0 to type_max do
	  let f = f.(pt)
	  and ccounts = ccounts.(pt) in
	    pcounts.(pt) <- 0;
	    for ct = 0 to type_max do
	      let f = f.(ct) in
		ccounts.(ct) <- 0;
		for dd_ind = 0 to ndds - 1 do
		  f.(dd_ind) <- Hist.make max_bins
		done
	    done
	done
    done


let get_normalized_counts model pd_ind pt ct cdd_ind =
  (** [get_normalized_counts model pd_ind pt ct cdd] gets a
      normalized copy of the histogram for the given features.  *)
  let total_parent_count = model.parent_counts.(pd_ind).(pt) in
  let total_child_count = model.child_counts.(pd_ind).(pt).(ct) in
    if total_parent_count > 0 && total_child_count > 0
    then
      let br = (float total_child_count) /. (float total_parent_count) in
      let hist = Hist.copy model.f.(pd_ind).(pt).(ct).(cdd_ind) in
      let frac = (Hist.total_weight hist) /. (float total_child_count) in
	Hist.normalize (br *. frac) hist;
	hist
    else Hist.make model.max_bins


let iteration_complete model completed_depth completed_bound =
  (** [iteration_complete model completed_depth completed_bound]
      should be called when an iteration is complete.  Portions of the
      model are reset and it is made read for another round of
      training. *)
  ()


let normalized model =
  (** [normalized model] gets a normalized copy of the model where the
      histograms represent the children generated from a single parent. *)
  let nds = model.d_max + 1
  and ntypes = model.type_max + 1
  and ndds = (model.dd_max - model.dd_min) + 1
  in { model with
	 f = (Wrarray.init_4d (nds + 1) ntypes ntypes ndds
		(fun pd_ind pt ct cdd_ind ->
		   get_normalized_counts model pd_ind pt ct cdd_ind))}


(************************************************************)
(* Finding the next bound.                                  *)
(************************************************************)


let get_model_hist model pd pt ct cdd =
  (** [get_model_hist model pd pt ct cdd] gets the histogram from the
      appropraite model.  Maybe the returned model is backed off. *)
  assert (pt <= model.type_max);
  assert (pd <= model.d_max);
  let parent_counts = model.parent_counts.(pd + 1).(pt) in
    if parent_counts <= model.backoff_threshold
    then model.f.(0).(pt).(ct).(dd_ind model cdd)
    else model.f.(pd + 1).(pt).(ct).(dd_ind model cdd)


let gen_children model depth next pt pd pf_dist bound desired =
  (** [gen_children model depth next pt pd pf_dist bound desired]
      Updates the [next] array for the children generated from parents
      with type [pt], d-value [pd] and an f distribution of
      [pf_dist]. *)
  let pt = int_of_float pt in
  let child_fs = ref [] in
    for t = 0 to model.type_max do
      (* If there have been any children of this type, see what
	 they look like *)
      let next = next.(t) in
	for dd = model.dd_min to model.dd_max do
	  let d = Math.imin (dd + pd) model.d_max in
	  let f_hist = get_model_hist model pd pt t dd in
	    if d >= 0 && (Hist.has_mass f_hist)
	    then begin
	      let f_dist = Hist.convolve_pruning f_hist pf_dist bound in
		if Hist.has_mass f_dist
		then begin
		  next.(d) <- Hist.add [ next.(d); f_dist ];
		  child_fs := f_dist :: !child_fs;
		end;
	    end;
	done;
    done;
    !child_fs


let prune_histograms total_fs next_fs prev_bound desired =
  (** [prune_histograms total_fs next_fs prev_bound desired] tries to
      find a better pruning bound.  Prunes the histograms and returns the
      new bound. *)
  let new_bound = (Hist.bound_for_wted_combo desired total_fs next_fs 1.0) in
    Verb.pr Verb.debug "next_fs pre-prune=%f\n%!" (Hist.total_weight next_fs);
    let bound = Math.fmin new_bound prev_bound in
      Hist.prune_value_right bound total_fs;
      Hist.prune_value_right bound next_fs;
      bound


let rec simulate ?(recursive_call=false) ?(depth=1) ?(bound=infinity) model
    ?(hists_by_depth=Garray.make (Hist.make model.max_bins))
    total_fs desired prev =
  (** [simulate ?depth ?bound model total_fs desired prev]
      simulates the search one step further down the tree.  [prev] is
      an array of parent_t * parent_d * parent_f count histograms. *)
  if not recursive_call && Verb.level Verb.debug
  then begin
    Verb.pr Verb.always
      "-------------------- simulating --------------------\n%!";
    Garray.set hists_by_depth depth total_fs;
  end;
  Verb.pr Verb.debug
    "-------------------- depth %d\n%!" depth;

  if depth mod 100 = 0
  then begin
    if depth = 100 || Verb.level Verb.debug
    then Verb.pr Verb.toplvl "depth: %d%!" depth
    else Verb.pr Verb.toplvl ", %d%!" depth;
    Verb.pr Verb.debug "\n"
  end;

  let next =
    Array.init (model.type_max + 1)
      (fun _ -> Array.init (model.d_max + 1)
	 (fun _ -> Hist.make model.max_bins))
  and fs = ref [] in
    for t = 0 to model.type_max do
      (* If there even is a parent of this type then see what its
	 children will be. *)
      let prev = prev.(t) in
	for d = 1 to model.d_max do	(* assume d = 0 is not fertile. *)
	  let prev = prev.(d) in
	    Hist.prune_value_right bound prev;
	    if Hist.has_mass prev && (Hist.total_weight prev) > cutoff_weight
	    then begin
	      let child_fs =
		gen_children model depth next (float t) d prev bound desired
	      in fs := child_fs @ !fs
	    end
	done
    done;
    let next_fs =
      if !fs <> [] then Hist.add !fs else Hist.make model.max_bins in
    let bound = prune_histograms total_fs next_fs bound desired in
    let total_fs =
      Hist.add
	~max_bins:(model.max_bins * accum_bin_factor)
	[total_fs; next_fs]
    in


      if Verb.level Verb.debug
      then begin
	Garray.set hists_by_depth (depth + 1) next_fs;
	Verb.pr Verb.debug "next_fs=%f\n%!" (Hist.total_weight next_fs);
	Verb.pr Verb.debug "total_fs=%f\n%!" (Hist.total_weight total_fs);
	Verb.pr Verb.debug "bound (pruning value)=%f\n%!" bound;
	Verb.pr Verb.debug
	  "next_fs weight left of bound=%f (cutoff=%f)\n%!"
	  (Hist.weight_left_of bound next_fs) cutoff_weight;
	Verb.pr Verb.debug
	  "total_fs weight left of bound=%f\n%!"
	  (Hist.weight_left_of bound total_fs);
      end;

      if (Hist.weight_left_of bound next_fs) > cutoff_weight
      then
	simulate
	  ~recursive_call:true
	  ~depth:(depth+1)
	  ~hists_by_depth:hists_by_depth
	  ~bound:bound model total_fs desired next
      else begin
	Verb.pr Verb.debug
	  "----------------------------------------------------\n%!";

	if depth > 100 && not (Verb.level Verb.debug)
	then Verb.pr Verb.toplvl "\n";

	Verb.pr Verb.toplvl "simulated to depth %d\n%!" depth;
	Verb.pr Verb.toplvl "final wt: %f\n%!" (Hist.total_weight total_fs);

	if Verb.level Verb.debug
	then begin
	  (* If debug is enabled then print the number of un-pruned
	     nodes from each depth layer. *)
	  let bound = Hist.val_for_weight desired total_fs in
	  let sum = ref 0. in
	    Garray.iteri (fun i hist ->
			    let wt = Hist.weight_left_of bound hist in
			      sum := !sum +. wt;
			      Verb.pr Verb.always "%2d: %10.2f\n" i wt)
	      hists_by_depth;
	    Verb.pr Verb.always "sum: %10.2f\n" !sum;
	end;
	total_fs, depth
      end


(************************************************************)
(* Using the model.                                         *)
(************************************************************)


let initial_layer model prev_fs seed_nodes =
  (** [initial_layer model prev_fs seed_nodes] creates the histograms
      for the initial layer of the simulation given the seed nodes
      ([seed_nodes]) and a list of the f-values of the nodes in the tree
      above the seed_nodes ([prev_fs]). *)
  let nodes =
    Array.init (model.type_max + 1)
      (fun _ -> Array.init (model.d_max + 1)
	 (fun _ -> Hist.make model.max_bins))
  and total_fs = Hist.make model.max_bins in
    List.iter (fun (t, d, f) ->
		 Verb.pr Verb.debug
		   "init layer t=%d d=%d f=%f\n%!" t d f;
		 Hist.add_mass f 1.0 total_fs;
		 Hist.add_mass f 1.0 nodes.(t).(d))
      seed_nodes;
    List.iter (fun f ->
		 Verb.pr Verb.debug "init layer f=%f\n%!" f;
		 Hist.add_mass f 1.0 total_fs) prev_fs;
    nodes, total_fs


let find_bound model seed_depth prev_fs seed_nodes desired =
  (** [find_bound model seed_depth prev_fs seed_nodes desired] find
      the bound that will give the desired number of node expansions
      and some seed nodes.  The seed nodes describe a portion of the
      top of the tree (for example: the root node, or the root and its
      children).  The format of the [seed_nodes] argument is a list of
      (t, d, f) values.  [seed_depth] is the search depth of the seed
      nodes.

      [prev_fs] is a list of the f-values that were encountered before
      the seed_nodes.  For example, if the seed nodes are the children
      of the root node, then [prev_fs] should have the f-value of the
      root node. *)
  let model = normalized model in
  let nodes, total_fs = initial_layer model prev_fs seed_nodes in
  let total_fs, depth =
    simulate ~depth:seed_depth model total_fs desired nodes
  in
  let bound = Hist.val_for_weight desired total_fs in
    (*
      if bound < 0. then Histogram.set_debug_mask ~-1;
    *)
    Verb.pr Verb.toplvl "Simulation:\n";
    Verb.pr Verb.toplvl "\t init depth=%d\n" seed_depth;
    Verb.pr Verb.toplvl "\tfinal depth=%d\n" depth;
    Verb.pr Verb.toplvl "\t      bound=%f\n" bound;
    Verb.pr Verb.toplvl "\t    desired=%f\n" desired;
    Verb.pr Verb.toplvl "\t     weight=%f\n%!" (Hist.total_weight total_fs);
    bound


let count_nodes model seed_depth prev_fs seed_nodes bound =
  (** [count_nodes model prev_fs seed_nodes bound] find the number
      of nodes that will be seen with a search to a specified bound
      give some seed nodes.  The seed nodes describe a portion of the
      top of the tree (for example: the root node, or the root and its
      children).  The format of the [seed_nodes] argument is a list of
      (t, d, f) values.

      [prev_fs] is a list of the f-values that were encountered before
      the seed_nodes.  For example, if the seed nodes are the children
      of the root node, then [prev_fs] should have the f-value of the
      root node. *)
  let model = normalized model in
  let nodes, total_fs = initial_layer model prev_fs seed_nodes in
  let total_fs, _ =
    simulate ~depth:(seed_depth + 1)
      ~bound:bound model total_fs infinity nodes in
  let weight = Hist.weight_left_of bound total_fs in
    Verb.pr Verb.toplvl "final weight=%f\n%!" weight;
    weight


(************************************************************)
(* Offline training.                                        *)
(************************************************************)

let train max_bins backoff_threshold
    type_max d_max dd_range expand_parent expand_noparent h d t rand =
  let m = make max_bins backoff_threshold type_max d_max dd_range in
  let count = ref 0 in
    ignore (rand ());			(* don't train on the goal *)
    try
      while true do
	if !count mod 100_000 = 0 then Verb.pr Verb.optional "%d\n" !count;
	let grand_parent = rand () in
	let parents = expand_parent grand_parent in
	  List.iter (fun (parent, _) ->
		       let children = expand_noparent parent in
			 incr count;
			 update_model m
			   ~-2		(* same depth when offline. *)
			   (t parent, d parent, 0., h parent)
			   (List.map (fun (c,_) ->
					t c, d c, 1., h c) children))
	    parents
      done;
      failwith "Shouldn't reach here"
    with End_of_file -> begin
      Verb.pr Verb.debug "Trained on %d transitions\n%!" !count;
      m
    end

