(** Yet another version of the incremental model.

    @author eaburns
    @since 2010-10-13
*)

(*module Hist = Histogram*)

module type VEC_TYPE = sig
  type 'a t
  val init : ?init_size:int -> (int -> 'a) -> 'a t
  val get : 'a t -> int -> 'a
  val set : 'a t -> int -> 'a -> unit
  val iteri : (int -> 'a -> unit) -> 'a t -> unit
end

module Garray_double_ended : VEC_TYPE = Garray.Double_ended

module Garray : VEC_TYPE = struct
  (* Wrapper for Garray that doesn't require init to be passed to each
     function. *)
  type 'a t = {
    init : int -> 'a;
    a : 'a Garray.t
  }

  let init ?(init_size=4) f =
    {
      init = f;
      a = Garray.init ~init_size f;
    }

  let get v i = Garray.get v.a ~init_fun:v.init i
  let set v i e = Garray.set v.a ~init_fun:v.init i e
  let iteri f v = Garray.iteri f v.a
end

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


let debug_print_hist lvl name hist =
  (** [debug_print_hist lvl name hist] prints a histogram if debugging is
      enabled. *)
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


(** {6 The model} ****************************************)


module Make (Vec : VEC_TYPE) (D_vec : VEC_TYPE) = struct

  let cutoff_weight = 0.001
    (** [cutoff_weight] is the weight at which we assume that it is no
	longer worth counting nodes. *)

  let init_size = 10


  type 'a vec = 'a Vec.t

  type 'a d_vec = 'a D_vec.t

  type hist = Hist.t

  type child_type = {
    mutable ccount : int;
    dds : hist d_vec;
    (* dds.(delta d) -> hist *)
  }

  type parent = {
    mutable pcount : int;
    depth : int;
    pd : int;
    pt : int;
    child_types : child_type vec;
    (* child_types.(child type) -> hist *)
  }

  type t = {
    mutable parents : parent d_vec vec vec;
    (* parents.(depth + 1).(pt).(pd) -> parent; depth=0 is backoff *)
    mutable prev_bound : float;
    mutable max_depth : int;
    max_bins : int;
  }


  let get_parent model ~pd ~pt ~depth =
    (** [get_parent model ~pd ~pt ~depth] gets the parent sturcture. *)
    let depth = depth + 1 in
      D_vec.get (Vec.get (Vec.get model.parents depth) pt) pd


  let get_parent_or_backoff model ~pd ~pt ~depth =
    (** [get_parent_or_backoff model ~pd ~pt ~depth] gets the parent
	structure.  If the depth is greater than any experienced depth
	then the backed off model is returned. *)
    let depth = if depth > model.max_bins then ~-1 else depth in
      get_parent model ~pd ~pt ~depth


  let get_child parent ct =
    (** [get_child parent ct] gets the child structure. *)
    Vec.get parent.child_types ct


  let get_hist child dd =
    (** [get_hist child dd] gets the histogram of values with the given
	delta-d for this child. *)
    D_vec.get child.dds dd


  let make_parents max_bins =
    (** [make_parents max_bins] makes the parents matrix. *)
    let make_children _ =
      Vec.init ~init_size
	(fun _ ->
	   { ccount = 0;
	     dds = D_vec.init ~init_size (fun _ -> Hist.make max_bins) })
    in
    let init_pd depth pt pd =
      { pcount = 0;
	depth = depth;
	pd = pd;
	pt = pt;
	child_types = make_children max_bins;
      }
    in
    let init_pt depth pt =
      D_vec.init ~init_size (init_pd depth pt)
    in
    let init_depth depth =
      Vec.init ~init_size (init_pt depth)
    in
      Vec.init ~init_size init_depth


  let make max_bins =
    (** [make max_bins] creates a new incremental model. *)
    {
      parents = make_parents max_bins;
      prev_bound = neg_infinity;
      max_depth = 0;
      max_bins = max_bins;
    }


  let reset model =
    (** [reset model] resets the model. *)
    model.prev_bound <- neg_infinity;
    model.max_depth <- 0;
    model.parents <- make_parents model.max_bins


  let iteration_complete model depth bound =
    (** [iteration_complete model depth bound] marks the given iteration
	as completed. *)
    model.prev_bound <- bound


  let see_child parent ~ct ~cdd cdf =
    (** [see_child parent ~ct ~cdd cdf] mark a child as having been
	seen by the search. *)
    let child = get_child parent ct in
    let hist = get_hist child cdd in
      child.ccount <- child.ccount + 1;
      Hist.add_mass cdf 1.0 hist


  let update model get_t get_d get_f bound depth node succs =
    (** [update model get_t get_d get_f depth node succs] learn from an
	expansion of [node] which generates successors [succs]. *)
    let rec iter_succs get_t get_d get_f ~parent ~b0 ~pf ~pd ~depth =
      function
	| s :: ss ->
	    let ct = get_t s and cd = get_d s and cf = get_f s in
	    let cdf = cf -. pf and cdd = cd - pd in
	      see_child parent ~ct ~cdd cdf;
              see_child b0 ~ct ~cdd cdf;

	      iter_succs get_t get_d get_f ~parent ~b0 ~pf ~pd ~depth ss
	| [] -> ()
    in
    let pf = get_f node in
      if pf > model.prev_bound then begin
	let pt = get_t node and pd = get_d node in
	let parent = get_parent model ~pd ~pt ~depth in
	let b0 = get_parent model ~pd ~pt ~depth:~-1 in
	  if depth > model.max_depth then model.max_depth <- depth;
	  parent.pcount <- parent.pcount + 1;
	  b0.pcount <- b0.pcount + 1;
	  iter_succs get_t get_d get_f ~parent ~b0 ~pf ~pd ~depth succs
      end


  (** {6 Simulation} ****************************************)


  let get_nodes next ~t ~d =
    Vec.get (Vec.get next t) d


  let set_nodes next ~t ~d hist =
    let by_d = Vec.get next t in
      Vec.set by_d d hist


  let make_node_set max_bins =
    Vec.init ~init_size
      (fun _ -> Vec.init ~init_size (fun _ -> Hist.make max_bins))


  let gen_children ~bound acc next npar ctypes ~depth ~pt ~pd pf =
    (** [get_nodes ~bound acc next npar cytpes ~depth ~pt ~pd pf]
	simulates the generation of children from the given parent. *)
    let handle_child_dd nch br ct cdd cdf =
      let cd = pd + cdd in
	if cd >= 0 && Hist.has_mass cdf then begin
	  let fs = get_nodes next ~t:ct ~d:cd in
	  let frac = (Hist.total_weight cdf) /. nch  in
	  let norm = Hist.copy cdf in
	    Hist.normalize (br *. frac) norm;
	    let fs' = Hist.convolve_pruning norm pf bound in
	      set_nodes next ~t:ct ~d:cd (add_two fs fs');
	      add_to acc fs'
	end
    in
    let handle_child_type ct child =
      let ccount = child.ccount in
	if ccount > 0 then begin
	  let nch = float ccount in
	  let br = nch /. npar in
	    D_vec.iteri (handle_child_dd nch br ct) child.dds
	end
    in
      Vec.iteri handle_child_type ctypes


  let expand_parents m bound depth nodes =
    (** [expand_parents m ~bound depth nodes] simulates the expanding of
	parents. *)
    let bins = m.max_bins in
    let next = make_node_set bins in
    let acc = ref (Hist.make bins) in
    let iter_parents pt pd pf =
      if pd > 0 && Hist.has_mass pf && (Hist.total_weight pf) > cutoff_weight
      then begin
	let parent = get_parent_or_backoff m ~pd ~pt ~depth in
	let npar = float parent.pcount in
	let ctypes = parent.child_types in
	  gen_children ~bound acc next npar ctypes ~depth ~pt ~pd pf
      end
    in
      Vec.iteri (fun pt by_d -> Vec.iteri (iter_parents pt) by_d) nodes;
      next, !acc


  let rec simulate m ?(bound=infinity) desired depth acc nodes =
    (** [simulate m ?bound desired depth acc nodes] simulates search. *)
    let nodes', next = expand_parents m bound depth nodes in
    let bound' = prune_histograms acc next bound desired in
    let acc' = add_two acc next in
    let wt = Hist.weight_left_of bound' next in
      if wt > cutoff_weight then
	simulate m ~bound:bound' desired (depth + 1) acc' nodes'
      else
	acc', depth



  let find_bound m root_f kids_tdf desired =
    (** [find_bound m root_f kids_tdf desired] finds the bound by
	simulation. *)
    let bins = m.max_bins in
    let acc = Hist.make bins in
    let nodes = make_node_set bins in
      Hist.add_mass root_f 1. acc;
      List.iter (fun (t, d, f) ->
		   let hist = get_nodes nodes ~t ~d in
		     Hist.add_mass f 1. acc;
		     Hist.add_mass f 1. hist)
	kids_tdf;
      let init_wt = Hist.total_weight acc in
      let init_depth = 1 in
      let fs, depth = simulate m desired init_depth acc nodes in
      let bound = Hist.val_for_weight desired fs in
      let bound = bound +. (bound *. 10. *. epsilon_float) in
      let expected = Hist.weight_left_of bound fs in
	if Verb.level Verb.toplvl
	then begin
	  debug_print_hist Verb.toplvl "final" fs;
	  Printf.printf "     desired = %f\n" desired;
	  Printf.printf "  init depth = %d\n" init_depth;
	  Printf.printf "     init wt = %.2f\n" init_wt;
	  Printf.printf " final depth = %d\n" depth;
	  Printf.printf "final weight = %.2f\n" (Hist.total_weight fs);
	  Printf.printf " final bound = %.2f\n" bound;
	  Printf.printf "    expected = %.2f\n%!" expected;
	end;
	(*
	  assert (Math.finite_p bound);
	*)
	if not (Math.finite_p bound) then begin
	  Verb.pr Verb.debug "--------------------infinite bound\n";
	  Hist.max_val fs, Hist.total_weight fs;
	end else
	  bound, expected
end

module Im_garray = Make(Garray)(Garray_double_ended)
module Im_lazy_vec = Make(Lazy_vec)(Lazy_vec.Double_ended)
