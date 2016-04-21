(** Tracks histograms estimated from the model and during the actual
    search.  This can be used to see if a model is predicting things
    properly.

    @author eaburns
    @since 2010-03-12
*)

open Printf

type record = {
  actual : Hist.t;
  mutable predicted : Hist.t;
}

type t = {
  my_node_delta : record Garray.t;
  my_node_parent_f : record Garray.t;
  my_node_child_f : record Garray.t;
  counts : (float * float * float) Garray.t;

  mutable plots : (int * int * string * string * string) list;
  mutable iter : int;

  max_bins : int;
}

let my_pt, my_pd, my_ct, my_cd = 6, 14, 7, 13

let make max_bins =
  {
    my_node_delta = Garray.init (fun _ -> { actual = Hist.make max_bins;
					    predicted = Hist.make max_bins; });
    my_node_child_f = Garray.init (fun _ -> { actual = Hist.make max_bins;
					predicted = Hist.make max_bins; });
    my_node_parent_f = Garray.init (fun _ -> { actual = Hist.make max_bins;
					predicted = Hist.make max_bins; });
    counts = Garray.init (fun _ -> 0., 0., 0.);
    plots = [];
    iter = 0;
    max_bins = max_bins;
  }


let see_actual v get_t get_d get_f bound depth node successors =
  (** [see_actual v get_t get_d get_f bound depth node successors] see an
      actual node during search. *)
  let f = get_f node in
  let t = get_t node and d = get_d node in
    if t = my_pt && d = my_pd
    then begin
      let parent_f_hist = Garray.get v.my_node_parent_f depth
      in Hist.add_mass f 1.0 parent_f_hist.actual;
    end;
    List.iter (fun c ->
		 let ct = get_t c and cd = get_d c in
		 let cf = get_f c in
		   if cd = my_cd && ct = my_ct && t = my_pt && d = my_pd
		   then begin
		     let delta_hist =
		       Garray.get v.my_node_delta depth
		     and child_f_hist =
		       Garray.get v.my_node_child_f depth
		     in
		       Hist.add_mass cf 1.0 child_f_hist.actual;
		       Hist.add_mass (cf -. f) 1.0 delta_hist.actual;
		   end
	      )
      successors


let recs_at_depth ary depth =
  (** [recs_at_depth ary depth] gets the records at the given depth in
      the array. *)
  let lst = ref [] in
    Garray.iteri
      (fun t ary ->
	 Garray.iteri (fun d record -> lst := (t, d, record) :: !lst) ary)
      (Garray.get ary depth);
    !lst


let most_different recs =
  (** [most_different recs] gets the records with the greatest
      absolute difference in the acutal and predicted weights. *)
  List.fold_left
    (fun ((diff, _) as prev) ((_, _, record) as x) ->
       let a_wt = Hist.total_weight record.actual
       and p_wt = Hist.total_weight record.predicted in
       let d = abs_float (a_wt -. p_wt) in
	 if d > diff then (d, x) else prev)
    (neg_infinity, List.hd recs) recs


let see_predicted t ~depth ~pt ~pd ~ct ~cd
    ~nparents ~tot_children ~nchildren deltas cfs pfs =
  (** [see_predicted t ~depth ~pt ~pd ~ct ~cd ~nparents ~tot_children
      ~nchildren deltas cfs pfs] sees a predicted set of delta fs. *)
  if pt = my_pt && pd = my_pd
  then begin
    let pf_r = Garray.get t.my_node_parent_f depth in
      pf_r.predicted <- pfs;
  end;
  if Hist.has_mass deltas && Hist.has_mass cfs
  then begin
    if pt = my_pt && pd = my_pd && ct = my_ct && cd = my_cd
    then begin
      let delta_r = Garray.get t.my_node_delta depth
      and cf_r = Garray.get t.my_node_child_f depth in
	delta_r.predicted <- Hist.add [delta_r.predicted; deltas];
	cf_r.predicted <- cfs;
	Garray.set t.counts depth (nparents, tot_children, nchildren);
    end
  end


let next_iteration t =
  (** [next_iteration t] go to the next iteration. *)
  Garray.clear t.my_node_delta;
  Garray.clear t.my_node_parent_f;
  Garray.clear t.my_node_child_f;
  Garray.clear t.counts;
  t.iter <- t.iter + 1


let plot dir title a p =
(*
  let data0, n0, l0 = Hist.get_plot_data a in
  let data1, n1, l1 = Hist.get_plot_data p in
  let (start, _), (finish, _) =
    Wrlist.min_and_max_by fst
      ((Array.to_list data0) @ (if Hist.has_mass p
				then Array.to_list data1
				else []))
  in
  let offs, start, finish =
    let o = (finish -. start) *. 0.01 in
      if Math.is_zero o
      then 0.001, start -. 0.05, finish +. 0.05
      else o, start, finish
  in
  let data1' =
    if not (Hist.has_mass p)
    then [| start, 0.; finish, 0. |]
    else Array.map (fun (x, y) -> x +. offs, y) data1 in
  let wt0 = Hist.total_weight a and wt1 = Hist.total_weight p in
*)
  let path = dir ^ title ^ ".eps" in
(*
    Ps_plot.line path [
      data0, (Wrutils.str "actual %d %s wt %.2f" n0 l0 wt0);
      [| start, 0.; finish, 0. |], "";
      [| start, 0.; finish, 0. |], "";
      data1', (Wrutils.str "pred %d %s wt %.2f" n1 l1 wt1);
    ] ~zero_line:true title "Value" "Weight";
*)
    path


let output v completed_depth bound =
  let dir = User_paths.plot_root in
    Garray.iteri
      (fun depth delta_r ->
	 if depth >= completed_depth
	 then begin
	   let title =
	     sprintf "iter-%d_bound-%.2f_depth-%d" v.iter bound depth in
	   let cf_r = Garray.get v.my_node_child_f depth in
	   let pf_r = Garray.get v.my_node_parent_f depth in
	   let np, ctot, nc = Garray.get v.counts depth in
	     if (Hist.has_mass delta_r.actual)
	       || (Hist.has_mass delta_r.predicted)
	       || (Hist.has_mass cf_r.actual)
	       || (Hist.has_mass cf_r.predicted)
	     then begin
	       Hist.prune_value_right bound pf_r.predicted;
	       Hist.prune_value_right bound cf_r.predicted;
	       let nparents = Hist.total_weight pf_r.actual
	       and nchildren = Hist.total_weight cf_r.actual
	       in
	       let normed_dfs = Hist.copy delta_r.actual in
		 Hist.normalize (nchildren /. nparents) normed_dfs;
		 let plot_delta =
		   plot dir (sprintf "%s_deltaf_np-%.2f_ctot-%.2f-nc-%.2f"
			       title np ctot nc)
		     normed_dfs delta_r.predicted
		 and plot_pf =
		   plot dir (sprintf "%s_pf_completed-%d"
			       title completed_depth)
		     pf_r.actual pf_r.predicted
		 and plot_cf =
		   plot dir (title ^ "_cf") cf_r.actual cf_r.predicted
		 in
		   v.plots <- ((v.iter, depth, plot_pf, plot_delta, plot_cf)
			       :: v.plots);
	     end
	 end
      ) v.my_node_delta;
    if v.iter = 12
    then begin
      let sorted = List.sort compare v.plots in
      let plots = Wrlist.mapcan (fun (_, _, p, q, r) -> [p; q; r]) sorted in
      let montage_path = sprintf "%splots-to-iter-%d.ps" dir v.iter in
	Verb.pr Verb.debug "Montaged to %s\n" montage_path;
(*
	Ps_plot.montage ~across:3 ~down:3 plots montage_path;
*)
	List.iter Unix.unlink plots;
	assert false
    end
