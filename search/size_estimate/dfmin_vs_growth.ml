(** Some code to track \Delta f_min versus the growth between f-layers
    of an A* search.

    @author eaburns
    @since 2010-12-01
*)

open Printf
open Float_ref

type 'state node = {
  state : 'state;
  h : float;
  mutable g : float;
  mutable f : float;

  mutable pq_pos : int;
}

let f_layers = "f layers"


let update_pq_pos n i =
  (** [update_pq_pos n i] updates the priority queue position of [n]
      to [i]. *)
  n.pq_pos <- i


let is_better a b =
  (** [is_better a b] tests if node [a] is better than node [b]. *)
  if a.f = b.f then
    a.g > b.g
  else
    a.f < b.f


let update_f_min cur prev f_min f_min_prev layer_count dfs growths o =
  (** [update_f_min cur prev f_min f_min_prev layer_count o dfs
      growths] updates the minimum f-value in the state space. *)
  incr cur;
  if not (Dpq.empty_p o) then begin
    let n = Dpq.peek_first o in
      if n.f > !!f_min then begin
	if !!f_min_prev >= 0. then begin
	  let delta_f = !!f_min -. !!f_min_prev in
	  let ratio = (float !cur) /. (float !prev) in
	    Datafile.write_alt_row_prefix stdout f_layers;
	    printf "%f\t%f\t%f\n" !!f_min delta_f ratio;
	    Hist.add_mass delta_f 1. dfs;
	    Hist.add_mass ratio 1. growths;
	end;
	incr layer_count;
	f_min_prev <-- !!f_min;
	f_min <-- n.f;
	prev := !cur;
	cur := 0;
      end
  end


let handle_child info key h reparent o c parent (state', g') =
  (** [handle_child info key h reparent o c parent (state', g')] handles the
      generation of a new child of [parent]. *)
  Limit.incr_gen info;
  try
    let node = Htable.find c (key state') in
      if node.g > g' then begin
	node.f <- node.h +. g';
	node.g <- g';
	reparent node.state parent;
	if node.pq_pos = Dpq.no_position then
	  Dpq.insert o node
	else
	  Dpq.see_update o node.pq_pos
      end
  with Not_found ->
    let h = h state' in
    let node =
      { state = state'; h = h; g = g'; f = g' +. h; pq_pos = Dpq.no_position }
    in
      Htable.add c (key state') node;
      Dpq.insert o node


let astar info hash eq key h reparent expand nlayers init =
  let o = Dpq.create is_better update_pq_pos 1024 init in
  let c = Htable.create hash eq 997 in
  let f_min = float_ref init.h and f_min_prev = float_ref ~-.1. in
  let cur = ref 0 and prev = ref 0 in
  let layer_count = ref ~-1 in
  let growths = Hist.make 1000 in
  let dfs = Hist.make 1000 in
  let init_layer = ref 0 in
  let handle_child = handle_child info key h reparent o c in
    Datafile.write_alt_colnames stdout f_layers
      [ "f min"; "delta f"; "growth rate" ];
    Dpq.insert o init;
    Htable.add c (key init.state) init;
    while not (Dpq.empty_p o) && not (Limit.halt_p info)
      && !layer_count < nlayers
    do
      let n = Dpq.extract_first o in
	Limit.incr_exp info;
	List.iter (handle_child n.state) (expand n.state n.g);
	if !!f_min_prev < 0. then incr init_layer;
	update_f_min cur prev f_min f_min_prev layer_count dfs growths o;
    done;
    dfs, growths, !init_layer, !!f_min_prev


let rec make_estimate dfs growths nnodes start_f end_f =
  (** [make_estimate dfs growths nnodes start_f end_f] makes an
      estimate about the number of nodes between start_f and end_f. *)
  if start_f < end_f then begin
    let delta_f = Hist.sample dfs in
    let factor = Hist.sample growths in
      assert (Math.finite_p delta_f);
      let f' = start_f +. delta_f in
      let nnodes' = nnodes *. factor in
	nnodes +. (make_estimate dfs growths nnodes' f' end_f)
  end else
    0.


let dups sface argv =
  let nlayers = Search_args.get_int "Dfmin_vs_growth.dups" argv 0 in
  let k = Search_args.get_int "Dfmin_vs_growth.dups" argv 1 in
  let state = sface.Search_interface.initial in
  let h = sface.Search_interface.h in
  let hash = sface.Search_interface.hash in
  let eq = sface.Search_interface.equals in
  let key = sface.Search_interface.key in
  let reparent = sface.Search_interface.parent_update in
  let expand = sface.Search_interface.domain_expand in
  let info = sface.Search_interface.info in
  let h_init = h state in
  let init =
    { state = state; h = h_init; g = 0.; f = h_init; pq_pos = Dpq.no_position}
  in
  let dfs, growths, init_layer, end_f =
    astar info hash eq key h reparent expand nlayers init
  in
  let ests =
    Array.init k
      (fun _ -> make_estimate dfs growths (float init_layer) h_init end_f)
  in
  let estimate = Vector.avg ests in
    Datafile.write_pairs stdout [
      "num layers", string_of_int nlayers;
      "num samples", string_of_int k;
      "h init", string_of_float h_init;
      "finish f", string_of_float end_f;
      "estimate", string_of_float estimate; ];
    Limit.unwrap_sol6
      (function
	 | Limit.Nothing -> None
	 | Limit.Incumbent (f, state) -> Some (state, f))
      (Limit.results6 info)
