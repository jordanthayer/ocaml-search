(* $Id: hist.ml,v 1.6 2004/06/14 20:30:43 ruml Exp ruml $

   Histograms

   Some possible improvements:
     - change points to use arrays instead of a list.
     - profile and hoist invariants out of important loops?
     - take advantage of the mutability of the pt type.
     - compile to assembly and try to reduce the amount of float boxing.
     - switch merge_points to use merge_points_tr
     - there's a bug with convolve max, getting index out of bounds
       sometimes
*)

type pt = {
  vl : float;
  wt : float;
}

type points = {
  (* all weights > 0. *)
  mutable points : pt list;
  mutable count : int;
  max_points : int;
}


type bins = {
  (* left edge of first bin *)
  mutable min : float;
  mutable bin_width : float;
  (* always has max_points entries.  some (even all!) bins may have
     zero weight. *)
  mutable weights : float array;
}

type hist = Points of points | Bins of bins


type t = {
  mutable h : hist;
}


(************** construction *************)


let make max_pts =
  assert (max_pts > 0);
  { h =  Points { points = []; count = 0; max_points = max_pts; } }


let max_with_margin min max =
  max +. ((max -. min) /. 1000000.) +. 0.00001


let make_bins min max num =
  (** Returns a fresh bins with [num] bins and room only for values
      from [min] to [max] inclusive.  Doesn't leave any room for
      growth, but does make sure that [max] will not miss the last
      bin.  The intention is that the caller will manipulate [min] and
      [max] for buffer room if appropriate. *)
  let max = max_with_margin min max in
  let width = (max -. min) /. (float num) in
    { min = min;
      bin_width = width;
      weights = Array.make num 0.; }


(************** utilities **************)


let pt_vl p = p.vl
let pt_wt p = p.wt

(**
 * returns the mean value of the histogram. 0 if no data.
 *)
let mean t =
  match t.h with
    | Points p ->
	let count = ref 0.
	and sum = ref 0. in
          List.iter (fun p ->
		       sum := !sum +. p.vl *. p.wt;
		       count := !count +. p.wt)
            p.points;
          if !count = 0. then 0. else !sum /. !count
    | Bins h ->
	let count = ref 0.
	and sum = ref 0. in
	  Array.iteri (fun i w ->
			 sum := (!sum +. w
				 *. (h.min +. (h.bin_width /. 2.))
				 +. (float_of_int i) *. h.bin_width);
			 count := !count +. w)
            h.weights;
	  if !count = 0. then 0. else !sum /. !count


let assert_an str x =
  if Math.nan_p x
  then failwith (Wrutils.str "assert_an failed, from %s: Math.nan_p x" str);
  if not (x < infinity)
  then failwith (Wrutils.str "assert_an failed, from %s: not (x < infinity)"
		   str);
  if not (x > neg_infinity)
  then failwith (Wrutils.str "assert_an failed, from %s: not (x > neg_infinity)"
		   str)

let check t =
  match t.h with
    | Points p ->
	List.iter (fun p ->
		     assert_an "check (Points v)" p.vl;
		     assert_an "check (Points w)" p.wt;
		     assert (p.wt > 0.))
	  p.points;
	let len = List.length p.points in
	  assert (p.count == len);
	  assert (len <= p.max_points)
    | Bins h ->
	Array.iter (fun w ->
		      assert_an "check (Bins w)" w;
		      assert (w >= 0.))
	  h.weights;
	assert_an "check (h.min)" h.min (*;
      assert_an h.bin_width;
      assert (h.bin_width > 0.) *)


let copy t =
  let h = match t.h with
    | Points p -> Points { p with max_points = p.max_points }
    | Bins b -> Bins { b with weights = Array.copy b.weights }
  in { h = h }


let num_bins h =
  (** for a bins only *)
  Array.length h.weights


let max_pts t =
  match t.h with
    | Points p -> p.max_points
    | Bins h -> num_bins h


let bin_start h i =
  (** value at the left edge of bin [i] in [h] *)
  h.min +. (h.bin_width *. (float i))


let bin_end h i =
  (** Value at the right edge of bin [i] in [h].

      This is not implemented interms of bin_start because (even when
      inlined) OCaml boxes the return value of bin_start. *)
  h.min +. (h.bin_width *. (float (i + 1)))


let bucket h value =
  (** the index of the bin that should contain [value].  Note that any
      value outside the range of representable integers, such as
      [infinity], may yield a garbage value, such as 0!  If you might pass
      such a value, test before calling!  (This routine is intended to be
      simple and fast.) *)
  truncate ((value -. h.min) /. h.bin_width)


let is_mass x =
  x > 0.


let lowest_val h =
  (** might return neg_infinity *)
  try
    bin_start h (Wrarray.find is_mass h.weights)
  with Not_found -> neg_infinity


let highest_val h =
  (** might return infinity *)
  try
    bin_end h (Wrarray.rfind is_mass h.weights)
  with Not_found -> infinity


let min_val t =
  match t.h with
    | Points p -> (match p.points with
		       [] -> neg_infinity
		     | ps -> snd (Wrlist.fmin_by pt_vl ps))
    | Bins h -> lowest_val h


let max_val t =
  match t.h with
    | Points p -> (match p.points with
		       [] -> infinity
		     | ps -> snd (Wrlist.fmax_by pt_vl ps))
    | Bins h -> highest_val h


let points_range p =
  (** fails if empty!  called by [convert_to_bins] and convolution *)
  let min, max = Wrlist.fmin_and_fmax_by pt_vl p.points in
    min.vl, max.vl


let range t =
  (** of values actually present.  called by [add] *)
  (min_val t), (max_val t)


let total_weight t =
  match t.h with
    | Points p -> Wrlist.sum_floats pt_wt p.points
    | Bins h -> Array.fold_left (+.) 0. h.weights


let has_mass t =
  match t.h with
    | Points p -> p.points <> []
    | Bins b -> Wrarray.exists is_mass b.weights


(* NOT tail recursive - see below *)
let rec merge_points = function
    (** merges adjacent points with same value.  sort first! *)
  | [] -> []
  | p1::t ->
      match merge_points t with
(*
	| p2::t when p1.vl = p2.vl -> {p1 with wt = p1.wt +. p2.wt} ::t
*)
	| p2::t when Math.within p1.vl p2.vl 0.000000001 ->
	    { p1 with wt = p1.wt +. p2.wt } ::t
	| t -> p1::t

(* tail recursive version *)
let rec merge_points_tr pts =
  let rec helper pts accum =
    match pts with
      | []    ->  accum
      | [p]   ->  p::accum
      | p1::p2::t ->
	  if p1.vl = p2.vl
	  then helper ({p1 with wt = p1.wt +. p2.wt}::t) accum
	  else helper (p2::t) (p1::accum)
  in
    List.rev (helper pts [])


let compress_points p =
  (** sort and merge identical values *)
  (* FIXME to use tail recursive version when its been verified *)
  merge_points (Wrlist.sort_on ~cmp:Math.fcompare pt_vl p)



let tidy_points p =
  (** destructive *)
  p.points <- compress_points p.points;
  p.count <- List.length p.points



let weight_left_of vl t =
  match t.h with
    | Points p ->
	(tidy_points p);
	List.fold_left (fun s p ->
			  if p.vl <= vl
			  then s +. p.wt
			  else s) 0. p.points;
    | Bins b ->
	let ind = ref 0 in
	  Array.fold_left (fun s wt ->
			     let i = !ind in
			       incr ind;
			       let left = bin_start b i
			       and right = bin_end b i in
				 if right < vl
				 then s +. wt
				 else
				   if left > vl
				   then s
				   else
				     let t = right -. left in
				     let p = (vl -. left) /. t in
				       s +. (wt *. p))
	    0.
	    b.weights


(************** enlarging bins **************)

(* assumptions (when facing new max value):
   it's not worth checking for empties on the left and sliding bins over.
   it's not worth checking for no mass in any bin.
   we want to preserve any empty space on the left.

   scaling by an integer is better than smearing bins.  true?? *)


let new_max h new_val =
  (** scale bin width by an integral factor to allow for [new_max].
    doesn't worry about room at the other end. *)
  let n = num_bins h in
  let factor = ceil ((new_val -. h.min) /.
		       ((bin_end h (n - 1)) -. h.min)) in
    assert (factor >= 1.);
    h.bin_width <- h.bin_width *. factor;
    let w = h.weights
    and dest = ref 0 in
      (let src = ref 0
       and save_w0 = w.(0)
       and f = truncate factor in
	 while (!src < n) do
	   w.(!dest) <- 0.;
	   (let next = Math.imin n (!src + f) in
	      while (!src < next) do
		w.(!dest) <- w.(!dest) +. w.(!src);
		incr src
	      done);
	   incr dest
	 done;
	 w.(0) <- w.(0) +. save_w0);
      while (!dest < n) do
	w.(!dest) <- 0.;
	incr dest
      done


let new_min h new_val =
  (** like new_max, but compress to the right to make room for a new
    min.  I thought it might be simpler to duplicate code rather than
    generalize with lots of parameters. *)
  let n = (num_bins h) - 1 in
  let h_max = bin_end h n in
  let factor = ceil ((h_max -. new_val) /. (h_max -. h.min)) in
    assert (factor >= 1.);
    h.bin_width <- h.bin_width *. factor;
    let w = h.weights
    and dest = ref n in
      (let src = ref n
       and save_wn = w.(n)
       and f = truncate factor in
	 while (!src >= 0) do
	   w.(!dest) <- 0.;
	   (let next = Math.imax (-1) (!src - f) in
	      while (!src > next) do
		w.(!dest) <- w.(!dest) +. w.(!src);
		decr src
	      done);
	   decr dest
	 done;
	 w.(n) <- w.(n) +. save_wn);
      while (!dest >= 0) do
	w.(!dest) <- 0.;
	decr dest
      done;
      let new_min = h_max -. (float n) *. h.bin_width in
	h.min <- new_min


(************** adding mass to a histogram **************)


let quick_add_mass_to_bin value weight h =
  (** no range check *)
  let bin = bucket h value
  and w = h.weights
  in w.(bin) <- w.(bin) +. weight


let convert_to_bins t =
  match t.h with
    | Bins _ -> invalid_arg "convert_to_bins: given a bins histogram"
    | Points p ->
	let min, max = points_range p in
	let range = (if min = max
		     then if max = 0. then 0.1 else max *. 0.1
		     else max -. min) in
	  (* fill only middle 50% of bins *)
	let buffer = range /. 2. in
	let min = min -. buffer in
	let max = max +. buffer in
	let h = make_bins min max p.max_points in
	  List.iter (fun p -> quick_add_mass_to_bin p.vl p.wt h) p.points;
	  t.h <- Bins h


let consider_bins t =
  match t.h with
    | Bins _ -> invalid_arg "consider_bins: given a bins histogram"
    | Points p ->
	tidy_points p;
	if p.count > (Math.mul p.max_points 0.8)
	then convert_to_bins t


let rec add_mass value weight t =
  assert_an "add_mass (value)" value;
  assert_an "add_mass (weight)" weight;
  assert (weight > 0.);
  match t.h with
    | Points p ->
	if p.count = p.max_points
	then begin
	  consider_bins t;
	  add_mass value weight t;
	end else begin
	  p.count <- 1 + p.count;
	  p.points <- {vl=value; wt=weight} :: p.points
	end
    | Bins h ->
	let num_bins = num_bins h
	and bin = bucket h value in
	  if bin >= num_bins then
	    (new_max h value;
	     quick_add_mass_to_bin value weight h)
	  else if bin < 0 then
	    (new_min h value;
	     let bin = bucket h value in
	       if bin < 0 then invalid_arg "bin still not small enough";
	       quick_add_mass_to_bin value weight h)
	  else
	    h.weights.(bin) <- h.weights.(bin) +. weight


(*************** scaling **************)


let scale factor t =
  if factor = 0.
  then begin
    t.h <- Points { points = [];
		    count = 0;
		    max_points = max_pts t; }
  end else
    match t.h with
      | Points p ->
	  tidy_points p;
	  p.points <- List.map (fun p -> {p with wt = p.wt *. factor}) p.points
      | Bins h ->
	  let w = h.weights in
	    for i = 0 to ((Array.length w) - 1) do
	      w.(i) <- w.(i) *. factor
	    done


let normalize desired t = scale (desired /. (total_weight t)) t


let multiply t s =
  (** [multiply hist s] multiplies the values of the histogram by a
      scalar [s].  *)
  assert (not (Math.is_zero s));
  match t.h with
    | Points p ->
	p.points <- List.map (fun p -> {p with vl = p.vl *. s}) p.points
    | Bins b ->
	b.min <- b.min *. s;
	b.bin_width <- b.bin_width *. s


(*************** adding histograms together **************)

(* for adding together the distributions of child costs in order to do
   only one convolution.  This trades accuracy of child costs (probably
   not a big hit since they will presumably be similar) for speed (adding
   is linear not squared). Child distributions need to be created
   separately in order to weight them appropriately.

   also for forming the cumulative histogram of total # of nodes
*)


let all_points_total list =
  (** given a list of histograms, checks whether all are points and
      their total count is less than max_points (of an arbitrary member).
      If so, returns the total count (as a Some), else returns None *)
  let rec accrue accum = function
      [] -> Some accum
    | {h = (Bins _)}::_ -> None
    | {h = (Points p)}::t ->
	tidy_points p;
	let new_accum = accum + p.count in
	  if new_accum > p.max_points
	  then None
	  else accrue new_accum t
  in accrue 0 list


let sprinkle weight left right h =
  (** adds a [weight] that is uniformly distributed between [left] and
      [right] to the bins in [h], apportioning the weight appropriately.
      assumes [h] is sized to contain the values.

      This implementation may seem a bit brute-force... but it doesnt
      box any floats! *)
  let first = bucket h left
  and last = bucket h right in
  let wt = weight /. (right -. left) in
  let default_portion = wt *. h.bin_width in
  let ws = h.weights in
    if first = last
    then
      ws.(first) <- ws.(first) +. wt *. (right -. left)
    else
      for i = first to last do
	if i = first
	then
	  let right = bin_end h first in
	    ws.(i) <- ws.(i) +. wt *. (right -. left)
	else if i = last
	then
	  let left = bin_start h last in
	    ws.(i) <- ws.(i) +. wt *. (right -. left)
	else
	  ws.(i) <- ws.(i) +. default_portion
      done


let add ?(max_bins=(~-)1) list =
  (** adds multiple histograms together simultaneously for maximum
      accuracy.  If any are bins, or if total # points mandates, result
      will be bins. *)
  match List.filter has_mass list with
      [] -> invalid_arg "no hists to add!"
    | one::[] -> copy one
    | list ->
	match all_points_total list with
	    None ->
	      let min, max = Wrlist.fmin_and_fmax_across range list in
	      let res = make_bins min max (Math.imax
					     max_bins
					     (max_pts (List.hd list))) in
		List.iter
		  (function
		     | { h = Points p } ->
			 List.iter (fun p ->
				      assert (p.vl >= min);
				      quick_add_mass_to_bin p.vl p.wt res)
			   p.points
		     | { h = Bins h } ->
			 Array.iteri (fun i w ->
					if Math.is_positive w then
					  let left = bin_start h i in
					    sprinkle w left
					      (left +. h.bin_width)
					      res)
			   h.weights)
		  list;
		{h = Bins res}
	  | Some c ->
	      let p =
		Points
		  { points = (Wrlist.mapcan
				(function
				   | {h = Bins _} -> invalid_arg "bins?"
				   | {h = Points p} -> p.points) list);

		    count = c;
		    max_points = Math.imax max_bins (max_pts (List.hd list));
		  }
	      in {h = p}


(********************* convolving ********************

  "additive convolution" adds the values and multiplies the probabilities:
  for each v1,w1,
  for each v2,w2,
  v1+v2, w1 * w2

  for instance, one hist is the current node cost distribution and the
  other is the distribution of costs of a particular outgoing arc.

  when convolving, we don't leave any spare room in the resulting histogram

**************)


let convolve_points p1 p2 =
  tidy_points p1;
  tidy_points p2;
  let total = p1.count * p2.count in
    if (total > p1.max_points) then
      (* make bins and add points *)
      let min1, max1 = points_range p1
      and min2, max2 = points_range p2 in
      let h = make_bins (min1 +. min2) (max1 +. max2) p1.max_points in
	List.iter (fun p2 ->
		     List.iter (fun p1 ->
				  (quick_add_mass_to_bin
				     (p1.vl +. p2.vl)
				     (p1.wt *. p2.wt) h))
		       p1.points)
	  p2.points;
	Bins h
    else (* concat points *)
      Points { points = Wrlist.mapcan (fun p2 ->
					 List.map (fun p1 ->
						     { vl = p1.vl +. p2.vl;
						       wt = p1.wt *. p2.wt;
						     })
					   p1.points)
	  p2.points;
	       count = total;
	       max_points = p1.max_points; }


let convolve_bins_points h p =
  (* superimpose copies of [h] for every point in [p] *)
  try
    let min1 = lowest_val h
    and max1 = highest_val h
    and min2, max2 = points_range p in
    let res = make_bins (min1 +. min2) (max1 +. max2) p.max_points in
      List.iter (fun p2 ->
		   Array.iteri (fun i w1 ->
				  if Math.is_positive (w1 *. p2.wt) then
				    let left = (bin_start h i) +. p2.vl  in
				      sprinkle (w1 *. p2.wt)
					left (left +. h.bin_width) res)
		   h.weights)
	p.points;
      Bins res
  with Not_found -> invalid_arg "convolve_bins_points: empty hist"


let convolve_bins h1 h2 =
  (* treat [h2] as points. (Lisp version treated both as points!) *)
  let res = make_bins
    ((lowest_val h1) +. (lowest_val h2))
    ((highest_val h1) +. (highest_val h2))
    (num_bins h1)
  and h2_half = h2.bin_width /. 2. in
    Array.iteri (fun i2 w2 ->
		   if w2 > 0. then
		     let shift = (bin_start h2 i2) +. h2_half in
		       Array.iteri (fun i1 w1 ->
				      if Math.is_positive (w1 *. w2) then
					let left = (bin_start h1 i1) +.
					  shift  in
					  sprinkle (w1 *. w2)
					    left (left +. h1.bin_width) res)
			 h1.weights)
      h2.weights;
    Bins res


let convolve a b =
  match (a, b) with
    (Points p1, Points p2) ->
      convolve_points p1 p2
  | (Points p, Bins h)
  | (Bins h, Points p) ->
      convolve_bins_points h p
  | (Bins h1, Bins h2) ->
      convolve_bins h1 h2


(****** convolve with pruning ******)


let convolve_points_pruning p1 p2 prune =
  tidy_points p1;
  tidy_points p2;
  let total = p1.count * p2.count in
    if (total > p1.max_points) then
      (* make bins and add points *)
      let min1, max1 = points_range p1
      and min2, max2 = points_range p2 in
      let h =
	make_bins
	  (Math.fmin (min1 +. min2) prune)
	  (Math.fmin (max1 +. max2) prune)
	  p1.max_points
      in
	List.iter (fun p2 ->
		     List.iter (fun p1 ->
				  let v = p1.vl +. p2.vl in
				    if v <= prune then
				      (quick_add_mass_to_bin
					 v (p1.wt *. p2.wt) h))
		       p1.points)
	  p2.points;
	Bins h
    else
      (* concat points *)
      let points =
	Wrlist.mapcan (fun p2 ->
			 Wrlist.map_opt (fun p1 ->
					   let v = p1.vl +. p2.vl in
					     if v <= prune then
					       Some { vl = v;
						      wt = p1.wt *. p2.wt; }
					     else None)
			   p1.points)
	  p2.points in
	Points { points = points;
		 count = List.length points;
		 max_points = p1.max_points; }


let convolve_bins_points_pruning h p prune =
  (* superimpose copies of [h] for every point in [p] *)
  try
    let min1 = lowest_val h
    and max1 = highest_val h
    and min2, max2 = points_range p in
    let res =
      make_bins
	(Math.fmin (min1 +. min2) prune)
	(Math.fmin (max1 +. max2) prune)
	p.max_points
    in
      List.iter (fun p2 ->
		   Array.iteri
		     (fun i w1 ->
			if Math.is_positive w1 && Math.is_positive p2.wt  then
			  let left = (bin_start h i) +. p2.vl  in
			    if left < prune then
			      let right =
				Math.fmin prune (left +. h.bin_width) in
			      let p = (right -. left) /. h.bin_width in
				if Math.is_positive (p *. (w1 *. p2.wt)) then
				  sprinkle (p *. (w1 *. p2.wt)) left right res)
		     h.weights)
	p.points;
      Bins res
  with Not_found -> invalid_arg "convolve_bins_points_pruning: empty hist"


let convolve_bins_pruning h1 h2 prune =
  (* treat [h2] as points. (Lisp version treated both as points!) *)
  let res = make_bins
    (Math.fmin ((lowest_val h1) +. (lowest_val h2)) prune)
    (Math.fmin ((highest_val h1) +. (highest_val h2)) prune)
    (num_bins h1)
  and h2_half = h2.bin_width /. 2. in
    Array.iteri (fun i2 w2 ->
		   if Math.is_positive w2 then
		     let shift = (bin_start h2 i2) +. h2_half in
		       Array.iteri
			 (fun i1 w1 ->
			    if Math.is_positive w1 then
			      let left = (bin_start h1 i1) +. shift in
				if left < prune then
				  let right =
				    Math.fmin prune (left +. h1.bin_width)
				  in let p = (right -. left) /. h1.bin_width in
				  let w = p *. (w1 *. w2) in
				    if Math.is_positive w
				    then sprinkle w left right res)
			 h1.weights)
      h2.weights;
    Bins res


let convolve_pruning a b v =
  let h = match (a.h, b.h) with
    | (Points p1, Points p2) -> convolve_points_pruning p1 p2 v
    | (Points p, Bins h)
    | (Bins h, Points p) -> convolve_bins_points_pruning h p v
    | (Bins h1, Bins h2) -> convolve_bins_pruning h1 h2 v
  in {h = h}


(*********** convolve_max ******************

  for each v1,w1
     for each v2,w2
        max(v1,v2), w1 * w2

  so for each value in h1, we get
    1) sum of that part of h2 lying to left of v1 as a single point at v
    2) copy of that part of h2 lying the right of v.

  even though processing seems asymmetric, the semantics are.

  FIXME - fix convolve_max bug - index out of bounds error

***********************************)


let rec incr_b_pts (av : float) b_pts b_wt =
  (** returns the suffix of [b_pts] containing pts whose value is
      greater than [av] and the sum of the weights of the
      points in the removed prefix (plus [b_wt]) *)
  match b_pts with
      b ::t when b.vl <= av ->
	incr_b_pts av t (b_wt +. b.wt)
    | _ -> b_pts, b_wt


let convolve_max_points a b =
  tidy_points a;
  tidy_points b;
  let total = a.count * b.count in
    if (total > a.max_points) then
      (* make bins and add points *)
      let a_pts = a.points
      and b_pts = ref b.points in
      let min = Math.fmax (pt_vl (List.hd a_pts))
	(pt_vl (List.hd !b_pts))
      and max = Math.fmax (pt_vl (Wrlist.last a_pts))
	(pt_vl (Wrlist.last !b_pts)) in
      let h = make_bins min max a.max_points in
	(* sum of weight of points removed from head of b_pts *)
      let b_wt = ref 0. in
	List.iter (fun apt ->
		     let bps,bw = incr_b_pts apt.vl !b_pts !b_wt in
		       b_pts := bps; b_wt := bw;
		       if !b_wt > 0. then
			 quick_add_mass_to_bin apt.vl (apt.wt *. !b_wt) h;
		       List.iter (fun bpt ->
				    quick_add_mass_to_bin
				      bpt.vl (apt.wt *. bpt.wt) h)
			 !b_pts)
	  a_pts;
	Bins h
    else (* concat points *)
      let b_pts = ref b.points
      and b_wt = ref 0. in
      let pts = Wrlist.mapcan (fun a ->
				 let bps,bw = incr_b_pts a.vl !b_pts !b_wt in
				   b_pts := bps; b_wt := bw;
				   let rest =
				     List.map
				       (fun b -> {b with wt = a.wt *. b.wt})
				       bps
				   in
				     if !b_wt > 0.
				     then {a with wt = (a.wt *. !b_wt)}::rest
				     else rest)
	a.points in
	Points { points = pts;
		 count = List.length pts;
		 max_points = a.max_points; }


let sprinkle_bins_clamped av aw b b_bin b_wt bound res =
  (** add bins of [b] to [res], clamping from below at [av] and above
    at [bound].  we assume that sequential calls encounter the [av] in
    ascending order.  [b_bin] is index of leftmost bin we will need to
    consider splitting. [b_wt] is the total weight of the bins before
    [b_bin]. we also assume that [av] <= [bound]. *)
  assert (aw > 0.);
  let bw = b.weights in
  let b_max = Array.length bw in
    while ((bin_end b !b_bin) <= av) && (!b_bin < b_max)  do
      b_wt := !b_wt +. bw.(!b_bin);
      incr b_bin;
    done;
    if !b_wt > 0. then
      quick_add_mass_to_bin av (aw *. !b_wt) res;
    if !b_bin != b_max then
      (* av is below end of b_bin (otherwise av is above all bins) *)
      let b_start = bin_start b !b_bin
      and next = ref !b_bin in
	if b_start < av then
	  (* av intersects bin - split it (otherwise av is below all bins) *)
	  (incr next;
	   let bw = bw.(!b_bin)
	   and portion_clamped = (av -. b_start) /. b.bin_width in
	   let wt_clamped = bw *. portion_clamped in
	     if wt_clamped > 0. then
	       quick_add_mass_to_bin av (aw *. wt_clamped) res;
	     let remaining = bw -. wt_clamped in
	       if Math.is_positive remaining then
		 sprinkle (aw *. remaining) av (bin_start b !next) res);
	(* up to start of next has been taken care of.
	   now, from next up, add entire bins *)

	(* NEED TO STOP sprinkling AT BOUND *)
	Wrutils.write_this ();


	for i = !next to b_max - 1 do
	  let bw = bw.(i) in
	    if Math.is_positive bw then
	      let left = bin_start b i in
		sprinkle (aw *. bw) left (left +. b.bin_width) res
	done


let sprinkle_bins av aw b b_bin b_wt res =
  sprinkle_bins_clamped av aw b b_bin b_wt infinity res


let convolve_max_pts_bins a b =
  tidy_points a;
  let pts = a.points in
  let min_v = Math.fmax (pt_vl (List.hd pts)) (lowest_val b)
  and max_v = Math.fmax (pt_vl (Wrlist.last pts)) (highest_val b) in
  let res = make_bins min_v max_v a.max_points
  and b_bin = ref 0
		(* weight from bins before b_bin *)
  and b_wt = ref 0. in
    List.iter (fun a ->
		 sprinkle_bins a.vl a.wt b b_bin b_wt res)
      pts;
    Bins res


let convolve_max_bins a b =
  (* treat [a] like points *)
  let min_v = Math.fmax (lowest_val a) (lowest_val b)
  and max_v = Math.fmax (highest_val a) (highest_val b) in
  let res = make_bins min_v max_v (num_bins a)
  and a_half = a.bin_width /. 2.
  and b_bin = ref 0
		(* weight from bins before b_bin *)
  and b_wt = ref 0. in
    Array.iteri (fun ai aw ->
		   if aw > 0. then
		     let av = (bin_start a ai) +. a_half in
		       sprinkle_bins av aw b b_bin b_wt res)
      a.weights;
    Bins res


let convolve_max parent arc =
  match (parent, arc) with
    (Points p1, Points p2) -> convolve_max_points p1 p2
  | (Points p, Bins h)
  | (Bins h, Points p) -> convolve_max_pts_bins p h
  | (Bins h1, Bins h2) -> convolve_max_bins h1 h2


(********** convolve_max with pruning! ***************)


let convolve_max_pruning_points a b bound =
  tidy_points a;
  tidy_points b;
  let total = a.count * b.count in
    if (total > a.max_points) then
      (* make bins and add points *)
      let a_pts = a.points
      and b_pts = ref b.points in
      let min = Math.fmin bound (Math.fmax (pt_vl (List.hd a_pts))
				   (pt_vl (List.hd !b_pts)))
      and max = Math.fmin bound (Math.fmax (pt_vl (Wrlist.last a_pts))
				   (pt_vl (Wrlist.last !b_pts))) in
      let h = make_bins min max a.max_points in
	(* sum of weight of points removed from head of b_pts *)
      let b_wt = ref 0. in
	List.iter (fun a ->
		     if a.vl <= bound then
		       let bps,bw = incr_b_pts a.vl !b_pts !b_wt in
			 b_pts := bps; b_wt := bw;
			 if !b_wt > 0. then
			   quick_add_mass_to_bin a.vl (a.wt *. !b_wt) h;
			 List.iter (fun b ->
				      if b.vl <= bound then
					quick_add_mass_to_bin
					  b.vl (a.wt *. b.wt) h)
			   !b_pts)
	  a_pts;
	Bins h
    else (* concat points *)
      let b_pts = ref b.points
      and b_wt = ref 0. in
      let pts = Wrlist.mapcan (fun a ->
				 if a.vl <= bound then
				   let bps,bw = incr_b_pts a.vl !b_pts !b_wt in
				     b_pts := bps; b_wt := bw;
				     let rest =
				       Wrlist.map_opt
					 (fun b ->
					    if b.vl <= bound then
					      Some {b with wt = a.wt *. b.wt}
					    else
					      None)
					 bps in
				       if !b_wt > 0.
				       then {a with wt = a.wt *. !b_wt}::rest
				       else rest
				 else [])
	a.points in
	Points { points = pts;
		 count = List.length pts;
		 max_points = a.max_points; }


let convolve_max_pruning_pts_bins a b bound =
  tidy_points a;
  let pts = a.points in
  let min_v = Math.fmin bound (Math.fmax (pt_vl (List.hd pts)) (lowest_val b))
  and max_v = Math.fmin bound (Math.fmax
				 (pt_vl (Wrlist.last pts)) (highest_val b)) in
  let res = make_bins min_v max_v a.max_points
  and b_bin = ref 0
  (* weight from bins before b_bin *)
  and b_wt = ref 0. in
    List.iter (fun a ->
		 if a.vl <= bound then
		   sprinkle_bins_clamped a.vl a.wt b b_bin b_wt bound res)
      pts;
    Bins res


let convolve_max_pruning_bins a b bound =
  (* treat [a] like points *)
  let min_v = Math.fmin bound (Math.fmax (lowest_val a) (lowest_val b))
  and max_v = Math.fmin bound (Math.fmax (highest_val a) (highest_val b)) in
  let res = make_bins min_v max_v (num_bins a)
  and a_half = a.bin_width /. 2.
  and b_bin = ref 0
  (* weight from bins before b_bin *)
  and b_wt = ref 0. in
    Array.iteri (fun ai aw ->
		   if aw > 0. then
		     let av = (bin_start a ai) +. a_half in
		       if av <= bound then
			 sprinkle_bins_clamped av aw b b_bin b_wt bound res)
      a.weights;
    Bins res


let convolve_max_pruning parent arc bound =
  let h = match (parent.h, arc.h) with
    | (Points p1, Points p2) -> convolve_max_pruning_points p1 p2 bound
    | (Points p, Bins h)
    | (Bins h, Points p) -> convolve_max_pruning_pts_bins p h bound
    | (Bins h1, Bins h2) -> convolve_max_pruning_bins h1 h2 bound
  in { h = h }


(*************** chopping off excess weight **************)


let rec get_more to_go = function
    (** returns prefix of pts whose weight is >= [to_go], and the
      number of points in the prefix *)
  | [] -> [], 0
  | h::t ->
      if to_go <= 0. then
	[], 0
      else
	let tail, count = get_more (to_go -. (pt_wt h)) t in
	  (h::tail), (count + 1)


let prune_weight_right weight hist =
  (** leaves at least [weight] in [hist] if possible *)
  match hist with
    Points p ->
      let truncated, count = get_more weight (compress_points p.points) in
	p.points <- truncated;
	p.count <- count
  | Bins h ->
      (* we assume that it's never worth converting back to points or
	 otherwise expanding out to fill num_bins *)
      let w = h.weights in
      let len = Array.length w
      and accum = ref w.(0)
      and next = ref 1 in
	while ((!accum < weight) &&
	       (!next < len)) do
	  accum := !accum +. w.(!next);
	  incr next
	done;
	(* next is the one *after* the one that put us at or over *)
	for i = !next to len - 1 do
	  w.(i) <- 0.
	done


(*************** pruning above a value **************)


let prune_value_right value t =
  match t.h with
    | Points p ->
	tidy_points p;
	p.points <- List.filter (fun a -> a.vl <= value) p.points;
	p.count <- List.length p.points
    | Bins h ->
	if value < infinity then
	  (* Leaves weight in bin for [value].  As in
	     [prune_weight_right], we assume it's not worth adjusting the
	     bins to fill empty slots. *)
	  let first = Math.imax 0 ((bucket h value) + 1)
	  and w = h.weights in
	    for i = first to (num_bins h) - 1 do
	      w.(i) <- 0.
	    done


(*************** weight-quantile **************)


let points_min pairs =
  (** left edge for interpolating around first point.  Ie, where we
      consider the distribution defined by the points to begin. *)
  match pairs with
    | p1 :: p2 :: _ -> p1.vl -. ((p2.vl -. p2.vl) /. 2.)
    | p1::[] -> p1.vl *. 0.99
    | [] -> neg_infinity


let rec find_val desired so_far prev_v = function
    (** given a list of points, returns the value to get [desired]
      total weight, where we have [so_far] already through [prev_v].
      similar to [get_more], above, except it doesn't construct prefix
      and it interpolates a value. *)
  | [] -> infinity
  | pt::t ->
      let accum = so_far +. pt.wt in
	if accum >= desired then
	  (* make an imaginary bin from halfway between v and the
	     points on each side.  assume wt accumulates linearly through
	     there *)
	  let next_v = (match t with
			  p2::_ -> p2.vl
			    (* this extension is reasonable because
			       we do in fact have enough accumulated
			       weight *)
			| [] -> pt.vl +. (pt.vl -. prev_v)) in
	  let start = (prev_v +. pt.vl) /. 2.
	  and finish = (pt.vl +. next_v) /. 2. in
	  let p = (desired -. so_far) /. pt.wt in
	    start +. ((finish -. start) *. p)
	else
	  find_val desired accum pt.vl t


let rec find_val_no_interp desired so_far prev_v = function
    (* get the value that will give at least the desired weight.
       Don't interpolate. *)
  | [] -> infinity
  | pt::t ->
      let accum = so_far +. pt.wt in
	if accum >= desired then
	  pt.vl
	else
	  find_val_no_interp desired accum pt.vl t


let find_sample_val wt pts =
  (** [find_sample_val wt pts] finds the value for the desired weight.
      This function has a random chance of being inclusive of a value
      just beyond the desired weight. *)
  let rec get_value prev accum = function
    | [] -> nan
    | pt :: [] -> pt.vl
    | pt :: tl ->
	let accum' = accum +. pt.wt in
	  if accum' >= wt then
	    if Random.bool () then pt.vl else prev
	  else
	    get_value pt.vl accum' tl
  in
    match pts with
      | [] -> nan
      | pt :: tl when pt.wt >= wt -> pt.vl
      | pt :: tl -> get_value pt.vl pt.wt tl


let scan_val h desired =
  let w = h.weights in
  let max_i = (Array.length w) - 1 in
  let rec accum_wt i before =
    let after = before +. w.(i) in
      if after < desired then
      	if i = max_i then
	  infinity
	else
	  accum_wt (i + 1) after
      else
	let p = (desired -. before) /. w.(i) in
	  (bin_start h i) +. (p *. h.bin_width)
  in
    accum_wt 0 0.


let val_for_weight desired t =
  (** want to accumulate exactly [desired] on the left *)
  match t.h with
    | Points p ->
	tidy_points p;
	(match p.points with
	   | [] -> infinity
(*
	   | sorted -> find_val desired 0. (points_min sorted) sorted)
*)
	   | sorted -> find_val_no_interp desired 0. (points_min sorted) sorted)
    | Bins h -> scan_val h desired


let sample t =
  (** [sample t] samples a value from this distribution.  Results in
      nan if the histogram is empty. *)
  let wt = (total_weight t) *. (Random.float 1.) in
    match t.h with
      | Points p ->
	  tidy_points p;
	  begin match p.points with
	    | [] -> nan
	    | sorted -> find_sample_val wt sorted
	  end
      | Bins h -> scan_val h wt


(*****************************************************************************)
(* Some code for testing on synthetic trees                                  *)
(*****************************************************************************)
let rec find_val_and_bound desired so_far prev_v = function
    (** given a list of points, returns the value to get [desired]
      total weight, where we have [so_far] already through [prev_v].
      similar to [get_more], above, except it doesn't construct prefix
      and it interpolates a value. *)
  |[] -> infinity, 0., infinity
  | pt::t ->
      let accum = so_far +. pt.wt in
	if accum < desired then
    find_val_and_bound desired accum pt.vl t
  else if accum > desired then
	  (* make an imaginary bin from halfway between v and the
	     points on each side.  assume wt accumulates linearly through
	     there *)
	  let next_v = (match t with
			  p2::_ -> p2.vl
			    (* this extension is reasonable because
			       we do in fact have enough accumulated
			       weight *)
			| [] -> pt.vl +. (pt.vl -. prev_v)) in
	  let start = (prev_v +. pt.vl) /. 2.
	  and finish = (pt.vl +. next_v) /. 2. in
	  let p = (desired -. so_far) /. pt.wt in
	    start +. ((finish -. start) *. p), so_far, accum
	else
	  pt.vl, accum, accum (* accum = desired *)


let scan_val_and_bound h desired =
  let w = h.weights in
  let max_i = (Array.length w) - 1 in
  let rec accum_wt i before =
    let after = before +. w.(i) in
      if after < desired then
      	if i = max_i then
	        infinity, after, infinity
      	else
	        accum_wt (i + 1) after
      else
	      let p = (desired -. before) /. w.(i) in
	      (bin_start h i) +. (p *. h.bin_width), before, after
  in
    accum_wt 0 0.

(** returns (val, upper_bound) - for testing on synthetic tree *)
let val_for_weight_and_bound desired t =
  (** want to accumulate exactly [desired] on the left *)
  match t.h with
    | Points p ->
	tidy_points p;
	(match p.points with
	   | [] -> infinity, 0., infinity
	   | sorted ->
	       find_val_and_bound desired 0. (points_min sorted) sorted)
    | Bins h -> scan_val_and_bound h desired


let weight_for_val t vl =
  match t.h with
    | Points p ->
	tidy_points p;
	List.fold_left
	  (fun ret p -> if p.vl = vl then p.wt else ret)
	  0. p.points
    | Bins b ->
	b.weights.(bucket b vl)

(*************** pruning combination on weight **************)


let accum_wt_pts p =
  tidy_points p;
  let p = ref p.points
  (* up to but not including head of p *)
  and accum = ref 0. in
  let peek_next () =
    (** next value at which accumulated weight will change *)
    match !p with
      [] -> infinity
    | pt::_ -> pt.vl
  and advance () =
    (** advance through next value and accumulate more weight *)
    match !p with
      [] -> failwith "can't advance point"
    | pt::t ->
	accum := !accum +. pt.wt;
	p := t
  and wt_before_val _ =
    (** guaranteed that arg will be <= next unless exhausted points *)
    !accum
  in
    peek_next, advance, wt_before_val


let accum_wt_bins h =
  (* bin at whose end the accumulated weight will change *)
  let curr = ref 0
  (* up to start of curr *)
  and accum = ref 0.
  and len = num_bins h in
  let peek_next () =
    if !curr = len then
      infinity
    else
      bin_end h !curr
  and advance () =
    accum := !accum +. h.weights.(!curr);
    incr curr
  and wt_before_val v =
    if v < h.min then 0.
    else if !curr = len then !accum
    else
      (* v is <= end. no other bin is better than curr *)
      !accum +. (((v -. (bin_start h !curr)) /. h.bin_width) *.
		   h.weights.(!curr))
  in
    peek_next, advance, wt_before_val


let accum_wt t =
  match t.h with
    | Points p -> accum_wt_pts p
    | Bins h -> accum_wt_bins h


let bound_for_wted_combo desired a b b_wt =
  (* Set the bound to a value that gets close to [desired] weight.
     This will get more than [desired] if it is closer than
     undershooting. *)
  let pt_a, advance_a, wt_a = accum_wt a
  and pt_b, advance_b, wt_b = accum_wt b in
  let v = ref (min (pt_a ()) (pt_b ())) in
    while (!v < infinity) && ((wt_a !v) +. ((wt_b !v) *. b_wt) < desired) do
      if (pt_a ()) < (pt_b ()) then
	advance_a ()
      else
	advance_b ();
      v := Math.fmin (pt_a ()) (pt_b ())
    done;
    !v
    (*

    (* remove the !v and uncomment the following to allow the bound
      code to over-shoot the desired weight if that is still closer. *)

    let v_wt = (wt_a !v) +. ((wt_b !v) *. b_wt) in
      if !v < infinity
      then begin
	let v' = (if (pt_a ()) < (pt_b ())
		  then advance_a ()
		  else advance_b ();
		  Math.fmin (pt_a ()) (pt_b ()))
	in
	let v'_wt = (wt_a v') +. ((wt_b v') *. b_wt) in
	  if abs_float (desired -. v'_wt) < abs_float (desired -. v_wt)
	  then v'
	  else !v
      end else infinity
    *)


(*************** I/O **************)

(**
 * Outputs this histogram. Format is:
 *
 *  For Points: count points max_points total, foreach: value weight weight_so_far so_far/total
 *  For Bins:   num_bins  total bin_weights, foreach: bin_start+half weight accum %total
 *)
let fprint ch x =
  let total = total_weight x in
    match x.h with
      | Points p ->
	  tidy_points p;

	  let sorted = compress_points p.points in
	    Printf.fprintf ch
	      "Histogram with %d=%d of %d possible points (wt %f):\n"
	      p.count (List.length sorted) p.max_points total;
	    let accum = ref 0. in
	      List.iter (fun pt ->
			   accum := !accum +. pt.wt;
			   Printf.fprintf ch "%f\t%f\t%f\t%f\n"
			     pt.vl pt.wt !accum (!accum /. total))
		sorted
      | Bins h ->
	  Printf.fprintf ch
	    "Histogram with %d bins width=%f (wt %f):\n"
	    (num_bins h) h.bin_width total;
	  let half = h.bin_width /. 2.
	  and accum = ref 0. in
	    Array.iteri (fun i w ->
			   accum := !accum +. w;
			   Printf.fprintf ch "%f\t%f\t%f\t%f\n"
			     ((bin_start h i) +. half)
			     w !accum (!accum /. total))
	      h.weights


(********* plotting **********)


let impulse_line = function
    [] -> [|0.,0.; 1.0, 0.|]
  | pairs ->
      let classes = Wrlist.classes pt_vl pairs in
      let classes = Wrlist.sort_on (function pt::_ -> pt.vl
				      | _ -> failwith "bad class") classes in
	Array.of_list (Wrlist.mapcan (function
					  (pt::_) as pairs ->
					    [ pt.vl, 0.;
					      pt.vl,
					      Wrlist.sum_floats pt_wt pairs;
					      pt.vl, 0.]
					| _ -> failwith "bad class!")
			 classes)

(***
let hist_of_pairs p =
  if not (Wrlist.constant_p (List.map snd p.points)) then
    invalid_arg "Hist.plot assumes all points have equal weight!";
  Ps_plot.histogram path [(Array.of_list (List.map fst p.points)), ""]
    ~title:title "Value" "Number of Points"
***)

let bin_line h =
  let len = num_bins h in
  let pairs = Array.make ((len * 2) + 2) (h.min, 0.) in
    Array.iteri (fun i w ->
		   let base = (i * 2) + 1 in
		     pairs.(base) <- (bin_start h i), w;
		     pairs.(base + 1) <- (bin_end h i), w)
      h.weights;
    pairs.((len * 2) + 1) <- (bin_end h (len - 1)), 0.;
    pairs



(*
(* See blfs/hist_plot.ml *)
let plot ?(title = "") path t =
  let data, n, l = (match t with
		      Points p -> (impulse_line p.points), p.count, "points"
		    | Bins h -> (bin_line h), num_bins h, "bins") in
    Ps_plot.line path [data, (Wrutils.str "%d %s" n l)]
      ~zero_line:true title "Value" "Weight"
*)

let get_plot_data t =
  (** [get_plot_data t] gets data for plotting. *)
  match t.h with
    | Points p ->
	tidy_points p;
	(impulse_line p.points), p.count, "points"
    | Bins h -> (bin_line h), num_bins h, "bins"

(* EOF *)
