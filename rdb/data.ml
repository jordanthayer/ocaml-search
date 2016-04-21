(* $Id: data.ml,v 1.6 2006/11/20 19:50:20 ebitton Exp ruml $

   manipulating experimental data
*)


(***************** averaging multiple runs ************************

  useful for averaging performance profiles (quality over time) when there
  are many points and all runs cover mostly the same duration
*)


let between min max x =
  (x >= min) && (x <= max)


let num_data_between min max a =
  Wrarray.count (between min max) a


let gather_values min max q a =
  (** augments Dpq [q] with all values from [a] that fall between [min] and
    [max] *)
  Array.iter (fun x ->
		if between min max x
		then Dpq.insert q x)
    a


let extract_values q =
  (** returns an array of all values from [q] in sorted order without
    duplicates.  destructively empties [q]. *)
  let a = Array.make (Dpq.count q) 0.
  and count = ref 0 in
    while (not (Dpq.empty_p q)) do
      let x = Dpq.extract_first q in
	if ((!count = 0) ||
	    (a.(!count - 1) <> x)) then
	  (a.(!count) <- x;
	   incr count)
    done;
    Array.sub a 0 !count


let within_common_range cols =
  (** given list of float arrays, returns "master col" (float array)
    containing union over all cols of those values that fall between largest
    first col value and smallest last col value *)
  let _, min = Wrlist.max_by (fun a -> a.(0)) cols
  and _, max = Wrlist.min_by Wrarray.last cols in
  let q = Dpq.create_with (<=) 0. in
    List.iter (gather_values min max q) cols;
    let master = extract_values q in
      if (Array.length master) < 2
      then failwith "not enough overlap between cols";
      master


let sample_step_data master (ind, dep) =
  (** assume [ind] and [dep] are a step function.  returns
    interpolation at points in [master].  assumes that values in
    [master] are within range of [ind] *)
  let i = ref 0
  and len = Array.length ind in
    Array.map (fun desired ->
		 (* ensure rightmost of any equal ind values *)
		 while ((!i < len) && (ind.(!i) <= desired)) do
		   incr i
		 done;
		 (* now have that ind.(!i) > desired *)
		 dep.(!i - 1))
      master


let normalized_cols pairs =
  (** takes list of pairs of float arrays.  interpolates inside all pairs
    to the largest common interval *)
  let master = within_common_range (List.map fst pairs) in
  let deps = List.map (sample_step_data master) pairs in
    master, deps


let mean_col cols =
  (** assumes [cols] are aligned and the same length *)
  let num_cols = float_of_int (List.length cols) in
    assert (num_cols > 0.);
    let len = Array.length (List.hd cols) in
      Array.init len
	(fun i ->
	   (Wrlist.sum_floats (fun a -> a.(i)) cols) /. num_cols)


let mean_pair pairs =
  (** takes list of pairs of float arrays.  assume step function when
    interpolating.  returns data for each x for which all pairs have data. *)
  let master, deps = normalized_cols pairs in
    master, (mean_col deps)


(********** another way of averaging runs ***********)


let all_points cols =
  let all = Wrarray.remove_dups_if (=) (Array.concat cols) in
    Array.sort compare all;
    all


let interpolate x1 y1 x2 y2 x =
  let p = (x -. x1) /. (x2 -. x1) in
    y1 +. (p *. (y2 -. y1))


let rec interpolate_from m_x x y i =
  (** given master x value and cols [x] and [y], determine if cols have
    data at that x, looking at and to the right of index [i].  if so, return
    Some y value and new [i] value.  Else None. *)
  if (x.(i) = m_x) then
    Some (y.(i), i)
  else if (x.(i) < m_x) then
    let i1 = i + 1 in
      if (i1 < (Array.length x)) then
	if (x.(i1) <= m_x) then
	  (* advance index *)
	  interpolate_from m_x x y i1
	else
	  let m_y = interpolate x.(i) y.(i) x.(i1) y.(i1) m_x in
	    Some (m_y, i)
      else
	(* m_x > x.(last) *)
	None
  else
    (* m_x < x.(0) *)
    None


let linear_interpolate_all master pairs =
  (** computes y values corresponding to the x values in [master].  uses
    data from whichever cols have data in that range.  returns +- confidence
    interval for each point as a second value *)
  let n = Array.length master in
  let conf = Array.make n 0.
  and count = Array.make n 0
		(* for each pair, earliest index to consider *)
  and indices = Array.make (List.length pairs) 0 in
  let mean = Array.mapi
	       (fun m_i m_x ->
		  let c = ref 0
		  and accum = ref 0.
		  and accum2 = ref 0. in
		    Wrlist.iteri (fun p_i (x,y) ->
				    match (interpolate_from m_x x y
					     indices.(p_i)) with
				      None -> ()
				    | Some (m_y, i) ->
					(accum := !accum +. m_y;
					 accum2 := !accum2 +. (m_y *. m_y);
					 c := !c + 1;
					 indices.(p_i) <- i))
		      pairs;
		    count.(m_i) <- !c;
		    let count = float !c in
		      (let var = (!accum2 -. ((!accum *. !accum) /. count)) /.
				   (count -. 1.) in
		       let radius = 1.96 *. (sqrt(var) /. sqrt(count)) in
			 conf.(m_i) <- radius);
		      !accum /. count)
	       master in
    mean, conf, count


let mean_pair_linear_union pairs =
  (** linear interpolation between data points, results for all x values *)
  let master = all_points (List.map fst pairs) in
  let mean, conf, count = linear_interpolate_all master pairs in
    master, mean, conf, count


let extend_to_max pairs =
  (* extend last y value of every pair to the maximum x of any pair *)
  let max_x = ref 0. in
    List.iter (fun (x,_) ->
		 let last_x = x.(-1 + (Array.length x)) in
		 if !max_x < last_x then
		   max_x := last_x)
      pairs;
    List.map (fun (x,y) ->
		 let last_x = x.(-1 + (Array.length x))
		 and last_y = y.(-1 + (Array.length y)) in
		   if last_x < !max_x then
		     (Wrarray.extend x 1 !max_x), (Wrarray.extend y 1 last_y)
		   else
		     x, y)
      pairs


let mean_pair_linear_common pairs =
  (** linear interpolation between data points, results for all x values *)
  let pairs = extend_to_max pairs in
  let master = within_common_range (List.map fst pairs) in
  let mean, conf, count = linear_interpolate_all master pairs in
    if not (Wrarray.constant_p count) then
      failwith "didn't use same number of pairs for every point!";
    master, mean, conf


(***************** time normalization ************************)


(* benchmark:

   original:
   ~/projects/csp/bin/csp_solver.linux < ~/projects/csp/data/instance/30/15/174/81/2

   as of Jan 6, 2005:
   ~/projects/csp/bin/csp_solver.linux dfs -never < ~/projects/csp/data/instance/random_binary/30/15/174/81/2

   To do:

   heuristic search benchmark
      make it standalone
   update katsura_id
   multiple benchmark types
     function set_timing_benchmark_type to set type
   run the benchmark on all our machines

*)
let get_benchmark =
  let benchmark_table =
    [
      "katsura.parc.xerox.com-i386-linux-i386-linux-intel-Unix-3.07+2", 33.1;
      "nova.parc.xerox.com-i386-linux-i386-linux-intel-Unix-3.07+2", 41.56;
      "moon.parc.xerox.com-i386-linux-i386-linux-intel-Unix-3.07+2", 38.16;

      "katsura.parc.xerox.com-i386-linux-i386-linux-intel-Unix-3.08.1", 32.98;
      "nova.parc.xerox.com-i386-linux-i386-linux-intel-Unix-3.08.1", 41.59;
      "moon.parc.xerox.com-i386-linux-i386-linux-intel-Unix-3.08.1", 33.65;

      "katsura.parc.xerox.com-i386-linux-i386-linux-intel-Unix-3.08.2", 34.74;
      "moon.parc.xerox.com-i386-linux-i386-linux-intel-Unix-3.08.2", 36.09;
      "nova.parc.xerox.com-i386-linux-i386-linux-intel-Unix-3.08.2", 40.42;

      (* these numbers are entirely provisional until I can recompile the
	 benchmark using ocaml 3.09.2 *)
      "carrot-i386-linux-i386-linux-intel-Unix-3.09.2", 15.25;
      "katsura.parc.xerox.com-i386-linux-i386-linux-intel-Unix-3.09.2", 33.5;
      "moon.parc.xerox.com-i386-linux-i386-linux-intel-Unix-3.09.2", 39.95;

      "penguin25-x86_64-linux-x86_64-linux-unknown-Unix-3.09.2", 12.4;
      "penguin26.parc.xerox.com-x86_64-linux-x86_64-linux-unknown-Unix-3.09.2", 13.1;
      "penguin27-x86_64-linux-x86_64-linux-unknown-Unix-64-3.09.2", 12.97;
      "penguin27-x86_64-linux-x86_64-linux-unknown-Unix-3.09.2", 12.97;
      "penguin27.parc.xerox.com-x86_64-linux-x86_64-linux-unknown-Unix-3.09.2", 13.1;
      "penguin28-x86_64-linux-x86_64-linux-unknown-Unix-3.09.2", 13.1;
      "penguin29.parc.xerox.com-x86_64-linux-x86_64-linux-unknown-Unix-3.09.2", 13.1;
      "penguin30.parc.xerox.com-x86_64-linux-x86_64-linux-unknown-Unix-3.09.2", 13.1;

      "soong-x86_64--GNU/Linux--Unix-64-3.10.2", 14.73;
      "byodoin-x86_64--GNU/Linux--Unix-64-3.10.2", 14.57;
      "cypress", 19.20;
      "cypress.cs.unh.edu-x86_64-linux-x86_64-linux-unknown-Unix-64-3.10.0", 19.20;

      "katsura", 18.71;
      "katsura.cs.unh.edu-x86_64-linux-x86_64-linux-unknown-Unix-64-3.10.0", 18.71;

      "ai1-x86_64--GNU/Linux--Unix-64-3.10.2", 14.90;
      "ai2-x86_64--GNU/Linux--Unix-64-3.10.2", 14.82;
      "ai3-x86_64--GNU/Linux--Unix-64-3.10.2", 14.84;

(*
      "ai1-x86_64--GNU/Linux--Unix-64-3.11.0", 14.90;
      "ai2-x86_64--GNU/Linux--Unix-64-3.11.0", 14.82;
      "ai3-x86_64--GNU/Linux--Unix-64-3.11.0", 14.84;
*)

      "c0.cs.unh.edu-x86_64-linux-x86_64-linux-unknown-Unix-64-3.10.0", 28.75;
      "c1.cs.unh.edu-x86_64-linux-x86_64-linux-unknown-Unix-64-3.10.0", 28.66;
      "c2.cs.unh.edu-x86_64-linux-x86_64-linux-unknown-Unix-64-3.10.0", 27.95;
      "c3.cs.unh.edu-x86_64-linux-x86_64-linux-unknown-Unix-64-3.10.0", 28.56;
      "c4.cs.unh.edu-x86_64-linux-x86_64-linux-unknown-Unix-64-3.10.0", 28.58;
      "c5.cs.unh.edu-x86_64-linux-x86_64-linux-unknown-Unix-64-3.10.0", 28.61;

      "c0.cs.unh.edu-x86_64--GNU/Linux--Unix-64-3.10.2",28.75;
      "c1.cs.unh.edu-x86_64--GNU/Linux--Unix-64-3.10.2",28.66;
      "c2.cs.unh.edu-x86_64--GNU/Linux--Unix-64-3.10.2",27.95;
      "c3.cs.unh.edu-x86_64--GNU/Linux--Unix-64-3.10.2",28.56;
      "c4.cs.unh.edu-x86_64--GNU/Linux--Unix-64-3.10.2",28.58;
      "c5.cs.unh.edu-x86_64--GNU/Linux--Unix-64-3.10.2",28.61;

      "c0.cs.unh.edu-----Unix-64-3.10.2",28.75;
      "c1.cs.unh.edu-----Unix-64-3.10.2",28.66;
      "c2.cs.unh.edu-----Unix-64-3.10.2",27.95;
      "c3.cs.unh.edu-----Unix-64-3.10.2",28.56;
      "c4.cs.unh.edu-----Unix-64-3.10.2",28.58;
      "c5.cs.unh.edu-----Unix-64-3.10.2",28.61;


      "c5.cs.unh.edu---linux--Unix-64-3.10.2",28.61;

(*
      "-----Unix-64-3.10.0", 14.6;
      "-----Unix-64-3.10.2", 14.6;
*)
    ]
  in
    (fun machine_id ->
       try
	 List.assoc machine_id benchmark_table
       with Not_found ->
	 failwith ("Can't find benchmark time for machine: " ^ machine_id))


let katsura_id =
  "katsura.parc.xerox.com-i386-linux-i386-linux-intel-Unix-3.08.1"


let normalize_times machine_id times =
  let actual = get_benchmark machine_id
  and base = get_benchmark katsura_id in
  let factor = base /. actual in
    Vector.multiply_by times factor


(************************** histgorams **************************)


type histogram_style = Counts | Frequency | Density


let histogram ?(min_bins = 5) ?(max_bins = 100) ?(style = Frequency) data =
  let num_bins = max min_bins
		   (min max_bins ((Array.length data) / 10)) in
  let minv, maxv = Wrarray.min_and_max_by Fn.identity data in
  let minv, maxv = (if minv = maxv then
		      (minv *. 0.9), (maxv *. 1.1)
		    else
		      minv, maxv) in
  let width = (maxv -. minv) /. (float num_bins)
  and bins = Array.make num_bins 0 in
    Array.iter (fun x ->
		  let i = min (num_bins - 1)
			    (truncate (floor ((x -. minv) /. width))) in
		    bins.(i) <- bins.(i) + 1)
      data;
    let bins = (match style with
		  Counts -> Array.map float bins
		| Frequency ->
		    let l = Array.length data in
		      Array.map (fun x -> Math.div x l) bins
		| Density ->
		    (* sum of area = 1.  this isn't quite right
		       because of box overhang *)
		    let f = (float (Array.length data)) *. width in
		      Array.map (fun x -> (float x) /. f) bins) in
      bins, minv, width


(* EOF *)
