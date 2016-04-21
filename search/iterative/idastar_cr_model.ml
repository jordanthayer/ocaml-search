(** IDA* with controlled re-expansions model. *)

open Printf

let get_bin nbins bound f =
  (** [get_bin nbins bound f] get the appropriate bin as described in
      the IDA*_CR paper. *)
  (* This should find i s.t:

     x(1 + i/100) < f(n) <= x(1 + (i + 1)/100), where x is the
     bound. *)
  let rec do_get_bin i =
    if i > nbins then
      i
    else
      let p = (float i) in
      let v = bound *. (1. +. p /. 100.) in
      if v >= f then
	if i = 1 || v = f then i else i - 1
      else
	do_get_bin (i + 1)
  in do_get_bin 1


let get_p_for_wt last_bin bins wt =
  (** [get_p_for_wt bins wt] get the p-value that will give the
      desired weight.  If there is not enough weight then return
      [list_bin].*)
  let n = Array.length bins in
  let rec do_get_p accum p =
    if p > n then begin
      last_bin
    end else begin
      assert (p > 0);
      let sum = accum + bins.(p - 1) in
      if (float sum) >= wt then p else do_get_p sum (p + 1)
    end
  in do_get_p 0 1

let spt_hist_fs n title ?desired bound tbl =
  let wt = Hashtbl.fold (fun _ v s -> v + s) tbl 0 in
  let min = Hashtbl.fold (fun v _ m -> Math.fmin v m) tbl infinity in
  let max = Hashtbl.fold (fun v _ m -> Math.fmax v m) tbl neg_infinity in
  Wrio.with_outfile (sprintf "data/%s%02d.spt" title n)
    (fun o ->
      fprintf o "(let* (";
      fprintf o "(pts (";
      Hashtbl.iter (fprintf o "(%g %d) ") tbl;
      fprintf o "))\n";
      fprintf o "(hist (histogram-dataset :points pts))\n";
      fprintf o "(plot (num-by-num-plot ";
      fprintf o ":title \"%s %d bound %g wt %d" title n bound wt;
      begin match desired with
	| None -> fprintf o "\"\n";
	| Some d -> fprintf o "(%g)\"\n" d;
      end;
      fprintf o ":x-label \"min %g max %g\"\n" min max;
      fprintf o ":dataset hist))\n";
      fprintf o ")\n(output \"%s%02d.ps\" plot))\n" title n;)

let oob_f, print_oob_fs =
  let tbl = Hashtbl.create 257 in
  (fun f ->
    try
      Hashtbl.replace tbl f ((Hashtbl.find tbl f) + 1)
    with Not_found ->
      Hashtbl.add tbl f 1),
  (fun n desired bound ->
    spt_hist_fs n "oob" ~desired bound tbl;
    Hashtbl.clear tbl;)

let expd_f, print_expd_fs =
  let tbl = Hashtbl.create 257 in
  (* root node *)
  Hashtbl.add tbl 0. 1;
  (fun f ->
    try
      Hashtbl.replace tbl f ((Hashtbl.find tbl f) + 1)
    with Not_found ->
      Hashtbl.add tbl f 1),
  (fun n bound ->
    spt_hist_fs n "expd" bound tbl;
    Hashtbl.clear tbl;
    (* root node *)
    Hashtbl.add tbl 0. 1)

let make nbins b get_f =
  (** [make nbins b get_f] gets an IDA*_CR bound model for IDA*.  [b]
      is the "control factor" which says how many nodes to try for at
      each iteration. *)

  let expansions = ref 0 in
  let iter_no = ref 0 in
  let this_bound = ref 0. in
  let last_bin = ref 1 in
  let min_out_of_bound = ref infinity in

  let bins = Array.make nbins 0 in
  (* bins are logically indexed from a base of 1. *)

  let reset_bound vl =
    Iterative_deepening.output_col_hdr ();
    iter_no := 0;
    expansions := 0;
    this_bound := vl;
    last_bin := 1;
    min_out_of_bound := infinity;
    for i = 0 to nbins - 1 do bins.(i) <- 0 done in

  let see_expansion depth n children =
    let handle_child ch =
      let f = get_f ch in
      if f > !this_bound && Math.finite_p f then begin
(*
	oob_f f;			(* INSTRUMENTATION *)
*)
	let i = get_bin nbins !this_bound f in
	if (i - 1) < nbins then begin
	  assert (i > 0);
	  bins.(i - 1) <- bins.(i - 1) + 1;
	  last_bin := max !last_bin i;
	end
      end
(*
  else
	expd_f f			(* INSTRUMENTATION *)
*)
    in
    incr expansions;
    List.iter handle_child children in

  let iteration_complete _ last =
    Iterative_deepening.output_row !iter_no !this_bound !expansions;
    if not last then begin
      incr iter_no;
      let desired = max (b *. (float !expansions)) (b ** (float !iter_no)) in
(*
      let desired = b *. (float !expansions) in
*)
(*
      print_oob_fs !iter_no desired !this_bound; (* INSTRUMENTATION *)
      print_expd_fs !iter_no !this_bound; (* INSTRUMENTATION *)
*)
      let bound =
	let j = float (get_p_for_wt !last_bin bins desired) in
	assert (Math.is_positive j);
	!this_bound *. (1. +. j /. 100.) in
      if Verb.level Verb.debug then begin
	Array.iteri (fun i vl ->
	  let p = !this_bound *. (1. +. (float i) /. 100.) in
	  Printf.printf "%d (%f): %d\n" i p vl)
	  bins;
      end;
      Verb.pr Verb.toplvl "bound estimated=%f\n%!" bound;
      expansions := 0;
      this_bound := Math.fmax !min_out_of_bound bound;
      last_bin := 0;
      min_out_of_bound := infinity;
      Verb.pr Verb.optional "next bound=%f\n%!" !this_bound;
      for i = 0 to nbins - 1 do bins.(i) <- 0 done
    end in

  let check_bound n =
    let in_bound = (get_f n) <= !this_bound in
    if not in_bound then
      min_out_of_bound := Math.fmin (get_f n) !min_out_of_bound;
    in_bound in

  reset_bound, see_expansion, iteration_complete, check_bound

