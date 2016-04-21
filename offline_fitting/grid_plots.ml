(* code for plotting some interesting things using the offline model *)

let scatter_plot (lhs,rhs) ls rs  title loc =
  let ar = Array.init (Array.length lhs) (fun i -> lhs.(i),rhs.(i)) in
  let data = [("",":x", ar)] in
    Ps_plot.scatter loc data title ls rs


let scatter_plot_nar (lhs,rhs) ls rs  title loc =
  let ar = Array.init (Array.length lhs) (fun i -> lhs.(i).(0),rhs.(i).(0)) in
  let data = [("",":x", ar)] in
    Ps_plot.scatter loc data title ls rs


let big_filter ele list =
    List.fold_left (fun accum a ->
		      if a = ele
		      then accum
		      else a::accum) [] list

(* painfully slow *)
let rec do_filter l =
  match l with
      [] -> []
    | hd::tl -> hd::(do_filter (big_filter hd tl))


let float_sort a b =
  let d = a -. b in
    if d < 0.
    then (-1)
    else (if d > 0.
	  then 1
	  else 0)

let pair_sort (a,b) (c,d) =
  float_sort a c

(* given a data file, gives you two arrays,
   f_vals contains the f value of every node
   fh_vals contains the fh value of every node,
   calculated using lms fit to the left hand side, right hand side,
   and model supplied.  AC is the argument count for the model, so if the
   model has 1 weight, thats 1, 2 for 2, et etc.  fn is the location of
   the data file.  Title is the plot title.  loc is the output location
   tn is the location of the truth file
*)

let rec mk_str wt_list =
  match wt_list with
      [] -> ""
    | hd::tl -> (Wrutils.str "%f_" hd) ^ mk_str tl

(* fhat_vs_f Models.c_m1_lhs Models.c_m1_rhs Models.m1_fhat 1 "/var/tmp/foo" *)
let fhat_vs_f ?(p = 0.0) ?(fhc = (fun fh n -> fh n))
    ?(ftc = Recorded_run.get_ht) ?(fc = Recorded_run.get_h) run lhs rhs m ac
    m_name p_name =
  let pairs = Recorded_run.get_best_pairs run in
  let lhs,rhs = (Recorded_run.build_conv_matrix lhs pairs,
		 Recorded_run.build_conv_matrix rhs pairs) in
  let left,right = pairs_to_mats (lhs,rhs) in
  let sol = get_least_squares (left,right) in
  let wts = ref [] in
    for i = 1 to ac
    do
      wts := sol.{i,1}::!wts
    done;
    wts := List.rev !wts;
    let out = "/var/tmp/fh_plots/" ^ m_name ^ "_" ^ p_name ^ "_" ^ (mk_str !wts) in
    Verb.pe Verb.toplvl "Regression complete\n%!";
    let fh = (m !wts) in
    let fh = fhc fh in
    let rs = Array.map
      (Hashtbl.find run.Recorded_run.run) run.Recorded_run.sequence in
    let miny1,miny2,minx = (ref infinity), (ref infinity),(ref infinity)
    and maxy1,maxy2,maxx = (ref (-. infinity)),(ref (-. infinity)),(ref (-. infinity)) in
      Verb.pe Verb.toplvl "Making pairs\n%!";
      let fh_v_f = Array.map
	(fun n ->
	   let ft = fh n
	   and f = fc n in
	     if !miny1 < f then miny1 := f;
	     if f > !maxy1 then maxy1 := f;
	     if !minx < ft then minx := ft;
	     if ft > !maxx then maxx := ft;
	     f,ft) rs
      and fh_v_ft = Array.map
	(fun n ->
	   let ft = fh n
	   and f = ftc n in
	     if !miny2 < f then miny2 := f;
	     if f > !maxy2 then maxy2 := f;
	     if !minx < ft then minx := ft;
	     if ft > !maxx then maxx := ft;
	     f,ft) rs in

      let some_not_inf num =
	if num = infinity || num = (-.infinity)
	then None else Some num in

      let min1 = min !minx !miny1
      and max1 = max !maxx !maxy1
      and min2 = min !minx !miny2
      and max2 = max !maxx !maxy2 in
	Ps_plot.scatter (Wrutils.str "%s_%s_vs_%s.eps"
			   out "hhat" "truth" )
	  ~min_x:(some_not_inf min1)
	  ~max_x:(some_not_inf max1)
	  ~min_y:(some_not_inf min1)
	  ~max_y:(some_not_inf max1)
	  [("",":DOT",fh_v_ft)] m_name "h*" "h^";

	Ps_plot.scatter (Wrutils.str "%s_%s_vs_%s.eps"
			   out "hhat" "h" )
	  ~min_x:(some_not_inf min2)
	  ~max_x:(some_not_inf max2)
	  ~min_y:(some_not_inf min2)
	  ~max_y:(some_not_inf max2)
	  [("",":DOT",fh_v_f)] m_name "h" "h^"


let online_regressions ?(fhc = (fun fh n -> fh n))
    ?(ftc = Recorded_run.get_ht) ?(fc = Recorded_run.get_h) run lhs rhs m ac
    m_name p_name =
  let pairs = Recorded_run.get_best_pairs run in
  let lhs,rhs = (Recorded_run.build_conv_matrix lhs pairs,
		 Recorded_run.build_conv_matrix rhs pairs) in
  let h_errs, wts, corr_calc = rand_stream_lms (lhs,rhs) in
  let out = "/var/tmp/fh_plots/stream/" ^ m_name ^ "_" ^ p_name ^ "_" ^
    (mk_str (Array.to_list wts)) in
    Verb.pe Verb.toplvl "Regression complete\n%!";
    let fh = (m (Array.to_list wts)) in
    let fh = fhc fh in
    let rs = Array.map
      (Hashtbl.find run.Recorded_run.run) run.Recorded_run.sequence in
    let miny1,miny2,minx = (ref infinity), (ref infinity),(ref infinity)
    and maxy1,maxy2,maxx = (ref (-. infinity)),(ref (-. infinity)),(ref (-. infinity)) in
      (* final models, after all learning *)
    let fh_v_f = Array.map
      (fun n ->
	 let ft = fh n
	 and f = fc n in
	   if !miny1 < f then miny1 := f;
	   if f > !maxy1 then maxy1 := f;
	   if !minx < ft then minx := ft;
	   if ft > !maxx then maxx := ft;
	   f,ft) rs

    and fh_v_ft = Array.map
      (fun n ->
	 let ft = fh n
	 and f = ftc n in
	   if !miny2 < f then miny2 := f;
	   if f > !maxy2 then maxy2 := f;
	   if !minx < ft then minx := ft;
	   if ft > !maxx then maxx := ft;
	   f,ft) rs

    (* Models as streamed *)
    and online_fh =
      Array.init (Array.length lhs)
	(fun i -> (Recorded_run.get_h (fst (List.nth pairs i))) +.
	   h_errs.(i))
    and online_f =
      Array.init (Array.length lhs)
	(fun i -> (Recorded_run.get_h (fst (List.nth pairs i))))
    and online_ft =
      Array.init (Array.length lhs)
	(fun i -> (Recorded_run.get_ht (fst (List.nth pairs i)))) in

      (* actually doing the plots *)

      Ps_plot.scatter (Wrutils.str "%s_%s_vs_%s.eps"
			 out "hhat" "truth" )
	[("",":DOT",fh_v_ft)] m_name "h*" "h^";
      Ps_plot.scatter (Wrutils.str "%s_%s_vs_%s.eps"
			 out "hhat" "h" )
	[("",":DOT",fh_v_f)] m_name "h" "h^"
      (*;
      Ps_plot.scatter (Wrutils.str "%s_%s_vs_%s_online.eps"
			 out "hhat" "h" )
	[("",":DOT",
	  (Array.init (Array.length online_f)
	     (fun i -> online_fh.(i), online_f.(i))))] m_name "h" "h^";
	 Ps_plot.scatter (Wrutils.str "%s_%s_vs_%s_online.eps"
			    out "hhat" "nlunt" )
	   [("",":DOT",(Array.init (Array.length online_f)
	     (fun i -> online_fh.(i), online_ft.(i))))] m_name "h" "h^"*)


  (* Builds a truth vs f plot.  Useful for baselining various
     learning approaches *)
  let ft_vs_f ?(p = 0.0) ?(ftc = Recorded_run.get_ht)
      ?(fc = Recorded_run.get_h) run m_name =
    let out = "/var/tmp/fh_plots/" ^ m_name in
    let rs = Array.map (Hashtbl.find run.Recorded_run.run)
      run.Recorded_run.sequence in
    let miny,minx = (ref infinity), (ref infinity)
    and maxy,maxx = (ref (-. infinity)),(ref (-. infinity)) in
    let f_v_ft = Array.map
      (fun n ->
	 let ft = ftc n
	 and f = fc n in
	   if !miny < f then miny := f;
	   if f > !maxy then maxy := f;
	   if !minx < ft then minx := ft;
	   if ft > !maxx then maxx := ft;
	   ft,f) rs
    in
    let min = min !minx !miny
    and max = max !maxx !maxy in
      Ps_plot.scatter (Wrutils.str "%s_%s_vs_%s.eps"
			 out "h" "truth" )
	~max_x:(Some 3000.)
	~max_y:(Some 3000.)
	[("",":DOT",f_v_ft)] m_name "h*" "h"




let make_histogram ?(run = None) w run out =
  let calc_fp = Recorded_run.calc_fp w in
  let rs = Array.fold_left (fun accum next ->
			     (Hashtbl.find run.Recorded_run.run next)::accum)
    [] run.Recorded_run.sequence in
  let f_vals = (List.fold_left
		  (fun accum next -> (Recorded_run.calc_f next)::accum) [] rs)
  and fp_vals = (List.fold_left
		   (fun accum next -> (calc_fp next)::accum) [] rs)
  in
  let far = Array.create (List.length f_vals) (-.1.)
  and par = Array.create (List.length f_vals) (-.1.) in
    List.fold_left (fun accum a -> far.(accum) <- a; accum + 1) 0 f_vals;
    List.fold_left (fun accum a -> par.(accum) <- a; accum + 1) 0 fp_vals;
    Array.sort float_sort far;
    Array.sort float_sort par;
    Ps_plot.histogram ~style:Data.Frequency
      out [(far,"f"); (par, "f'")] "value" "frequency"


let fvt_plot run =
    ft_vs_f run "Plaino"


let m1_plot ?(r = None) tn fn structs =
  let run = match r with None -> Run_reader.grid_run tn fn structs
    | Some rn -> rn in
    Verb.pe Verb.toplvl "run loaded\n%!";
    fhat_vs_f
      run
      Models.c_m1_lhs Models.c_m1_rhs
      Fcalc.d_hhat 1
      "Model_1"


let m2_plot ?(r = None) tn fn structs =
  let run = match r with None -> Run_reader.grid_run tn fn structs
    | Some rn -> rn in
    fhat_vs_f run Models.c_m2_lhs Models.c_m2_rhs Fcalc.h_hhat 1 "Model_2"

let m3_plot ?(r = None)  tn fn structs =
  let run = match r with None -> Run_reader.grid_run tn fn structs
    | Some rn -> rn in
    fhat_vs_f run Models.c_m3_lhs Models.c_m3_rhs Fcalc.hd_hhat 2 "Model_3"


let m4_plot ?(r = None) tn fn structs =
  let run = match r with None -> Run_reader.grid_run tn fn structs
    | Some rn -> rn in
    fhat_vs_f run Models.c_m4_lhs Models.c_m4_rhs Fcalc.hdg_hhat 3 "Model_4"


let m5_plot ?(r = None) tn fn structs =
  let run = match r with None -> Run_reader.grid_run tn fn structs
    | Some rn -> rn in
    fhat_vs_f run Models.c_m5_lhs Models.c_m5_rhs Fcalc.hdgD_hhat 4 "Model_5"


(*
let m1_root_plot ?(r = None) fn tn =
  let run = Run_reader.grid_run tn fn [] in
  let lhs,rhs = Root_models.make_lhs_rhs_m1 run in
    fhat_vs_f run lhs rhs Fcalc.h_hhat 1 "Root_M1"


let m3_root_plot ?(r = None) fn tn =
  let run = Run_reader.grid_run tn fn [] in
  let lhs,rhs = Root_models.make_lhs_rhs_m3 run in
  fhat_vs_f run lhs rhs Fcalc.hd_hhat 2 "Root_M3"
*)


let m1truth_plot ?(r = None) tn fn =
  let run =
    match r with None -> Run_reader.grid_run tn fn []
      | Some a -> a
  in
    fhat_vs_f run Models.truth_m1_lhs Models.truth_m1_rhs Fcalc.h_hhat 1 "M1"


let m2truth_plot ?(r = None) tn fn =
  let run =
    match r with None -> Run_reader.grid_run tn fn []
      | Some a -> a
  in
    fhat_vs_f run Models.truth_m2_lhs Models.truth_m2_rhs Fcalc.hd_hhat 2 "M2"


let m3truth_plot ?(r = None) tn fn =
  let run =
    match r with None -> Run_reader.grid_run tn fn []
      | Some a -> a
  in
    fhat_vs_f run Models.truth_m3_lhs Models.truth_m3_rhs Fcalc.hdg_hhat 3 "M3"


let m4truth_plot ?(r = None) tn fn =
  let run =
    match r with None -> Run_reader.grid_run tn fn []
      | Some a -> a
  in
    fhat_vs_f run Models.truth_m4_lhs Models.truth_m4_rhs Fcalc.hdgD_hhat 4 "M4"


let m5truth_plot ?(r = None) tn fn =
  let run =
    match r with None -> Run_reader.grid_run tn fn []
      | Some a -> a
  in
    fhat_vs_f run Models.truth_m1_lhs Models.truth_m1_rhs Fcalc.h_hhat 4 "M5"


let single_batch true_file run_file structs g_name =
  let run = Some (Run_reader.grid_run true_file run_file structs) in
    (match run with Some r -> fvt_plot r | _ -> failwith "buh?");
    m1_plot ~r:run true_file run_file structs g_name;
    m2_plot ~r:run true_file run_file structs g_name;
    m3_plot ~r:run true_file run_file structs g_name;
    m4_plot ~r:run true_file run_file structs g_name;
    m5_plot ~r:run true_file run_file structs g_name



let truth_batch ?(r = None) true_file run_file structs g_name =
  let run =
    match r with
	None -> Run_reader.grid_run true_file run_file structs
      | Some a -> a in
    fvt_plot run;
    m1truth_plot ~r:(Some run) true_file run_file g_name;
    m2truth_plot ~r:(Some run) true_file run_file g_name;
    m3truth_plot ~r:(Some run) true_file run_file g_name;
    m4truth_plot ~r:(Some run) true_file run_file g_name;
    run


let do_batch h w mv cost num =
truth_batch
  ("/var/tmp/recorded_runs/grid/uniform/" ^
     mv ^ "/" ^ cost ^ "/0.35_" ^ w ^ "_" ^ h ^ "_" ^ num ^ ".truth")
  ("/var/tmp/recorded_runs/grid/uniform/" ^ mv ^ "/" ^ cost ^ "/0.35_" ^
    w ^ "_" ^ h ^ "_" ^ num ^ "_astar.exp")
  []
  (cost ^ mv ^ w ^ "x" ^ h ^ "_" ^ num)

(* Streamed models *)

let om1truth_plot ?(r = None) tn fn =
  let run =
    match r with None -> Run_reader.grid_run tn fn []
      | Some a -> a
  in
    online_regressions run
      Models.truth_m1_lhs Models.truth_m1_rhs Fcalc.h_hhat 1 "M1"

let om2truth_plot ?(r = None) tn fn =
  let run =
    match r with None -> Run_reader.grid_run tn fn []
      | Some a -> a
  in
    online_regressions run Models.truth_m2_lhs Models.truth_m2_rhs Fcalc.hd_hhat 2 "M2"


let om3truth_plot ?(r = None) tn fn =
  let run =
    match r with None -> Run_reader.grid_run tn fn []
      | Some a -> a
  in
    online_regressions run Models.truth_m3_lhs Models.truth_m3_rhs Fcalc.hdg_hhat 3 "M3"


let om4truth_plot ?(r = None) tn fn =
  let run =
    match r with None -> Run_reader.grid_run tn fn []
      | Some a -> a
  in
    online_regressions run Models.truth_m4_lhs Models.truth_m4_rhs Fcalc.hdgD_hhat 4 "M4"


let om5truth_plot ?(r = None) tn fn =
  let run =
    match r with None -> Run_reader.grid_run tn fn []
      | Some a -> a
  in
    online_regressions run Models.truth_m1_lhs Models.truth_m1_rhs Fcalc.h_hhat 4 "M5"


let otruth_batch ?(r = None) true_file run_file structs g_name =
  let run =
    match r with
	None -> Run_reader.grid_run true_file run_file structs
      | Some a -> a in
    fvt_plot run;
    om1truth_plot ~r:(Some run) true_file run_file g_name;
    om2truth_plot ~r:(Some run) true_file run_file g_name;
    om3truth_plot ~r:(Some run) true_file run_file g_name;
    om4truth_plot ~r:(Some run) true_file run_file g_name;
    run


let do_obatch sz mv cost =
otruth_batch
  ("/var/tmp/recorded_runs/grid/uniform/" ^
     mv ^ "/" ^ cost ^ "/0.45_" ^ sz ^ "_" ^ sz ^ "_1.truth")
  ("/var/tmp/recorded_runs/grid/uniform/" ^ mv ^ "/" ^ cost ^ "/0.45_" ^
    sz ^ "_" ^ sz ^ "_1_astar.exp")
  []
  (cost ^ mv ^ sz ^ "x" ^ sz ^ "_1")
(* EOF *)
