(* code for plotting some interesting things using the offline model *)

let scatter_plot (lhs,rhs) ls rs title loc =
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


let fhat_vs_f ?(run = None) ?(p = 0.0) ?(fhc = (fun fh n -> fh n))
    ?(ftc = Recorded_run.get_ht) ?(fc = Recorded_run.get_h) lhs rhs m ac fn
    tn m_name p_name =
  let out = "/var/tmp/tiles/" ^ m_name ^ "_" ^ p_name in
  let run = match run with
      None -> Run_reader.open_et_data_file fn
    | Some r -> r in
  let t = Run_reader.open_et_data_file tn  in
    Recorded_run.zip run t;
    let pairs = Recorded_run.get_best_pairs run in
    let lhs,rhs = (Recorded_run.build_conv_matrix lhs pairs,
		   Recorded_run.build_conv_matrix rhs pairs) in
    let vm,tm = pairs_to_mats (lhs,rhs) in
    let sol = get_least_squares (vm,tm) in
    let wts = ref [] in
      for i = 1 to ac
      do
	wts := sol.{i,1}::!wts
      done;
      let rs = List.fold_left (fun accum next ->
				 (Hashtbl.find run.Recorded_run.run next)::accum)
	[] run.Recorded_run.sequence
      and fh = (m !wts) in
      let fh = fhc fh in
      let miny1,miny2,minx = (ref infinity), (ref infinity),(ref infinity)
      and maxy1,maxy2,maxx = (ref (-. infinity)),(ref (-. infinity)),(ref (-. infinity)) in
      let f_vals = (List.fold_left
		      (fun accum next ->
			 let next = fc next in
			 ((if next < !miny1
			   then miny1 := next);
			  (if next > !maxy1
			   then maxy1 := next));
			   next::accum) [] rs)
      and ft_vals = (List.fold_left
		       (fun accum next ->
			let next = ftc next in
			  (if next < !minx
			   then minx := next);
			  (if next > !maxx
			   then maxx := next);
			   next::accum) [] rs)
      and fh_vals = (List.fold_left
		       (fun accum next ->
			  let next = fh next in
			 ((if next < !miny2
			   then miny2 := next);
			  (if next > !maxy2
			   then maxy2 := next));
			  next::accum) [] rs) in
	Verb.pe Verb.toplvl "Making pairs\n%!";
	let fh_v_f = (Array.of_list
			(List.sort pair_sort
			   (List.fold_left2 (fun accum a b ->
					       if (Random.float 1.) > p
					       then (b,a)::accum
					       else accum) [] fh_vals f_vals)))
	and fh_v_ft = (Array.of_list
			 (List.sort pair_sort
			    (List.fold_left2 (fun accum a b ->
						if (Random.float 1.) > p
						then (b,a)::accum
						else accum) [] fh_vals ft_vals))) in
	let min1 = min !minx !miny1
	and max1 = max !maxx !maxy1
	and min2 = min !minx !miny2
	and max2 = max !maxx !maxy2 in
	  Ps_plot.scatter (Wrutils.str "%s_%s_vs_%s.eps"
			     out "hhat" "truth" )
	    ~jitter:true
	    ~min_x:(Some min1)
	    ~max_x:(Some max1)
	    ~min_y:(Some min1)
	    ~max_y:(Some max1)
	    [("",":DOT",fh_v_ft)] m_name "h*" "h^";

	  Ps_plot.scatter (Wrutils.str "%s_%s_vs_%s.eps"
			     out "hhat" "h" )
	    ~jitter:true
	    ~min_x:(Some min1)
	    ~max_x:(Some max1)
	    ~min_y:(Some min1)
	    ~max_y:(Some max1)
	    [("",":DOT",fh_v_f)] m_name "h" "h^"


(* Builds a truth vs f plot.  Useful for baselining various
   learning approaches *)
let ft_vs_f ?(run = None) ?(p = 0.0) ?(ftc = Recorded_run.get_ht)
    ?(fc = Recorded_run.get_h) fn tn m_name =
  let out = "/var/tmp/tiles/" ^ m_name in
  let run = match run with
      None -> Run_reader.open_et_data_file fn
    | Some r -> r in
  let t = Run_reader.open_et_data_file tn  in
    Recorded_run.zip run t;
    let rs = List.fold_left
      (fun accum next ->
	 (Hashtbl.find run.Recorded_run.run next)::accum)
      [] run.Recorded_run.sequence in
    let miny,minx = (ref infinity), (ref infinity)
    and maxy,maxx = (ref (-. infinity)),(ref (-. infinity)) in
    let f_vals = (List.fold_left
		    (fun accum next ->
		       let next = fc next in
			 ((if next < !miny
			   then miny := next);
			  (if next > !maxy
			   then maxy := next));
			 next::accum) [] rs)
    and ft_vals = (List.fold_left
		     (fun accum next ->
			let next = ftc next in
			  (if next < !minx
			   then minx := next);
			  (if next > !maxx
			   then maxx := next);
			  next::accum) [] rs) in
    let f_v_ft = (Array.of_list
		    (List.sort pair_sort
		       (List.fold_left2 (fun accum a b ->
					   if (Random.float 1.) > p
					   then (b,a)::accum
					   else accum) [] f_vals ft_vals)))
    in
    let min = min !minx !miny
    and max = max !maxx !maxy in
      Ps_plot.scatter (Wrutils.str "%s_%s_vs_%s.eps"
			 out "h" "truth" )
	~min_x:(Some min)
	~max_x:(Some max)
	~min_y:(Some min)
	~max_y:(Some max)
	[("",":DOT",f_v_ft)] m_name "h*" "h"


let ft_vs_gf ?(run = None) ?(p = 0.0) ed eh fn tn m_name =
  let calc_gf = Recorded_run.calc_gf ed eh in
  let out = "/var/tmp/tiles/" ^ m_name in
  let run = match run with
      None -> Run_reader.open_et_data_file fn
    | Some r -> r in
  let t = Run_reader.open_et_data_file tn  in
    Recorded_run.zip run t;
    let rs = List.fold_left
      (fun accum next ->
	 (Hashtbl.find run.Recorded_run.run next)::accum)
      [] run.Recorded_run.sequence in
    let f_vals = (List.fold_left
		    (fun accum next -> (calc_gf next)::accum) [] rs)
    and ft_vals = (List.fold_left
		     (fun accum next -> (Recorded_run.calc_ft next)::accum) [] rs) in
    let f_v_ft = (Array.of_list
		    (List.sort pair_sort
		       (List.fold_left2 (fun accum a b ->
					   if (Random.float 1.) > p
					   then (a,b)::accum
					   else accum) [] f_vals ft_vals)))
    in
      Ps_plot.scatter (Wrutils.str "%s_%s_vs_%s.eps"
			 out "f" "ft" )
	[("",":DOT",f_v_ft)] m_name "f" "f*"


let make_histogram ?(run = None) w fn out =
  let calc_fp = Recorded_run.calc_fp w in
  let run = match run with
      None -> Run_reader.open_et_data_file fn
    | Some r -> r in
  let rs = List.fold_left (fun accum next ->
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



let fvt_plot fn tn =
  ft_vs_f fn tn "Plaino"

let m1_plot fn tn =
  fhat_vs_f Models.c_m1_lhs Models.c_m1_rhs Fcalc.d_hhat 1 fn tn "Model_1"


let m2_plot fn tn =
  fhat_vs_f Models.c_m2_lhs Models.c_m2_rhs Fcalc.h_hhat 1 fn tn "Model_2"


let m3_plot fn tn =
  fhat_vs_f Models.c_m3_lhs Models.c_m3_rhs Fcalc.hd_hhat 2 fn tn "Model_3"


let m4_plot fn tn =
  fhat_vs_f Models.c_m4_lhs Models.c_m4_rhs Fcalc.hdg_hhat 3 fn tn "Model_4"


let m5_plot fn tn =
  fhat_vs_f Models.c_m5_lhs Models.c_m5_rhs Fcalc.hdgD_hhat 4 fn tn "Model_5"


let m1truth_plot fn tn =
  fhat_vs_f Models.truth_m1_lhs Models.truth_m1_rhs Fcalc.h_hhat 1 fn tn "M1"


(****************************** Scatters *********************************)

let splot_arb fl fr nl nr fn outname =
  let run = Run_reader.open_et_data_file fn  in
  let p = Recorded_run.get_best_pairs run in
  let lhs = Array.map (fun x-> x.(0))
    (Recorded_run.build_conv_matrix fl p)
  and rhs = Array.map (fun x-> x.(0))
    (Recorded_run.build_conv_matrix fr p) in
    scatter_plot (lhs,rhs) nl nr "" outname

(* Various probably interesting scatter plots *)

let rh_v_g fn outname =
  splot_arb Models.rh Models.g "rev h" "g" fn outname

let rd_v_dep fn outname =
  splot_arb Models.rd Models.dep "rev d" "d" fn outname

let rh_v_h fn outname =
  splot_arb Models.rh Models.h "rev h" "h" fn outname

let rd_v_d fn outname =
  splot_arb Models.rd Models.d "rev d" "d" fn outname

let g_v_h fn outname =
  splot_arb Models.g Models.h "g" "h" fn outname

let dep_v_g fn outname =
  splot_arb Models.dep Models.g "dep" "g" fn outname


let do_batch run_file true_file g_name =
m1_plot run_file true_file g_name;
m2_plot run_file true_file g_name;
m3_plot run_file true_file g_name;
m4_plot run_file true_file g_name;
m5_plot run_file true_file g_name


(* EOF *)
