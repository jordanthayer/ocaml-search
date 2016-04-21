(**
   @author jtd7
   @since 2010-10-14

   Plotting tool based on spt plot instead of the old ps plot for the offline
   analysis tools

   TRUTH WILL ALWAYS BE THE Y AXIS, THIS IS THE FIRST LAW OF THAYER
*)

let use_color = ref true

let get_ann_string str =
  match str with
    | "vacuum" ->
"\132\149\166\190\000\000\000\158\000\000\000\014\000\000\000A\000\000\0000\160\160\014\003\244\184\179\248\155\017\240?\131\019\005\243\224\\\235\191\130\193\236\027\246\029\240?(identity\160\160\014\004kbt\160)\246\011@\223\202w|z\197\006@)lN=Rg\255?\193\162\192\153qp\235\191'sigmoid\160\160\014\0048\020D\165\025\250\226\191N\031\1450\163t\231\191L\187.\215\212<\004@\1746\197B?\159\226\191\004\004\160\160\014\004A\138NmR^\006@\001\204\022\183\255\191\007@'\217kA\026D\004@\0162\nBsq\024\192\004\007@"

    | "tiles" ->
"\132\149\166\190\000\000\000\158\000\000\000\014\000\000\000A\000\000\0000\160\160\014\003W\\\158\207\195\030\227?u\027\133\165}\026\004\192Y\141|c\217\237\241?(identity\160\160\014\004\147\029\133;\184@\233?\245\011\220uo\233\235?\236\255\145<_!\015\192*\029q\028\213\169\239?'sigmoid\160\160\014\004\135\2089\219\147G \192\213\165W+\224B \192\144\173\2161\223\159\001\192\004N\001!\152\167\211\191\004\004\160\160\014\004 -\161W\176\167\241?T\216\242{\144\006\241?\020\021 \005\141\253\005@\221\174\159.Xv\b\192\004\007@"

    | "l4grid" ->
	"\132\149\166\190\000\000\000\158\000\000\000\014\000\000\000A\000\000\0000\160\160\014\003txfl}{\253?\127\192\227\152\186\245\000\192\169\r\162\026\027\198\000@(identity\160\160\014\004\198<\171`\019\228\248\191\200W\187\128\154\005\236?\t\234\132\249\167{\006@\004\177\t\194\142\003\t\192'sigmoid\160\160\014\004\213>\023\005%\014\190\191\239\238p\180\bJ\211?4\151\174\156\001V\007@\225C\242:\003[\232?\004\004\160\160\014\004\143$T\220\196\028\030@\253\255\249x\024p\219?~\255\000\234]\015\250?\225\159\029\157V\230\211?\004\007@"

    | "u4grid" ->
"\132\149\166\190\000\000\000\158\000\000\000\014\000\000\000A\000\000\0000\160\160\014\003\199@\203\228\235:\004@\173\014\134\026\245\182\250\191y\016KMP\031\254?(identity\160\160\014\004\157b\001\139*.\244?\012,c[\143\136\231?\128]Ap\2311\014@;\170\016\r$$\006\192'sigmoid\160\160\014\004\225\025K\031x|\224?`\002\244$\017\135\177?\164\148\146@oB\018@\243\143\133\176.\019\002\192\004\004\160\160\014\004\027\147\128\"\148\158\201?\255\129\159\194\0063\192?\161\022\1777\169\017\005\192\251v\131\030\221S\241\191\004\007@"
    | _ -> failwith "Not found"


let offline_l4g = function
  | [|h; d; g; c|] ->
      1.024316 *. h +. 0.09924 *. d +. -0.042736 *. g +. 0.076587 *. c
  | _ -> nan


let offline_l4g_nog = function
  | [|h; d; g; c|] ->
      1.566267 *. h +. -0.231067 *. d +. 0.062114 *. c
  | _ -> nan


let offline_vacuum = function
   | [|h; d; g; c|] ->
      0.561294 *. h +. 0.674096 *. d +. -0.168611 *. g +. 0.146787 *. c
  | _ -> nan


let offline_tiles = function
   | [|h; d; g; c|] ->
      0.270720 *. h +. -0.079006 *. d +. 0.393354 *. g +. 0.295857 *. c
  | _ -> nan


let online_lms run =
  Single_step_model.streamed_lms (Single_step_model.get_features_targets
				    Single_step_model.standard_features run)

let online_lms_m2 run =
  Single_step_model.streamed_lms (Single_step_model.get_features_targets_m2
				    Single_step_model.standard_features run)

let online_ann run =
  Single_step_model.streamed_ann (Single_step_model.get_features_targets
				    Single_step_model.standard_features run)


let rev_lms run =
  Single_step_model.streamed_lms (Single_step_model.rev_features_targets
				    Single_step_model.standard_features run)

let rev_ann run =
  Single_step_model.streamed_ann (Single_step_model.rev_features_targets
				    Single_step_model.standard_features run)


let offline_ann dom =
  let ann = (match (Ann.ann_from_string (get_ann_string dom))
		   with Ann.Twolayer t -> t | _ -> failwith "bad string") in
    (Ann.two_layer_output ann)

let standard_esth n = n.Recorded_run.cost -. (Recorded_run.get_g n)
let greedy_esth n = n.Recorded_run.cost


let make_estf est run =
  let get_feat = (fun n -> [|Recorded_run.get_h n;
			     Recorded_run.get_d n;
			     Recorded_run.get_g n;
			     1.|]) in
  let scale_out, norms = Batched_regression.get_maxes_run get_feat run in
    (fun n ->
       scale_out *. (est (Batched_regression.norm norms (get_feat n))));;


let h_versus_truth_scatter run =
  let i = ref 0 in
    (* make the array for the points to go into *)
  let points = (Array.create (Hashtbl.length run.Recorded_run.run)
		  (Geometry.point ~x:0. ~y:0.)) in
    (* get the points into the array *)
    Hashtbl.iter
      (fun key node ->
	 points.(!i) <- (Geometry.point ~x:(Recorded_run.get_h node)
			   ~y:(Recorded_run.get_ht node));
	 i := !i + 1;) run.Recorded_run.run;
    let dset = Scatter_dataset.scatter_dataset (Drawing.Plus_glyph) points in
    let plot = Num_by_num.plot
      ~title:"h* vs h"
      ~xlabel:"h"
      ~ylabel:"h*" [dset] in
      plot#display


let h_versus_truth_histogram run =
  (** Plots the error as a histogram *)
  let i = ref 0 in
    (* make the array for the points to go into *)
  let points = (Array.create (Hashtbl.length run.Recorded_run.run) 0.) in
    (* get the points into the array *)
    Hashtbl.iter
      (fun key node ->
	 points.(!i) <- (Recorded_run.get_ht node) -. (Recorded_run.get_h node);
	 i := !i + 1;) run.Recorded_run.run;
    let dset = Histogram_outline_dataset.values_histogram_dataset ([||]) points
    in let plot = Num_by_num.plot
	~title:"h* vs h"
	~xlabel:"error"
	~ylabel:"frequency" [dset] in
      plot#display


let hd_versus_truth_histogram run =
  (** Plots the error as a histogram *)
  let module RR = Recorded_run in
  let module HOD = Histogram_outline_dataset in
  let line = Factories.default_dash_factory ()
  and color = Factories.default_color_factory () in
  let i = ref 0 in
  let hpoints = (Array.create (Hashtbl.length run.RR.run) 0.) in
  let dpoints = (Array.create (Hashtbl.length run.RR.run) 0.) in
    Hashtbl.iter
      (fun key node ->
	 hpoints.(!i) <- (RR.get_h node) /. (RR.get_ht node);
	 dpoints.(!i) <- (RR.get_d node) /. (RR.get_dt node);
	 i := !i + 1;) run.RR.run;
    let hdset = HOD.values_histogram_dataset ~line_color:(color()) (line())
      ~line_width:(Length.Pt 2.) ~name:"Cost" hpoints
    and ddset = HOD.values_histogram_dataset ~line_color:(color()) (line())
      ~line_width:(Length.Pt 2.) ~name:"Actions" dpoints
    in let plot = Num_by_num.plot
	~title:"Life Grids Heuristic Accuracy"
	~xlabel:"Estimate / Truth"
	~ylabel:"Frequency" [hdset;ddset] in
      plot#display




let inv_fp_h_versus_truth_histogram tr_file =
  (** Plots the error as a histogram *)
  let r = ref 0 in
  let truth,_,_ = Run_reader.proc_truth_file (fun _ -> r := !r + 1; !r) tr_file in
  Verb.pe Verb.always "got truth file\n%!";
  let i = ref 0 in
    (* make the array for the points to go into *)
  let points = (Array.create (Hashtbl.length truth) (Geometry.point ~x:0. ~y:0.)) in
    (* get the points into the array *)
    Hashtbl.iter
      (fun key tr_val ->
	let hstar = List.nth tr_val.Run_reader.true_heuristics 0
(*	and dstar = List.nth tr_val.Run_reader.true_heuristics 1 *)
	and h = List.nth tr_val.Run_reader.admi_heuristics 0
(*	and d = List.nth tr_val.Run_reader.admi_heuristics 1 *) in
(*	assert(h <= hstar);*)
	 points.(!i) <- Geometry.point ~x:hstar ~y:h;
	i := !i + 1;) truth;
  let points = Wrarray.sample (min ((Array.length points) - 1) 10_000) points in
  let dset = Scatter_dataset.scatter_dataset (Drawing.Point_glyph) points in
  let plot = Num_by_num.plot
       ~title:"h* vs h"
       ~xlabel:"h"
       ~ylabel:"h*" [dset] in
     plot#display



let hhat_versus_truth_scatter run est_h =
  let i = ref 0 in
    (* make the array for the points to go into *)
  let points = (Array.create (Hashtbl.length run.Recorded_run.run)
		  (Geometry.point ~x:0. ~y:0.)) in
    (* get the points into the array *)
    Hashtbl.iter
      (fun key node ->
	 points.(!i) <- (Geometry.point ~x:(est_h node)
			   ~y:(Recorded_run.get_ht node));
	 i := !i + 1;) run.Recorded_run.run;
    let dset = Scatter_dataset.scatter_dataset (Drawing.Plus_glyph) points in
    let plot = Num_by_num.plot
      ~title:"h* vs h^"
      ~xlabel:"h^"
      ~ylabel:"h*" [dset] in
      plot#display

let max_points = ref 100_000


let hhat_versus_truth_box domain run est_h =
  (** Plots the error as a histogram *)
  let i = ref 0 in
    (* make the array for the points to go into *)
  let est_h = (Recorded_run.get_h, "Base")::est_h in
  let len = min !max_points ((Hashtbl.length run.Recorded_run.run) -1) in
    Verb.pe Verb.always "%i data points to crunch\n%!" len;
    let points = (Array.map (fun _ -> Array.create len nan)
		    (Array.of_list est_h)) in
    let to_visit = Wrarray.sample len run.Recorded_run.sequence in
      (* get the points into the array *)
      Verb.pe Verb.always "Making points\n%!";
      Array.iter
	(fun key ->
	     Wrlist.iteri (fun j (eh,_) ->
			     try
			       let node = Recorded_run.get_node run key in
			       points.(j).(!i) <-
				 ((Recorded_run.get_ht node) -. (eh node))
			     with _ -> points.(j).(!i) <- nan) est_h;
	     i := !i + 1;) to_visit;

      Verb.pe Verb.always "Filtering Nans\n%!";
      for ind = 0 to (Array.length points) - 1 do
	points.(ind) <- Wrarray.filter Math.finite_p points.(ind);
      done;
      Array.iter (fun ar ->
		    Verb.pe Verb.always "%i points remaining\n%!"
		      (Array.length ar)) points;
      let points = Wrarray.filter (fun ar -> Array.length ar > 0) points in
	Verb.pe Verb.always "Plotting\n%!";
	let dsets = List.map2 (fun data (_,nm) ->
				 Num_by_nom.boxplot_dataset ~outliers:false
				   nm data) (Array.to_list points) est_h in
	let plot = Num_by_nom.plot
	  (*~horiz_lines:[0.]*) (* puts a line at 0 *)
	  ~title:domain
	  ~ylabel:"Error"
	  dsets in
	  plot#display


let abs_error_hhat_versus_truth_box domain run est_h =
  (** Plots the error as a histogram *)
  let i = ref 0 in
    (* make the array for the points to go into *)
  let est_h = (Recorded_run.get_h, "Base")::est_h in
  let len = min !max_points ((Hashtbl.length run.Recorded_run.run) -1) in
    Verb.pe Verb.always "%i data points to crunch\n%!" len;
    let points = (Array.map (fun _ -> Array.create len nan)
		    (Array.of_list est_h)) in
    let to_visit = Wrarray.sample len run.Recorded_run.sequence in
      (* get the points into the array *)
      Verb.pe Verb.always "Making points\n%!";
      Array.iter
	(fun key ->
	     Wrlist.iteri (fun j (eh,_) ->
			     try
			       let node = Recorded_run.get_node run key in
				 points.(j).(!i) <-
				   (*log*) ((*abs_float*)
					  ((Recorded_run.get_ht node) -.
					     (eh node)))
			     with _ -> points.(j).(!i) <- nan) est_h;
	     i := !i + 1;) to_visit;

      Verb.pe Verb.always "Filtering Nans\n%!";
      for ind = 0 to (Array.length points) - 1 do
	points.(ind) <- Wrarray.filter Math.finite_p points.(ind);
      done;
      Array.iter (fun ar ->
		    Verb.pe Verb.always "%i points remaining\n%!"
		      (Array.length ar)) points;
      let points = Wrarray.filter (fun ar -> Array.length ar > 0) points in
	Verb.pe Verb.always "Plotting\n%!";
	let dsets = List.map2 (fun data (_,nm) ->
				 Num_by_nom.boxplot_dataset ~outliers:false
				   nm data) (Array.to_list points) est_h in
	let plot = Num_by_nom.plot
	  (*~horiz_lines:[0.]*) (* puts a line at 0 *)
	  ~title:domain
	  ~ylabel:"Error"
	  dsets in
	  plot#display

let depths_to_plot depths =
  let points = List.map (List.fold_left
			   (fun accum (a,b) ->
			      (Geometry.point ~x:(float a) ~y:(float b))::accum)
			   []) depths in
  let points = Array.of_list (List.map Array.of_list points) in
    Array.iter (Array.sort (fun a b -> compare a.Geometry.x b.Geometry.x))
      points;
  let iref = ref 1 in
  let ds = (Line_errbar_dataset.line_errbar_dataset
	      { Line_errbar_dataset.dashes = [||];
		Line_errbar_dataset.number = 0;
		Line_errbar_dataset.count = iref;}
	      ~name:"opt_rank" points) in
  let plot = Num_by_num.plot [ds] in
    plot#display


let avg_depths_to_plot depths =
  let points = Array.mapi (fun ind y ->
			     Geometry.point ~x:(float ind) ~y) depths in
  let iref = ref 1 in
  let ds = (Line_errbar_dataset.line_errbar_dataset
	      { Line_errbar_dataset.dashes = [||];
		Line_errbar_dataset.number = 0;
		Line_errbar_dataset.count = iref;}
	      ~name:"opt_rank" [|points|]) in
  let plot = Num_by_num.plot [ds] in
    plot#display


let vstr num ext =
  Printf.sprintf "/var/tmp/offline_fitting/data/vacuum/200x200_0.35_5_%i.%s"
    num ext

let do_vac_plot ?(r = None) num =
  let run = match r with Some run -> run
    | _ -> Run_reader.vac_run ~tr_file:(vstr num "truth") (vstr num "exhaust")
	[] in
    Verb.pe Verb.always "Run file loaded\n%!";
(*
  let online_lms = make_estf (online_lms run) run in
  let reverse_lms = make_estf (rev_lms run) run in
  let offline_lms = make_estf (offline_vacuum) run in
  let online_ann = make_estf (online_ann run) run in
  let offline_ann = make_estf (offline_ann "vacuum") run in
  let reverse_ann = make_estf (rev_ann run) run in
    *)
  let ss_path = (Single_step_model.path_ss run) in
  let ss_global = (Single_step_model.global_ss run) in
  let cost_path = (Single_step_model.justh_ss run) in
  let cost_global = (Single_step_model.global_justh run) in
    abs_error_hhat_versus_truth_box "Vacuum World" run
      [(*online_lms, "Online LMS";
       reverse_lms, "Reverse LMS";
       online_ann, "Online ANN";
       reverse_ann, "Reverse ANN";*)
	cost_path, "Cost Path";
	cost_global, "Cost Global";
	ss_path, "SS Path";
	(*ss_global, "SS Global";*)]


let estr ext = Printf.sprintf "/var/tmp/offline_fitting/data/eight/%s" ext


let do_8_plot ?(r = None) num =
  let run = match r with Some run -> run
    | _ -> Run_reader.ep_run ~tr_file:(estr "truth") (estr "1.exhaust")
	[] in
    Verb.pe Verb.always "Run file loaded\n%!";

  let online_lms = make_estf (online_lms run) run in
  let reverse_lms = make_estf (rev_lms run) run in
(*  let offline_lms = make_estf (offline_tiles) run in*)
  let online_ann = make_estf (online_ann run) run in
(*  let offline_ann = make_estf (offline_ann "tiles") run in*)
  let reverse_ann = make_estf (rev_ann run) run in
  let ss_path = (Single_step_model.path_ss run) in
(*  let ss_global = (Single_step_model.global_ss run) in
  let cost_path = (Single_step_model.justh_ss run) in
  let cost_global = (Single_step_model.global_justh run) in*)
    abs_error_hhat_versus_truth_box "Eight Puzzle" run
      [online_lms, "Online LMS";
       reverse_lms, "Reverse LMS";
       online_ann, "Online ANN";
       reverse_ann, "Reverse ANN";
       ss_path, "SS Path";];
    run


(* EOF *)
