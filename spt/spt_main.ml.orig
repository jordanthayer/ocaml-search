(** The main function for the spt program.

    For now this is just for testing spt, but eventually it
    should be a full program that reads plots from a file (or stdin)
    and will build the plot to the output.

    @author eaburns
    @since 2010-04-15
*)

open Geometry
open Drawing
open Spt
open GMain

let num_by_nom_plot () =
  (*
    let barchart =
    Barchart_dataset.barchart_datasets
    ~use_color:true
    ["jan", 10.; "feb", 40.; "mar", 20.; "apr", -10.;
    "may", 10.; "jun", 10.; "jul", 10.; "aug", 10.;
    "sep", 10.; "oct", 10.; "nov", 10.; "dec", 10.;
    ]
    in
  *)
  (*let bar_err =
    (Barchart_dataset.barchart_errbar_datasets
       ~group:"Winter"
       ~use_color:true
       [
	 "dec", (Array.init 1 (fun _ -> Random.float 1000.));
	 "jan", (Array.init 1 (fun _ -> Random.float 1000.));
	 "feb", (Array.init 1 (fun _ -> Random.float 1000.));
       ]) @
      (Barchart_dataset.barchart_errbar_datasets
	 ~group:"Spring"
	 ~use_color:true
	 [
	   "mar", (Array.init 10 (fun _ -> Random.float 1000.));
	   "apr", (Array.init 10 (fun _ -> Random.float 1000.));
	   "may", (Array.init 10 (fun _ -> Random.float 1000.));
	 ]) @
      (Barchart_dataset.barchart_errbar_datasets
	 ~group:"Summer"
	 ~use_color:true
	 [
	   "jun", (Array.init 100 (fun _ -> Random.float 1000.));
	   "jul", (Array.init 100 (fun _ -> Random.float 1000.));
	   "aug", (Array.init 100 (fun _ -> Random.float 1000.));
	 ]) @
      (Barchart_dataset.barchart_errbar_datasets
	 ~group:"Autumn"
	 ~use_color:true
	 [
	   "sep", (Array.init 1000 (fun _ -> Random.float 1000.));
	   "oct", (Array.init 1000 (fun _ -> Random.float 1000.));
	   "nov", (Array.init 1000 (fun _ -> Random.float 1000.));
	 ])*)
  let stacked =
    Barchart_dataset.stacked_barchart_datasets
      ~group:"Stacked"
      ~fill_factory:(Factories.default_color_fill_pattern_factory ())
      [ None,[|"a1", 10.; "a2", 5.; "a3", 7.;|];
	(Some "b"), [|"b1", 10.; "b2", 2.; "b3", 9.;|];
	(Some "c"), [|"c1", 8.; "c2", 13.; "c3", 1.;|]]

  and layered =
    Barchart_dataset.layered_barchart_datasets
      ~group:"Layered"
      ~fill_factory:(Factories.default_color_fill_pattern_factory ())
      [ (Some "a"), [|"a1", 10.; "a2", 5.; "a3", 7.;|];
	(Some "b"), [|"b1", 10.; "b2", 2.; "b3", 9.;|];
	None, [|"c1", 8.; "c2", 13.; "c3", 1.;|]]
  in
    new Num_by_nom.plot ~title:"Title text" ~ylabel:"Y label text"
      (stacked @ layered)

let num_by_num_plot () =
  let next_dash = Factories.default_dash_factory () in
  let next_glyph = Factories.default_glyph_factory () in
  let next_line_err = Num_by_num.line_errbar_factory next_dash () in
  let count_map = Num_heatmap_dataset.countmap_dataset
    ~bin_size:(Geometry.point 1. 1.)
    ~name:"Countmap" (Array.init 10000 (fun i ->
					 { x = Random.float 100.;
					   y = Random.float 100.;})) in
    (*
      let histogram = new Num_by_num.histogram_dataset (next_dash ())
      ~name:"histo" (Array.init 100 (fun i -> Random.float 1000.))
      and histogram2 = new Num_by_num.histogram_dataset (next_dash ())
      ~name:"histo" (Array.init 100 (fun i -> Random.float 1000.))
      in
    *)

    new Num_by_num.plot
      ~title:"Title text"
      ~xlabel:"X label text"
      ~ylabel:"Y label text"
      ~legend_loc:Legend.Upper_right
      [(*
	Num_by_num.bestfit_dataset
	  (next_glyph ()) (next_dash ())
	  ~color:green ~name:"Best fit"
	  [|
	    point 0. 0.;
	    point 1. 1.;
	    point 2. 2.;
	    point 3. 1.;
	  |];

	Num_by_num.line_points_dataset (next_dash ()) (next_glyph ())
	  ~name:"Lines and points"
	  [| point 0.5 7.5; point 1.0 1.4; point 2.0 7.0; point 10.0 3.5; |];

	new Num_by_num.line_errbar_dataset
	  ~name:"Lines with error bars"
	  ~color:blue (next_line_err ())
	  [|
	    [| point 0.5 17.5; point 1.0 11.4;
	       point 2.0 17.0; point 10.0 13.5; |];
	    [| point 8.5 17.5; point 1.8 17.1;
	       point 5.7 12.6; point 20.7 13.5; |];
	    [| point 3.2 17.6; point 4.7 12.1;
	       point 3.6 17.7; point 10.0 13.7; |];
	    [| point 1.9 17.5; point 9.2 18.0;
	       point 8.5 17.1; point 10.1 13.9; |];
	  |];

	new Num_by_num.function_dataset (next_dash ())
	  ~name:"y=x^2" (fun x -> x ** 2.);*)
	count_map
      ]


let main () =
(*
    Random.init 17;
*)
  Random.self_init ();
  let plot = num_by_num_plot () in
(*
    Printf.eprintf "Suggested Ratio: %f\n%!"
      plot#suggest_aspect;
    plot#use_suggested_aspect;
*)
    plot#display

let _ = main ()
