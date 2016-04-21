(** Performs model-based estimation on pancake instances.

    2010-10-20 eaburns -- adapted from tiles_model_estimation.ml

    @author eaburns
    @since 2010-06-28
*)


let data_root = Filename.concat User_paths.data_root "pancake"
let instance_root = Filename.concat User_paths.instance_root "pancake"


let load_cdp path =
  let inch = open_in path in
  let model = (Marshal.from_channel inch : Gkre.t) in
    close_in inch;
    model


let load_im path =
  let inch = open_in path in
  let model = (Marshal.from_channel inch : Im.t) in
    close_in inch;
    model


let run_cdp model sface opt prob () =
  let root = sface.Search_interface.initial in
  let h = sface.Search_interface.h
  and t = (fun _ -> 0)
  and expand = sface.Search_interface.domain_expand in
  let kids = List.map (fun (c, _) -> truncate (h c), t c) (expand root 0.) in
    Gkre.cdp model (truncate (h root)) (t root) kids opt


let run_im model sface opt prob () =
  let root = sface.Search_interface.initial in
  let h = sface.Search_interface.h
  and d = sface.Search_interface.d
  and expand = sface.Search_interface.domain_expand in
  let kids =
    List.map
      (fun (c, g) -> 0, truncate (d c), g +. (h c))
      (expand root 0.)
  in snd (Im.count_nodes_from model (h root) kids opt)


let output_estimations ~ncakes cdp_model im_model ~size =
  let cdp_model = load_cdp cdp_model in
  let im_model = load_im im_model in
  let attrs = ["type", "instance"; "ncakes", string_of_int ncakes; ] in
  let cost_name = "unit" in
(*
  let cost = Pancake.cost_function_by_name cost_name in
*)
    List.iter
      (fun prob_attrs ->
	 let prob_path = Rdb.path_for instance_root prob_attrs in
	 let num = List.assoc "num" prob_attrs in
	 let run_attrs =
	   ("alg", "offline_estimations")
	   :: ("training set size", string_of_int size)
	   :: ("cost", "unit") :: prob_attrs
	 in
	 let run_file = Rdb.path_for data_root run_attrs in
	 let opt_data_path =
	   Rdb.path_for data_root
	     ["alg", "idastar_soong"; "ncakes", string_of_int ncakes;
	      "num", num ]
	 in
	 let opt_df = Datafile.load opt_data_path in
	 let opt_cost =
	   float_of_string (Datafile.get_val opt_df "final sol cost") in
	 let opt_nodes =
	   int_of_string (Datafile.get_val opt_df "total nodes expanded") in
	 let opt_nodesf = float opt_nodes in
	 let prob = Pancake.load prob_path in
	 let sface = Pancake.hgap_interface cost_name prob [] in
	   Printf.printf "%3d:   opt = %g\n     nodes = %d\n%!"
	     (int_of_string num) opt_cost opt_nodes;
	   let cdp_estimation, cdp_time =
	     Wrsys.with_time (run_cdp cdp_model sface (truncate opt_cost) prob)
	   in
	   let cdp_frac = cdp_estimation /. opt_nodesf in
	     Printf.printf "       cdp = %f (frac = %g, time = %g s)\n%!"
	       cdp_estimation cdp_frac cdp_time;
	     let im_estimation, im_time =
	       Wrsys.with_time (run_im im_model sface opt_cost prob)
	     in
	     let im_frac = im_estimation /. opt_nodesf in
	       Printf.printf "        im = %f (frac = %g, time = %g s)\n%!"
		 im_estimation im_frac im_time;
	       Wrio.with_outfile run_file
		 (fun outch ->
		    Datafile.write_header_pairs outch;
		    Datafile.write_pairs outch run_attrs;
		    Datafile.write_pairs outch
		      ["attrs", (Wrstr.encode_pairs attrs)];
		    Datafile.write_pairs outch
		      ["cdp estimation", string_of_float cdp_estimation;
		       "cdp wall time", string_of_float cdp_time;
		       "cdp fraction", string_of_float cdp_frac;
		       "im estimation", string_of_float im_estimation;
		       "im wall time", string_of_float im_time;
		       "im fraction", string_of_float im_frac;
		       "training set size", string_of_int size;
		       "idastar expansions", string_of_int opt_nodes; ];)
      )
      (Rdb.matching_attrs instance_root attrs)

let main () =
  let verb = ref Verb.always in
  let im_file = ref "50cakes-100mil.im" in
  let cdp_file = ref "50cakes-100mil.cdp" in
  let size = ref 10_000_000_000 in
  let ncakes = ref 50 in
    ignore
      (Arg.parse
	 [
	   "-v", Arg.Set_int verb, "verbosity (default: always)";
	   "--im", Arg.Set_string im_file, "(default: 50cakes-100mil.im)";
	   "--cdp", Arg.Set_string cdp_file, "(default: 50cakes-100mil.cdp))";
	   "--size", Arg.Set_int size, "model size (default: 10000000000)";
	   "--ncakes", Arg.Set_int ncakes, "number of pancakes (default: 50)";
	 ]
	 (fun s -> Printf.eprintf "Ignoring exrta argument: %s\n" s)
	 ("pancake_model_estimation [--ncakes <num>] [--im <file>] "
	  ^ "[--cdp <file>] [--size <num>] [-v <num>]")
      );
    Verb.set_default !verb;
    output_estimations !ncakes !cdp_file !im_file !size


let _ = main ()
