(* $Id: csp_instances.ml,v 1.1 2004/01/13 23:45:08 ruml Exp ruml $

   experiments on binary csps
*)

open Do_runs


(******************** running experiments ***********************)


let run_batch n m p1 p2 num_instances halt alg =
  (** runs many experiments, each one gets a line inthe log file *)
  assert_alg alg;
  let prob = [ "model", "random_binary";
	       "num_vars", string_of_int n;
	       "num_vals", string_of_int m;
	       "p1", string_of_int p1;
	       "p2", string_of_int p2; ] in
  let attrs = ["type", "run";
	       "alg", alg;
	       "num_instances", string_of_int num_instances;
	       "halt", Info.string_from_limit halt;] @
	      prob in
  let path = Rdb.path_for Csp_instances.data_root attrs in
    Wrio.with_outfile path
      (fun ch ->
	 Wrutils.pr "Writing to %s\n" path;
      	 Datafile.write_header_pairs ch;
	 Datafile.write_pairs ch attrs;
	 Datafile.write_pairs ch ["attrs",
				  (Wrstr.encode_pairs attrs)];
	 Datafile.write_colnames ch
	   ["best cost"; "nodes"; "leaves"; "raw cpu time"];
	 for i = 1 to num_instances do
	   let path = Rdb.matching_path Csp_instances.data_root
			(prob @ ["type", "instance";
				 "instance_num", string_of_int i]) in
	   let res, n, l, t = get_result alg halt path in
	     Wrutils.pf ch "%d\t%d\t%d\t%f\n%!" res n l t;
	     Wrutils.pr "%c" (if res < 0 then '#' else '.');
	     flush_all ()
	 done;
	 Datafile.write_trailer_pairs ch);
    Wrutils.pr "done.\n"


let do_binary alg =
  let limit = Info.Nodes 1000000 in
    List.iter (fun (n,m,p1,p2) -> run_batch n m p1 p2 100 limit alg)
      Csp_instances.binary_params


let test alg v i =
  Verb.with_level v
    (fun () ->
       Do_runs.run_alg alg Info.Never (Csp_instances.load_csp (Wrutils.str "/tilde/ruml/projects/csp/data/instance/random_binary/30/15/174/81/%d" i)))


(************************** analyzing the data ***************************)


let get_alg_data n m p1 p2 alg =
  let attrs = ["type", "run";
	       "model", "random_binary";
	       "alg", alg;
	       "num_vars", string_of_int n;
	       "num_vals", string_of_int m;
	       "p1", string_of_int p1;
	       "p2", string_of_int p2; ] in
  let path = Rdb.matching_path Csp_instances.data_root attrs in
  let df = Datafile.load path in
    assert (Datafile.has_val df "wall finish time");
    df


let plot_cdf path n m p1 p2 algs key =
  let data = List.map (fun alg ->
			 alg, (Datafile.get_col (get_alg_data n m p1 p2 alg)
				 key))
	       algs in
    Ps_plot.cdf path data key "fraction of problems solved"


let filize s =
  Wrstr.replace_strings s [" ", "_"]


let plot_binary algs key =
  let num_batches = 100 in
  let plots = List.map (fun (n, m, p1, p2) ->
			  let path = Filename.temp_file
				       "binary_csp_plot" ".ps" in
			    plot_cdf path n m p1 p2 algs key;
			    path)
		(Wrlist.firstn Csp_instances.binary_params num_batches)
  in
    Ps_plot.montage plots (Filename.concat plot_dir
			     ("binary_csps_" ^ (filize key) ^ ".ps"))


(*************************)


let plot_scatter path n m p1 p2 algs keya keyb =
  let data = List.map (fun alg ->
			 let df = get_alg_data n m p1 p2 alg in
			   alg, "", (Wrarray.combine
				       (Datafile.get_col df keya)
				       (Datafile.get_col df keyb)))
	       algs in
    Ps_plot.scatter path data keya keyb



let time_per_node algs =
  let keya = "log10 nodes"
  and keyb = "log10 cpu time" in
  let plots = List.map (fun (n, m, p1, p2) ->
			  let path = Filename.temp_file
				       "binary_csp_plot" ".ps" in
			    plot_scatter path n m p1 p2 algs keya keyb;
			    path)
		(Wrlist.firstn Csp_instances.binary_params 4)
  in
    Ps_plot.montage plots (Filename.concat plot_dir
			     ("binary_csps_" ^ (filize keya) ^ "_" ^ (filize keyb) ^ ".ps"))


(* EOF *)
