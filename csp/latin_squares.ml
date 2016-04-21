(* $Id: csp_instances.ml,v 1.1 2004/01/13 23:45:08 ruml Exp ruml $

   experiments on latin squares
*)


(******************** running experiments ***********************)


let run_batch preassigned order num_instances halt alg =
  (** runs many experiments, each one gets a line in the log file *)
  Do_runs.assert_alg alg;
  let prob = [ "model", "latin_square";
	       "preassigned", string_of_float preassigned;
	       "order", string_of_int order ] in
  let attrs = ["type", "run";
	       "alg", alg;
	       "num_instances", string_of_int num_instances;
	       "halt", Info.string_from_limit halt;] @
	      prob in
  let path = Rdb.path_for Csp_instances.data_root attrs in
    Wrio.with_outfile path
      (fun ch ->
	 Wrutils.pr "Writing to %s\n%!" path;
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
	   let res, n, l, t = Do_runs.get_result alg halt path in
	     Wrutils.pf ch "%d\t%d\t%d\t%f\n%!" res n l t;
	     Wrutils.pr "%c" (if res < 0 then '#' else '.');
	     flush_all ()
	 done;
	 Datafile.write_trailer_pairs ch);
    Wrutils.pr "done.\n"


let do_21 alg =
  let limit = Info.Nodes 10000 in
    run_batch 0.3 21 1000 limit alg


let test i alg v =
  (* at 21, 3, 4, 8 cause multiple BLFS iterations *)
  Verb.with_level v
    (fun () ->
       Do_runs.run_alg alg Info.Never (Csp_instances.load_csp (Wrutils.str "/tilde/ruml/projects/csp/data/instance/latin_square/0.3/21/%d" i)))


(************************** analyzing the data ***************************)


let get_alg_data order alg =
  let attrs = ["type", "run";
	       "alg", alg;
	       "model", "latin_square";
	       "order", string_of_int order] in
  let path = Rdb.matching_path Csp_instances.data_root attrs in
    Datafile.load path


let filize s =
  Wrstr.replace_strings s [" ", "_"]


let plot_cdf ?(max_x = None) order algs key =
  let data = List.map (fun alg ->
			 let df = get_alg_data order alg in
			   alg, (Datafile.clamped_col df key))
	       algs
  and path = Wrutils.str "lsc-%d-%s.ps" order (filize key) in
    Ps_plot.cdf (Filename.concat Do_runs.plot_dir path)
      data ~max_x:max_x key "Fraction of Problems Solved"


let plot_leaves
  ?(basic = ["dfs"; "ilds_bottom"; "ilds_top"; "dds"])
  others =
  plot_cdf 21 (basic @ others) "leaves"


(* EOF *)
