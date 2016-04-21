(* $Id: main.ml,v 1.1 2004/01/13 23:45:14 ruml Exp ruml $

   running CSP algorithms
*)
         

let alg_table = [ "dfs", Csp_tree_algs.dfs;
		  "ilds_top", Csp_tree_algs.ilds_top;
		  "ilds_bottom", Csp_tree_algs.ilds_bottom;
		  "dds", Csp_tree_algs.dds;
		  "indec_sum", Csp_tree_algs.indecision_sum ]

		  
let get_alg name =
  try
    List.assoc name alg_table
  with Not_found -> failwith ("unknown alg: " ^ name)

    
let assert_alg name =
  if not (List.mem_assoc name alg_table) then
    failwith ("unknown alg: " ^ name)
    
      
(********** solving an instance ***********)

		    
let run_alg alg_name halt csp =
  (** prints various levels of output, depending on current verbosity *)
  let alg = get_alg alg_name in
    Gc.full_major ();
    let (sol, stats, opt, complete), time =
      Wrsys.with_time (fun () -> alg csp halt Info.null_logger) in
      Verb.force 3
	(lazy (Utils.pr "Solver finished in %f seconds.\n" time;
	       Utils.pr "  (that's about %d nodes/second)\n"
		 (Math.round ((float_of_int stats.Info.nodes) /. time));
	       Info.print_results stdout stats opt complete;
	       if opt then
		 Utils.pr "Solver claims SAT.\n"
	       else if complete then
		 Utils.pr "Solver claims UNSAT.\n"));
      Csp.check (Csp_tree.sol sol) csp opt;
      Verb.pr 3 "Solution checks out.\n";
      (* if opt, then SAT.  if not opt and complete, UNSAT. *)
      Utils.pr "%d\t%d\t%d\t%f\n"
	(if opt then 1 else if complete then 0 else -1)
	stats.Info.nodes stats.Info.leaves time;
      flush_all ()


let test alg i =
  Verb.with_level 3
    (fun () ->
       run_alg alg Info.Never (Csp_instances.load_csp
				 (Utils.str "/tilde/ruml/projects/csp/data/instance/30/15/174/81/%d" i)))
      
      
(**************** running a batch of experiments *************)

(* calls an external solver *)

let solver = "/tilde/ruml/projects/csp/bin/csp_solver.linux"


let get_result alg halt csp_path =
  let s = Wrsys.shell_output (Utils.str "%s %s %s < %s"
				solver alg (Info.arg_string_from_limit halt)
				csp_path) in
    try
      Scanf.sscanf s "%d\t%d\t%d\t%f\n" (fun s n l t -> s,n,l,t)
    with Scanf.Scan_failure _ -> failwith ("bad solver output: " ^ s)
  
      
let run_batch n m p1 p2 num_instances halt alg =
  (** runs many experiments, each one gets a line inthe log file *)
  assert_alg alg;
  let prob = [ "num_vars", string_of_int n;
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
	 Utils.pr "Writing to %s\n" path;
      	 Datafile.write_header_pairs ch;
	 Datafile.write_pairs ch attrs;
	 Datafile.write_pairs ch ["attrs",
				  (Datafile.encode_pairs attrs)];
	 Datafile.write_colnames ch
	   ["best cost"; "nodes"; "leaves"; "raw cpu time"];
	 for i = 1 to num_instances do
	   let path = Rdb.matching_path Csp_instances.data_root
			(prob @ ["type", "instance";
				 "instance_num", string_of_int i]) in
	   let res, n, l, t = get_result alg halt path in
	     Utils.pf ch "%d\t%d\t%d\t%f\n%!" res n l t;
	     Utils.pr "%c" (if res < 0 then '#' else '.');
	     flush_all ()
	 done;
	 Datafile.write_trailer_pairs ch);
    Utils.pr "done.\n"


let do_binary alg =
  let limit = Info.Nodes 1000000 in
    List.iter (fun (n,m,p1,p2) -> run_batch n m p1 p2 100 limit alg)
      Csp_instances.binary_params


(****************** analyze data **************************)


let plot_dir = "/tilde/ruml/projects/csp/plots"
		 

let get_col data key =
  if key = "cpu time" then
    let d = Datafile.get_col data "raw cpu time"
    and m = Datafile.get_val data "machine id" in
      Data.normalize_times m d
  else
    Datafile.get_col data key
      

let get_alg_data n m p1 p2 alg key =
  let attrs = ["type", "run";
	       "alg", alg;
	       "num_vars", string_of_int n;
	       "num_vals", string_of_int m;
	       "p1", string_of_int p1;
	       "p2", string_of_int p2; ] in
  let path = Rdb.matching_path Csp_instances.data_root attrs in
  let df = Datafile.load path in
    get_col df key

  
let plot_algs path n m p1 p2 algs key =
  let data = List.map (fun alg ->
			 alg, (get_alg_data n m p1 p2 alg key))
	       algs in
    Ps_plot.cdf path data key "fraction of problems solved"


let filize s =
  Wrstr.replace_strings s [" ", "_"]
    

let plot_binary algs key =
  let plots = List.map (fun (n, m, p1, p2) ->
			  let path = Filename.temp_file
				       "binary_csp_plot" ".ps" in
			    plot_algs path n m p1 p2 algs key;
			    path)
		(Wrlist.firstn Csp_instances.binary_params 10) in
    Ps_plot.montage plots (Filename.concat plot_dir
			     ("binary_csps_" ^ (filize key) ^ ".ps"))
  
      

(* EOF *)
