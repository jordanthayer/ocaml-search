(* $Id: main.ml,v 1.1 2004/01/13 23:45:14 ruml Exp ruml $

   running CSP algorithms and anlyzing the results.
   This file is intended to have problem-type-independent stuff.
*)


let alg_table = [ "dfs", Csp_tree_algs.dfs;
		  "ilds_top", Csp_tree_algs.ilds_top;
		  "ilds_bottom", Csp_tree_algs.ilds_bottom;
		  "dds", Csp_tree_algs.dds;
		  "indec_sum", Csp_tree_algs.indecision_sum;
		  "indec_max", Csp_tree_algs.indecision_max;
		  "blfs_sep", Csp_tree_algs.blfs_sep;
		  "blfs_quad", Csp_tree_algs.blfs_quad;
		]


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
	(lazy (Wrutils.pr "Solver finished in %f seconds.\n" time;
	       Wrutils.pr "  (that's about %d nodes/second)\n"
		 (Math.round ((float_of_int stats.Info.nodes) /. time));
	       Info.print_results stdout stats opt complete;
	       if opt then
		 Wrutils.pr "Solver claims SAT.\n"
	       else if complete then
		 Wrutils.pr "Solver claims UNSAT.\n"));
      Csp.check (Csp_tree.sol sol) csp opt;
      Verb.pr 3 "Solution checks out.\n";
      (* if opt, then SAT.  if not opt and complete, UNSAT. *)
      Wrutils.pr "%d\t%d\t%d\t%f\n"
	(if opt then 1 else if complete then 0 else -1)
	stats.Info.nodes stats.Info.leaves time;
      flush_all ()


let test alg i =
  Verb.with_level 3
    (fun () ->
       run_alg alg Info.Never (Csp_instances.load_csp (Wrutils.str "/tilde/ruml/projects/csp/data/instance/30/15/174/81/%d" i)))


(* for indec_max bug, run:
   "src/csp_solverdb -never -v 4 indec_max < data/instance/latin_square/0.3/21/10"
   from the command line *)


(**************** running a batch of experiments *************)


let solver = "/tilde/ruml/projects/csp/bin/csp_solver.linux"

let get_result alg halt csp_path =
  (** calls an external solver *)
  let s = Wrsys.shell_output (Wrutils.str "%s %s %s < %s"
				solver alg (Info.arg_string_from_limit halt)
				csp_path) in
    try
      Scanf.sscanf s "%d\t%d\t%d\t%f\n" (fun s n l t -> s,n,l,t)
    with Scanf.Scan_failure _ -> failwith ("bad solver output: " ^ s)


(****************** analyze data **************************)


let plot_dir = "/tilde/ruml/projects/csp/plots"




(* EOF *)
