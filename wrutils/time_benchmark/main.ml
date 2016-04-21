(* $Id: main.ml,v 1.1 2004/01/13 23:45:14 ruml Exp ruml $

   main file for CSP solver
*)


let process_args () =
  let usage = "give alg and time limit.  expects csp on stdin"
  and alg = ref "dfs"
  and halt = ref Info.Never
  and verb = ref 1 in
    Arg.parse ["-nodes", Arg.Int (fun i -> halt := Info.Nodes i),
	       "search limit";
	       "-leaves", Arg.Int (fun i -> halt := Info.Leaves i),
	       "search limit";
	       "-secs", Arg.Float (fun t -> halt := Info.Time t),
	       "search limit";
	       "-v", Arg.Int (fun i -> verb := i),
	       "specifies the verbosity level (1-5)";
	      ]
      (fun a -> alg := a) usage;
    !alg, !halt, !verb


let main () =
  let alg, halt, verb = process_args () in
    Verb.with_level verb
      (fun () ->
	 Do_runs.run_alg alg halt (Csp_instances.read_csp stdin))
      
 
let _ = main ()
  

(* EOF *)
