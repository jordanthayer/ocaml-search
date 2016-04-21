(**
   main program for the hanoi solver.

   Christopher Wilt
   November 13, 2010
*)


let run_alg get_len alg b limit =
  (** writes results to stdout. assumes that header stuff like problem
    attrs are already written.  This is the main function for the stand-alone
    solver. *)
  let do_run = alg b limit in
  let (s, e, g, p, m, d), t = Wrsys.with_time do_run in
    (match s with
	 None -> (Datafile.write_pairs stdout ["found solution", "no"];)
       | Some (p,f) -> Datafile.write_pairs stdout ["found solution", "yes"]);
  let cost, len = (match s with
		       None -> infinity, 0
		     | Some (p, f) -> f, get_len p) in
    let trail_pairs = ["final sol cost", string_of_float cost;
		       "final sol length", string_of_int len;
		       "total raw cpu time", string_of_float t;
		       "total nodes expanded", string_of_int e;
		       "total nodes generated", string_of_int g] @
      (Limit.to_trailpairs limit) in
      Datafile.write_pairs stdout trail_pairs


let with_path6 (s, e, g, p, m, d) =
  (match s with None -> None | Some (n, f) -> Some (n, f)), e, g, p, m, d


let get_alg interface name =
  let sh = with_path6 in
  let arg_count, alg_call = (Wrlist.get_entry
			       (Alg_table_dups.table @ Alg_table_dd.table)
			       "alg" name) in
    arg_count, (Alg_initializers.string_array_init alg_call sh interface)


let parse_non_alg_args () =
  let v = ref 2
  and limit = ref [Limit.Never]
  and pdb_down = ref (-1)
  and pdb_up = ref (-1)
  and cost = ref "Unit"
  and others = ref []
  and old_pdb = ref false
  and bp_pdb = ref false 
  and bp_bottom = ref false 
  in 

  let parse_memory_limit mls = 
    match mls with 
	"max" -> limit := (Limit.MachineMemory)::!limit
      | bad_string -> failwith 
	  (Printf.sprintf "\"%s\" is an invalid memory limit" bad_string) in

    Arg.parse
      [ "-v", Arg.Set_int v,
	"Verbosity setting (between 1=nothing and 5=everything, default 2)";

	"--wall", Arg.Float
	  (fun l -> limit := (Limit.WallTime l)::!limit),
	"wall time limit (in seconds, default no limit)";

	"--time", Arg.Float (fun l -> limit := (Limit.Time l)::!limit),
	"search time limit (in seconds, default no limit)";

	"--gen", Arg.Int (fun l -> limit := (Limit.Generated l)::!limit),
	"search generation limit";

	"--exp", Arg.Int (fun l -> limit := (Limit.Expanded l)::!limit),
	"search expansion limit";

	"--memory", Arg.String parse_memory_limit,
	"specifies how much memory to use";

	"--max_mem", Arg.Unit (fun () -> limit := (Limit.MachineMemory)::!limit),
	"quit when main memory is exhausted";

	"--pdb-down", Arg.Set_int pdb_down,
	"Specify how many disks to abstract going down";

	"--pdb-up", Arg.Set_int pdb_up,
	"Specify how many disks to abstract going up";

	"--cost", Arg.Set_string cost,
	"Specify what cost function to use";

	"--old_pdb", Arg.Set old_pdb, 
	"tells the solver to use old style pattern databases";

	"--bp_pdb", Arg.Set bp_pdb, 
	"tells the solver to use bit packed pattern databases";

	"--bp_bottom", Arg.Set bp_bottom, 
	"tells the solver to use stacked bit packed pattern databases";

	  "--", Arg.Rest (fun str ->
			    let vals = Str.split (Str.regexp " ") str in
			      List.iter (fun s -> Wrutils.push s others)
				vals),
	"Everything after this is interpreted as an argument to the solver";
      ]
      (fun s -> Wrutils.push s others) "";
    !v, !limit, !cost, !pdb_down, !pdb_up, !old_pdb, !bp_pdb,
  !bp_bottom, 
  Array.of_list (List.rev !others)



let set_up_alg 
    iface
    inst args =
  (** Looks at command line.  returns alg_func (which is world -> node
      option * int * int) and list of string pairs for logging
      parameters. *)


  let n = Array.length args in
    if n < 1 then
      (Wrutils.pr "Expects a board on stdin, writes to stdout.\n";
       Wrutils.pr "First arg must be an algorithm name (such as \"a_star\")\n";
       failwith "not enough command line arguments")
    else
      (*
	Printf.fprintf stderr "there are %d arguments\n" (Array.length
	args);

	Wrarray.fprint_array stderr Fn.identity " " args;
      *)
      let alg_name = args.(0) in

      let count, initer = get_alg iface alg_name in
	if n <> (count) then
	  (
	    (Printf.fprintf stderr "n is %d count is %d\n" n count);
	    failwith
	      (Wrutils.str "%s takes %d arguments after alg name"
		 alg_name (count - 1)));

	  initer args


let main () =
  (** alg from command line, board from stdin, results to stdout *)


  let v, limit, cost_name, pdb_down, pdb_up, old_pdb, bp_pdb, 
    bp_bottom, args = 
    parse_non_alg_args () in
    Verb.with_level v
      (fun () ->
	 let inst = Hanoi.read stdin in

	   assert(pdb_down >= 0);
	   assert(pdb_down <= (Hanoi.n_disks inst));
	   assert(pdb_up >= 0);
	   assert(pdb_up <= (Hanoi.n_disks inst));
	   
	   let abstractions = [|
	     (Hanoi_interfaces.make_abs Hanoi_interfaces.Low 
		(Hanoi.n_disks inst)
		pdb_down);
	     (Hanoi_interfaces.make_abs Hanoi_interfaces.High 
		(Hanoi.n_disks inst)
		pdb_up);
	   |] in


	     if(old_pdb) then 
	       (
		 let iface =  
		   Hanoi_interfaces.pdb_interface abstractions
		     cost_name in
		 let alg = set_up_alg iface inst args in
		   run_alg Hanoi.sol_length alg inst limit
	       ) 
	     else if (bp_pdb) then
	       (
		 let iface = Hanoi_interfaces.bp_pdb_interface 
		   pdb_up 
		   pdb_down 
		   cost_name in
		 let alg = set_up_alg iface inst args in
		   run_alg Min_hanoi.sol_length alg inst limit
	       )
	     else if (bp_bottom) then
	       (
		 let iface = Hanoi_interfaces.bp_pdb_stacked_interface 
		   pdb_up 
		   pdb_down 
		   cost_name in
		 let alg = set_up_alg iface inst args in
		   run_alg Min_hanoi.sol_length alg inst limit
	       )
	     else
	       (
		 let iface = Hanoi_interfaces.new_pdb_interface 
		   pdb_up 
		   pdb_down 
		   cost_name in
		 let alg = set_up_alg iface inst args in
		   run_alg Min_hanoi.sol_length alg inst limit
	       )
      )


let _ = main ()
