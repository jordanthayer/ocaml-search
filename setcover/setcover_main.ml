(** The main program for weighted setcover.

    @author eaburns
    @since 2010-01-05
*)

let parse_args () =
  (** [parse_args ()] parses the arguments and results in a tuple of
      their values. *)
  let limit = ref Info.Never
  and verb = ref 2
  and others = ref []
  in
    Arg.parse
      [
	"--time",
	Arg.String (fun s -> limit := Info.Time (float_of_string s)),
	"Use a time limit";

	"--nodes",
	Arg.String (fun s -> limit := Info.Expanded (int_of_string s)),
	"Use a node limit";

	"--leaves",
	Arg.String (fun s -> limit := Info.Leaves (int_of_string s)),
	"Use a leaf limit";

	"--branches",
	Arg.String (fun s -> limit := Info.Branches (int_of_string s)),
	"Use a branch limit";

	"-v", Arg.Set_int verb, "Verbosity (default: 2)";
	"--verbose", Arg.Set_int verb, "Verbosity (default: 2)";
      ]
      (fun s -> Wrutils.push s others)
      "<algorithm> [<param>...]";
    !limit, !verb, Array.of_list (List.rev !others)


let output leaf_cost res =
  (** [output leaf_cost res] outputs the result. *)
  let (sol, nodes, leaves, branches, prunes, optimal, complete), t = res in
    begin match sol with
      | None -> Datafile.write_pairs stdout ["found solution", "no"]
      | Some p ->
(*
	  Verb.pr Verb.toplvl "Solution: %s\n"
	    (Setcover.string_of_node p);
*)
	  let diff = leaf_cost p in
	    Datafile.write_pairs stdout ["found solution", "yes"];
	    Datafile.write_pairs stdout ["final sol cost",
					 string_of_float diff; ]
    end;
    Datafile.write_pairs stdout ["total raw cpu time", string_of_float t;
				 "total nodes expanded", string_of_int nodes;
				 "total leaves seen", string_of_int leaves;
				 "total branches seen", string_of_int branches;
				 "optimal", string_of_bool optimal;
				 "completed", string_of_bool complete;
				]


let make_incumbent_logger leaf_cost =
  (** [make_incumbent_logger leaf_cost] makes a logger that records
      the incumbent solutions. *)
  let time = Sys.time () in
    Datafile.write_alt_colnames stdout "incumbent" ["sol cost";
						    "nodes expanded";
						    "branches seen";
						    "leaves seen";
						    "raw cpu time"; ];
    (fun i p ->
       (** [log_incumbent i p] logs an incumbent solution. *)
       Datafile.write_alt_row_prefix stdout "incumbent";
       (* cost, nodes, leaves, branches, time. *)
       Verb.pr Verb.always "%f\t%d\t%d\t%d\t%f\n"
	 (leaf_cost p) i.Info.nodes i.Info.branches i.Info.leaves
	 ((Sys.time ()) -. time))


let run args limit inst =
  (** [run alg limit ints] runs the algorithm on the
      instance using TSP. *)
  let module I = Bounded_depth_interface in
  let alg = Setcover_algs.by_args args in
  let init, inter = Setcover.make_interface inst in
  let make_interface _ = init, inter in
  let log_incumbent = make_incumbent_logger inter.I.leaf_cost in
    output
      inter.I.leaf_cost
      (Wrsys.with_time (fun () -> alg make_interface inst limit log_incumbent))


let main () =
  (** [main ()] is the main function. *)
  let limit, verb, args = parse_args () in
  let inst = Setcover_problem.read stdin
  in Verb.with_level verb (fun () -> run args [limit] inst)


let _ = main ()
