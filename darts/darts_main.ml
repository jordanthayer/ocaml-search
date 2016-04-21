(** The main program for the darts solver.

    @author eaburns
    @since 2010-04-08
*)

let parse_args () =
  (** [parse_args ()] parses the arguments and results in a tuple of
      their values. *)
  let limit = ref Info.Never
  and n_darts = ref 0
  and n_regions = ref 0
  and verb = ref 2
  and others = ref []
  in
    Arg.parse
      [
	"--darts", Arg.Set_int n_darts , "The number of darts";
	"-d", Arg.Set_int n_darts , "The number of darts";
	"--regions", Arg.Set_int n_regions , "The number of regions";
	"-r", Arg.Set_int n_regions , "The number of regions";

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
    !n_darts, !n_regions, !limit, !verb, Array.of_list (List.rev !others)


let output res =
  (** [output res] outputs the result. *)
  let (sol, (nodes, leaves, branches, prunes), optimal, complete), t = res in
    begin match sol with
      | None ->
	  Datafile.write_pairs stdout ["found solution", "no";
				       "final sol cost", "-1"; ]
      | Some s ->
	  Datafile.write_pairs stdout ["found solution", "yes"];
	  Datafile.write_pairs stdout ["final sol cost",
				       string_of_int
					 (s.Darts.min_unattainable); ];
	  Datafile.write_pairs stdout ["final regions",
				       Darts.get_regions s; ];
    end;
    Datafile.write_pairs stdout ["total raw cpu time", string_of_float t;
				 "total nodes expanded", string_of_int nodes;
				 "total leaves seen", string_of_int leaves;
				 "total branches seen", string_of_int branches;
				 "optimal", string_of_bool optimal;
				 "completed", string_of_bool complete;
				]


let make_incumbent_logger () =
  (** [make_incumbent_logger ()] makes a logger that records the
      incumbent solutions. *)
  let time = Sys.time () in
    Datafile.write_alt_colnames stdout "incumbent" ["sol cost";
						    "nodes expanded";
						    "branches seen";
						    "leaves seen";
						    "raw cpu time"; ];
    (fun i s ->
       (** [log_incumbent i s] logs an incumbent solution. *)
       Datafile.write_alt_row_prefix stdout "incumbent";
       (* cost, nodes, leaves, branches, time. *)
       Printf.printf "%d\t%d\t%d\t%d\t%f\n"
	 s.Darts.min_unattainable i.Info.nodes i.Info.branches i.Info.leaves
	 ((Sys.time ()) -. time))


let output_limit = function
  | Info.Time s ->
      Datafile.write_pairs stdout ["time limit", string_of_float s ]
  | Info.Expanded s ->
      Datafile.write_pairs stdout ["expansion limit", string_of_int s ]
  | Info.Leaves s ->
      Datafile.write_pairs stdout ["leaf limit", string_of_int s ]
  | Info.Branches s ->
      Datafile.write_pairs stdout ["branch limit", string_of_int s ]
  | _ -> ()


let run args limit inst () =
  (** [run args limit inst ()] runs the algorithm on the instance. *)
  let alg = Darts_algs.by_args args in
  let logger = make_incumbent_logger () in
    output_limit limit;
    output (Wrsys.with_time (fun () -> alg inst [limit] logger))


let main () =
  (** [main ()] is the main function. *)
  let n_darts, n_regions, limit, verb, args = parse_args () in
  let inst = { Darts.n_darts = n_darts;
	       Darts.n_regions = n_regions;
	     }
  in
    if n_darts = 0 then invalid_arg "No darts specified";
    if n_regions = 0 then invalid_arg "No regions specified";
    Verb.with_level verb (run args limit inst)


let _ = main ()
