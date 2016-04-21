(** The main program for the job shop scheduler.

    @author eaburns
    @since 2010-02-18
*)

(*
*)
module Job_shop = Jsp

let parse_args () =
  (** [parse_args ()] parses the arguments and results in a tuple of
      their values. *)
  let limit = ref []
  and verb = ref 2
  and others = ref []
  and deadline = ref max_int
  in
    Arg.parse
      [
	"--deadline", Arg.Set_int deadline, "deadline for the schedule";
	"-d", Arg.Set_int deadline, "deadline for the schedule";

	"--time",
	Arg.String (fun s -> limit := (Info.Time (float_of_string s))::!limit),
	"Use a time limit";

	"--nodes",
	Arg.String
	  (fun s -> limit := (Info.Expanded (int_of_string s))::!limit),
	"Use a node limit";

	"--leaves",
	Arg.String (fun s -> limit := (Info.Leaves (int_of_string s))::!limit),
	"Use a leaf limit";

	"--branches",
	Arg.String (fun s -> limit :=
		      (Info.Branches (int_of_string s))::!limit),
	"Use a branch limit";

	"-v", Arg.Set_int verb, "Verbosity (default: 2)";
	"--verbose", Arg.Set_int verb, "Verbosity (default: 2)";
      ]
      (fun s -> Wrutils.push s others)
      "<algorithm> [<param>...]";
    !limit, !verb, Array.of_list (List.rev !others), !deadline


let output leaf_cost limits res =
  (** [output leaf_cost limits res] outputs the result. *)
  let (sol, nodes, leaves, branches, prunes, optimal, complete), t = res in
    begin match sol with
      | None ->
	  Datafile.write_pairs stdout ["found solution", "no";
				       "final sol cost",
				       string_of_float infinity;]
      | Some p ->
	  (*
	    Verb.pr Verb.toplvl "Solution: %s\n"
	    (Job_shop.string_of_node p);
	  *)
	  Datafile.write_pairs stdout ["found solution", "yes"];
	  Datafile.write_pairs stdout ["final sol cost",
				       string_of_int p.Job_shop.make_span; ]
    end;
    Info.output_limits_to_dfs stdout limits;
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
    Datafile.write_alt_colnames stdout "incumbent"
      ["sol cost"; "nodes expanded"; "branches seen"; "leaves seen";
       "raw cpu time"; ];
    (fun i p ->
       (** [log_incumbent i p] logs an incumbent solution. *)
       Datafile.write_alt_row_prefix stdout "incumbent";
       (* cost, nodes, leaves, branches, time. *)
       Verb.pr Verb.always "%f\t%d\t%d\t%d\t%f\n"
	 (leaf_cost p) i.Info.nodes i.Info.branches i.Info.leaves
	 ((Sys.time ()) -. time))


let run_job_shop args limit inst deadline =
  (** [run_job_shop alg limit inst deadline] runs the algorithm on the
      instance. *)
  let module I = Bounded_depth_interface in
  let alg = Job_shop_algs.by_args args in
  let init, iface, st = Job_shop.default_interface inst deadline in
  let leaf_cost = iface.I.leaf_cost in
  let log_incumbent = make_incumbent_logger leaf_cost in
    output leaf_cost limit
      (Wrsys.with_time (fun () -> alg init iface st inst limit log_incumbent))


let main () =
  (** [main ()] is the main function. *)
  let limit, verb, args, deadline = parse_args () in
    Verb.with_level verb
      (fun () ->
	 let inst = Job_shop_instance.read stdin in
	 let ub = Job_shop_instance.make_span_ub inst in
	 let deadline = if deadline > ub then ub else deadline in
	   Verb.pr Verb.debug "deadline = %d\n%!" deadline;
	   run_job_shop args limit inst deadline)


let _ = main ()
