(** The main program for the 2-way number partitioning binary.

    @author eaburns
    @since 2009-12-30
*)

open Big_int

let parse_args () =
  (** [parse_args ()] parses the arguments and results in a tuple of
      their values. *)
  let kind = ref "ckk"
  and limit = ref [Info.Never]
  and verb = ref 2
  and others = ref []
  in
    Arg.parse
      [
	"-k", Arg.Set_string kind, "The kind name (default: ckk)";
	"--kind", Arg.Set_string kind, "The kind name (default: ckk)";

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
    !kind, !limit, !verb, Array.of_list (List.rev !others)


let solution_cost (one, two) =
  (** [solution_cost p] finds the cost of the solution. *)
  let one_sum = List.fold_left add_big_int zero_big_int one
  and two_sum = List.fold_left add_big_int zero_big_int two in
    abs_big_int (sub_big_int one_sum two_sum)


let output res =
  (** [output res] outputs the result. *)
  let (sol, nodes, leaves, branches, prunes, optimal, complete), t = res in
    begin match sol with
      | None -> Datafile.write_pairs stdout ["found solution", "no"]
      | Some p ->
	  let diff = solution_cost p in
	    Datafile.write_pairs stdout ["found solution", "yes"];
	    Datafile.write_pairs stdout ["final sol cost",
					 string_of_big_int diff; ]
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
    (fun i p ->
       (** [log_incumbent i p] logs an incumbent solution. *)
       Datafile.write_alt_row_prefix stdout "incumbent";
       (* cost, nodes, leaves, branches, time. *)
       Verb.pr Verb.always "%s\t%d\t%d\t%d\t%f\n"
	 (string_of_big_int (solution_cost p))
	 i.Info.nodes i.Info.branches i.Info.leaves
	 ((Sys.time ()) -. time))


let run_ckk args limit inst =
  (** [run_ckk alg limit ints] runs the algorithm on the
      instance using CKK. *)
  let alg = Two_partition_algs.by_args args in
  let log_incumbent = make_incumbent_logger () in
  let logger i n =
    log_incumbent i (Two_partition_ckk.partitions n)
  in
    output (Wrsys.with_time
	      (fun () ->
		 alg Two_partition_ckk.make_interface inst limit logger))


let run_ckk_list args limit inst =
  (** [run_ckk_list alg limit ints] runs the algorithm on the instance
      using CKK. *)
  let alg = Two_partition_algs.by_args args in
  let log_incumbent = make_incumbent_logger () in
  let logger i n =
    log_incumbent i (Two_partition_ckk_list.partitions n)
  in
    output (Wrsys.with_time
	      (fun () ->
		 alg Two_partition_ckk_list.make_interface inst limit logger))


let run_ckk_inplace args limit inst =
  (** [run_ckk_inplace alg limit ints] runs the algorithm on the
      instance using CKK with inplace modification. *)
  let alg = Two_partition_algs.by_args args in

  (* This is a little bit of a workaround because the logger takes
     'node types but the solution cost calculation needs 'saved
     types. *)
  let module I = Bounded_depth_interface in
  let init, inter, partitions =
    Two_partition_ckk_inplace.make_interface inst
  in
  let make_interface _ = init, inter, partitions in
  let log_incumbent = make_incumbent_logger () in
  let logger i n = log_incumbent i (partitions (inter.I.copy_state n)) in
    output (Wrsys.with_time
	      (fun () -> alg make_interface inst limit logger))


let run_greedy args limit inst =
  (** [run_greedy alg limit ints] runs the algorithm on the
      instance using the greedy approach. *)
  let alg = Two_partition_algs.by_args args in
  let log_incumbent = make_incumbent_logger () in
  let logger i n =
    log_incumbent i (Two_partition_greedy.make_partitions inst n)
  in
    output (Wrsys.with_time
	      (fun () ->
		 alg Two_partition_greedy.make_interface inst limit logger))


let run_greedy_inplace args limit inst =
  (** [run_greedy_inplace alg limit ints] runs the algorithm on the
      instance using the greedy approach. *)
  let alg = Two_partition_algs.by_args args in
  let module I = Bounded_depth_interface in
  let init, inter, partitions =
    Two_partition_greedy_inplace.make_interface inst
  in
  let make_interface _ = init, inter, partitions in
  let log_incumbent = make_incumbent_logger () in
  let logger i n = log_incumbent i (partitions (inter.I.copy_state n)) in
    output (Wrsys.with_time (fun () -> alg make_interface inst limit logger))


let main () =
  (** [main ()] is the main function. *)
  let kind_name, limit, verb, args = parse_args () in
  let inst = Two_partition_instance.read stdin
  in Verb.with_level verb
       (fun () ->
	  match kind_name with
	    | "ckk" -> run_ckk args limit inst
	    | "ckk_list" -> run_ckk_list args limit inst
	    | "ckk_inplace" -> run_ckk_inplace args limit inst
	    | "greedy" -> run_greedy args limit inst
	    | "greedy_inplace" -> run_greedy_inplace args limit inst
	    | _ -> invalid_arg ("kinds are: ckk, ckk_list, ckk_inplace, "
				^ "greedy and greedy_inplace"))


let _ = main ()
