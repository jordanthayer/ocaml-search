(**

    @author eaburns
    @since 2011-01-10
*)

open Knapsack_bounded_depth

let parse_args () =
  (** [parse_args ()] parses the arguments and results in a tuple of
      their values. *)
  let limit = ref []
  and verb = ref 2
  and others = ref []
  in
    Arg.parse
      [
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
    !limit, !verb, Array.of_list (List.rev !others)


let print_solution inst node =
  let sorted = inst.sorted in
  let values = inst.values in
  let take = ref [] in
  let leave = ref [] in
  let vl = ref 0. in
  let rec do_sep i items =
    if i >= 0 then
      match items with
	| Take :: rest ->
	    let item = sorted.(i) in
	      take := item :: !take;
	      vl := !vl +. values.(item);
	      do_sep (i - 1) rest
	| Leave :: rest ->
	    leave := sorted.(i) :: !leave;
	    do_sep (i - 1) rest
	| [] -> ()
  in
    do_sep (inst.nitems - 1) node.items;
    Printf.printf "computed value: %g\n" !vl;
    Printf.printf "Left Behind:";
    List.iter (Printf.printf " %d") !leave;
    Printf.printf "\nTake:";
    List.iter (Printf.printf " %d") !take;
    Printf.printf "\n"


let output inst leaf_cost res =
  (** [output leaf_cost res] outputs the result. *)
  let (sol, (nodes, leaves, branches, prunes), optimal, complete), t = res in
    begin match sol with
      | None -> Datafile.write_pairs stdout [ "found solution", "no";
					      "final sol cost", "infinity";
					    ]
      | Some node ->
	  let c = leaf_cost node in
	    if Verb.level Verb.debug then print_solution inst node;
	    Datafile.write_pairs stdout ["found solution", "yes"];
	    Datafile.write_pairs stdout ["final sol cost",
					 string_of_float c; ]
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
       Verb.pr Verb.always "%f\t%d\t%d\t%d\t%f\n"
	 (leaf_cost p) i.Info.nodes i.Info.leaves i.Info.branches
	 ((Sys.time ()) -. time))


(** Run the algorithm on the given instance. *)
let run_solver args limit inst =
  Array.iter (fun id ->
		Verb.pr Verb.debug "id=%d, density=%g, value=%g, weight=%g\n"
		  id inst.densities.(id) inst.values.(id) inst.weights.(id))
    inst.sorted;
  Verb.pr Verb.debug "allowance=%g\n" inst.allowance;
  let module I = Bounded_depth_interface in
  let alg = Knapsack_bounded_depth_algs.by_args args in
  let log_incumbent = make_incumbent_logger () in
    output inst leaf_cost
      (Wrsys.with_time (fun () -> alg inst limit log_incumbent))


let main () =
  (** [main ()] is the main function. *)
  let limit, verb, args = parse_args () in
  let inst = Knapsack_bounded_depth.read "<stdin>" stdin in
    Verb.with_level verb (fun () -> run_solver args limit inst)


let _ = main ()
