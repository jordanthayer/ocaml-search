(** The main program for the pancake puzzle.

    @author eaburns
    @since 2009-12-03
*)


let run_alg alg b limit =
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
		     | Some (p, f) -> f, Pancake.sol_length p) in
    let trail_pairs = ["final sol cost", string_of_float cost;
		       "final sol length", string_of_int len;
		       "total raw cpu time", string_of_float t;
		       "total nodes expanded", string_of_int e;
		       "total nodes generated", string_of_int g] @
      (Limit.to_trailpairs limit) in
      Datafile.write_pairs stdout trail_pairs


let select_interface iface pdb_h pdb_d alg =
  match iface with
    | "" ->
	(match pdb_h, pdb_d with
	   | Some pdb_h, Some pdb_d ->
	       begin match alg with
		 | _ ->
		     Verb.pr Verb.optional "Using PDBs\n%!";
		     Pancake_interfaces.pdb_interface pdb_h pdb_d
	       end
	   | None, None ->
	       begin match alg with
		 | _ ->
		     Verb.pr Verb.optional "Using h_gap\n%!";
		     Pancake_interfaces.hgap_interface
	       end
	   | _ -> invalid_arg "Must have both h and d PDBs")
    | "pdb_exhaust" ->
	(match pdb_h, pdb_d with
	   | Some ph, Some pd -> Pancake_interfaces.exhaust_pdb ph pd
	   | _ ->  failwith "Tried to use exhaust pdb with no pdbs!")
    | "hgap_exhaust" -> Pancake_interfaces.hgap_interface
    | _ -> invalid_arg "iface not recognized"


let with_path6 (s, e, g, p, m, d) =
  (match s with None -> None | Some (n, f) -> Some (n, f)), e, g, p, m, d


let get_alg interface name =
  let sh = with_path6 in
  let arg_count, alg_call = (Wrlist.get_entry
			       (Alg_table_dups.table @ Alg_table_dd.table)
			       "alg" name) in
    arg_count, (Alg_initializers.string_array_init alg_call sh interface)


let set_up_alg iface pdb_h pdb_d cost_name inst args =
  (** Looks at command line.  returns alg_func (which is world -> node
      option * int * int) and list of string pairs for logging
      parameters. *)
  let n = Array.length args in
    if n < 1 then
      (Wrutils.pr "Expects a board on stdin, writes to stdout.\n";
       Wrutils.pr "First arg must be an algorithm name (such as \"a_star\")\n";
       failwith "not enough command line arguments")
    else
      let alg_name = args.(1) in
      let interface = select_interface iface pdb_h pdb_d alg_name cost_name in
      let count, initer = get_alg interface alg_name in
	if n <> (1 + count) then
	  failwith
	    (Wrutils.str "%s takes %d arguments after alg name"
	       alg_name (count - 1));
	let args = Array.sub args 1 count in
	  initer args


let parse_non_alg_args () =
  let v = ref 2
  and limit = ref [Limit.Never]
  and cost = ref "unit"
  and make_pdb = ref false
  and pdb_size = ref ~-1
  and iface = ref ""
  and others = ref ["pad"] in
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

	"--max_mem", Arg.Unit (fun () -> limit := (Limit.MachineMemory)::!limit),
	"Dynamic Memory limit";

	"--cost", Arg.Set_string cost,
	"The cost function name (default=unit)";

	"--make-pdb", Arg.Set make_pdb,
	"If passed, then the PDB is created if it doesn't exist.";

	"--pdb-size", Arg.Set_int pdb_size,
	"Specify the size of the PDB";

	"--iface", Arg.Set_string iface, "Specify an interface to use";

	"--", Arg.Rest (fun str ->
			  let vals = Str.split (Str.regexp " ") str in
			    List.iter (fun s -> Wrutils.push s others)
			      vals),
	"Everything after this is interpreted as an argument to the solver";
      ]
      (fun s -> Wrutils.push s others) "";
    !v, !limit, !cost, !make_pdb, !pdb_size, !iface,
  Array.of_list (List.rev !others)


let consider_pdbs ~ncakes make_pdb ~pdb_size cost_name =
  (** [consider_pdbs ~ncakes make_pdb ~pdb_size cost_name] possibly
      loads PDBs if the PDB size was specified. *)
  let load = Pancake.PDB.load in
    if pdb_size > 0
    then
      let pdb_h = load ~should_make:make_pdb ncakes cost_name pdb_size in
      let pdb_d =
	if cost_name = "unit"
	then pdb_h
	else load ~should_make:make_pdb ncakes "unit" pdb_size
      in Some pdb_h, Some pdb_d
    else None, None


let main () =
  (** alg from command line, board from stdin, results to stdout *)
  let v, limit, cost_name, make_pdb, pdb_size, iface,
    args = parse_non_alg_args () in
    Verb.with_level v
      (fun () ->
	 let inst = Pancake.read stdin in
	 let ncakes = Pancake.npancakes inst in
	 let pdb_h, pdb_d =
	   consider_pdbs ~ncakes make_pdb ~pdb_size cost_name
	 in
	 let alg = set_up_alg iface pdb_h pdb_d cost_name inst args in
	   run_alg alg inst limit)


let _ = main ()

(* EOF *)
