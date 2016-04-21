(** The main function for the sliding tiles domain.

    @author eaburns
    @since 2010-09-01
*)

open Printf

module STI = Sliding_tiles_interfaces

let verb = ref Verb.always
let limit = ref []
let cost = ref "unit"
let macro = ref false
let heuristic = ref "manhattan"
let moves = ref "standard"
let iface = ref ""
let add = ref false
let other_args = ref [ "pad... apparently" ]
let glued = ref false
let tile_weight = ref 0

let run_algorithm ?(limit=[]) alg inst =
  let (sol_opt, e, g, p, q, d), t = Wrsys.with_time (alg inst limit) in
    begin match sol_opt with
      | None ->
	  Datafile.write_pairs stdout
	    [ "found solution", "no";
	      "final sol cost", "infinity";
	      "final sol length", "-1"; ]
      | Some (goal_node, cost) ->
	  let path = Sliding_tiles.unroll_path inst goal_node in
	    Datafile.write_pairs stdout
	      [ "found solution", "yes";
		"final sol cost", string_of_float cost;
		"final sol length", string_of_int (List.length path); ]
    end;
    let mem = Wrsys.get_proc_status [| "VmPeak:" |] (Unix.getpid ()) in
      Datafile.write_pairs stdout
	([ "total raw cpu time", string_of_float t;
	   "peak virtual mem usage kb", mem.(0);
	   "number of duplicates found", string_of_int d;
	   "total nodes expanded", string_of_int e;
	   "total nodes generated", string_of_int g; ]
	 @ (Limit.to_trailpairs limit))



let get_algorithm () =
  (** [algorithm_by_name ()] gets the algorithm (arg count and
      function) by its name. *)
  let args = Array.of_list !other_args in
  let alg_name = args.(1) in
  let make_iface =
    match !iface with
      | "recording" -> STI.recording_interface !cost
      | "record_truth" -> STI.record_truth_interface !cost
      | "exhaustive" -> STI.exhaustive_recording_interface !cost
      | _ ->
	  (if !macro || (!moves = "macro")
	   then (match !heuristic with
		   | "manhattan" -> Macro_sliding_tiles.default_interface
		   | str ->
		       Sliding_pdb_interfaces.get_pdb_iface !add !cost
			 "macro" str)
	   else if (!glued) then
	     (match !heuristic with
		  "manhattan" -> Glued_interfaces.default_interface !cost
		| bad_h -> failwith "glued interfaces only work with manhattan")
	   else (match !heuristic with
		   | "manhattan" -> STI.default_interface
		       ~cost_ratio:!tile_weight !cost
		   | "misplaced" -> STI.misplaced !cost
		   | "linear_conflicts" -> STI.linear_conflicts_interface !cost
		   | "mult_lc" ->
		       Sliding_inadmiss_interfaces.mult_lc_iface !cost
		   | str -> (* We assume it's a pdb now *)
		       Sliding_pdb_interfaces.get_pdb_iface !add !cost
			 "standard" str)) in
  let sh = Fn.identity (* replace this if we want validation in the future *)
  and argc, alg_fun =
    Wrlist.get_entry (Alg_table_dups.table @ Alg_table_dd.table) "alg" alg_name
  in
  let num_args = Array.length args in
    if num_args <> (argc + 1)
    then failwith (sprintf "Expected %d arguments, got %d"
		     (argc - 1) (num_args - 2));
    let alg_args = if argc > 1 then Array.sub args 1 argc else [| |] in
      Alg_initializers.string_array_init alg_fun sh make_iface alg_args


let parse_args () =
  (** [parse_args ()] parses the arguments. *)
  let arg_spec =
    [
      "-v", Arg.Set_int verb, (sprintf "Verbosity level (default: %d)" !verb);
      "-c", Arg.Set_string cost, (sprintf "Cost function (default: %s)" !cost);
      "--cost", Arg.Set_string cost, (sprintf "Cost function (default: %s)"
					!cost);
      "--tile_weight", Arg.Set_int tile_weight, (sprintf "Weight for the tiles");
      "--moves", Arg.Set_string moves, (sprintf "Cost function (default: %s)"
					  !moves);
      "-m", Arg.Set macro, "Use macro moves";
      "--glued", Arg.Set glued, "Load and respect glued tiles";
      "--heuristic", Arg.Set_string heuristic,
      (sprintf "Heuristic function (default: %s)" !heuristic);
      "--add", Arg.Set add, "Use Additive PDB's";
      "--iface", Arg.Set_string iface, "Force a parcticular search interface";

      "--", Arg.Rest (fun str ->
			let vals = Str.split (Str.regexp " ") str in
			  List.iter (fun s -> Wrutils.push s other_args)
			    vals),
      "Everything after this is interpreted as an argument to the solver";

    ] @ (Limit.arg_specs limit)
  in
  let usage_str =
    "sliding-tiles [-v <verb>] [-c <cost>] " ^ Limit.usage_string
  in
    Arg.parse arg_spec (fun s -> other_args := s :: !other_args) usage_str;
    other_args := List.rev !other_args


let main () =
  parse_args ();
  if !macro && !cost <> "unit"
  then failwith "Macro tiles only supports unit cost";
  Verb.with_level !verb
    (fun () ->
       let alg = get_algorithm () in
       let inst = Sliding_tiles_inst.read ~read_glued:!glued stdin in
	 run_algorithm ~limit:!limit alg inst)


let _ = main ()
