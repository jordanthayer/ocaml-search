(** The main program for the scanalyzer domain.

    @author eaburns
    @since 2011-02-28
*)

open Printf

let verb = ref Verb.always
let limit = ref []
let other_args = ref [ "pad... apparently" ]

let run_algorithm ?(limit=[]) alg inst =
  let (sol_opt, e, g, p, q, d), t = Wrsys.with_time (alg inst limit) in
    begin match sol_opt with
      | None ->
	  Datafile.write_pairs stdout
	    [ "found solution", "no";
	      "final sol cost", "infinity";
	      "final sol length", "-1"; ]
      | Some (goal_node, cost) ->
	    Datafile.write_pairs stdout
	      [ "found solution", "yes";
		"final sol cost", string_of_float cost;
		(*"final sol length", string_of_int (List.length path);*) ]
    end;
    let mem = Wrsys.get_proc_status [| "VmPeak:" |] (Unix.getpid ()) in
      Datafile.write_pairs stdout
	([ "total raw cpu time", string_of_float t;
	   "peak virtual mem usage kb", mem.(0);
	   "number of duplicates found", string_of_int d;
	   "total nodes expanded", string_of_int e;
	   "total nodes generated", string_of_int g; ]
	 @ (Limit.to_trailpairs limit))

let parse_args () =
  (** [parse_args ()] parses the arguments. *)
  let arg_spec =
    [
      "-v", Arg.Set_int verb, (sprintf "Verbosity level (default: %d)" !verb);
    ] @ (Limit.arg_specs limit)
  in
  let usage_str =
    "sliding-tiles [-v <verb>] [-c <cost>] " ^ Limit.usage_string
  in
    Arg.parse arg_spec (fun s -> other_args := s :: !other_args) usage_str;
    other_args := List.rev !other_args

let get_algorithm () =
  (** [algorithm_by_name ()] gets the algorithm (arg count and
      function) by its name. *)
  let args = Array.of_list !other_args in
  let alg_name = args.(1) in
  let make_iface = Scanalyzer_interfaces.default_interface in
  let sol_handler = Fn.identity  in
  let argc, alg_fun =
    Wrlist.get_entry (Alg_table_dups.table @ Alg_table_dd.table)
      "alg" alg_name in
  let expt_nargs = argc - 1 and got_nargs = Array.length args - 2 in
    if expt_nargs <> got_nargs then
      failwith (sprintf "Expected %d arguments, got %d" expt_nargs got_nargs);
    let alg_args = if argc > 1 then Array.sub args 1 argc else [| |] in
      Alg_initializers.string_array_init alg_fun sol_handler make_iface
	alg_args

let main () =
  parse_args ();
  Verb.with_level !verb
    (fun () ->
       let alg = get_algorithm () in
       let inst = Scanalyzer_inst.read stdin in
	 run_algorithm ~limit:!limit alg inst)


let _ = main ()
