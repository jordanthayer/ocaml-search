let run_alg ?(lim = [Limit.Never]) alg prob =
  (** writes results to stdout. assumes that header stuff like problem
      attrs are already written.  This is the main function for the
      stand-alone solver. *)
  let do_run = alg prob lim in
  let (n, e, g, p, q, d),t = Wrsys.with_time do_run in
  let s = Wrsys.get_proc_status [| "VmPeak:" |] (Unix.getpid ()) in
    match n with
	None ->
	  (let trail_pairs = ["found solution", "no";
			      "final sol cost", "infinity";
			      "number of duplicates found", string_of_int d;
			      "peak virtual mem usage kb", s.(0);
			      "total raw cpu time", string_of_float t;
			      "total nodes expanded", string_of_int e;
			      "total nodes generated", string_of_int g] @
	     (Limit.to_trailpairs lim) in
	     Datafile.write_pairs stdout trail_pairs)
      | _ -> (* The solution was found and thus already printed *)
	  (let sol,cost = match n with
	     | None -> failwith "no solution!"
	     | Some (node, cost) -> node, cost
	   in
	   let trail_pairs = ["found solution", "yes";
			      "final sol cost", string_of_float cost;
			      "number of duplicates found", string_of_int d;
			      "peak virtual mem usage kb", s.(0);
			      "total raw cpu time", string_of_float t;
			      "total nodes expanded", string_of_int e;
			      "total nodes generated", string_of_int g] @
	     (Limit.to_trailpairs lim) in
	     Datafile.write_pairs stdout trail_pairs)



let select_interface ?(iface = "") algname =
  match iface with
    | "default" -> Dock_interfaces.default_interface
    | "deepest" -> Dock_interfaces.deepest_interface
    | "sum_deep" -> Dock_interfaces.sum_deepest_interface
    | "dump" -> Dock_interfaces.dump_interface
    | _ -> failwith (Printf.sprintf "|%s| not recognized interface" iface)


let get_alg ?(iface = "") name =
  let sh = (fun (a,b,c,d,e,f) -> (a,b,c,d,e,f))
  and ib = select_interface ~iface name
  and arg_count, alg_call = (Wrlist.get_entry
			       (Alg_table_dups.table @ Alg_table_dd.table)
			       "alg" name) in
    arg_count, (Alg_initializers.string_array_init alg_call sh ib)


let set_up_alg alg_get args =
  (** looks at command line.  returns alg_func (which is world -> node
    option * int * int) and list of string pairs for logging parameters. *)
  let n = Array.length args in
    if n < 1 then
      (Wrutils.pr "expects a problem on stdin, writes log info to stdout.\n";
       Wrutils.pr "First arg must be an algorithm name (such as \"a_star\"),\n";
       Wrutils.pr "other args depend on alg (eg, weight for wted_a_star).\n";
       failwith "not enough command line arguments")
    else
      let alg_name = args.(1) in
      let count, initer = alg_get alg_name in
	if n <> (count + 1) then
	  failwith (Wrutils.str "%s takes %d arguments after alg name"
		      alg_name (count - 1));
	let args = Array.sub args 1 count in
	initer args

let parse_non_alg_args () =
  let v = ref 2
  and limits = ref []
  and cost = ref "unit"
  and moves = ref ""
  and iface = ref "default"
  and others = ref ["pad"] in
    Arg.parse
      ([
	 "-v", Arg.Set_int v,
	 "verbosity setting (between 1=nothing and 5=everything, default 2)";
	 "--iface", Arg.Set_string iface, "Picks an interface from list";
       ] @ (Limit.arg_specs limits))
      (fun s -> Wrutils.push s others) "";
    !v, !limits, !cost, !moves, !iface, Array.of_list (List.rev !others)


let main () =
  (** alg from command line, instance from stdin, results to stdout *)
  let v, limit, cost_name, move_name, iface, args = parse_non_alg_args () in
  let a = set_up_alg (get_alg ~iface) args
  and p = Dock_robot_instance.read stdin in
    Verb.with_level v (fun () -> run_alg ~lim:limit a p)


let _ = main ()


(* EOF *)
