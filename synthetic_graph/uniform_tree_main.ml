(** The main program for running on uniform trees.

    @author eaburns
    @since 2010-08-02
*)

let run_alg ?(lim=[Limit.Never]) alg prob =
  (** [run_alg ?lim alg prob] actually runs the algorithm and builds
      the output datafile. *)
  let do_run = alg prob lim in
  let (n, e, g, p, q),t = Wrsys.with_time do_run in
    match n with
      | None ->
	  let trail_pairs = ["found solution", "no";
			     "final sol cost", "infinity";
			     "total raw cpu time", string_of_float t;
			     "total nodes expanded", string_of_int e;
			     "total nodes generated", string_of_int g] @
	    (Limit.to_trailpairs lim)
	  in Datafile.write_pairs stdout trail_pairs
      | Some (_, cost) ->
	  let trail_pairs = ["found solution", "yes";
			     "final sol cost", string_of_float cost;
			     "total raw cpu time", string_of_float t;
			     "total nodes expanded", string_of_int e;
			     "total nodes generated", string_of_int g] @
	    (Limit.to_trailpairs lim)
	  in Datafile.write_pairs stdout trail_pairs


let get_alg name =
  (** [get_alg name] gets the algorithm's search function based on its
      name. *)
  let iface = Uniform_tree.default_interface in
  let argc, alg_call = Wrlist.get_entry Alg_table_nodups.table "alg" name in
    argc, (Alg_initializers.string_array_init alg_call Fn.identity iface)


let setup_alg args =
  (** looks at command line.  returns alg_func (which is world -> node
      option * int * int) and list of string pairs for logging
      parameters. *)
  let n = Array.length args in
    if n < 1 then
      (Wrutils.pr "expects a problem on stdin, writes log info to stdout.\n";
       Wrutils.pr "First arg must be an algorithm name (such as \"a_star\"),\n";
       Wrutils.pr "other args depend on alg (eg, weight for wted_a_star).\n";
       failwith "not enough command line arguments")
    else
      let alg_name = args.(1) in
      let count, initer = get_alg alg_name in
	if n <> (count + 1) then
	  failwith (Wrutils.str "%s takes %d arguments after alg name"
		      alg_name (count - 1));
	let args = Array.sub args 1 count in
	initer args


let parse_non_alg_args () =
  let v = ref 2 in
  let limit = ref [Limit.Never] in
  let others = ref [""] in
    Arg.parse
      ([ "-v", Arg.Set_int v,
	 "verbosity setting (between 1=nothing and 5=everything, default 2)";
       ] @ (Limit.arg_specs limit))

      (fun s -> Wrutils.push s others) "";
    Verb.set_default !v;
    Verb.pe Verb.debug "Args Parsed\n";
    (!v, !limit, Array.of_list (List.rev !others))


let main () =
  let v, limit, args = parse_non_alg_args () in
    Verb.with_level v
      (fun () ->
	 let p = Uniform_tree.read stdin in
	 let a = setup_alg args in
	   run_alg ~lim:limit a p)


let _ = main ()
