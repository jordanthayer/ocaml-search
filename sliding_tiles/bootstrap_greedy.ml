(**

    @author jtd7
    @since 2012-09-30
*)


let verb = ref Verb.always
let other_args = ref []
let args = ref [||]

let parse_args () =
  (** [parse_args ()] parses the arguments. *)
  let arg_spec =
    [
      "-v", Arg.Set_int verb, (Printf.sprintf
				 "Verbosity level (default: %d)" !verb);
      "--", Arg.Rest (fun str ->
			let vals = Str.split (Str.regexp " ") str in
			  List.iter (fun s -> Wrutils.push s other_args)
			    vals),
      "Everything after this is interpreted as an argument to the solver";

    ]
  in
  let usage_str =
    "bootstrap_greedy <min ins> <tmin> <tmax>"
  in
    Arg.parse arg_spec (fun s -> other_args := s :: !other_args) usage_str;
    args := (Array.of_list (List.rev !other_args))


let main () =
  parse_args ();
  Verb.set_default !verb;
  Bootstrapping_runs.greedier_run !args


let _ = main ()
