(** The main program for dumping all leaves in a 2-way partitioning
    problem.

    @author eaburns
    @since 2009-12-30
*)

open Big_int

let parse_args () =
  (** [parse_args ()] parses the arguments and results in a tuple of
      their values. *)
  let kind = ref "ckk"
  and verb = ref 2
  and outfile = ref ""
  in
    Arg.parse
      [
	"-k", Arg.Set_string kind, "The kind name (default: ckk)";
	"--kind", Arg.Set_string kind, "The kind name (default: ckk)";

	"-v", Arg.Set_int verb, "Verbosity (default: 2)";
	"--verbose", Arg.Set_int verb, "Verbosity (default: 2)";
      ]
      (fun s ->if !outfile <> "" then invalid_arg s else outfile := s)
      "[options...] <outfile>";
    !kind, !verb, !outfile


let dump kind_name outfile inst () =
  let dump_fun =
    match kind_name with
      | "ckk" -> Two_partition_ckk.dump_leaves
      | _ -> invalid_arg ("kinds are: ckk")
  in
  let _, time =
    Wrsys.with_time (fun () -> Wrio.with_outfile outfile (dump_fun inst))
  in
    Wrutils.pr "Dumped in %f seconds\n" time



let main () =
  (** [main ()] is the main function. *)
  let kind_name, verb, outfile = parse_args () in
  let inst = Two_partition_instance.read stdin
  in Verb.with_level verb (dump kind_name outfile inst)


let _ = main ()
