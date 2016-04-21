(** A main function for sampling a 2-way number partitioning instance.

    @author eaburns
    @since 2010-01-15
*)

let parse_args () =
  (** [parse_args ()] parses the arguments and results in a tuple of
      their values. *)
  let kind = ref "ckk"
  and num = ref 1000
  and save_to = ref ""
  and separate = ref ""
  and verb = ref 2
  in
    Arg.parse
      [
	"-k", Arg.Set_string kind, "The kind name (default: ckk)";
	"--kind", Arg.Set_string kind, "The kind name (default: ckk)";

	"-n", Arg.Set_int num, "Number of samples (default: 1000)";
	"--number", Arg.Set_int num, "Number of samples (default: 1000)";

	"-s", Arg.Set_string save_to, "Save the model to a file";
	"--save", Arg.Set_string save_to,  "Save the model to a file";

	"--separate", Arg.Set_string separate,
	"Build a separate model and plot it to the given file";

	"-v", Arg.Set_int verb, "Verbosity (default: 2)";
	"--verbose", Arg.Set_int verb, "Verbosity (default: 2)";
      ]
      invalid_arg "";
    !kind, !num, !save_to, !separate, !verb


let run_ckk inst number =
  (** [run_ckk ints number] runs the algorithm on the instance using CKK. *)
  let module I = Bounded_depth_interface in
  let root, i, _ = Two_partition_ckk.make_interface inst in
    (Wrsys.with_time (fun () -> Blfs_sampling.run
			i.I.is_leaf
			i.I.leaf_cost
			i.I.num_children
			i.I.nth_child
			root
			number),
     i.I.max_depth, 2 (* max_children *))


let run_greedy inst number =
  (** [run_greedy ints number] runs the algorithm on the instance
      using the greedy representation. *)
  let module I = Bounded_depth_interface in
  let root, i, _ = Two_partition_greedy.make_interface inst in
    (Wrsys.with_time (fun () -> Blfs_sampling.run
			i.I.is_leaf
			i.I.leaf_cost
			i.I.num_children
			i.I.nth_child
			root
			number),
     i.I.max_depth, 2 (* max_children *))


let go kind_name inst number save_to separate () =
  let (ary, time), max_depth, max_children =
    match kind_name with
      | "ckk" ->
	  run_ckk inst number
      | "greedy" ->
	  run_greedy inst number
      | _ ->
	  invalid_arg ("kinds are: ckk and greedy")
  in
    Wrutils.pr "%d samples in %f seconds\n" number time;

    if save_to <> ""
    then begin
      Verb.pr Verb.often "Saving samples to %s\n" save_to;
      Blfs_sampling.save ary save_to
    end;

    if separate <> ""
    then begin
      Verb.pr Verb.often "Building separate model\n";
      let costs, time =
	Wrsys.with_time (fun () ->
			   Blfs_sampling.to_separate
			     max_depth max_children ary)
      in
	Verb.pr Verb.often "Built in %f seconds\n" time;
	Verb.pr Verb.often "Plotting separate model to %s\n" separate;
	Blfs_plot_model.plot_separate max_depth max_children costs separate;
	 let saved = separate ^ ".sep" in
	   Verb.pr Verb.often "Saving separate model to %s\n" saved;
	   Wrio.with_outfile saved
	     (fun out -> Marshal.to_channel out costs []);
    end



let main () =
  (** [main ()] is the main function. *)
  let kind_name, number, save_to, separate, verb = parse_args () in
  let inst = Two_partition_instance.read stdin
  in Verb.with_level verb (go kind_name inst number save_to separate)


let _ = main ()
