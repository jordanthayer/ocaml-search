(** The main file for spt-it-out.

    @author eaburns
    @since 2010-05-14
*)

open Verbosity


let verb_string = "Set the verbosity level"

let parse_args () =
  (** [parse_args ()] parses the command-line arguments. *)
  let file = ref ""
  and verb = ref verb_normal in
    Arg.parse [ "-v", Arg.Set_int verb, verb_string; ]
      (fun s ->
	 if !file <> ""
	 then invalid_arg "Specify only one input file (-h for help)"
	 else file := s)
      "./spt-it-out [-v <level 0-3>] [<input file>]";
    let ichan = if !file = "" then stdin else open_in !file in
      ichan, !verb


let main () =
  let ichan, verb = parse_args () in
    Verb_level.set verb;
    vprintf verb_normal "spt-it-out\n";
    if ichan = stdin
    then begin
      try
	while true do
	  Printf.printf "> %!";
	  let ast = Sexpr.parse (Stream.of_channel ichan) in
	    ignore (Eval_spt_it_out.eval Evaluate.init_environment ast);
	done;
      with End_of_file -> ()
    end else begin
      let ast = Sexpr.parse (Stream.of_channel ichan) in
	ignore (Eval_spt_it_out.eval Evaluate.init_environment ast);
	close_in ichan
    end


let _ = main ()
