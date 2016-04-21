(**

    @author jtd7
    @since 2011-01-14
*)

let parse_non_alg_args () =
  let v = ref 3
  and others = ref ["pad"]
  and approx = ref false
  and res = ref 1 in
    Arg.parse
      [ "-v", Arg.Set_int v,
	"verbosity setting (between 1=nothing and 5=everything, default 3)";

	"--approx", Arg.Set approx, "Sets approximation mode";
	"--res", Arg.Set_int res, "Sets resolution on approximation";

	"--", Arg.Rest (fun str ->
			  let vals = Str.split (Str.regexp " ") str in
			    List.iter (fun s -> Wrutils.push s others) vals),
	"Everything after this is interpreted as an argument to the solver";
      ]
      (fun s -> Wrutils.push s others) "";
    !others, !approx, !res, !v

let main () =
  let (moves::_), approx, res, v = parse_non_alg_args () in
  Verb.with_level v
    (fun () ->
       if not approx then
	 Contract_astar.make_table Search_interface.Tiles (int_of_string moves)
       else
	 (Verb.pe Verb.always "Contstructing approximate table at res %i\n%!"
	    res;
	  Contract_astar.make_approx_table ~res Search_interface.Dock_robot
	   (int_of_string moves)))

let _ = main ()
