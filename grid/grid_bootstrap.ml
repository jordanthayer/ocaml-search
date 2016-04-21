(**

    @author jtd7
    @since 2011-03-25
*)


let feasible_p inst () =
  Speedy.dups_silent
    (Grid_interfaces.default_interface inst [Limit.Time 5.]) [||]


let make_instance ?(width = 500) ?(height = 500) ?(blk = 0.35) (c,m) inum =
  (** this isn't really in keeping with the holte work.  Maybe actually do the
      random walk, or choose smaller instances for bootstrapping *)
  (*Verb.pe Verb.always "Making a new instance";*)
    Printf.sprintf "%i" inum,
  (match c with
     | Grid.Life -> Grid_instance.life_four_board feasible_p width height blk
     | Grid.Unit -> Grid_instance.unit_four_board feasible_p width height blk)


let parse_non_alg_args () =
  let v = ref 3
  and others = ref ["pad"]
  and ins_min = ref "75"
  and tmin = ref "1."
  and tmax = ref "512" in
    Arg.parse
      [ "-v", Arg.Set_int v,
	"verbosity setting (between 1=nothing and 5=everything, default 3)";
	"--ins_min", Arg.Set_string ins_min,
	"Number of instances needed to solve before training";
	"--t_min", Arg.Set_string tmin,
	"Minimum Time To Solve Instance";
	"--t_max", Arg.Set_string tmax,
	"Max Time To Solve Instance";
	"--", Arg.Rest (fun str ->
			  let vals = Str.split (Str.regexp " ") str in
			    List.iter (fun s -> Wrutils.push s others) vals),
	"Everything after this is interpreted as an argument to the solver";
      ]
      (fun s -> Wrutils.push s others) "";
    !v, Array.of_list (List.rev !others), [|!ins_min; !tmin; !tmax|]


let get_alg alg =
  match alg with
    | "astar" -> Astar_bootstrap.do_dups
    | _ -> failwith "Alg not recognized"


let run_alg alg gtype args =
  let make_instance = make_instance gtype
  and alg = get_alg alg
  and gtype = (match (fst gtype) with
		 | Grid.Unit -> Search_interface.UGrid
		 | Grid.Life -> Search_interface.LGrid) in
  let (s, e, g, p, m, d), t = Wrsys.with_time
    (fun () -> alg
       make_instance
       Grid_instance.load
       Grid_interfaces.default_interface
       gtype args) in
  let trail_pairs = ["total raw cpu time", string_of_float t;
		     "total nodes expanded", string_of_int e;
		     "total nodes generated", string_of_int g] in
    Datafile.write_pairs stdout trail_pairs


let main () =
  let v, others, args = parse_non_alg_args () in
  let gtype = (match others.(1) with
		 | "Unit" -> (Grid.Unit, Grid.Fourway)
		 | "Life" -> (Grid.Life, Grid.Fourway)
		 | _ -> failwith "Not Recognised Gridtype argument")
  and alg = others.(2) in
    Datafile.write_header_pairs stdout;
    Verb.with_level v
      (fun () -> run_alg alg gtype args)


let _ = main ()
