(**

   @author jtd7
   @since 2011-03-28
*)

module St = Sliding_tiles

let get_alg alg =
  match alg with
    | "astar" -> Astar_bootstrap.do_dups
    | "idastar" -> Astar_bootstrap.do_ida
    | _ -> failwith "Alg not recognized"


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


let main () =
  let v, others, args = parse_non_alg_args () in
  let make_instance _ = "-1", St.rwalk ~nrows:4 ~ncols:4 30
  and alg = get_alg others.(1) in
    (* I don't know why the other solvers aren't writing this as well.
       Maybe we should change that in the future *)
    Datafile.write_header_pairs stdout;
    let (s, e, g, p, m, d), t = Wrsys.with_time
      (fun () ->
	 Verb.with_level v
	   (fun () ->
	      alg
		~make_instance
		~load_instance:Sliding_tiles_inst.load
		~base_sface:(Sliding_tiles_interfaces.default_interface "unit")
		~domain:Search_interface.Tiles
		~args)) in
    let trail_pairs = ["total raw cpu time", string_of_float t;
		       "total nodes expanded", string_of_int e;
		       "total nodes generated", string_of_int g] in
      Datafile.write_pairs stdout trail_pairs


let _ = main ()

(* EOF *)
