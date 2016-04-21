(* $Id: main.ml,v 1.1 2004/12/22 20:17:58 ruml Exp ruml $

   main file
*)


(******** particular algorithms *********)


let init_basic a args =
  a


let init_wted a args =
  (** wted_a_star, anytime_a_star *)
  let wt = float_of_string args.(1) in
    a wt


let init_wt_cut a args =
  (** wted_a_star, anytime_a_star *)
  let wt = float_of_string args.(1)
  and co = float_of_string args.(2) in
    a wt co


let init_gcoff a args =
  let co = float_of_string args.(1) in
    a co

let init_bugsy a args =
  let cost = float_of_string args.(1)
  and time = float_of_string args.(2)
	       (* bytecode: 4-way is 18,000,  8-way is 10,000 *)
	       (* native code: 4-way is 48,000, 8-way is 75,000? *)
	       (* and expands_per = (if args.(1) = "4-way" then 45000
		  else if args.(1) = "8-way" then 20000
		  else failwith "no expands_per for this -way") *)
  and expands_per = 45000 in
    a cost time expands_per


(********** pulling it all together ***********)


let alg_table =
  [
    "retro_a_star", (1, init_basic (Grid_retro_algs.retro_a_star ~log_ch:stdout));
  ]

let get_alg name =
  Wrlist.get_entry alg_table "alg" name


let set_up_alg args =
  (** looks at command line.  returns alg_func (which is world -> node
      option * int * int) and list of string pairs for logging parameters. *)
  let n = Array.length args in
    if n < 1 then
      (Wrutils.pr "expects a board on stdin, writes to stdout.\n";
       Wrutils.pr "First arg must be an algorithm name (such as \"a_star\"),\n";
       Wrutils.pr "second arg must be either \"4-way\" or \"8-way\",\n";
       Wrutils.pr "other args depend on alg (eg, \"max_dim\").\n";
       failwith "not enough command line arguments")
    else
      let alg_name = args.(1) in
      let count, initer = get_alg alg_name in
	if n <> (1 + count) then
	  failwith (Wrutils.str "%s takes %d arguments after alg name" alg_name (count - 1));
	let args = Array.sub args 1 count in
	  initer args

let parse_non_alg_args () =
  let v = ref 3
  and limit = ref Limit.Never
  and others = ref ["pad"] in
    Arg.parse [ "-v", Arg.Set_int v, "verbosity setting (between 1=nothing and 5=everything, default 3)";
		"-limit", Arg.Float (fun l -> limit := Limit.Time l), "search time limit (in seconds, default no limit)"]
      (fun s -> Wrutils.push s others) "";
    !v, !limit, Array.of_list (List.rev !others)


let main () =
  (** alg from command line, board from stdin, results to stdout *)
  let v,limit,args = parse_non_alg_args () in
  let a = set_up_alg args
  and b = Grid_instance.read (-1) stdin in
    Verb.with_level v
      (fun () ->
	 Grid_retro_runs.run_retro a b [limit])


let _ = main ()


(* EOF *)
