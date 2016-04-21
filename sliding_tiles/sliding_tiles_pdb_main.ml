(* Tool for Generating PDB's for the sliding tiles puzzle, based on packed
   int arrays *)

let get_pattern string =
  Verb.pe Verb.debug "Parsing pattern %s\n" string;
  let vals = Str.split (Str.regexp "-") string in
    List.map int_of_string vals


let get_cost_fun string =
  Verb.pe Verb.debug "Parsing cost fn %s\n" string;
  match string with
    | "unit" -> "unit", Sliding_tiles.Tile_cost.unit
    | _ -> failwith (Wrutils.str "%s: Unrecognized cost function" string)



let parse_non_alg_args () =
  let others = ref ["pad"]
  and add = ref false in
    Arg.parse [ "--add", Arg.Set add,
		"Generates an additive pdb instead of a standard one" ]
      (fun s -> Wrutils.push s others) "";
    let args = Array.of_list (List.rev !others) in
      assert ((Array.length args) = 4);
      get_pattern args.(1), get_cost_fun args.(2), args.(3), !add


let main () =
  (* make with make_pdb target*)
  let pattern, cost, moves, add = parse_non_alg_args ()
  and p = Sliding_tiles_inst.read stdin in
    match fst cost with
      | "unit" ->
	  (Verb.pe Verb.always "Unit cost pdb\n%!";
	   Sliding_unit_pdb.make_and_save p add pattern cost moves)
      | _ -> Sliding_tiles_pdb.make_and_save p add pattern cost moves


let _ =
  main ()

(* EOF *)
