(** A main function that generates a CSV file with information sampled
    from a tiles puzzle.

    @author eaburns
    @since 2009-12-05
*)

open Pancake_sampling

let parse_args () =
  let v = ref 2
  and num = ref ~-1
  and size = ref ~-1
  and pdb_size = ref ~-1
  and filename = ref ""
  and costname = ref ""
  in
    Arg.parse [ "-v", Arg.Set_int v, "verbosity (between 1 and 5, default 2)";
		"-s", Arg.Set_int size, "size (number of tiles)";
		"--size", Arg.Set_int size, "size (number of pancakes)";
		"-p", Arg.Set_int pdb_size, "PDB size";
		"--pdb-size", Arg.Set_int pdb_size, "PDB size";
		"-c", Arg.Set_string costname, "cost (unit, sqrt, etc.)";
		"--cost", Arg.Set_string costname, "cost (unit, sqrt, etc.)";
		"-n", Arg.Set_int num, "number of samples";
		"--num", Arg.Set_int num, "number of samples";
	      ]
      (fun s ->
	 if !filename <> ""
	 then invalid_arg "too many arugments"
	 else filename := s)
      "";
    if !filename = ""
    then failwith "You must specify a filename as the final argument";
    if !pdb_size = ~-1
    then failwith "You must specify a PDB size";
    if !size = ~-1
    then failwith "You must specify a size"
    else !v, !num, !size, !pdb_size, !costname, !filename


let random_child ?parent s =
  (** [random_child ?parent s] gets a random child of the sampled
      state [s].  If [parent] is given, then this will not return a
      child that is the same as the parent. *)
  let n = List.length s.successors in
    match parent with
      | None -> List.nth s.successors (Random.int (n - 1))
      | Some p ->
	  assert (n > 1);
	  let succ = ref (List.nth s.successors (Random.int (n - 1))) in
	    while !succ = p do
	      succ := (List.nth s.successors (Random.int (n - 1)))
	    done;
	    !succ


let output_state outch gp =
  (** [output_state outch gp] outputs a row of a CSV file for a random
      child of state [s]. *)
  let parent = random_child gp in
  let child = random_child ~parent:parent parent in
  let delta_h = child.h -. parent.h in
  let delta_d = child.d - parent.d in
  let delta_f = (child.g +. child.h) -. (parent.g +. parent.h) in
  let nsuccessors = List.length child.successors in
    (* contents. *)
    Wrutils.pf outch "\"";
    Wrarray.write_ints outch parent.contents;
    Wrutils.pf outch "\"";
    Wrutils.pf outch "\t%f\t%f" child.h parent.h;
    Wrutils.pf outch "\t%d\t%d" child.d parent.d;
    Wrutils.pf outch "\t%d\t%d" child.t parent.t;
    Wrutils.pf outch "\t%f" child.g;
    Wrutils.pf outch "\t%f\t%d\t%f" delta_h delta_d delta_f;
    Wrutils.pf outch "\t%d\n" nsuccessors


let make_sample size cost pdb_size num filename =
  (** [make_sample size cost num filename] creates the CSV file from a
      random sample of tiles states. *)
  Wrio.with_outfile filename
    (fun outch ->
       (* the column names. *)
       Wrutils.pf outch "\"configuration\"";
       Wrutils.pf outch "\t\"h\"";
       Wrutils.pf outch "\t\"h_parent\"";
       Wrutils.pf outch "\t\"d\"";
       Wrutils.pf outch "\t\"d_parent\"";
       Wrutils.pf outch "\t\"t\"";
       Wrutils.pf outch "\t\"t_parent\"";
       Wrutils.pf outch "\t\"cost\"";
       Wrutils.pf outch "\t\"delta_h\"";
       Wrutils.pf outch "\t\"delta_d\"";
       Wrutils.pf outch "\t\"delta_f\"";
       Wrutils.pf outch "\t\"nsuccessors\"\n";
       Pancake_sampling.sample size cost pdb_size (output_state outch) 3 num)


let main () =
  let v, num, size, pdb_size, cost, filename = parse_args ()
  in Verb.with_level v (fun () -> make_sample size cost pdb_size num filename)


let _ = main ()
