(** A binary that outputs the solution list for the darts problem.

    @author eaburns
    @since 2010-04-08
*)

let data_root = User_paths.data_root ^ "darts"

let main () =
  let files = Rdb.matching_paths data_root [] in
    List.iter (fun file ->
		 if file <> Darts_runs.tmp_file
		 then begin
		   let df = Datafile.load file in
		   let n_darts =
		     int_of_string (Datafile.get_val df "num darts")
		   and n_regions =
		     int_of_string (Datafile.get_val df "num regions")
		   and regions = Datafile.get_val df "final regions" in
		   let lst = Str.split (Str.regexp_string " ") regions in
		     assert ((List.length lst) = n_regions);
		     Printf.printf "%d: %s;\n" n_darts regions;
		 end)
      files

let _ = main ()
