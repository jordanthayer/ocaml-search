(*

  generates pancake puzzles.

*)

let make ncakes start_id = 


  Printf.printf "return from mkdir:%s\n"
    (Wrsys.shell_output (
    "mkdir ./group/data/pancake/instance/"^
      (string_of_int ncakes)
  ));


  Printf.printf "return from touch:%s\n" 
    (Wrsys.shell_output (
    "touch ./group/data/pancake/instance/"^
      (string_of_int ncakes)
    ^"/"^
      "KEY=num"
  ));


  for id = 0 to start_id do
    let filename = "./group/data/pancake/instance/"^
      (string_of_int ncakes)
      ^"/"^
      (string_of_int id) in
    let a = Array.make ncakes 0 in
    let b = Array.mapi (fun i _ -> i) a in
      Wrarray.shuffle b;
      Wrio.with_outfile_append (filename)
	(fun oc -> Printf.fprintf oc "%d\n" ncakes);

      Wrio.with_outfile_append (filename)
	(fun oc -> Wrarray.write_ints oc b);
      Wrio.with_outfile_append (filename)
	(fun oc -> Printf.fprintf oc "\n");

  done;;
