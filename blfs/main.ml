(* test main for BLFS

*)


let main () =
  if (Array.length Sys.argv) <> 4 then
    Wrutils.pr "args: branching depth nodes\n"
  else
    let branching = int_of_string Sys.argv.(1)
    and depth = int_of_string Sys.argv.(2)
    and nodes = int_of_string Sys.argv.(3) in
      Sep_test.test_bound depth branching nodes

let _ = main ()


(* EOF *)
