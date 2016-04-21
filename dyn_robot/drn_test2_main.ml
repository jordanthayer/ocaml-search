(* $Id: runs.ml,v 1.1 2004/12/22 20:18:00 ruml Exp ruml $
   
   stand-alone solver
*)


let main () =
  let n = Array.length Sys.argv in
    if n < 5 then failwith "usage: x y p wt [seed]";
    let x = int_of_string Sys.argv.(1)
    and y = int_of_string Sys.argv.(2)
    and p = float_of_string Sys.argv.(3)
    and wt = float_of_string Sys.argv.(4)
    and seed = (if n = 6 then
		  int_of_string Sys.argv.(5)
		else
		  (Random.self_init ();
		   Random.int 10000)) in
      Runs.test2 ~seed:seed x y p wt


let _ = main ()
	  

(* EOF *)
