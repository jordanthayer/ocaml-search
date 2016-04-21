(* $Id: main.ml,v 1.1 2004/01/13 23:45:14 ruml Exp ruml $

   main file for CSP solver
*)


let main () =
  Csp_instances.make_random_binary 100
      
 
let _ = main ()
  

(* EOF *)
