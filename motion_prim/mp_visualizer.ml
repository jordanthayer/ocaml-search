
let usage () = Printf.printf "usage:  visualizer infile\n"

let main () = 
  if (Array.length Sys.argv) <> 2 then (usage (); exit 1);
  let mp_file = Sys.argv.(1) in
    Printf.printf "using motion prim file: %s\n%!" mp_file;
  let mp_list = Wrio.map_file Motion_prim.read mp_file in
    Motion_prim_vis.visualize_mps 200.0 mp_list  

let _ = main ()
