(**

   main function that makes pattern databases.

*)

let main () = 
  let n_pegs = ref (-1) in
  let n_disks = ref (-1) in
  let n_abs_disks = ref (-1) in
  let old_pdb = ref false in
  let direction = ref "" in
  let bp_pdb = ref false in
    
    Arg.parse 
      [
	"--n_pegs", Arg.Set_int n_pegs, "Number of pegs";
	"--n_disks", Arg.Set_int n_disks, "Number of disks";
	"--n_abs_disks", Arg.Set_int n_abs_disks, "Number of abstracted disks";
	"--dir",Arg.Set_string direction, "Direction abstract disks go";
	"--old",Arg.Set old_pdb, "Make an old style PDB";
	"--bp",Arg.Set bp_pdb, "Make a bit packed PDB";
      ] (Fn.no_op1) "";


    if(!old_pdb) then 
      (let dir_to_use = match !direction with
	   "Low" -> Hanoi_interfaces.Low
	 | "High" -> Hanoi_interfaces.High
	 | _ -> failwith "invalid direction choice" in 

	 Hanoi_interfaces.make_pdb dir_to_use !n_pegs !n_disks !n_abs_disks
      )
    else if(!bp_pdb) then 
      Hanoi_interfaces.make_bp_pdb !n_pegs !n_disks
    else 
      Hanoi_interfaces.make_new_pdb !n_pegs !n_disks


let _ = main ()
