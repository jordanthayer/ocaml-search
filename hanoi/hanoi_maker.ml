(**

   for making towers of hanoi instances.

*)
let path_root = User_paths.instance_root



let path domain_name domain_attrs pdb_name =
  (** [path domain_name domain_attrs pdb_name pdb_size] gets the path
      to the pattern database. *)
  Rdb.path_for (path_root ^ domain_name)
    (("type", "instance") :: (domain_attrs
			 @ ["num", pdb_name;]))


let make_instances n_pegs n_disks n_instances = 
  for i = 0 to n_instances 
  do
    let instance_name = path "hanoi" 
      [
	"npegs",string_of_int n_pegs;
	"ndisks",string_of_int n_disks;
      ] (string_of_int i) in

    let new_instance = Hanoi.make_empty n_disks n_pegs in
      for j = 0 to n_disks - 1
      do
	Hanoi.add_disk new_instance 
	  (Random.int n_pegs)
	  (Char.chr (n_disks - j))
      done;
      if(Hanoi.verify_hanoi new_instance) then ()
      else
	failwith "the new instance is invalid";
      let outch = open_out instance_name in
	Hanoi.to_outch new_instance outch;
	close_out outch;

  done
