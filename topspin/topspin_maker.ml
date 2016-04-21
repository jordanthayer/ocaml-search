

let path_root = User_paths.instance_root



let path domain_name domain_attrs pdb_name =
  (** [path domain_name domain_attrs pdb_name pdb_size] gets the path
      to the pattern database. *)
  Rdb.path_for (path_root ^ domain_name)
    (("type", "instance") :: (domain_attrs
			 @ ["num", pdb_name;]))



let make_instances ?(start = 0) flipper disks n_instances = 

  let prob = 
    {
      Topspin.n_disks = disks;
      Topspin.k_flipper = flipper;
      Topspin.initial_configuration = Topspin.make_goal disks;
    } in 
    for i = start to start + n_instances - 1
    do
      let instance_name = path "topspin" 
	[
	  "disks",string_of_int disks;
	  "flipper",string_of_int flipper;
	] (string_of_int i) in

      let new_instance = Topspin.make_goal disks in
	for i = 0 to 1000000
	do
	  Topspin.use_turnstile new_instance prob (Random.int disks);
	done;
	let outch = open_out instance_name in
	  Topspin_io.write_problem outch 
	    {
	      Topspin.n_disks = disks;
	      Topspin.k_flipper = flipper;
	      Topspin.initial_configuration = new_instance;
	    };
	  close_out outch;
    done
