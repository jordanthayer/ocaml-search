(**

   Interface for towers of hanoi.  

   Christopher Wilt
   November 14, 2010

*)


let path_root = User_paths.instance_root


let bad_pdb_iter ?(display_number= 1) bad_pdb = 
  let display_counter = ref 0 in
    Ht_pdb.Pdb_hash.iter 
      (
	fun hanoi cost -> 
	  if(!display_counter > display_number) then
	    failwith "bad pdb iterator terminating";
	  Printf.fprintf stderr "%s\n with a cost of %f" 
	    (Hanoi.print_hanoi hanoi) cost;
	  flush stderr;
	  display_counter := !display_counter + 1;
      ) 
      bad_pdb


let bad_pdb_iter_new ?(display_number= 1) bad_pdb = 
  let display_counter = ref 0 in
    Ht_pdb.Pdb_hash_new.iter 
      (
	fun hanoi cost -> 
	  if(!display_counter > display_number) then
	    failwith "bad pdb iterator terminating";
	  Printf.fprintf stderr "%s\n with a cost of %f" 
	    (Min_hanoi.print_hanoi hanoi) cost;
	  flush stderr;
	  display_counter := !display_counter + 1;
      ) 
      bad_pdb




let abs_to_string (abs:bool array) = 
  Array.fold_left (fun accum item -> item ^ accum) "" 
    (Array.map (fun a -> match a with true -> "T" | _ -> "F") abs)


type abs_direction = 
    Low 
  | High


let make_abs abs_dir n_disk n_abs_disk = 
  assert (n_disk >= n_abs_disk);
  let abs = Array.make n_disk false in
    (match abs_dir with
	 Low ->
	   (
	     for i = 0 to (n_abs_disk - 1) do
	       abs.(i) <- true;
	     done
	   )
       | High -> 
	   (
	     for i = 1 to (n_abs_disk) do
	       abs.(n_disk - i) <- true;
	     done
	   ));
    abs


let path domain_name domain_attrs pdb_name =
  (** [path domain_name domain_attrs pdb_name pdb_size] gets the path
      to the pattern database. *)
  Rdb.path_for (path_root ^ domain_name)
    (("type", "pdb") :: (domain_attrs
			 @ ["pdb_name", pdb_name;]))


let make_new_pdb n_pegs n_disk = 
  let pdb_path = path "hanoi" 
    [
      "npegs",string_of_int (n_pegs);
      "ndisks",string_of_int (n_disk);
    ] "new_pdb" in
    Verb.pr Verb.toplvl "creating pattern database\n";

    let initial = 
      (Min_hanoi.make_goal n_disk n_pegs) in
      

    let pdb = 
      (
	Ht_pdb.search_dups_new (Min_hanoi.expand ~cost:Min_hanoi.Unit) initial
      ) in

      Wrio.with_outfile pdb_path
	(fun outch -> Marshal.to_channel outch pdb [])


let make_bp_pdb n_pegs n_disk = 
  if(n_pegs != 4) then
    failwith "bit packed pdb can only be made with 4 pegs";
  let pdb_path = path "hanoi" 
    [
      "npegs",string_of_int (n_pegs);
      "ndisks",string_of_int (n_disk);
    ] "bp_pdb" in
    Verb.pr Verb.toplvl "creating pattern database\n";

    

    let pdb = 
      match n_disk with 
	  0 -> Bp_array.make_bpa_zero 1 1
	| _ -> 
	    (
	      let initial = 
		(Min_hanoi.make_goal n_disk n_pegs) in

		Bp_pdb.make_bp_pdb 
		  (Min_hanoi.expand ~cost:Min_hanoi.Unit) 
		  initial
		  (Min_hanoi.abstract_state_up_int n_disk)
		  (int_of_float (4. ** (float_of_int n_disk)))
		  n_disk
	    ) in

      Wrio.with_outfile pdb_path
	(fun outch -> Marshal.to_channel outch pdb [])



let make_pdb direction n_pegs n_disk n_abs_disk = 
  let pdb_abstraction = make_abs direction n_disk n_abs_disk in
  let abs_name = abs_to_string pdb_abstraction in
  let pdb_path = path "hanoi" 
    [
      "npegs",string_of_int (n_pegs);
      "ndisks",string_of_int (n_disk);
    ] abs_name in
    Verb.pr Verb.toplvl "creating pattern database\n";

    let initial = 
      Hanoi.abstract_state (Hanoi.make_goal n_disk n_pegs)
	pdb_abstraction in
      

    let pdb = 
      (
	Ht_pdb.search_dups (Hanoi.expand ~cost:Hanoi.AbstractedFree) initial
      ) in

      Wrio.with_outfile pdb_path
	(fun outch -> Marshal.to_channel outch pdb [])



let pdb_interface pdb_abstractions cost_name inst lim =
  (**
     A pdb_abstraction is an array of booleans, true for abstracted
     false for not abstracted disk.  

     The cost name isn't used yet, but if we ever make a non-unit cost
     version of this domain it might become important.

     inst is the instace upon which the problem is to be run.  

     lim is the limits to be applied to the solver.
  *)

  (*first thing to do is to load the pattern databases.*)

  let pdbs = Array.make 
    (Array.length pdb_abstractions) 
    (Ht_pdb.Pdb_hash.create 1) in

  let n_pdbs = Array.length pdbs in

  let pdb_names = Array.map abs_to_string pdb_abstractions in
  let pdb_files = Array.map 
    (
      path "hanoi" 
	[
	  "npegs",string_of_int (Hanoi.n_pegs inst);
	  "ndisks",string_of_int (Hanoi.n_disks inst);
	]
    )
    pdb_names in
    (*check to make sure that the pdb files actually exist*)
    for i = 0 to (Array.length pdb_files) - 1
    do
      if(Sys.file_exists pdb_files.(i)) then ()
      else
	failwith (Printf.sprintf 
		    "one of the pattern databases requested is missing\n%s"
		    pdb_files.(i)
		 );
    done;


    (*now that we know the pattern databases exist, we should load
      them into the pdb array.*)

    for i = 0 to Array.length pdbs - 1 
    do
      let inch = open_in pdb_files.(i) in
	pdbs.(i) <- Marshal.from_channel inch;
	close_in inch;
    done;
    
    (*now that the pattern databases have been loaded, we can
      construct the heuristicis.  *)

    let h_fun node = 
      (
	let h_to_return = ref 0.0 in
	  for i = 0 to n_pdbs - 1 
	  do
	    (*abstract the state*)
	    let abs_state = Hanoi.abstract_state node pdb_abstractions.(i) in

	    let this_pdb_h = 

	      try (Ht_pdb.Pdb_hash.find pdbs.(i) abs_state) with
		  Not_found -> (
		    Printf.fprintf stderr "tried to find:\n%s"
		      (Hanoi.print_hanoi abs_state);
		    bad_pdb_iter pdbs.(i);
		    failwith "abort";) in

	      h_to_return := !h_to_return +. this_pdb_h;
	  done;
	  !h_to_return
      ) in

    let hd_fun node = 
      (
	let h_to_return = h_fun node in
	  h_to_return,h_to_return
      ) in


      (Search_interface.make
	 ~h:h_fun
	 ~d:h_fun
	 ~hd:hd_fun
	 (*       ~t:node_type*)
	 ~domain_expand:(Hanoi.expand)
	 (*       ~predecessor:(make_expand ncakes cost h_fun)*)
	 ~key:Fn.identity
	 ~hash:Hanoi.hash
	 ~key_print:Hanoi.print_hanoi
	 ~equals:Hanoi.hash_compare
	 ~goal_p:Hanoi.is_goal
	 ~halt_on:lim
	 ~get_sol_length:Hanoi.sol_length
	 ~p_update:(fun _ _ -> ())
	 Search_interface.Hanoi
	 inst
	 (fun _ _ -> false)
	 (fun _ -> ()))






let new_pdb_interface 
    (pdb_up:int)
    (pdb_down:int)
    cost_name 
    inst lim =
  (**
     A pdb_abstraction is an array of booleans, true for abstracted
     false for not abstracted disk.  

     The cost name isn't used yet, but if we ever make a non-unit cost
     version of this domain it might become important.

     inst is the instace upon which the problem is to be run.  

     lim is the limits to be applied to the solver.
  *)

  let inst = Min_hanoi.make_from_hanoi inst in

  (*first thing to do is to load the pattern databases.*)

  let pdbs = Array.make 
    2 (*one up, one down*)
    (Ht_pdb.Pdb_hash_new.create 1) in

  let pdb_names = [|"new_pdb";"new_pdb"|] in
  let disk_arr = [|pdb_up;pdb_down|] in

  let pdb_files = Wrarray.map2 
    (
      fun pdb_name abstracted_disk ->
	path "hanoi" 
	  [
	    "npegs",string_of_int (Min_hanoi.n_pegs inst);
	    "ndisks",string_of_int (abstracted_disk);
	  ] pdb_name
    )
    pdb_names disk_arr in
    (*check to make sure that the pdb files actually exist*)
    for i = 0 to (Array.length pdb_files) - 1
    do
      if(Sys.file_exists pdb_files.(i)) then ()
      else
	failwith (Printf.sprintf 
		    "one of the pattern databases requested is missing\n%s"
		    pdb_files.(i)
		 );
    done;
    (*now that we know the pattern databases exist, we should load
      them into the pdb array.*)
    for i = 0 to Array.length pdbs - 1 
    do
      let inch = open_in pdb_files.(i) in
	pdbs.(i) <- (Marshal.from_channel inch);
	close_in inch;
    done;
    (*now that the pattern databases have been loaded, we can
      construct the heuristicis.  *)
    let h_fun node = 
      (
	let h_to_return = ref 0.0 in

	(*abstract the state*)
	let up_abs_state = Min_hanoi.abstract_state_up pdb_up node in
	let down_abs_state = Min_hanoi.abstract_state_down pdb_down node in


	let up_pdb_h = 
	  try (Ht_pdb.Pdb_hash_new.find pdbs.(0) up_abs_state) with
	      Not_found -> (
		Printf.fprintf stderr "tried to find:\n%s"
		  (Min_hanoi.print_hanoi up_abs_state);
		bad_pdb_iter_new pdbs.(0);
		failwith "abort";) in

	let down_pdb_h = 
	  try (Ht_pdb.Pdb_hash_new.find pdbs.(1) down_abs_state) with
	      Not_found -> (
		Printf.fprintf stderr "tried to find:\n%s"
		  (Min_hanoi.print_hanoi down_abs_state);
		bad_pdb_iter_new pdbs.(1);
		failwith "abort";) in

	  h_to_return := !h_to_return +. up_pdb_h +. down_pdb_h;
	  !h_to_return
      )
    in

    let hd_fun node = 
      (
	let h_to_return = h_fun node in
	  h_to_return, h_to_return
      ) 
    in


      (Search_interface.make
	 ~h:h_fun
	 ~d:h_fun
	 ~hd:hd_fun
	 (*       ~t:node_type*)
	 ~domain_expand:(Min_hanoi.expand)
	 (*       ~predecessor:(make_expand ncakes cost h_fun)*)
	 ~key:Fn.identity
	 ~hash:Min_hanoi.hash
	 ~key_print:Min_hanoi.print_hanoi
	 ~equals:Min_hanoi.hash_compare
	 ~goal_p:Min_hanoi.is_goal
	 ~halt_on:lim
	 ~get_sol_length:Min_hanoi.sol_length
	 ~p_update:(fun _ _ -> ())
	 Search_interface.Hanoi
	 inst
	 (fun _ _ -> false)
	 (fun _ -> ()))



let bp_pdb_interface 
    (pdb_up:int)
    (pdb_down:int)
    cost_name 
    inst lim =
  (**
     The cost name isn't used yet, but if we ever make a non-unit cost
     version of this domain it might become important.

     inst is the instace upon which the problem is to be run.  

     lim is the limits to be applied to the solver.
  *)

  let inst = Min_hanoi.make_from_hanoi inst in

  (*first thing to do is to load the pattern databases.*)

  let pdbs = Array.make 
    2 (*one up, one down*)
    (Bp_array.make_bpa_zero 1 1) in

  let pdb_names = [|"bp_pdb";"bp_pdb"|] in
  let disk_arr = [|pdb_up;pdb_down|] in

  let pdb_files = Wrarray.map2 
    (
      fun pdb_name abstracted_disk ->
	path "hanoi" 
	  [
	    "npegs",string_of_int (Min_hanoi.n_pegs inst);
	    "ndisks",string_of_int (abstracted_disk);
	  ] pdb_name
    )
    pdb_names disk_arr in
    (*check to make sure that the pdb files actually exist*)
    for i = 0 to (Array.length pdb_files) - 1
    do
      if(Sys.file_exists pdb_files.(i)) then ()
      else
	failwith (Printf.sprintf 
		    "one of the pattern databases requested is missing\n%s"
		    pdb_files.(i)
		 );
    done;
    (*now that we know the pattern databases exist, we should load
      them into the pdb array.*)
    for i = 0 to Array.length pdbs - 1 
    do
      let inch = open_in pdb_files.(i) in
	pdbs.(i) <- (Marshal.from_channel inch : Bp_array.bpa);
	close_in inch;
    done;
    (*now that the pattern databases have been loaded, we can
      construct the heuristicis.  *)
    let h_fun node = 
      (
	(*abstract the state*)
	let up_abs_state = Min_hanoi.abstract_state_up_int pdb_up node in
	let down_abs_state = Min_hanoi.abstract_state_down_int pdb_down node in


	let up_pdb_h = Bp_array.get pdbs.(0) up_abs_state in
	let down_pdb_h = Bp_array.get pdbs.(1) down_abs_state  in

	  float_of_int (down_pdb_h + up_pdb_h)
      )

    in

    let hd_fun node = 
      (
	let h_to_return = h_fun node in
	  h_to_return, h_to_return
      ) 
    in


      (Search_interface.make
	 ~h:h_fun
	 ~d:h_fun
	 ~hd:hd_fun
	 (*       ~t:node_type*)
	 ~domain_expand:(Min_hanoi.expand)
	 (*       ~predecessor:(make_expand ncakes cost h_fun)*)
	 ~key:Fn.identity
	 ~hash:Min_hanoi.hash
	 ~key_print:Min_hanoi.print_hanoi
	 ~equals:Min_hanoi.hash_compare
	 ~goal_p:Min_hanoi.is_goal
	 ~halt_on:lim
	 ~get_sol_length:Min_hanoi.sol_length
	 ~p_update:(fun _ _ -> ())
	 Search_interface.Hanoi
	 inst
	 (fun _ _ -> false)
	 (fun _ -> ()))




let bp_pdb_stacked_interface 
    (pdb_up:int)
    (pdb_down:int)
    cost_name 
    inst lim =
  (**
     the second pattern database is now on top of the first instead of
     at the top.
     
     The cost name isn't used yet, but if we ever make a non-unit cost
     version of this domain it might become important.

     inst is the instace upon which the problem is to be run.  

     lim is the limits to be applied to the solver.
  *)

  let inst = Min_hanoi.make_from_hanoi inst in

  (*first thing to do is to load the pattern databases.*)

  let pdbs = Array.make 
    2 (*one up, one down*)
    (Bp_array.make_bpa_zero 1 1) in

  let pdb_names = [|"bp_pdb";"bp_pdb"|] in
  let disk_arr = [|pdb_up;pdb_down|] in

  let pdb_files = Wrarray.map2 
    (
      fun pdb_name abstracted_disk ->
	path "hanoi" 
	  [
	    "npegs",string_of_int (Min_hanoi.n_pegs inst);
	    "ndisks",string_of_int (abstracted_disk);
	  ] pdb_name
    )
    pdb_names disk_arr in
    (*check to make sure that the pdb files actually exist*)
    for i = 0 to (Array.length pdb_files) - 1
    do
      if(Sys.file_exists pdb_files.(i)) then ()
      else
	failwith (Printf.sprintf 
		    "one of the pattern databases requested is missing\n%s"
		    pdb_files.(i)
		 );
    done;
    (*now that we know the pattern databases exist, we should load
      them into the pdb array.*)

    for i = 0 to Array.length pdbs - 1 
    do
      let inch = open_in pdb_files.(i) in
	pdbs.(i) <- (Marshal.from_channel inch : Bp_array.bpa);
	close_in inch;
    done;

    (*now that the pattern databases have been loaded, we can
      construct the heuristicis.  *)
    let h_fun node = 
      (
	(*abstract the state*)
	let up_abs_state = Min_hanoi.abstract_state_up_int pdb_up node in
	let down_abs_state = Min_hanoi.abstract_state_exact 
	  pdb_up pdb_down node in


	let up_pdb_h = Bp_array.get pdbs.(0) (up_abs_state) in
	let down_pdb_h = Bp_array.get pdbs.(1) (down_abs_state) in

	  float_of_int (down_pdb_h + up_pdb_h)
      )

    in

    let hd_fun node = 
      (
	let h_to_return = h_fun node in
	  h_to_return, h_to_return
      ) 
    in


      (Search_interface.make
	 ~h:h_fun
	 ~d:h_fun
	 ~hd:hd_fun
	 (*       ~t:node_type*)
	 ~domain_expand:(Min_hanoi.expand)
	 (*       ~predecessor:(make_expand ncakes cost h_fun)*)
	 ~key:Fn.identity
	 ~hash:Min_hanoi.hash
	 ~key_print:Min_hanoi.print_hanoi
	 ~equals:Min_hanoi.hash_compare
	 ~goal_p:Min_hanoi.is_goal
	 ~halt_on:lim
	 ~get_sol_length:Min_hanoi.sol_length
	 ~p_update:(fun _ _ -> ())
	 Search_interface.Hanoi
	 inst
	 (fun _ _ -> false)
	 (fun _ -> ()))
