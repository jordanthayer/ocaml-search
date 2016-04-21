(** General functions for PDBs.

    @author eaburns
    @since 2010-01-25
*)

let path_root = User_paths.instance_root


let path domain_name domain_attrs pdb_name pdb_size =
  (** [path domain_name domain_attrs pdb_name pdb_size] gets the path
      to the pattern database. *)
  Rdb.path_for (path_root ^ domain_name)
    (("type", "pdb") :: (domain_attrs
			 @ ["pdb size", string_of_int pdb_size;
			    "pdb", pdb_name;]))


let fetch ?make domain_name domain_attrs pdb_name pdb_size =
  (** [fetch ?make domain_name domain_size pdb_name pdb_size]
      either fetches or creates the given PDB. *)
  let p = path domain_name domain_attrs pdb_name pdb_size in
    if not (Sys.file_exists p) && make <> None
    then begin
      match make with
	| None -> failwith "fetch: should not reach here"
	| Some make_fun ->
	    Verb.pr Verb.toplvl "creating pattern database\n";
	    let pdb, time = Wrsys.with_time make_fun in
	      Verb.pr Verb.toplvl "saving pattern database to %s\n" p;
	      Wrio.with_outfile p
		(fun outch -> Marshal.to_channel outch pdb []);
	      let st = Unix.stat p in
		Verb.pr Verb.toplvl "%d bytes in %f seconds \n%!"
		  st.Unix.st_size time;
		pdb
    end else begin
      Verb.pr Verb.toplvl "loading pattern database from %s\n" p;
      let inch = open_in p in
      let pdb = Marshal.from_channel inch in
	close_in inch;
	pdb
    end
