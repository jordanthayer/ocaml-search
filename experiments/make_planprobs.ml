(**

    @author jtd7
    @since 2011-11-20
   Makes problems for the planning system
*)

let translate = ref "fastdownward/src/translate/translate.py"
let preprocess = ref "fastdownward/src/preprocess/preprocess"
let translate_output = "output.sas"
let preprocess_output = "output"
let planning_root = "./group/data/planning"

let make_instance ?(bench_root = "") ~dom ~inst dest =
  (** constructs a planning instance from domain and instance files,
      moves the translated instance to ~dest. Returns time used in
      translation *)
  let base = User_paths.get_user_root() in
  let tpath = base ^ !translate in
  let ppath = base ^ !preprocess in
  let start_time = Unix.gettimeofday () in
  let domain = dom
  and instance = inst in
    Verb.pe Verb.debug "Calling %s %s %s\n" tpath domain instance;
    ignore (Wrsys.shell_output
	      (Printf.sprintf "%s %s %s" tpath domain instance));
    Verb.pe Verb.debug "Calling %s < %s\n" ppath translate_output;
    ignore (Wrsys.shell_output
	      (Printf.sprintf "%s < %s" ppath translate_output));
    Verb.pe Verb.debug "Moving %s to %s\n" preprocess_output dest;
    ignore (Wrsys.shell_output
	      (Printf.sprintf "mv %s %s" preprocess_output dest));
    (Unix.gettimeofday()) -. start_time


let record_instance ~dom_name ~dom ~inst ~inum =
  let attrs = ["domain", dom_name;] in
  let inst_attrs = attrs @ ["num", inum; "type", "instance"] in
  let time_attrs = attrs @ ["num", inum; "type", "timing"] in
  let inst_path = Rdb.path_for planning_root inst_attrs
  and time_path = Rdb.path_for planning_root time_attrs in
    Wrio.with_outfile time_path
      (fun ch ->
	 Datafile.write_header_pairs ch;
	 let time = make_instance ~dom ~inst inst_path in
	   Datafile.write_pairs ch time_attrs;
	   Datafile.write_pairs ch ["problem_path", inst_path;
				    "dom_file", dom;
				    "inst_file", inst;
				    "time", string_of_float time;];
	   Datafile.write_trailer_pairs ch)


let single_dom_file dom_name dom inst_list =
  Verb.pe Verb.toplvl "Single domain file\n%!";
  Wrlist.iteri
    (fun i inst -> record_instance ~dom_name ~dom ~inst ~inum:(string_of_int i))
    inst_list


let many_dom_files ~dom_name files =
  let rec help ind rem =
    match rem with
      | [] -> ()
      | a::b::tl -> (record_instance ~dom_name ~dom:b ~inst:a
		       ~inum:(string_of_int ind);
		     help (ind+1) tl)
      | _ -> failwith "Mismatched domains problems" in
  Verb.pe Verb.toplvl "many domain files\n";
  help 0 files


let handle_directory ~dom_name ~dir =
  let files, dirs = Wrfname.dir_contents dir in
  let files = List.sort compare files in
  let dom_reg = Str.regexp "domain.pddl" in
    match files with
      | [] -> failwith "Empty directory"
      | fst::tl -> (try
		      ignore(Str.search_forward dom_reg fst 0);
		      single_dom_file dom_name fst tl;
		    with Not_found -> many_dom_files ~dom_name files)


let move_preprocessed ?(root_dir = "./research/fd_icaps_12/results") dom =
  let pp = root_dir ^ "/preprocess/" ^ dom
  and tr = root_dir ^ "/translate/" ^ dom in
  let iroot = ".planning2/" ^ dom in
  let _,problems = Wrfname.dir_contents pp in
  let problems = List.map (Str.split (Str.regexp "/")) problems in
  let problems = List.map (fun l -> List.hd (List.rev l)) problems in
    ignore (Wrsys.shell_output (Printf.sprintf "mkdir %s" iroot));
    ignore (Wrsys.shell_output (Printf.sprintf "touch %s/KEY=name" iroot));
  List.iter (fun p ->
	       let output_path = pp ^ "/" ^ p ^ "/output" in
	       let all_path = tr ^ "/" ^ p ^ "/all.groups" in
	       let outdir = iroot ^ "/" ^ p in
		 assert(Sys.file_exists output_path);
		 assert(Sys.file_exists all_path);
		 Verb.pe Verb.always "%s\n%s\n%s\n\n%!" output_path all_path outdir;
		 ignore (Wrsys.shell_output (Printf.sprintf "mkdir %s" outdir));
		 ignore (Wrsys.shell_output (Printf.sprintf "touch %s/KEY=file" outdir));
		 ignore (Wrsys.shell_output (Printf.sprintf "cp %s %s/output"
					       output_path outdir));
		 ignore (Wrsys.shell_output (Printf.sprintf "cp %s %s/all.groups"
					       all_path outdir))) problems

