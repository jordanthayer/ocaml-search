let instance_root name =
  match name with
      "tiles" -> Filename.concat Experiments.instance_root "tiles_instances"
    | "santa" -> Filename.concat Experiments.instance_root "santa"
    | _ -> failwith (Printf.sprintf "invalid instance root selection: %s" name)

let solver_binary ss =
  (Printf.sprintf 
     "java -Xmx7000m -Xms7000m -Xss%dm -XX:+UseConcMarkSweepGC -jar " ss)
  ^ (Filename.concat Experiments.bin_root "search.jar ")

type alg_data = {
  alg_name : string;
  stack_size: int;
  alg_attrs: (string * string) list;
  alg_args: string;
}

type problem_data = {
  problem_irtn: string;
  problem_name: string;
  problem_path: string;
  problem_attrs: (string * string) list;
  problem_args: string;
}




let get_all_attrs pd ad = 
  let attrs = (ad.alg_attrs) @ (pd.problem_attrs) in
  let attrs = ("lang","java_hotspot") :: attrs in
    attrs

let call_solver pd ad limits outch = 
  let limit_args = Limit.to_string limits in

  let attrs = get_all_attrs pd ad in


  let cmd = Printf.sprintf "%s %s --probargs %s --type %s --problem %s --alg %s --rest %s" 
    (solver_binary ad.stack_size) limit_args pd.problem_args pd.problem_name
    pd.problem_path 
    ad.alg_name
    ad.alg_args
  in
    Verb.pe Verb.debug "calling::\n%s\n" cmd;
    let run cmd () =
      Verb.pe Verb.debug "calling [%s]\n" cmd;
      Wrsys.with_subprocess_all_pipes cmd
	(fun to_solver from_solver err_solver ->
	   let buf = Buffer.create 16 in
	     (try
		while true do
		  Buffer.add_channel buf err_solver 1
		done
	      with
		  End_of_file -> ()
	     );
	     let errors = (Buffer.contents buf) in
	       if((String.length errors) = 0) then ()
	       else
		 Verb.pe Verb.always "\nErrors: ***start here***\n%s\n***end here***\n" (Buffer.contents buf);

	       Datafile.write_header_pairs outch;
	       Datafile.write_pairs outch attrs;
	       Datafile.write_pairs outch [ "attrs", (Wrstr.encode_pairs attrs) ];
	       let res = Datafile.pipe_data from_solver outch in
		 Datafile.write_trailer_pairs outch;
		 res);
    in
      Notify.try_and_email_on_fail (run cmd) "Sliding_tiles_runs"

let do_run ?(lazy_run= false)
    overwrite
    limits (pd:problem_data) (ad:alg_data) =

  let attrs = get_all_attrs pd ad in

  let data_root =
    Filename.concat Experiments.data_root pd.problem_name in 
  let res_file = Rdb.path_for data_root attrs in
    if Sys.file_exists res_file
      && Datafile.seems_complete res_file
      && not (Limit.has_new_limits res_file limits)
      && not overwrite
      && not lazy_run
    then Printf.printf "%s exists -- skipping!\n%!" res_file
    else if
      lazy_run
      && Sys.file_exists res_file
      && Datafile.seems_complete res_file
      && (let df = Datafile.load res_file in
	  let found_solution = Datafile.get_val df "found solution" in
	    found_solution = "yes"
	 )
    then Printf.printf "%s exists -- skipping (lazy mode)!\n%!" res_file
    else begin
      Wrio.with_outfile res_file
	(fun outch ->
	   let rel_path = Wrfname.make_relative 
	     (instance_root pd.problem_irtn) pd.problem_path in
	     Printf.printf "Running %s on %s...%!" pd.problem_args rel_path;
	     let c, t = call_solver pd ad limits outch in
	       Printf.printf "%.1f in %.3f.\n%!" c t)
    end


 
let assemble_basic_batch alg_name = 
  [{
    alg_name=alg_name;
    stack_size = 1;
    alg_attrs = ["alg",alg_name;
		];
    alg_args = "";
  }]

let assemble_weighted_batch alg_name weights = 
  List.map (
    fun wt -> 
      {
	alg_name = alg_name;
	stack_size = 1;
	alg_attrs = [
	  "alg",alg_name;
	  "wt",string_of_float wt;
	];
	alg_args = string_of_float wt;
      }
  ) weights

let assemble_tile_data 
    ?(cost="unit")
    ?(heuristic="manhattan")
    ?(macro=false) 
    ?(model="korf")
    ?(rows=4)
    ?(cols=4) () 
    = 
  let batch_attrs = [
    "model",model;
    "rows",string_of_int rows;
    "cols",string_of_int cols;
  ] in
  let attr_sets = Rdb.matching_attrs (instance_root "tiles") batch_attrs in
    List.map (fun attrs ->
		{ problem_irtn = "tiles";
		  problem_name = "tiles";
		  
		   problem_path = Rdb.path_for (instance_root "tiles" ) attrs;
		   problem_attrs = attrs;
		   problem_args = "";
		}) attr_sets

let assemble_santa_naive_data 
    typ disc=
  let batch_attrs = ["type", typ] in
  let attr_sets = Rdb.matching_attrs (instance_root "santa") batch_attrs in
    List.map (fun attrs ->
		{  problem_name = "santa_naive";
		   problem_irtn = "santa";
		   problem_path = Rdb.path_for (instance_root "santa" ) attrs;
		   problem_attrs = attrs;
		   problem_args = string_of_float disc;
		}) attr_sets


let run_prep ?(lazy_run = false)?(overwrite = false) 
    (pdl:problem_data list) 
    (adl: alg_data list) 
    limits = 
  List.iter (
    fun pd -> (
      List.iter (
	fun ad -> 
	  do_run ~lazy_run:lazy_run overwrite limits pd ad;
      ) adl
    )
  ) pdl


(*



Java_runs2.run_prep 
(Java_runs2.assemble_santa_naive_data "joe9" 45.) 
(Java_runs2.assemble_weighted_batch "wastar" [1.0; 2.0]) 
[Limit.Time 1.0];;


Java_runs2.run_prep 
(Java_runs2.assemble_tile_data ()) 
(Java_runs2.assemble_weighted_batch "wastar" [1.0; 2.0]) 
[Limit.Time 1.0];;


*)
