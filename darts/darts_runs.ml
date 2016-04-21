(** Runs the darts solver with an increasing time-bound on each of the
    problems for the competition (skipping complete instances).

    @author eaburns
    @since 2010-04-08
*)

open Printf
open Fn

let data_root = User_paths.data_root ^ "darts"
let solver_binary = ref (User_paths.bin_root ^ "darts_solver")
let algorithm = ref "ilds_bottom"
let tmp_file = User_paths.data_root ^ "darts/tmp"

let insts =
  (List.map (fun r -> 3, r) (Wrlist.range 40))
  @ (List.map (fun r -> 4, r) (Wrlist.range 30))
  @ (List.map (fun r -> 5, r) (Wrlist.range 20))
  @ (List.map (fun r -> 6, r) (Wrlist.range 10))


let call_solver args attrs outch =
  (** [call_solver args attrs outch] calls the solver
      on the given instance. *)
  let go () =
    let cmd = Wrutils.str "%s %s" !solver_binary args in
      Verb.pr Verb.debug "running [%s]\n" cmd;
      Wrsys.with_subprocess_pipes cmd
	(fun to_solver from_solver ->
	   Datafile.write_header_pairs outch;
	   Datafile.write_pairs outch attrs;
	   Datafile.write_pairs outch ["attrs", (Wrstr.encode_pairs attrs)];
	   let res = Datafile.pipe_data from_solver outch in
	     Datafile.write_trailer_pairs outch;
	     res) in
    go ()
(*
    Notify.try_and_email_on_fail go "Darts_runs"
*)


let should_rerun time_limit run_file =
  (** [should_rerun run_file] test if the search completed. *)
  if (Sys.file_exists run_file) && (Datafile.seems_complete run_file)
  then begin
    let df = Datafile.load run_file in
    let complete = bool_of_string (Datafile.get_val df "completed") in
    let tl = float_of_string (Datafile.get_val df "time limit") in
    let old_cost = float_of_string (Datafile.get_val df "final sol cost") in
      (not complete && tl < time_limit), old_cost
  end else true, neg_infinity


let run_instance_if_needed time_limit inst args alg_attrs =
  (** [run_instance_if_needed time_limit inst args alg_attrs] runs the
      given instance if it needs to be re-run with this time limit. *)
  let inst_attrs = [ "num darts", string_of_int (fst inst);
		     "num regions", string_of_int (snd inst); ] in
  let attrs = alg_attrs @ inst_attrs in
  let run_file  = Rdb.path_for data_root attrs in
  let re_run, prev_cost = should_rerun time_limit run_file in
    if re_run then begin
      let args = Printf.sprintf "-d %d -r %d --time %f %s"
	(fst inst) (snd inst) time_limit args in
      let c, t = Wrio.with_outfile tmp_file (call_solver args attrs)
      in
	if c >= prev_cost then Sys.rename tmp_file run_file;
	Wrutils.pr "%d darts %d regions %.0f secs: %.1f in %.3f (%s).\n%!"
	  (fst inst) (snd inst) time_limit c t
	  (if c > prev_cost then "improved" else "not improved");
    end else Wrutils.pr "%s complete - skipping!\n%!" run_file


let rec iterative_solve ?(time=1.) args attrs =
  (** [iterative_solve ?time args attrs] solves the instances with a
      given time limit then doubles the time limit and restarts. *)
  List.iter (fun inst -> run_instance_if_needed time inst args attrs) insts;
  iterative_solve ~time:(time *. 2.) args attrs


let do_batch ?(choose=constantly1 true) time_limit args attrs =
  List.iter (fun inst ->
	       if choose inst then
		 run_instance_if_needed time_limit inst args attrs)
    insts


let do_batches () =
  let alg = "dfs" in do_batch 300. alg ["alg", alg];
    let alg = "ilds_top" in do_batch 300. alg ["alg", alg];
      let alg = "ilds_bottom" in do_batch 300. alg ["alg", alg];

	do_batch 300. "indecision_rbfs false"
	  ["alg", "indecision_rbfs"; "norm", "false"; ];
	do_batch 300. "indecision_rbfs true"
	  ["alg", "indecision_rbfs"; "norm", "true"; ];

	do_batch 300. "indecision_sum_bottom true 2000"
	  ["alg", "indecision_sum_bottom"; "norm", "true";
	   "max-bins", "2000"];
	do_batch 300. "indecision_sum_bottom false 2000"
	  ["alg", "indecision_sum_bottom"; "norm", "false";
	   "max-bins", "2000"];

	do_batch 300. "indecision_sum_top true 2000"
	  ["alg", "indecision_sum_top"; "norm", "true"; "max-bins", "2000"];
	do_batch 300. "indecision_sum_top false 2000"
	  ["alg", "indecision_sum_top"; "norm", "false"; "max-bins", "2000"]


let do_enum time =
  let inst = 3, 9 in
    run_instance_if_needed time inst
      "indecision_enum" ["alg", "indecision_enum";]


(** Trains on instances with even numbers of regions. *)
let train_windecision overwrite ?(traversal="probe") ?(time=300.)
    ?(degree=2) ?(nleaves=512) () =
  let train_attrs =
    [ "degree", string_of_int degree;
      "leaves per inst", string_of_int nleaves;
      "traversal", traversal; ]
  in
  let alg_attrs = ("alg", "train_windecision") :: train_attrs in
  let sf_attrs = ("alg", "windecision_samples") :: train_attrs in
  let attrs = sf_attrs in
  let sf = Rdb.path_for data_root attrs in
  let args =
    sprintf "train_windecision %d %d %s %s"
      degree nleaves traversal sf
  in
  let choose (d, r) = r mod 2 = 0 in
    do_batch ~choose time args alg_attrs



(** Trains on instances with even numbers of regions. *)
let train_windecision overwrite ?(traversal="probe") ?(time=300.)
    ?(degree=2) ?(nleaves=512) () =
  let train_attrs =
    [ "degree", string_of_int degree;
      "leaves per inst", string_of_int nleaves;
      "traversal", traversal; ]
  in
  let alg_attrs = ("alg", "train_windecision") :: train_attrs in
  let sf_attrs = ("alg", "windecision_samples") :: train_attrs in
  let attrs = sf_attrs in
  let sf = Rdb.path_for data_root attrs in
  let args =
    sprintf "train_windecision %d %d %s %s"
      degree nleaves traversal sf
  in
  let choose (d, r) = r mod 2 = 0 in
    do_batch ~choose time args alg_attrs


let windecision_rbfs ?(traversal="probe") ?(degree=2) ?(nleaves=512)
    ?(time=300.) () =
  let sample_attrs =
    [ "alg", "windecision_samples";
      "degree", string_of_int degree;
      "leaves per inst", string_of_int nleaves;
      "traversal", traversal;
    ] in
  let sf = Rdb.path_for data_root sample_attrs in
  let coeffs = Offline_windecision.learn_coeffs sf in
  let coeff_string = Offline_windecision.string_of_coeffs coeffs in
  let args = sprintf "windecision_rbfs \"%s\"" coeff_string in
  let alg_attrs = [ "alg", "windecision_rbfs";
		    "coeffs", coeff_string;
		    "degree", string_of_int degree;
		    "leaves per inst", string_of_int nleaves;
		    "traversal", traversal; ] in
    Verb.pr Verb.debug "args: [%s]\n" args;
    do_batch time args alg_attrs


let windecision_sum_bottom ?(traversal="probe") ?(degree=2) ?(nleaves=512)
    ?(time=300.) bins =
  let sample_attrs =
    [ "alg", "windecision_samples";
      "degree", string_of_int degree;
      "leaves per inst", string_of_int nleaves;
      "traversal", traversal;
    ] in
  let sf = Rdb.path_for data_root sample_attrs in
  let coeffs = Offline_windecision.learn_coeffs sf in
  let coeff_string = Offline_windecision.string_of_coeffs coeffs in
  let args = sprintf "windecision_sum_bottom \"%s\" %d" coeff_string bins in
  let alg_attrs = [ "alg", "windecision_sum_bottom";
		    "coeffs", coeff_string;
		    "degree", string_of_int degree;
		    "leaves per inst", string_of_int nleaves;
		    "traversal", traversal;
		    "max-bins", string_of_int bins; ] in
    Verb.pr Verb.debug "args: [%s]\n" args;
    do_batch time args alg_attrs


let windecision_sum_top ?(traversal="probe") ?(degree=2) ?(nleaves=512)
    ?(time=300.) bins =
  let sample_attrs =
    [ "alg", "windecision_samples";
      "degree", string_of_int degree;
      "leaves per inst", string_of_int nleaves;
      "traversal", traversal;
    ] in
  let sf = Rdb.path_for data_root sample_attrs in
  let coeffs = Offline_windecision.learn_coeffs sf in
  let coeff_string = Offline_windecision.string_of_coeffs coeffs in
  let args = sprintf "windecision_sum_top \"%s\" %d" coeff_string bins in
  let alg_attrs = [ "alg", "windecision_sum_top";
		    "coeffs", coeff_string;
		    "degree", string_of_int degree;
		    "leaves per inst", string_of_int nleaves;
		    "traversal", traversal;
		    "max-bins", string_of_int bins; ] in
    Verb.pr Verb.debug "args: [%s]\n" args;
    do_batch time args alg_attrs
