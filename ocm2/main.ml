(** The main driver for ocm2.

    @author eaburns
    @since 2010-12-02
*)

open Printf
open Verb
open Types

(** Set to true when the objects should be cleaned before building the
    result.  *)
let clean = ref false

(** Given the 'main system' fir the current directory, load all
    dependency systems. *)
let load_systems sys_path =
  let sys_dir = Filename.dirname sys_path in
  let parse_system = Parse.system (Lex.system_token) in
  let load_conf = Conf.load (Parse.config Lex.config_token) in
    load_conf (Fname.normalize "~/.ocmrc");
    load_conf (Filename.concat sys_dir ".ocmrc");
    System.load load_conf parse_system sys_path


(** Builds the command-line argument list. *)
let args () =
  let a =
    [
      [ "-j"; "--jobs" ], Arg.Set_int Object.nprocs,
      sprintf "Number of processes to use for building (default: %d)"
	!Object.nprocs;

      [ "-f"; "--force" ], Arg.Set Object.force,
      "Force rebuild of up-to-date objects";

      [ "--clean"; ], Arg.Set clean,
      "Remove previous build files before building";

      [ "-v"; "--verbose" ], Arg.Int Verb_level.set,
      sprintf "Verbosity level (default: %d)" (Verb_level.get ());
    ]
  in
    List.fold_left (fun lst (ops, act, str) ->
		      (List.map (fun o -> o, act, str) ops) @ lst)
      [] a


let parse_args () =
  let res = ref "" in
  let arg s =
    if !res <> "" then
      failwith "Expected a single result"
    else
      res := s
  in
    Arg.parse (args ()) arg "";
    !res


(** Output statics about the build. *)
let output_build_stats () =
  if !Build.Source_files.nbuilt > 0 then
    vprintf "Built %d source files in %f seconds\n"
      !Build.Source_files.nbuilt !Build.Source_files.time;
  if !Build.Dep_files.nbuilt > 0 then
    vprintf "Built %d depend files in %f seconds\n"
      !Build.Dep_files.nbuilt !Build.Dep_files.time;
  if !Build.Object_files.nbuilt > 0 then
    vprintf "Built %d object files in %f seconds\n"
      !Build.Object_files.nbuilt !Build.Object_files.time;
  if !Build.Annot_files.nbuilt > 0 then
    vprintf "Considered %d annot files in %f seconds\n"
      !Build.Annot_files.nbuilt !Build.Annot_files.time;
  ()


let main () =
  let res_name = parse_args () in
  let sys_path = System.find_main () in
  let sys = load_systems sys_path in
    if !clean then System.clean sys;
    begin match res_name, sys.sys_results with
      | "", _ when !clean ->
	  exit 0
      | "", [] ->
	  failwith "No result, and no --clean"
      | "", r :: _ ->
	  Result.build sys r.res_name;
      | res_name, _ ->
	  Result.build sys res_name;
    end;
    output_build_stats ();
    if !Object.build_failures then begin
      vprintf "Build failed\n";
      exit 1
    end else
      vprintf "Build successful\n"


let _ = main ()
