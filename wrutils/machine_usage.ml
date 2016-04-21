(**

   Checks to see if a machine is idle by looking for process that look
   like they might be a solver binary executing.

*)

open Printf
open Fn

(** looks for processes whose name includes the word solver *)
let is_idle () =
  let nl = Str.regexp "\n" in
  let solver_regexp = Str.regexp ".*solve.*" in
    try
      Wrsys.shell_output "ps -ef | awk '{ print $8 \" by user \" $1 }'"
      |> Str.split nl
      |> List.find (fun s -> Str.string_match solver_regexp s 0)
      |> fprintf stderr "\n%s appears to be a running solver\n%!";
      false
    with Not_found ->
      let idle = Wrsys.machine_percent_idle () > 95.0 in
	if not idle then
	  fprintf  stderr "\nMachine does not appear to be idle\n%!";
	idle
