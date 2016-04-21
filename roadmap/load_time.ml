(** Loads the given binary file and outputs the time.

    @author eaburns
    @since 2010-08-18
*)


let _ =
  Verb.with_level Verb.debug
    (fun () ->
       let _, time =
	 Wrsys.with_time (fun () -> Roadmap.Binary_format.load Sys.argv.(1))
       in
       let pid = Unix.getpid () in
       let peak_str = (Wrsys.get_proc_status [|"VmPeak:"|] pid).(0) in
       let peak = (int_of_string peak_str) / 1024 in
	 Verb.pr Verb.always "Time: %f seconds\n" time;
	 Verb.pr Verb.always "Peak memory usage: %d MB\n" peak;
    )
