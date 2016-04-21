(* utilities for dealing with stuff from the threads library

   don't forget to compile *everything* with -thread and include the
   threads library when linking.
*)


let self_id () =
  Thread.id (Thread.self ())


let with_mutex m f =
  (** returns the result of [f] *)
  Mutex.lock m;
  Wrutils.unwind_protect f () Mutex.unlock m


(********** Unix with threads ************)


let handle_unix_error f arg =
  (** like Unix.handle_unix_error but only exits thread, not process *)
  try
    f arg
  with Unix.Unix_error(err, fun_name, arg) ->
    Wrutils.pe "%s, thread %d: %s failed%s: %s\n"
    Sys.argv.(0)
    (self_id ())
    fun_name
    (if String.length arg > 0 then Wrutils.str " on %s" arg else "")
    (Unix.error_message err);
    exit 2


(********** networking with threads ************)


let accept_and_spawn f sock =
  let (inchan, outchan, caller) = Wrsys.accept sock in
    Thread.create (fun () ->
		     f inchan outchan caller;
		     close_out outchan
		     (* don't close filedescriptor twice
			close_in inchan *)
		  )
      ()


let spawn_server f port =
  (** starts a thread that calls [f] in a new thread for every
    connection to [port].  returns the Thread.t of the spawner
    thread. like Unix.establish_server but doesn't fork *)
  let sock = Wrsys.listen port in
    Thread.create
      (fun () ->
	 while true do
	   let (inchan, outchan, caller) = Wrsys.accept sock in
	     ignore (Thread.create
		       (fun () ->
			  f inchan outchan caller;
			  close_out outchan;
			  close_in inchan)
		       ())
	 done)
      ()


(* EOF *)
