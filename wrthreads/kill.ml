1) The best way to circumvent the lack of Thread.kill ?
Archive: <http://thread.gmane.org/gmane.comp.lang.caml.general/31022>

  For Unix (including MacOS X) you can implement send_exception yourself
by sending a signal to the thread, and defining a signal handler that
just raises an exception.

In Windows there are no signals. An extension of the O'Caml runtime
would be needed to get this behaviour.

 > What is the best solution ? start a new process and use the kill  
at the
 > operating system level ?

Which is unavailable in Windows.

 > (to make things even worse I need something which works on linux,
 > windows and macosx)

There is a hack that works (Xavier forgive):

exception User_interrupt

let do_something() =
   ignore(7 * 6)
;;

let compute() =
   try
     while true do
       do_something()
     done;
     assert false
   with
     | User_interrupt ->
	prerr_endline "Thread interrupted!"
;;

let vt_signal =
   match Sys.os_type with
     | "Win32" -> Sys.sigterm
     | _ -> Sys.sigvtalrm
;;

let interrupt = ref None;;

let force_interrupt old_action_ref n =
   (* This function is called just before the thread's timeslice ends *)
   if Some(Thread.id(Thread.self())) = !interrupt then
     raise User_interrupt;
   match !old_action_ref with
     | Sys.Signal_handle f -> f n
     | _ -> failwith "Not in threaded mode"
;;

let main() =
   (* Install the signal handler: *)
   let old_action_ref = ref Sys.Signal_ignore in
   let old_action =
     Sys.signal vt_signal (Sys.Signal_handle (force_interrupt  
old_action_ref)) in
   old_action_ref := old_action;
   (* Fire up the compute thread: *)
   let t = Thread.create compute () in
   (* Wait for user: *)
   print_string "Press Return: ";
   flush stdout;
   let _ = read_line() in
   interrupt := Some (Thread.id t);
   (* Wait until the thread terminates: *)
   Thread.join t
;;

main();;

** Gerd Stolpmann added:

 > let do_something() =
 >   ignore(7 * 6)
 > ;;

Just an addition: This function is too primitive for ocamlopt (it won't
check for pending events). Use

let rec do_something() =
   ignore(7 * 6); flush stdout
;;
