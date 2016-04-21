(* $Id: wrsys.ml,v 1.2 2003/12/12 02:17:48 ruml Exp ruml $

   system calls.  should work on both Unix and Windows.

   some system-dependent stuff might be in wrfilename.ml?
*)

(******* time and date **********)


let walltime () =
  Unix.gettimeofday ()


let with_time f =
  let start = Sys.time () in
  let result = f () in
  let finish = Sys.time () in
    result, (finish -. start)


let divide_secs s =
  (** given seconds as a float, returns hours and minutes as integers *)
  let mins = truncate (s /. 60.) in
  let hrs = mins / 60 in
  let mins = mins - (hrs * 60) in
    hrs,mins


let get_tm () =
  Unix.localtime (Unix.gettimeofday ())


let string_of_tm tm =
  (* "Thu Jul 31 10:57:04 PDT 2003" *)
  let days = [|"Sun"; "Mon"; "Tue"; "Wed"; "Thu"; "Fri"; "Sat"|]
  and months = [|"Jan"; "Feb"; "Mar"; "Apr"; "May"; "Jun";
		 "Jul"; "Aug"; "Sep"; "Oct"; "Nov"; "Dec"|] in
    Wrutils.str "%s %s %d %d:%02d:%02d %d"
      days.(tm.Unix.tm_wday)
      months.(tm.Unix.tm_mon)
      tm.Unix.tm_mday
      tm.Unix.tm_hour tm.Unix.tm_min tm.Unix.tm_sec
      (tm.Unix.tm_year + 1900)


let time_string () =
  (** returns the current time in a string format reminiscent of the
    Unix date command, but without a timezone *)
  string_of_tm (get_tm ())


(* moved, as it is now used in machineID *)
let shell_output cmd =
  (** returns everything printed to stdout as a single string.  from a
    posting by Issac Trotts. *)
  let buf = Buffer.create 16 in
  let inch = Unix.open_process_in cmd in
    Wrutils.unwind_protect
      (fun () ->
	 try
	   while true do
	     Buffer.add_channel buf inch 1
	   done
	 with End_of_file -> ())
      ()
      Unix.close_process_in
      inch;
    Buffer.contents buf

(******* machine and OS info ********)

(* clone exists in wrfname *)
let windows_p = match Sys.os_type with
    "Win32" | "Cygwin" -> true
  | _ -> false


let safe_getenv k =
  try Sys.getenv k with Not_found -> ""


let system_id () =
  (** what kind of system are we running on? *)
  let id = Wrutils.str "%s-%s-%s-%s"
	     (safe_getenv "HOSTTYPE")
	     (safe_getenv "MACHTYPE")
	     (safe_getenv "OSTYPE")
	     (safe_getenv "VENDOR")
  in
    if id = "---"
    then
      match Sys.os_type with
	"Win32" | "Cygwin" -> "windows"
      | "Unix" -> failwith "Unix shell environment is impoverished"
      | "MacOS" -> "macos-9"
      | _ -> failwith "don't recognize this machine type"
    else
      id


let machine_id () =
  (** which machine and set-up are we running on? *)
  let remNL s = String.sub s 0 ((String.length s) - 1) in
  let hostname = remNL (shell_output "uname -n")
  and hosttype = remNL (shell_output "uname -m")
  and ostype =  remNL (shell_output "uname -o") in
    Wrutils.str "%s-%s-%s-%s-%s-%s-%d-%s"
      hostname
      hosttype
      (safe_getenv "MACHTYPE")
      ostype
      (safe_getenv "VENDOR")
      Sys.os_type
      Sys.word_size
      Sys.ocaml_version


let with_cwd dir f =
  (** evaluates [f] on unit with currend directory set to [dir] *)
  let start = Sys.getcwd () in
    Sys.chdir dir;
    let res = f () in
      Sys.chdir start;
      res


let curr_umask () =
  match Sys.os_type with
    "Win32" -> 0o666
  | _ ->
      let curr = Unix.umask 0o666 in
      let mine = Unix.umask curr in
	assert (mine = 0o666);
	curr


(********  files *********)


let remove_if_exists filename =
  if Sys.file_exists filename
  then Sys.remove filename


let rename_clobbering src dest =
  (** ensures that [dest] is clobbered if it exists.  (this is not the
    default behavior under Windows) *)
  remove_if_exists dest;
  Sys.rename src dest


(******************** Process information ********************)

let get_proc_status keys pid =
  (** [get_proc_status keys pid] gets the values associated with
      the given keys in the /proc/<pid>/status file.  The
      resulting values are strings.  This works simply by
      reading each token from the file one at-a-time.  If a
      token matches one of the keys then the next token is set
      as the value of the key. *)
  let n_keys = Array.length keys in
  let values = Array.create n_keys "" in
  let status_file = "/proc/" ^ (string_of_int pid) ^ "/status" in
  let n_seen = ref 0 in
  let inch = open_in status_file in
  let key_inds = Hashtbl.create (n_keys * 3) in
    Array.iteri (fun i k -> Hashtbl.add key_inds k i) keys;
    begin try
      while !n_seen < n_keys do
	let t = Wrio.read_token inch in
	  if t = "" then raise End_of_file;
	  try
	    let i = Hashtbl.find key_inds t in
	      incr n_seen;
	      values.(i) <- Wrio.read_token inch;
	  with Not_found -> ()
      done;
    with End_of_file -> ()
    end;
    close_in inch;
    values


      let mem_usage pid =
	(** [mem_usage pid] gets the memory usage of a process.  The result
	    is (heap * stack * size) usage in kilobytes. *)
  let s = get_proc_status [| "VmData:"; "VmStk:"; "VmSize:"; |] pid in
    (int_of_string s.(0)), (int_of_string s.(1)), (int_of_string s.(2))

(************ running shell commands ***************)


  let date_string () =
    (** returns the output of running the 'date' command *)
  if windows_p then
    failwith "Wrsys.date_string is for Unix only!"
  else
    Wrstr.chop (shell_output "date")


let shell_lines cmd =
  let inch = Unix.open_process_in cmd in
  Wrutils.unwind_protect
    (fun () -> Wrio.input_lines inch) ()
    (fun () -> Unix.close_process_in inch) ()


let shell_iter f cmd =
  (** from a posting by Issac Trotts.  Eg, sys_iter print_endline "ls" *)
  let inch = Unix.open_process_in cmd in
    (try
       while true do
	 f (input_line inch)
       done
     with _ -> ());
    Unix.close_process_in inch;;


let open_process shell_cmd =
  (** [open_process shell_cmd] opens a subprocess returning the pin,
      pout, perr, pid.  Apparently ocaml's Unix.open_process doesn't
      return a PID, so there is no way to kill the subproces if it is
      desired. *)
  let shell = "/bin/sh" in
    (* according to the documentation of Unix.open_process, the
       process is executed with /bin/sh. *)

  let pin_read, pin_write = Unix.pipe ()
  and pout_read, pout_write = Unix.pipe ()
  and perr_read, perr_write = Unix.pipe () in
  let pid = Unix.fork () in
    if pid = 0
    then begin 				(* Child *)

      (* unblock any signals that the parent may have blocked. *)
      let blocked_sigs = Unix.sigprocmask Unix.SIG_UNBLOCK [] in
	ignore (Unix.sigprocmask Unix.SIG_UNBLOCK blocked_sigs);

	(* setup stdin, stdout and stderr *)
	Unix.close pin_write;
	Unix.close pout_read;
	Unix.close perr_read;
	Unix.dup2 pin_read Unix.stdin;
	Unix.dup2 pout_write Unix.stdout;
	Unix.dup2 perr_write Unix.stderr;

	(* exec with shell. *)
	Unix.execvp shell [| shell; "-c"; shell_cmd |];
    end else begin			(* Parent *)
      Unix.close pin_read;
      Unix.close pout_write;
      Unix.close perr_write;
      (Unix.out_channel_of_descr pin_write,
       Unix.in_channel_of_descr pout_read,
       Unix.in_channel_of_descr perr_read,
       pid)
    end


let close_process pin pout perr pid =
  flush pin;
  ignore (match Unix.waitpid [] pid with
	    | pid, Unix.WEXITED 0 -> ()
	    | _ -> failwith "messy return from subprocess!");
  close_out pin;
  close_in pout;
  close_in perr


let open_subprocess shell_cmd =
  (** returns input (an out_channel for us), output, and error of new
      subprocess.  Use close_subprocess when done. *)
  let pout, pin, perr = Unix.open_process_full
    shell_cmd (Unix.environment ()) in
    pin,pout,perr


let close_subprocess (pin, pout, perr) =
  (** waits for subprocess to end, raises Failure if process doesn't
    return 0.  Doesn't read process output. *)
  flush pin;
  match Unix.close_process_full (pout, pin, perr) with
    Unix.WEXITED 0 -> ()
  | _ -> failwith "messy return from subprocess!"


let with_subprocess_all_pipes ?sig_on_exception shell_cmd f =
  (** Calls [f] with stdin and stout of [shell_cmd], returning its
      result.  If [kill_on_execption] is not None and an exception is
      caught (by a ^C, for example at the toplvl) then the subprocess
      is sent the desired signal. *)
  let to_sub, from_sub, perr, pid = open_process shell_cmd in
    try
      let res = f to_sub from_sub perr in
	close_process to_sub from_sub perr pid;
	res
    with e -> begin
      match sig_on_exception with
	| None ->
	    close_out to_sub;
	    close_in from_sub;
	    close_in perr;
	    raise e
	| Some s ->
	    Verb.pr Verb.always "Got an exception killing child\n%!";
	    Unix.kill pid s;
	    close_out to_sub;
	    close_in from_sub;
	    close_in perr;
	    raise e
    end



let with_subprocess_pipes ?sig_on_exception shell_cmd f =
  (** Calls [f] with stdin and stout of [shell_cmd], returning its
      result.  If [kill_on_execption] is not None and an exception is
      caught (by a ^C, for example at the toplvl) then the subprocess
      is sent the desired signal. *)
  let to_sub, from_sub, perr, pid = open_process shell_cmd in
    try
      let res = f to_sub from_sub in
	close_process to_sub from_sub perr pid;
	res
    with e -> begin
      match sig_on_exception with
	| None ->
	    close_out to_sub;
	    close_in from_sub;
	    close_in perr;
	    raise e
	| Some s ->
	    Verb.pr Verb.always "Got an exception killing child\n%!";
	    Unix.kill pid s;
	    close_out to_sub;
	    close_in from_sub;
	    close_in perr;
	    raise e
    end


(***************** sending email *****************)


let send_email_dep sender_address recipient_address subject message =
  (* -f is necessary from katsura, otherwise sender is
     "ruml@katsura...".  I can't figure out how to make -F work with
     -f.  The -t flag might also be relevant if we want to support CC,
     BCC, and multiple recipients *)
  let ch = Unix.open_process_out (Wrutils.str "/usr/bin/sendEmail -f '%s' -t %s"
				    sender_address recipient_address) in
    Wrutils.pf ch "To: %s\n" recipient_address;
    Wrutils.pf ch "Subject: %s\n" subject;
    Wrutils.pf ch "%s\n." message;
    match Unix.close_process_out ch with
	Unix.WEXITED 0 -> ()
      | Unix.WEXITED 67 -> failwith "sendmail: user name not recognized"
      | Unix.WEXITED 75 -> failwith "sendmail: temporary failure, msg queued"
      | Unix.WEXITED x ->
	  failwith (Wrutils.str "sendmail: exit code %d (see <sysexits.h>)" x)
      | _ -> failwith "sendmail didn't terminate normally"


let send_email sender_address recipient_address subject message =
 (* requires the sendemail package *)
  let ch = Unix.open_process_out (Wrutils.str
                                   "/usr/bin/sendEmail -q -s 'mailhost.cs.unh.edu' -f '%s' -t '%s' -u '%s'"
                                   sender_address
                                   recipient_address
                                   subject) in
   Wrutils.pf ch "%s" message;
   match Unix.close_process_out ch with
       Unix.WEXITED 0 -> ()
     | Unix.WEXITED x ->
         failwith (Wrutils.str "sendEmail: exit code %d (see <sysexits.h>?)" x)
     | _ -> failwith "sendEmail didn't terminate normally"


(***************** network stuff *****************)


let sock_addr hostname port =
  (** returns a Unix.sockaddr *)
  let host = Unix.gethostbyname hostname in
  let addrs = host.Unix.h_addr_list in
    if (Array.length addrs) < 1 then
      failwith (hostname ^ " doesn't have any internet addresses!")
    else
      Unix.ADDR_INET (addrs.(0), port)


let sock_addr_of_string s =
  (** takes a name:port string, returns a Unix.sockaddr.  Internet
    addresses only. *)
  let i = String.rindex s ':' in
  let name = String.sub s 0 i
  and port = int_of_string (Wrstr.peel ~n:(i + 1) s) in
    sock_addr name port


let string_of_sockaddr a =
  match a with
    Unix.ADDR_UNIX fname -> "local:" ^ fname
  | Unix.ADDR_INET (ia, port) ->
      Wrutils.str "%s:%d" (try
			   (Unix.gethostbyaddr ia).Unix.h_name
			 with Not_found -> "<unknown host>") port


let listen ?(max_pending_requests = 1) port =
  (** returns a socket listening for TCP connections on the given
    local [port] *)
  (* based on code in establish_server in otherlibs/unix/unix.ml *)
  let sock = Unix.socket Unix.PF_INET Unix.SOCK_STREAM 0 in
    Unix.setsockopt sock Unix.SO_REUSEADDR true;
    Unix.bind sock (sock_addr (Unix.gethostname ()) port);
    Unix.listen sock max_pending_requests;
    sock


let accept sock =
  (** given a listening [socket], wait for a connection and then
    return in_ and out_channels and the sock_addr of the caller *)
  let (s, caller) = Unix.accept sock in
  let inch = Unix.in_channel_of_descr s in
  let outch = Unix.out_channel_of_descr s in
    inch, outch, caller


let fd_waiting fd =
  (** returns true iff there is input waiting to be read from the
    given file descriptor.  On Windows, works only for sockets *)
  (* one microsecond (0.000001) is minimum blocking wait under Unix
     and Windows. 0.0 means return immediately (nonblocking), negative
     means block forever *)
  let wait = 0.0 in
    match Unix.select [fd] [] [] wait with
	[],[],[] -> false
      | [x],[],[] when x = fd -> true
      | _ -> failwith "strange return value from Unix.select in waiting_input"


let input_waiting inch =
  (** returns true iff there is input waiting to be read from the
    given input channel.  On Windows, works only for sockets *)
  fd_waiting (Unix.descr_of_in_channel inch)

(*
let input_block_waiting inch =
  (** returns true iff there is input waiting to be read from the
    given file descriptor.  On Windows, works only for sockets *)
  (* one microsecond (0.000001) is minimum blocking wait under Unix
     and Windows. 0.0 means return immediately (nonblocking), negative
     means block forever *)
  let wait = - 0.001
  and fd = Unix.descr_of_in_channel inch in
    match Unix.select [fd] [] [] wait with
	[],[],[] -> false (* consider remove this line later *)
      | [x],[],[] when x = fd -> true
      | _ -> failwith "strange return value from Unix.select in waiting_input"
*)

(** see Udp for communication using UDP *)

(** see Wrthreads for accept_and_spawn and spawn_server *)

(******************** system status ********************)

let read_sys_usage () =
  (** [read_sys_usage ()] reads the current ticks for time spent in
      user mode, user mode with nice, system mode and the idle
      process. *)
  Wrio.with_infile "/proc/stat"
    (fun inch ->
       let c = input_char inch in
       let p = input_char inch in
       let u = input_char inch in
       let space = input_char inch in
	 assert (c = 'c' && p = 'p' && u = 'u' && space = ' ');
	 let user = Wrio.input_int inch in
	 let nice = Wrio.input_int inch in
	 let sys = Wrio.input_int inch in
	 let idle = Wrio.input_int inch in
	   user, nice, sys, idle)


let machine_percent_idle () =
  (** [machine_percent_idle ()] approximates the percent of time that
      the machine is idle. *)
  let user, nice, sys, idle = read_sys_usage () in
    Unix.sleep 1;
    let user', nice', sys', idle' = read_sys_usage () in
    let idle_cyc = idle' - idle in
    let cycles = (user' - user) + (nice' - nice) + (sys' - sys) + idle_cyc in
      (float idle_cyc) /. (float cycles) *. 100.



let load_avg () =
  Wrio.with_infile "/proc/loadavg"
    (fun inch ->
       Wrio.input_float inch, Wrio.input_float inch, Wrio.input_float inch)



let current_users () =
  (** [current_users ()] gets the list of users currently logged into
      the system.

      NOTE: this farms the work out to the 'who' process because OCaml
      doesn't seem to have a way to read utmp. *)
  let first_word s =
    (* gets the first word from a tab-delimited string. *)
    let rec get_chars b s i =
      if i < (String.length s) then begin
	match s.[i] with
	  | ' ' | '\t' | '\n' -> b
	  | c ->
	      Buffer.add_char b c;
	      get_chars b s (i + 1)
      end else b
    in
      Buffer.contents (get_chars (Buffer.create 10) s 0)
  in
  let module Strings = Set.Make(String) in
  let users =
    List.fold_left
      (fun ss s -> let user = first_word s in Strings.add user ss)
      Strings.empty
      (shell_lines "who")
  in
    Strings.fold (fun user lst -> user :: lst) users []


(* EOF *)
