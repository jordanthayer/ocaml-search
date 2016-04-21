(** Notification System -
    Contains code for sending email and some boiler plate emails.
    Also used to do the timing for said emails eventaully *)

(* These functions are for toggling email on and off or even setting it*)
let email_on = ref true
let dont_send_mail () = email_on := false
and do_send_mail () = email_on := true
and set_mail_state s = email_on := s


(* Tools for calculating the duration of batch and meta runs. *)
let batch_timer = ref 0.
and meta_timer = ref 0.


let start_timer t = t := Unix.time()
  (** starts the timer [t] *)

and calculate_time t = Wrsys.divide_secs (Unix.time() -. !t)
  (** Returns the number of hours and seconds since [t] was started
      as a tuple of (hrs,mins).  If something took less that a minute, you
      get back 0.  Returned values are ints *)

and time_string (hrs,mins) =
  Wrutils.str "%i hrs, %i min" hrs mins

let start_batchtime() = start_timer batch_timer
and start_metatime() = start_timer meta_timer
and get_batchtime() = time_string (calculate_time batch_timer)
and get_metatime() = time_string (calculate_time meta_timer)

(******************* Boiler Plate Messages ********************************)

let email_to_user ?(user = (User_paths.get_user_safe()) ^ "@unh.edu")
    sender subject message =
  (** Sends an email message from the current user to the current user, as
      gotten by user_paths.  Sender is actually prepended to the subject line,
      subject and message are exactly what they say.  You probably don't want
      to call this directly, you probably want to use one of the boiler plate
      messages below, which all use this to send out their messages *)
  if !email_on
  then
    (try
       Wrsys.send_email user user (Wrutils.str "%s: %s" sender subject)
	 message
     with _ -> ())


let send_batch_completed_mail run_type batch_type alg =
  (** Fires off an email to the user when a domain batch is completed.
      Assumes the batch timer was started up front, otherwise it will look
      like the batch took much longer than it did *)
  email_to_user run_type
    (Wrutils.str "%s Batch Completed" batch_type)
    (Wrutils.str "%s batch with the %s algorithm completed on %s on machine %s in %s"
       batch_type alg (Wrsys.time_string()) (Wrsys.machine_id())
       (get_batchtime()))


let send_metarun_completed_mail batch_type alg =
  (** Send an email to the user when a meta run batch is completed *)
  email_to_user "MetaRun"
    (Wrutils.str "%s MetaRun Completed" batch_type)
    (Wrutils.str "%s batch with the %s algorithm completed on %s on machine %s in %s"
       batch_type alg (Wrsys.time_string()) (Wrsys.machine_id())
       (get_metatime()))


let send_failure_mail ex_string call_loc =
  (** For emailing the user whenever an exception is encountered during
      experiment runs *)
  email_to_user call_loc "Exception Raised"
    (Wrutils.str "The Experiments system saw an exception get raised while you were generating results. The error message follows\n%s\n"
       ex_string)


let try_and_email_on_fail fn call_loc =
  (** Accepts a call location and a function which takes unit as an argument.
      attempts to call fn on unit, if it succeeds, nothing happens.
      On failure, an email is sent to the user, AND the failure is raised again
      in order to stop execution *)
  try
    fn()
  with Failure str -> (send_failure_mail str call_loc;
		       failwith str)
(* EOF *)
