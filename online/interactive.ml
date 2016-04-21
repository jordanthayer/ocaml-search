(*
  interact with the simulator:
  (1) wait for goals
  (2) ask for future committments
  (3) build plan and output to the simulator

  NOTE: simplify/use message reading functions from interface.ml and
  continual.ml in MUP.
*)

(****** reading message from a lexbuf (mimic MUP) *******)

let log_msg m =
  flush_all ();
  (match m with
     Message.NewGoals _ -> Verb.pe 4 "Got new goal message.\n"
(*   | Message.Toggles _ -> Verb.pe 3 "Got Toggle message.\n"
   | Message.FutureEvents _ -> Verb.pe 3 "Got FutureEvent message.\n"
       j.Message.id); *)
     | Message.Quit -> Verb.pe 3 "Got quit message");
  flush_all ()


let read_msg lexbuf =
  try
    Parse_pddl_o.message Lex_pddl_o.lexer lexbuf
  with Parsing.Parse_error -> Wrlexing.parse_error "message" lexbuf


let spawn_message_reader lexbuf =
  (** reader is in separate thread and reads messages until receiving
    a Quit msg.  (Must be separate thread because no non-blocking reads
    under Windows!)  [spawn_message_reader] returns the Sharedq to which
    messages will be added by the reader. *)
  let q = Sharedq.create () in
  let post_messages () =
    try
      while true do
	(* read may block *)
	let m = read_msg lexbuf in
	  (* updating q may block *)
	  Sharedq.add q m;
	  match m with
	    Message.Quit -> Thread.exit ()
	  | _ -> ()
      done
    with e ->
      (* any exception in this thread should quit the other one too *)
      Sharedq.add q Message.Quit;
      raise e
  in
    ignore (Thread.create post_messages ());
    q


let process_message msg goals=
  (** process one message, returning optional Continual.msg (no
    message to return if the message concerned interface stuff like job
    creation or releasing) *)
  log_msg msg;
  match msg with
      Message.NewGoals g ->
	Verb.pe 4 "new goals:\n%s\n" (Domain.problem_str g);
	flush_all ();
	O_utils.add_goal g goals
(*    | Message.FutureEvents e ->
	failwith "Let's handle the future committment later\n"
    | Message.Toogles _ ->
	failwith  "Let's handle the toggle later\n" *)
    | Message.Quit ->
	failwith "Quit!!!!!!\n"
    


let rec get_waiting_msgs msg_q goals =
  (** returns all waiting messages *)
  (* allow reader to add to queue if possible *)
  Thread.yield ();
  if Sharedq.is_empty msg_q then ()
  else
    (process_message (Sharedq.take msg_q) goals ;
     get_waiting_msgs msg_q goals )

      
let rec block_for_msgs msg_q goals  =
  (** avoid busy waiting - block until at least one message is available *)
  (* allow reader to read if possible *)
  (* Thread.yield (); *)
  flush stdout;
  if Sharedq.is_empty msg_q then
    (Verb.pe 4 "Main thread is blocking on incoming messages.\n";
     flush_all ();
     process_message (Sharedq.take_next msg_q) goals ;
     Verb.pe 4 "Main thread unblocked.\n";
     flush_all ();
     get_waiting_msgs msg_q goals )
  else
    get_waiting_msgs msg_q goals 


let get_msgs msg_q  goals  =
  (** block for message if idle (no goal & no new message) *)
  if (O_utils.has_goal goals) then
    get_waiting_msgs msg_q goals 
  else
    block_for_msgs msg_q goals 



(**** planning: ***)

let est_plan_time = ref 0.1 (* plan-start = goal-received-time + est_plan_time *)
and max_expansion_time = ref 0.01

			   

let do_planning  =
  (** maintain "global plan", mesh in new plan for new goals. Right
    now, the global plan contains all actions even if they are obsolete
    (finished). In the future, can "clean up" the global plan to remove
    those actions *)
  let global_plan = ref []
  and multiplier = ref 1. in
    (fun domain init_prob new_prob ->
       let prob = Domain.correct_problem domain
		    (Domain.combine init_prob new_prob)
       and p_start_time = (Time.timeofday_to_reftime ())  in
	 if (Apsp.process_locs_newgoal new_prob) = infinity then
	   Verb.pe 3 "Skip this set of new goals\n"
	 else
	   (O_utils.output_goal_literals new_prob.Domain.goal;
	    Verb.pe 4  "combined problem: %s\n" (Domain.problem_str prob);
	    flush_all ();
	    let rec find_plan () =
	      let gprob = Ground.ground_problem domain prob
			    !global_plan p_start_time  in
		(* return new plan *)
		Verb.pe 4 "Grounded prob:\n%s" (Ground.gproblem_str gprob);
		flush_all ();
		let new_plan =
		  match !Args.p_alg with
		      Args.PROGRESSION ->
			Search_o.run_alg_pro gprob !global_plan
			(p_start_time +. !est_plan_time *. !multiplier)
		    | Args.POP ->
			Search_o.run_alg_pop gprob !global_plan
			  (p_start_time +. !max_expansion_time)
		in
		  match new_plan with
		      None ->
			if (!Args.p_alg = Args.PROGRESSION) &&
			  ((Time.timeofday_to_reftime ()) -. p_start_time < !Args.runtime)
			then (multiplier := 2. *. !multiplier;
			      find_plan ())
			else
			  Verb.pe 3 "No plan found within alloted time, skip this goal\n"
		    | Some plan ->
			if (!Args.p_alg = Args.PROGRESSION)
			then multiplier := 1.;
			let plan = 
			  if (!Args.p_alg = Args.PROGRESSION) && !Args.pp
			  then
			    O_utils.shift_plan_left plan !global_plan
			  else
			    plan
			in
			  global_plan := O_utils.merge_plan !global_plan plan;
			  O_utils.print_all plan
	    in
	      find_plan ()))
      

(****** main interaction loop ******)
    
let sim_interact domain problem lexbuf =
  (** keep reading goals, initial state, and exceptions from buf *)
  let goals = O_utils.init_goals () in
    (try
       Sys.catch_break true;
       let msg_q = spawn_message_reader lexbuf in
	 while true do
	   get_msgs msg_q goals ;
	   (if (O_utils.has_goal goals) then
	      do_planning domain problem (O_utils.get_goal goals)
	    else ());
	 done
     with Exit -> ());
    Verb.pe 3 "Online planner terminated at time %f!\n" (Unix.gettimeofday ())


(* EOF *)
