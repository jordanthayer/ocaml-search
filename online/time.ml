(*********** getting `current' timeofday *****)

let curr_time_func =
  (** no offset, just returns the local timeofday *)
  ref Unix.gettimeofday


let curr_time () =
  !curr_time_func ()


(*
let using_simulated_time, set_sim_time =
  let sim_time = ref curr_time in
    (fun () -> curr_time := (fun () -> !sim_time)),
    (fun t -> sim_time := t)
*)

(****** maintain time ****)

let timeofday_to_reftime, reftime_to_timeofday, set_basetime, get_basetime =
  let base_time = ref (Unix.gettimeofday ()) in
    (fun () ->
       (Unix.gettimeofday ()) -. !base_time),
    (fun reftime ->
       reftime +. !base_time),
    (fun t ->
       base_time := t),
    (fun () ->
       !base_time)


let start_timer, elapsed_time =
  (** time keeping *)
  let t = ref 0. in
    (fun () -> t := Unix.gettimeofday ()),
    (fun () -> (Unix.gettimeofday ()) -. !t)


(** copied from MUP's time.ml *)

(*********** converting to/from ints *********)


let (set_quantum,
     timeofday_to_int,
     int_to_timeofday,
     dur_to_int,
     int_to_dur) =
  let quantum = ref 0.01
  and quantum_set = ref false
  and base_time = curr_time () in
    (fun q ->
       if !quantum_set then
	 failwith "can not reset the quantum from the default value now"
       else
	 (quantum := q;
	  quantum_set := true)),
    (fun t ->
       let new_t = (t -. base_time) /. !quantum in
	 if (new_t < (float min_int)) or (new_t > (float max_int)) then
	   failwith "Time.timeofday_to_int: value out of [min_int,max_int]"
	 else
	   Math.round new_t),
    (fun i ->
       ((float i) *. !quantum) +. base_time),
    (fun t ->
       	 if t = neg_infinity then
	   (Wrutils.pe "Time.dur_to_int: convert neg_infinity to min_int\n";
	    min_int)
	 else if t = infinity then
	   (Wrutils.pe "Time.dur_to_int: convert infinity to max_int\n";
	    max_int)
	 else
	   let new_t = t /. !quantum in
	     if (new_t < (float min_int)) or (new_t > (float max_int)) then
	       failwith "Time.dur_to_int: value out of [min_int,max_int]"
	     else
	       Math.round new_t),
    (fun i ->
       if i = max_int then
	 infinity
       else if i = min_int then
	 neg_infinity
       else
	 (float i) *. !quantum)


let curr_time_as_int () =
  timeofday_to_int (curr_time ())


(* EOF *)
